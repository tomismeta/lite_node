(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type phase =
  | Open
  | Advanced
  | Finalized
  | Canceled

type t = {
  target_root : string;
  request_root : string;
  sequence : int;
  phase : phase;
  output_root : string;
  committed_effort : int;
}

type error =
  | Bad_sequence of int * int
  | Terminal_session
  | Target_root_mismatch of string * string
  | Request_root_mismatch of string * string
  | Pin_root_mismatch of string * string
  | Entrypoint_unsupported of string
  | Entrypoint_missing of int
  | Execution_failed
  | Effort_exceeded of int * int
  | Effort_overflow of int * int

let phase_name = function
  | Open -> "open"
  | Advanced -> "advanced"
  | Finalized -> "finalized"
  | Canceled -> "canceled"

let to_json session =
  `Assoc [
    "target_root", `String session.target_root;
    "request_root", `String session.request_root;
    "sequence", `Int session.sequence;
    "phase", `String (phase_name session.phase);
    "output_root", `String session.output_root;
    "committed_effort", `Int session.committed_effort;
  ]

let root session =
  let payload = Yojson.Safe.to_string (to_json session) in
  Digestif.SHA256.(
    digest_string ("octra:inference:session\000" ^ payload) |> to_hex)

let output_root ~request_root ~sequence ~committed_effort =
  Digestif.SHA256.(
    digest_string
      (Printf.sprintf
         "octra:inference:output\000%s:%d:%d"
         request_root
         sequence
         committed_effort)
    |> to_hex)

let initial_output_root request_root =
  output_root ~request_root ~sequence:0 ~committed_effort:0

let check_sequence expected session =
  if session.sequence = expected then Ok ()
  else Error (Bad_sequence (expected, session.sequence))

let terminal = function
  | Finalized
  | Canceled -> true
  | Open
  | Advanced -> false

let check_identity ~target ~request session =
  let target_root = Inference_target.root target in
  let request_root = Inference_request.root request in
  if not (String.equal session.target_root target_root) then
    Error (Target_root_mismatch (session.target_root, target_root))
  else if not (String.equal session.request_root request_root) then
    Error (Request_root_mismatch (session.request_root, request_root))
  else
    Ok ()

let entrypoint_pc code label =
  let rec loop pc =
    if pc = Array.length code then None
    else
      match code.(pc) with
      | Contract_vm.JDEST value when value = label -> Some pc
      | _ -> loop (pc + 1)
  in
  loop 0

let run_entrypoint ~limit code pc =
  let fixed = Contract.fix_jumps code in
  let storage = Hashtbl.create 16 in
  let state =
    Contract_vm.create_state
      ~limit
      ~strict_values:true
      ~caller:"oct11111111111111111111111111111111111111111111"
      ~origin:"oct11111111111111111111111111111111111111111111"
      ~address:"oct22222222222222222222222222222222222222222222"
      ~value:Z.zero
      ~storage
      ()
  in
  state.Contract_vm.pc <- pc;
  if Contract_vm.run state fixed && not state.Contract_vm.reverted then
    Ok state.Contract_vm.effort_used
  else
    Error Execution_failed

let receipt ~status (prior : t) (next : t) =
  let prior_session_root = root prior in
  let next_session_root = root next in
  Inference_receipt.{
    target_root = prior.target_root;
    request_root = prior.request_root;
    prior_session_root;
    next_session_root;
    prior_sequence = prior.sequence;
    next_sequence = next.sequence;
    output_root = next.output_root;
    committed_effort = next.committed_effort - prior.committed_effort;
    completion_status = status;
    consensus_accepted = false;
  }

let open_session ~target ~request ~pins =
  let target_root = Inference_target.root target in
  let request_root = Inference_request.root request in
  if not (String.equal request.Inference_request.target_root target_root) then
    Error (Request_root_mismatch (request.target_root, target_root))
  else if not (String.equal pins.Inference_store.model_root target.model_root) then
    Error (Pin_root_mismatch (pins.model_root, target.model_root))
  else if not (String.equal pins.Inference_store.store_root target.store_root) then
    Error (Pin_root_mismatch (pins.store_root, target.store_root))
  else
    Ok {
      target_root;
      request_root;
      sequence = 0;
      phase = Open;
      output_root = initial_output_root request_root;
      committed_effort = 0;
    }

let advance ~admitted ~target ~request ~expected_sequence session =
  match check_sequence expected_sequence session with
  | Error error -> Error error
  | Ok () ->
    if terminal session.phase then Error Terminal_session
    else
      match check_identity ~target ~request session with
      | Error error -> Error error
      | Ok () ->
        (match Inference_target.entry_label target request.entrypoint with
         | None -> Error (Entrypoint_unsupported request.entrypoint)
         | Some label ->
           let code = Admission.code admitted in
           match entrypoint_pc code label with
           | None -> Error (Entrypoint_missing label)
           | Some pc ->
             match
               run_entrypoint
                 ~limit:request.max_advance_effort
                 code
                 pc
             with
             | Error error -> Error error
             | Ok effort ->
               if effort > request.max_advance_effort then
                 Error (Effort_exceeded
                          (effort, request.max_advance_effort))
               else if session.committed_effort > max_int - effort then
                 Error (Effort_overflow
                          (session.committed_effort, effort))
               else
                 let committed_effort = session.committed_effort + effort in
                 let next_sequence = session.sequence + 1 in
                 let next = {
                   session with
                   sequence = next_sequence;
                   phase = Advanced;
                   output_root =
                     output_root
                       ~request_root:session.request_root
                       ~sequence:next_sequence
                       ~committed_effort;
                   committed_effort;
                 } in
                 Ok (next, receipt ~status:"advanced" session next))

let finalize ~expected_sequence session =
  match check_sequence expected_sequence session with
  | Error error -> Error error
  | Ok () ->
    if terminal session.phase then Error Terminal_session
    else
      let next = {
        session with
        sequence = session.sequence + 1;
        phase = Finalized;
      } in
      Ok (next, receipt ~status:"finalized" session next)

let cancel ~expected_sequence session =
  match check_sequence expected_sequence session with
  | Error error -> Error error
  | Ok () ->
    if terminal session.phase then Error Terminal_session
    else
      Ok {
        session with
        sequence = session.sequence + 1;
        phase = Canceled;
      }

let error_message = function
  | Bad_sequence (expected, actual) ->
    Printf.sprintf
      "session sequence mismatch: expected %d actual %d"
      expected actual
  | Terminal_session -> "session is terminal"
  | Target_root_mismatch (expected, actual) ->
    Printf.sprintf
      "session target root mismatch: expected %s actual %s"
      expected actual
  | Request_root_mismatch (expected, actual) ->
    Printf.sprintf
      "session request root mismatch: expected %s actual %s"
      expected actual
  | Pin_root_mismatch (expected, actual) ->
    Printf.sprintf
      "session pin root mismatch: expected %s actual %s"
      expected actual
  | Entrypoint_unsupported name ->
    Printf.sprintf "unsupported session entrypoint: %s" name
  | Entrypoint_missing label ->
    Printf.sprintf "missing session entrypoint label: %d" label
  | Execution_failed -> "session execution failed"
  | Effort_exceeded (required, available) ->
    Printf.sprintf
      "session effort exceeded: required %d available %d"
      required available
  | Effort_overflow (current, next) ->
    Printf.sprintf
      "session effort overflow: current %d next %d"
      current next
