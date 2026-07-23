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
  model_ranges_root : string;
  model_deployment_root : string option;
  session_limit : int;
  sequence : int;
  phase : phase;
  logical_position : int;
  committed_target_state_root : string option;
  output_root : string;
  output_prefix_root : string;
  candidate_root : string;
  committed_effort : int;
}

type error =
  | Bad_sequence of int * int
  | Terminal_session
  | Invalid_phase of string
  | Target_root_mismatch of string * string
  | Request_root_mismatch of string * string
  | Model_ranges_root_mismatch of string * string
  | Model_deployment_root_mismatch of string option * string option
  | Entrypoint_unsupported of string
  | Entrypoint_missing of int
  | Execution_error of string
  | Execution_failed
  | Effort_exceeded of int * int
  | Effort_overflow of int * int
  | Session_limit_exceeded of int * int

let phase_name = function
  | Open -> "open"
  | Advanced -> "advanced"
  | Finalized -> "finalized"
  | Canceled -> "canceled"

let optional_root_field name = function
  | None -> []
  | Some root -> [name, `String root]

let identity_json session =
  `Assoc (
    [
      "target_root", `String session.target_root;
      "request_root", `String session.request_root;
      "model_ranges_root", `String session.model_ranges_root;
    ]
    @ optional_root_field
        "model_deployment_root"
        session.model_deployment_root
    @ [
      "sequence", `Int session.sequence;
      "phase", `String (phase_name session.phase);
      "logical_position", `Int session.logical_position;
      "committed_target_state_root",
      (match session.committed_target_state_root with
       | None -> `Null
       | Some root -> `String root);
      "output_root", `String session.output_root;
      "output_prefix_root", `String session.output_prefix_root;
      "committed_effort", `Int session.committed_effort;
    ])

let root session =
  let payload = Yojson.Safe.to_string (identity_json session) in
  Digestif.SHA256.(
    digest_string ("octra:inference:session\000" ^ payload) |> to_hex)

let session_size session =
  String.length (Yojson.Safe.to_string (identity_json session))

let check_session_size session =
  let length = session_size session in
  if length > session.session_limit then
    Error (Session_limit_exceeded (length, session.session_limit))
  else
    Ok session

let initial_output_prefix_root request_root =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:output-prefix\000" ^ request_root ^ "\000open")
    |> to_hex)

let output_prefix_step_json ~prior_root ~output_root =
  `Assoc [
    "prior_root", `String prior_root;
    "output_root", `String output_root;
  ]

let append_output_prefix ~prior_root ~output_root =
  let payload =
    Yojson.Safe.to_string (output_prefix_step_json ~prior_root ~output_root)
  in
  Digestif.SHA256.(
    digest_string ("octra:inference:output-prefix\000" ^ payload) |> to_hex)

let initial_output_root request_root =
  Digestif.SHA256.(
    digest_string ("octra:inference:output\000" ^ request_root ^ "\000open")
    |> to_hex)

let initial_candidate_root ~request_root ~model_ranges_root =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:candidate\000" ^ request_root ^ "\000" ^ model_ranges_root)
    |> to_hex)

let check_sequence expected session =
  if session.sequence = expected then Ok ()
  else Error (Bad_sequence (expected, session.sequence))

let terminal = function
  | Finalized
  | Canceled -> true
  | Open
  | Advanced -> false

let check_identity ~plan session =
  let target_root = Inference_target.root (Inference_plan.target plan) in
  let request_root = Inference_request.root (Inference_plan.request plan) in
  let model_ranges_root = Inference_plan.model_ranges_root plan in
  let model_deployment_root = Inference_plan.model_deployment_root plan in
  if not (String.equal session.target_root target_root) then
    Error (Target_root_mismatch (session.target_root, target_root))
  else if not (String.equal session.request_root request_root) then
    Error (Request_root_mismatch (session.request_root, request_root))
  else if not (String.equal session.model_ranges_root model_ranges_root) then
    Error (Model_ranges_root_mismatch
             (session.model_ranges_root, model_ranges_root))
  else if session.model_deployment_root <> model_deployment_root then
    Error
      (Model_deployment_root_mismatch
         (session.model_deployment_root, model_deployment_root))
  else
    Ok ()

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
    candidate_root = next.candidate_root;
    effort_delta = next.committed_effort - prior.committed_effort;
    completion_status = status;
    consensus_accepted = false;
  }

let open_session ~plan =
  let target_root = Inference_target.root (Inference_plan.target plan) in
  let request_root = Inference_request.root (Inference_plan.request plan) in
  let model_ranges_root = Inference_plan.model_ranges_root plan in
  let model_deployment_root = Inference_plan.model_deployment_root plan in
  let limits = Inference_plan.limits plan in
  check_session_size {
    target_root;
    request_root;
    model_ranges_root;
    model_deployment_root;
    session_limit = limits.Execution_requirement.max_session_bytes;
    sequence = 0;
    phase = Open;
    logical_position = 0;
    committed_target_state_root = None;
    output_root = initial_output_root request_root;
    output_prefix_root = initial_output_prefix_root request_root;
    candidate_root = initial_candidate_root ~request_root ~model_ranges_root;
    committed_effort = 0;
  }

let advance ~plan ~expected_sequence session =
  match check_sequence expected_sequence session with
  | Error error -> Error error
  | Ok () ->
    if terminal session.phase then Error Terminal_session
    else if session.phase = Advanced then
      Error (Invalid_phase "advance requires persistent target state")
    else
      match check_identity ~plan session with
      | Error error -> Error error
      | Ok () ->
        (match Inference_execution.run ~plan () with
         | Error (Inference_execution.Entrypoint_unsupported name) ->
           Error (Entrypoint_unsupported name)
         | Error (Inference_execution.Entrypoint_missing label) ->
           Error (Entrypoint_missing label)
         | Error error ->
           Error (Execution_error (Inference_execution.error_message error))
         | Ok execution ->
           let effort = execution.Inference_execution.effort_used in
           let request = Inference_plan.request plan in
           if effort > request.max_advance_effort then
             Error (Effort_exceeded (effort, request.max_advance_effort))
           else if session.committed_effort > max_int - effort then
             Error (Effort_overflow (session.committed_effort, effort))
           else
             let committed_effort = session.committed_effort + effort in
             let next = {
               session with
               sequence = session.sequence + 1;
               phase = Advanced;
               logical_position = session.logical_position + 1;
               output_root = execution.output_root;
               output_prefix_root =
                 append_output_prefix
                   ~prior_root:session.output_prefix_root
                   ~output_root:execution.output_root;
               candidate_root = execution.candidate_root;
               committed_effort;
             } in
             (match check_session_size next with
              | Error error -> Error error
              | Ok next -> Ok (next, receipt ~status:"advanced" session next)))

let finalize ~expected_sequence session =
  match check_sequence expected_sequence session with
  | Error error -> Error error
  | Ok () ->
    if terminal session.phase then Error Terminal_session
    else if session.phase = Open then
      Error (Invalid_phase "finalize requires an advanced session")
    else
      let next = {
        session with
        sequence = session.sequence + 1;
        phase = Finalized;
      } in
      (match check_session_size next with
       | Error error -> Error error
       | Ok next -> Ok (next, receipt ~status:"finalized" session next))

let cancel ~expected_sequence session =
  match check_sequence expected_sequence session with
  | Error error -> Error error
  | Ok () ->
    if terminal session.phase then Error Terminal_session
    else
      check_session_size {
        session with
        sequence = session.sequence + 1;
        phase = Canceled;
      }

let sequence session = session.sequence
let phase session = session.phase
let logical_position session = session.logical_position
let committed_target_state_root session = session.committed_target_state_root
let output_prefix_root session = session.output_prefix_root
let output_root session = session.output_root
let candidate_root session = session.candidate_root
let committed_effort session = session.committed_effort
let model_deployment_root session = session.model_deployment_root

let root_option_message = function
  | None -> "none"
  | Some root -> root

let error_message = function
  | Bad_sequence (expected, actual) ->
    Printf.sprintf
      "session sequence mismatch: expected %d actual %d"
      expected actual
  | Terminal_session -> "session is terminal"
  | Invalid_phase error -> "invalid inference session phase: " ^ error
  | Target_root_mismatch (expected, actual) ->
    Printf.sprintf
      "session target root mismatch: expected %s actual %s"
      expected actual
  | Request_root_mismatch (expected, actual) ->
    Printf.sprintf
      "session request root mismatch: expected %s actual %s"
      expected actual
  | Model_ranges_root_mismatch (expected, actual) ->
    Printf.sprintf
      "session model ranges root mismatch: expected %s actual %s"
      expected actual
  | Model_deployment_root_mismatch (actual, expected) ->
    Printf.sprintf
      "session model deployment root mismatch: actual %s expected %s"
      (root_option_message actual)
      (root_option_message expected)
  | Entrypoint_unsupported name ->
    Printf.sprintf "unsupported session entrypoint: %s" name
  | Entrypoint_missing label ->
    Printf.sprintf "missing session entrypoint label: %d" label
  | Execution_error error -> "inference execution error: " ^ error
  | Execution_failed -> "session execution failed"
  | Effort_exceeded (required, available) ->
    Printf.sprintf
      "session effort exceeded: required %d available %d"
      required available
  | Effort_overflow (current, next) ->
    Printf.sprintf
      "session effort overflow: current %d next %d"
      current next
  | Session_limit_exceeded (required, available) ->
    Printf.sprintf
      "session bytes exceeded: required %d available %d"
      required available
