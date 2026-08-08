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


type result = {
  effort_used : int;
  output_payload : string;
  output_root : string;
  committed_target_state_root : string option;
  committed_target_state_payload : string option;
  candidate_root : string;
}

type execution_profile = {
  phase : string;
  microseconds : int;
}

type profile_config = {
  clock : unit -> float;
  opcode_name : Contract_vm.instr -> string;
}

type profile = {
  execution_profile : execution_profile list;
  opcode_profile : Contract_vm.opcode_profile list;
}

type profiled_result = {
  result : result;
  profile : profile;
}

let profile_microseconds started stopped =
  int_of_float ((stopped -. started) *. 1_000_000.0)

let profile_phase profile phases phase f =
  match profile with
  | None -> f ()
  | Some config ->
    let started = config.clock () in
    let result = f () in
    let stopped = config.clock () in
    phases :=
      { phase; microseconds = profile_microseconds started stopped }
      :: !phases;
    result

type error =
  | Entrypoint_unsupported of string
  | Entrypoint_missing of int
  | Session_context_mismatch of string
  | Opaque_value
  | Invalid_output of string
  | Missing_output_cell of int
  | Output_limit_exceeded of int * int
  | Scratch_limit_exceeded of int * int
  | Invalid_committed_target_state of string
  | Missing_committed_target_state_payload of string
  | Committed_target_state_root_mismatch of string * string
  | Execution_failed

let entrypoint_pc code label =
  let rec loop pc =
    if pc = Array.length code then None
    else
      match code.(pc) with
      | Contract_vm.JDEST value when value = label -> Some pc
      | _ -> loop (pc + 1)
  in
  loop 0

let length_prefix value =
  string_of_int (String.length value) ^ ":" ^ value

let value_payload = function
  | Contract_vm.VInt value -> Ok ("int:" ^ Z.to_string value)
  | Contract_vm.VBool value -> Ok ("bool:" ^ if value then "1" else "0")
  | Contract_vm.VString value -> Ok ("string:" ^ length_prefix value)
  | Contract_vm.VBytes value -> Ok ("bytes:" ^ length_prefix value)
  | Contract_vm.VBytes32 value -> Ok ("bytes32:" ^ length_prefix value)
  | Contract_vm.VU64 value -> Ok ("u64:" ^ Z.to_string value)
  | Contract_vm.VU128 value -> Ok ("u128:" ^ Z.to_string value)
  | Contract_vm.VU256 value -> Ok ("u256:" ^ Z.to_string value)
  | Contract_vm.VAddr value -> Ok ("address:" ^ length_prefix value)
  | Contract_vm.VCipher _
  | Contract_vm.VPubKey _ -> Error Opaque_value

let sorted_bindings table compare_key =
  Hashtbl.fold (fun key value values -> (key, value) :: values) table []
  |> List.sort (fun (left, _) (right, _) -> compare_key left right)

let candidate_root_and_size ~target_root state =
  (* One pass computing both the candidate payload size (scratch limit
     check) and the SHA256 of "octra:inference:candidate\000" ^ target_root
     ^ "\000" ^ memory_payload state, without materializing the (large)
     payload string: each cell encodes as length_prefix(key) ^ ":" ^ value,
     joined by "|". *)
  let digest = ref (Digestif.SHA256.init ()) in
  let size = ref 0 in
  let feed_counted value =
    digest := Digestif.SHA256.feed_string !digest value;
    size := !size + String.length value
  in
  feed_counted "octra:inference:candidate\000";
  feed_counted target_root;
  feed_counted "\000";
  let feed_value = function
    | Contract_vm.VInt value ->
      feed_counted ("int:" ^ Z.to_string value);
      Ok ()
    | Contract_vm.VBool value ->
      feed_counted (if value then "bool:1" else "bool:0");
      Ok ()
    | Contract_vm.VString value ->
      feed_counted "string:";
      feed_counted (string_of_int (String.length value));
      feed_counted ":";
      feed_counted value;
      Ok ()
    | Contract_vm.VBytes value ->
      feed_counted "bytes:";
      feed_counted (string_of_int (String.length value));
      feed_counted ":";
      feed_counted value;
      Ok ()
    | Contract_vm.VBytes32 value ->
      feed_counted "bytes32:";
      feed_counted (string_of_int (String.length value));
      feed_counted ":";
      feed_counted value;
      Ok ()
    | Contract_vm.VU64 value -> feed_counted ("u64:" ^ Z.to_string value); Ok ()
    | Contract_vm.VU128 value -> feed_counted ("u128:" ^ Z.to_string value); Ok ()
    | Contract_vm.VU256 value -> feed_counted ("u256:" ^ Z.to_string value); Ok ()
    | Contract_vm.VAddr value ->
      feed_counted "address:";
      feed_counted (string_of_int (String.length value));
      feed_counted ":";
      feed_counted value;
      Ok ()
    | Contract_vm.VCipher _
    | Contract_vm.VPubKey _ -> Error Opaque_value
  in
  let feed_cell (key, value) =
    let key_payload = string_of_int key in
    feed_counted (string_of_int (String.length key_payload));
    feed_counted ":";
    feed_counted key_payload;
    feed_counted ":";
    feed_value value
  in
  let bindings = sorted_bindings state.Contract_vm.memory.data compare in
  let rec loop = function
    | [] -> Ok ()
    | [cell] -> feed_cell cell
    | cell :: rest ->
      (match feed_cell cell with
       | Error error -> Error error
       | Ok () ->
         feed_counted "|";
         loop rest)
  in
  match loop bindings with
  | Error error -> Error error
  | Ok () ->
    Ok (!size, Digestif.SHA256.to_hex (Digestif.SHA256.get !digest))

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let output_integer = function
  | Contract_vm.VInt value
  | Contract_vm.VU64 value
  | Contract_vm.VU128 value
  | Contract_vm.VU256 value
    when Z.fits_int value -> Some (Z.to_int value)
  | _ -> None

(* The session ABI reserves r0/r1 for output bounds and writes request.input_root
   into memory before entry. ABI v2 also exposes prior session progress through
   fixed cells. The retained candidate state is canonical memory; immutable
   blobs are bound by the plan, not copied into scratch. *)
let output_payload state ~max_bytes =
  match
    output_integer
      state.Contract_vm.regs.(Inference_session_abi.output_base_register),
    output_integer
      state.Contract_vm.regs.(Inference_session_abi.output_count_register)
  with
  | None, _
  | _, None -> Error (Invalid_output "r0/r1 must contain integer output bounds")
  | Some base, Some length when base < 0 || length < 0 ->
    Error (Invalid_output "output bounds must be non-negative")
  | Some base, Some length when base > max_int - length ->
    Error (Invalid_output "output span overflows the VM address space")
  | Some _, Some length when length > max_bytes ->
    Error (Output_limit_exceeded (length, max_bytes))
  | Some base, Some length ->
    let rec read_values index acc =
      if index = length then Ok (List.rev acc)
      else
        let cell = base + index in
        match Hashtbl.find_opt state.Contract_vm.memory.data cell with
        | None -> Error (Missing_output_cell cell)
        | Some value ->
          (match value_payload value with
           | Error error -> Error error
           | Ok value -> read_values (index + 1) (value :: acc))
    in
    (match read_values 0 [] with
     | Error error -> Error error
     | Ok values ->
       let payload =
         String.concat
           "|"
           [
             "base=" ^ string_of_int base;
             "length=" ^ string_of_int length;
             "values=" ^ String.concat "," values;
           ]
       in
       if String.length payload > max_bytes then
         Error (Output_limit_exceeded (String.length payload, max_bytes))
       else Ok payload)

let output_root ~target_root ~session_abi_root payload =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:output\000"
       ^ target_root ^ "\000" ^ session_abi_root ^ "\000" ^ payload)
    |> to_hex)

let add_pins state pins =
  List.iter
    (fun (range : Inference_store.pinned_range) ->
      Hashtbl.replace state.Contract_vm.blobs range.range_root range.bytes)
    (Inference_store.ranges pins)

let bind_request_input state request input =
  Hashtbl.replace
    state.Contract_vm.memory.data
    Inference_session_abi.input_root_cell
    (Contract_vm.VString request.Inference_request.input_root);
  Hashtbl.replace
    state.Contract_vm.blobs
    request.Inference_request.input_root
    input

let bind_session_context state context =
  let open Inference_session_abi in
  let root_or_empty = function
    | None -> ""
    | Some root -> root
  in
  Hashtbl.replace
    state.Contract_vm.memory.data
    sequence_cell
    (Contract_vm.VInt (Z.of_int context.sequence));
  Hashtbl.replace
    state.Contract_vm.memory.data
    logical_position_cell
    (Contract_vm.VInt (Z.of_int context.logical_position));
  Hashtbl.replace
    state.Contract_vm.memory.data
    output_root_cell
    (Contract_vm.VString context.output_root);
  Hashtbl.replace
    state.Contract_vm.memory.data
    output_prefix_root_cell
    (Contract_vm.VString context.output_prefix_root);
  Hashtbl.replace
    state.Contract_vm.memory.data
    committed_target_state_root_cell
    (Contract_vm.VString
       (root_or_empty context.committed_target_state_root));
  match
    context.committed_target_state_root,
    context.committed_target_state_payload
  with
  | None, None -> Ok ()
  | Some root, Some payload ->
    let actual = sha256 payload in
    if not (String.equal actual root) then
      Error (Committed_target_state_root_mismatch (root, actual))
    else
      (match Hashtbl.find_opt state.Contract_vm.blobs root with
       | None ->
         Hashtbl.replace state.Contract_vm.blobs root payload;
         Ok ()
       | Some existing when String.equal existing payload -> Ok ()
       | Some _ ->
         Error
           (Invalid_committed_target_state
              "committed target state blob collision"))
  | Some root, None -> Error (Missing_committed_target_state_payload root)
  | None, Some _ ->
    Error
      (Invalid_committed_target_state
         "committed target state payload requires a root")

let check_scratch_payload payload_size ~max_bytes =
  if payload_size > max_bytes then
    Error (Scratch_limit_exceeded (payload_size, max_bytes))
  else Ok ()

let plain_ctx =
  {
    Contract_vm.default_ctx with
    allow_fhe_capability = (fun _ -> false);
  }

let hex = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let valid_root value =
  String.length value = 64 && String.for_all hex value

let check_context_values context =
  if context.Inference_session_abi.sequence < 0 then
    Error (Session_context_mismatch "sequence must be non-negative")
  else if context.logical_position < 0 then
    Error (Session_context_mismatch "logical position must be non-negative")
  else if not (valid_root context.output_root) then
    Error (Session_context_mismatch "output root must be 64 lowercase hex")
  else if not (valid_root context.output_prefix_root) then
    Error
      (Session_context_mismatch
         "output prefix root must be 64 lowercase hex")
  else
    match
      context.committed_target_state_root,
      context.committed_target_state_payload
    with
    | None, None -> Ok ()
    | Some root, None ->
      if valid_root root then Ok ()
      else
        Error
          (Session_context_mismatch
             "committed target state root must be 64 lowercase hex")
    | Some root, Some payload ->
      if not (valid_root root) then
        Error
          (Session_context_mismatch
             "committed target state root must be 64 lowercase hex")
      else if not (String.equal (sha256 payload) root) then
        Error
          (Session_context_mismatch
             "committed target state payload hash mismatch")
      else Ok ()
    | None, Some _ ->
      Error
        (Session_context_mismatch
           "committed target state payload requires a root")

let check_session_context ~committed_state_supported = function
  | Some context, false
    when context.Inference_session_abi.committed_target_state_payload <> None ->
    Error
      (Session_context_mismatch
         "committed target state payload requires committed-state session ABI")
  | Some _, false ->
    Error
      (Session_context_mismatch
         "continuation context requires a continuation-capable session ABI")
  | None, true ->
    Error
      (Session_context_mismatch
         "continuation-capable session ABI requires continuation context")
  | Some context, true ->
    if
      context.Inference_session_abi.committed_target_state_payload <> None
      && not committed_state_supported
    then
      Error
        (Session_context_mismatch
           "committed target state payload requires committed-state session ABI")
    else
      check_context_values context
  | None, false -> Ok ()

let committed_target_state state =
  let open Inference_session_abi in
  match Hashtbl.find_opt state.Contract_vm.memory.data committed_target_state_root_cell with
  | None -> Ok (None, None)
  | Some (Contract_vm.VString "") -> Ok (None, None)
  | Some (Contract_vm.VString root) ->
    if not (valid_root root) then
      Error
        (Invalid_committed_target_state
           "committed target state root must be 64 lowercase hex")
    else
      (match Hashtbl.find_opt state.Contract_vm.blobs root with
       | None -> Error (Missing_committed_target_state_payload root)
       | Some payload ->
         let actual = sha256 payload in
         if String.equal actual root then Ok (Some root, Some payload)
         else Error (Committed_target_state_root_mismatch (root, actual)))
  | Some _ ->
    Error
      (Invalid_committed_target_state
         "committed target state root cell must contain a string")

let run_internal ?profile ?session_context ~plan () =
  let admitted = Inference_plan.admitted plan in
  let target = Inference_plan.target plan in
  let request = Inference_plan.request plan in
  let pins = Inference_plan.pins plan in
  let requirement = Inference_plan.requirement plan in
  let continuation_supported =
    Inference_session_abi.continuation_supported
      target.Inference_target.session_abi_root
  in
  let committed_state_supported =
    Inference_session_abi.committed_state_supported
      target.Inference_target.session_abi_root
  in
  match
    check_session_context
      ~committed_state_supported
      (session_context, continuation_supported)
  with
  | Error error -> Error error
  | Ok () ->
    if not
      (String.equal
         request.Inference_request.entrypoint
         Inference_session_abi.advance_entrypoint)
    then
      Error (Entrypoint_unsupported request.entrypoint)
    else
      (match entrypoint_pc (Admission.code admitted) Inference_session_abi.advance_label with
     | None -> Error (Entrypoint_missing Inference_session_abi.advance_label)
     | Some pc ->
       let state =
         Contract_vm.create_state
           ~limit:request.Inference_request.max_advance_effort
           ~ctx:plain_ctx
           ~strict_values:true
           ~strict_blobs:true
           ~caller:"oct11111111111111111111111111111111111111111111"
           ~origin:"oct11111111111111111111111111111111111111111111"
           ~address:"oct22222222222222222222222222222222222222222222"
           ~value:Z.zero
           ~storage:(Hashtbl.create 16)
           ()
       in
       let execution_profile = ref [] in
       profile_phase profile execution_profile "add_pins" (fun () ->
         add_pins state pins);
       profile_phase profile execution_profile "bind_request_input" (fun () ->
         bind_request_input state request (Inference_plan.input plan));
       let bound_context =
         match session_context with
         | None -> Ok ()
         | Some context ->
           profile_phase
             profile
             execution_profile
             "bind_session_context"
             (fun () -> bind_session_context state context)
       in
       (match bound_context with
        | Error error -> Error error
        | Ok () ->
       let fixed =
         profile_phase profile execution_profile "fix_jumps" (fun () ->
           Contract.fix_jumps (Admission.code admitted))
       in
       state.Contract_vm.pc <- pc;
       let success, opcode_profile =
         profile_phase profile execution_profile "vm_run" (fun () ->
           match profile with
           | None -> Contract_vm.run state fixed, []
           | Some config ->
             Contract_vm.run_profiled
               ~clock:config.clock
               ~opcode_name:config.opcode_name
               state
               fixed)
       in
       if not success || state.Contract_vm.reverted then
         Error Execution_failed
       else
          (match
             profile_phase profile execution_profile "candidate_payload" (fun () ->
               candidate_root_and_size
                 ~target_root:(Inference_target.root target)
                 state)
          with
           | Error error -> Error error
           | Ok (candidate_size, candidate_root) ->
             (match
                profile_phase profile execution_profile "scratch_check" (fun () ->
                  check_scratch_payload
                    candidate_size
                    ~max_bytes:
                      requirement.Execution_requirement.limits.max_scratch_bytes)
             with
             | Error error -> Error error
             | Ok () ->
               (match
                  profile_phase profile execution_profile "output_payload" (fun () ->
                    output_payload
                      state
                      ~max_bytes:request.Inference_request.max_output_bytes)
                with
                | Ok output ->
                  let output_root =
                    profile_phase profile execution_profile "output_root" (fun () ->
                      output_root
                        ~target_root:(Inference_target.root target)
                        ~session_abi_root:target.Inference_target.session_abi_root
                        output)
                  in
                  let committed =
                    if committed_state_supported then
                      profile_phase
                        profile
                        execution_profile
                        "committed_target_state"
                        (fun () -> committed_target_state state)
                    else Ok (None, None)
                  in
                  (match committed with
                   | Error error -> Error error
                   | Ok (committed_root, committed_payload) ->
               Ok
                 ( {
                     effort_used = state.Contract_vm.effort_used;
                     output_payload = output;
                     output_root;
                     committed_target_state_root = committed_root;
                     committed_target_state_payload = committed_payload;
                     candidate_root;
                   },
                   {
                     execution_profile = List.rev !execution_profile;
                     opcode_profile;
                   } )
                  )
                | Error error -> Error error))))
        )

let run ?session_context ~plan () =
  match run_internal ?session_context ~plan () with
  | Ok (result, _) -> Ok result
  | Error error -> Error error

let run_profiled ?session_context ~profile ~plan () =
  match run_internal ?session_context ~profile ~plan () with
  | Ok (result, profile) -> Ok { result; profile }
  | Error error -> Error error

let error_message = function
  | Entrypoint_unsupported name ->
    Printf.sprintf "unsupported inference entrypoint: %s" name
  | Entrypoint_missing label ->
    Printf.sprintf "missing inference entrypoint label: %d" label
  | Session_context_mismatch error ->
    "inference session context mismatch: " ^ error
  | Opaque_value ->
    "inference execution encountered an opaque value"
  | Invalid_output error -> "invalid inference output: " ^ error
  | Missing_output_cell cell ->
    Printf.sprintf "inference output cell is not initialized: %d" cell
  | Output_limit_exceeded (required, available) ->
    Printf.sprintf
      "inference output exceeds limit: required %d available %d"
      required available
  | Scratch_limit_exceeded (required, available) ->
    Printf.sprintf
      "inference scratch exceeds limit: required %d available %d"
      required available
  | Invalid_committed_target_state error ->
    "invalid committed target state: " ^ error
  | Missing_committed_target_state_payload root ->
    "missing committed target state payload: " ^ root
  | Committed_target_state_root_mismatch (expected, actual) ->
    Printf.sprintf
      "committed target state root mismatch: expected %s actual %s"
      expected actual
  | Execution_failed -> "inference execution failed"
