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


module Admission = Octra_vm.Admission
module Bytecode = Octra_vm.Bytecode
module Model = Octra_vm.Inference_model
module Plan = Octra_vm.Inference_plan
module Program_envelope = Octra_vm.Program_envelope
module Program_effects = Octra_vm.Program_effects
module Program_type_flow = Octra_vm.Program_type_flow
module Req = Octra_vm.Execution_requirement
module Request = Octra_vm.Inference_request
module Abi = Octra_vm.Inference_session_abi
module Execution = Octra_vm.Inference_execution
module Policy = Octra_vm.Inference_opcode_policy
module Session = Octra_vm.Inference_session
module Store = Octra_vm.Inference_store
module Target = Octra_vm.Inference_target
module VM = Octra_vm.Contract_vm

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let facts_json =
  `Assoc [
    "root", `List [];
    "entries", `List [];
    "calls", `List [];
    "xcalls", `List [];
  ]

let source_with_effects code effects =
  let bytecode = Bytecode.encode code in
  let cert =
    Yojson.Safe.to_string (`Assoc [
      "schema", `String "aml_bytecode_certificate_v2";
      "compiler", `String "octra_aml";
      "compiler_version", `String "test";
      "declaration", `String "program";
      "source_mode", `String "single";
      "source_hash", `String (sha256 "test source");
      "bytecode_hash", `String (sha256 bytecode);
      "verification_hash", `String (sha256 "test verification");
      "effects", `List (List.map (fun value -> `String value) effects);
      "facts_hash",
      `String (Program_type_flow.facts_hash Program_type_flow.empty_facts);
      "facts", facts_json;
    ])
  in
  match Program_envelope.encode ~code:bytecode ~cert with
  | Ok raw -> raw
  | Error error -> failwith (Program_envelope.error_message error)

let capability name capability_root =
  Req.{ name = name; root = capability_root }

let limits =
  Req.{
    max_model_bytes = 64;
    max_view_bytes = 64;
    max_session_bytes = 768;
    max_scratch_bytes = 4096;
    max_output_bytes = 64;
    max_advance_effort = 10000;
  }

let requirement =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities = [
      capability "storage.authenticated-range" (hex_root 'd');
    ];
    limits;
  }

let support =
  Req.{
    support_vm_semantics_root = requirement.vm_semantics_root;
    support_numerical_roots = [requirement.numerical_root];
    support_effort_roots = [requirement.effort_root];
    support_capabilities = requirement.capabilities;
    support_limits = limits;
  }

let encoded bytes =
  Base64.encode_exn bytes

let code ?(write_output = true) range_root =
  let output_code = if write_output then [|VM.MSTORE (0, 3)|] else [||] in
  Array.concat
    [
      [|
        VM.JDEST Abi.advance_label;
        VM.LDI (4, VM.VString range_root);
        VM.FLOAD (3, 4);
      |];
      output_code;
      [|
        VM.LDI (0, VM.VInt Z.zero);
        VM.LDI (1, VM.VInt Z.one);
        VM.STOP;
      |];
    ]

let input_code ?(write_output = true) () =
  let output_code = if write_output then [|VM.MSTORE (0, 3)|] else [||] in
  Array.concat
    [
      [|
        VM.JDEST Abi.advance_label;
        VM.MLOAD (2, Abi.input_root_cell);
        VM.FLOAD (3, 2);
      |];
      output_code;
      [|
        VM.LDI (0, VM.VInt Z.zero);
        VM.LDI (1, VM.VInt Z.one);
        VM.STOP;
      |];
    ]

let fp_bits value =
  VM.VInt (Z.of_int64 (Int64.bits_of_float value))

let argmax_output_code values =
  let count = List.length values in
  let writes =
    values
    |> List.mapi (fun index value ->
      [|
        VM.LDI (2, fp_bits value);
        VM.MSTORE (100 + index, 2);
      |])
    |> Array.concat
  in
  Array.concat
    [
      [|VM.JDEST Abi.advance_label|];
      writes;
      [|
        VM.LDI (0, VM.VInt (Z.of_int 100));
        VM.LDI (1, VM.VInt (Z.of_int count));
        VM.ARGMAX_FP (2, 0, 1);
        VM.MSTORE (200, 2);
        VM.LDI (0, VM.VInt (Z.of_int 200));
        VM.LDI (1, VM.VInt Z.one);
        VM.STOP;
      |];
    ]

let run_payload_result ?(write_output = true) ?(max_output_bytes = 64)
    ?(max_scratch_bytes = 4096)
    ?capabilities
    ?code_range_root ?program ?(input = "") bytes =
  let limits = Req.{ limits with max_scratch_bytes } in
  let capabilities =
    match capabilities with
    | None -> requirement.capabilities
    | Some capabilities -> capabilities
  in
  let requirement = Req.{ requirement with capabilities; limits } in
  let support =
    Req.{ support with support_capabilities = capabilities; support_limits = limits }
  in
  let owner = encoded bytes in
  let owner_root = sha256 owner in
  let range =
    Model.{
      owner_root;
      offset = 0;
      length = String.length owner;
      encoding = "tensor.q1-g128";
      shape_root = None;
    }
  in
  let code_root =
    match code_range_root with
    | Some root -> root
    | None -> Model.range_root range
  in
  let code =
    match program with
    | Some code -> code
    | None -> code ~write_output code_root
  in
  let admitted =
    Inference_cert.admit ~support ~requirement code
  in
  let target =
    Target.{
      program_root = Target.program_root admitted;
      requirement_root = Req.root requirement;
      model_root = hex_root 'f';
      execution_descriptor_root = hex_root '1';
      store_root = hex_root '2';
      session_abi_root = Abi.v1_root;
      entrypoints = [{
        entry_name = Abi.advance_entrypoint;
        entry_label = Abi.advance_label;
      }];
    }
  in
  let request =
    Request.{
      schema = Abi.request_schema;
      target_root = Target.root target;
      entrypoint = Abi.advance_entrypoint;
      input_root = sha256 input;
      request_nonce = hex_root '5';
      max_output_bytes;
      max_advance_effort = 10000;
    }
  in
  let model =
    Model.{
      model_root = target.model_root;
      store_root = target.store_root;
      ranges = [range];
    }
  in
  let pins =
    match Store.pin ~limits ~read:(fun root ->
      if String.equal root owner_root then Some owner else None) model with
    | Ok pins -> pins
    | Error error -> failwith (Store.error_message error)
  in
  let plan =
    match Plan.create ~admitted ~target ~request ~model ~pins ~input with
    | Ok plan -> plan
    | Error error -> failwith (Plan.error_message error)
  in
  let session =
    match Session.open_session ~plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  match
    Session.advance ~plan ~expected_sequence:0 session
  with
  | Ok (advanced, _) ->
    (match Session.output_payload advanced with
     | None -> Error "missing output payload"
     | Some payload -> Ok (payload, Session.output_root advanced))
  | Error error -> Error (Session.error_message error)

let run_result ?write_output ?max_output_bytes ?max_scratch_bytes
    ?capabilities
    ?code_range_root ?program ?input bytes =
  match
    run_payload_result
      ?write_output
      ?max_output_bytes
      ?max_scratch_bytes
      ?capabilities
      ?code_range_root
      ?program
      ?input
      bytes
  with
  | Ok (_, root) -> Ok root
  | Error error -> Error error

let run bytes =
  match run_result bytes with
  | Ok output_root -> output_root
  | Error error -> failwith error

let execution_plan ?(session_abi_root = Abi.v1_root) ?program ?capabilities bytes =
  let capabilities =
    match capabilities with
    | None -> requirement.capabilities
    | Some capabilities -> capabilities
  in
  let requirement = Req.{ requirement with capabilities } in
  let support =
    Req.{ support with support_capabilities = capabilities }
  in
  let owner = encoded bytes in
  let owner_root = sha256 owner in
  let range =
    Model.{
      owner_root;
      offset = 0;
      length = String.length owner;
      encoding = "tensor.q1-g128";
      shape_root = None;
    }
  in
  let code =
    match program with
    | Some code -> code
    | None -> code (Model.range_root range)
  in
  let admitted =
    Inference_cert.admit ~support ~requirement code
  in
  let target =
    Target.{
      program_root = Target.program_root admitted;
      requirement_root = Req.root requirement;
      model_root = hex_root 'f';
      execution_descriptor_root = hex_root '1';
      store_root = hex_root '2';
      session_abi_root;
      entrypoints = [{
        entry_name = Abi.advance_entrypoint;
        entry_label = Abi.advance_label;
      }];
    }
  in
  let request =
    Request.{
      schema = Abi.request_schema;
      target_root = Target.root target;
      entrypoint = Abi.advance_entrypoint;
      input_root = sha256 "";
      request_nonce = hex_root '5';
      max_output_bytes = 64;
      max_advance_effort = 10000;
    }
  in
  let model =
    Model.{
      model_root = target.model_root;
      store_root = target.store_root;
      ranges = [range];
    }
  in
  let pins =
    match Store.pin ~limits ~read:(fun root ->
      if String.equal root owner_root then Some owner else None) model with
    | Ok pins -> pins
    | Error error -> failwith (Store.error_message error)
  in
  match Plan.create ~admitted ~target ~request ~model ~pins ~input:"" with
  | Ok plan -> plan
  | Error error -> failwith (Plan.error_message error)

let continuation_context =
  Abi.{
    sequence = 0;
    logical_position = 0;
    output_root = hex_root '6';
    output_prefix_root = hex_root '7';
    committed_target_state_root = None;
    committed_target_state_payload = None;
  }

let committed_state_payload = "state:0"
let committed_state_payload_root = sha256 committed_state_payload
let committed_state_next_payload = "state:1"
let committed_state_next_payload_root = sha256 committed_state_next_payload

let committed_state_capability =
  capability "session.committed-state" (hex_root '8')

let committed_state_capabilities =
  requirement.capabilities @ [committed_state_capability]

let committed_state_reload_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.MLOAD (5, Abi.committed_target_state_root_cell);
    VM.FLOAD (6, 5);
    VM.MSTORE (30, 6);
    VM.LDI (0, VM.VInt (Z.of_int 30));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let committed_state_replace_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (5, VM.VString committed_state_next_payload);
    VM.FSTORE (6, 5);
    VM.MSTORE (Abi.committed_target_state_root_cell, 6);
    VM.MSTORE (30, 5);
    VM.LDI (0, VM.VInt (Z.of_int 30));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let committed_state_clear_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (5, VM.VString "");
    VM.MSTORE (Abi.committed_target_state_root_cell, 5);
    VM.MSTORE (30, 5);
    VM.LDI (0, VM.VInt (Z.of_int 30));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let committed_state_non_string_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (5, VM.VInt Z.one);
    VM.MSTORE (Abi.committed_target_state_root_cell, 5);
    VM.MSTORE (30, 5);
    VM.LDI (0, VM.VInt (Z.of_int 30));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let committed_state_missing_blob_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (5, VM.VString (hex_root '9'));
    VM.MSTORE (Abi.committed_target_state_root_cell, 5);
    VM.LDI (6, VM.VInt Z.one);
    VM.MSTORE (30, 6);
    VM.LDI (0, VM.VInt (Z.of_int 30));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let check_execution_rejects_v1_context () =
  let plan = execution_plan "context mismatch" in
  match Execution.run ~session_context:continuation_context ~plan () with
  | Error (Execution.Session_context_mismatch _) -> ()
  | Error error -> failwith (Execution.error_message error)
  | Ok _ -> failwith "expected v1 context mismatch"

let check_execution_rejects_v2_without_context () =
  let plan =
    execution_plan
      ~session_abi_root:Abi.v2_root
      "missing context"
  in
  match Execution.run ~plan () with
  | Error (Execution.Session_context_mismatch _) -> ()
  | Error error -> failwith (Execution.error_message error)
  | Ok _ -> failwith "expected v2 context mismatch"

let check_profiled_execution_rejects_v2_without_context () =
  let plan =
    execution_plan
      ~session_abi_root:Abi.v2_root
      "missing profiled context"
  in
  let profile =
    {
      Execution.clock = (fun () -> 0.0);
      opcode_name = (function
        | VM.STOP -> "STOP"
        | _ -> "other");
    }
  in
  match Execution.run_profiled ~profile ~plan () with
  | Error (Execution.Session_context_mismatch _) -> ()
  | Error error -> failwith (Execution.error_message error)
  | Ok _ -> failwith "expected profiled v2 context mismatch"

let check_execution_rejects_bad_context_values () =
  let plan =
    execution_plan
      ~session_abi_root:Abi.v2_root
      "bad context"
  in
  let bad_context =
    Abi.{
      sequence = -1;
      logical_position = 0;
      output_root = hex_root '6';
      output_prefix_root = hex_root '7';
      committed_target_state_root = None;
      committed_target_state_payload = None;
    }
  in
  match Execution.run ~session_context:bad_context ~plan () with
  | Error (Execution.Session_context_mismatch _) -> ()
  | Error error -> failwith (Execution.error_message error)
  | Ok _ -> failwith "expected v2 context value rejection"

let check_execution_binds_committed_state_payload () =
  let plan =
    execution_plan
      ~session_abi_root:Abi.committed_state_root
      ~program:committed_state_reload_code
      ~capabilities:committed_state_capabilities
      "committed state context"
  in
  let context =
    Abi.{
      sequence = 1;
      logical_position = 1;
      output_root = hex_root '6';
      output_prefix_root = hex_root '7';
      committed_target_state_root = Some committed_state_payload_root;
      committed_target_state_payload = Some committed_state_payload;
    }
  in
  match Execution.run ~session_context:context ~plan () with
  | Error error -> failwith (Execution.error_message error)
  | Ok result ->
    check
      "committed-state output payload"
      (String.equal
         result.Execution.output_payload
         "base=30|length=1|values=string:7:state:0");
    check
      "committed-state root retained"
      (result.committed_target_state_root = Some committed_state_payload_root);
    check
      "committed-state payload retained"
      (result.committed_target_state_payload = Some committed_state_payload)

let check_profiled_committed_state_equivalence () =
  let plan =
    execution_plan
      ~session_abi_root:Abi.committed_state_root
      ~program:committed_state_reload_code
      ~capabilities:committed_state_capabilities
      "committed state profiled"
  in
  let context =
    Abi.{
      sequence = 1;
      logical_position = 1;
      output_root = hex_root '6';
      output_prefix_root = hex_root '7';
      committed_target_state_root = Some committed_state_payload_root;
      committed_target_state_payload = Some committed_state_payload;
    }
  in
  let profile =
    {
      Execution.clock = (fun () -> 0.0);
      opcode_name = (function
        | VM.FLOAD _ -> "FLOAD"
        | VM.STOP -> "STOP"
        | _ -> "other");
    }
  in
  match
    Execution.run ~session_context:context ~plan (),
    Execution.run_profiled ~profile ~session_context:context ~plan ()
  with
  | Ok plain, Ok profiled ->
    check
      "profiled committed output root"
      (String.equal
         profiled.Execution.result.output_root
         plain.output_root);
    check
      "profiled committed state root"
      (profiled.result.committed_target_state_root
       = plain.committed_target_state_root);
    check
      "profiled committed state payload"
      (profiled.result.committed_target_state_payload
       = plain.committed_target_state_payload);
    check
      "profiled committed phase"
      (List.exists
         (fun (row : Execution.execution_profile) ->
            String.equal row.phase "committed_target_state")
         profiled.profile.execution_profile)
  | Error error, _
  | _, Error error -> failwith (Execution.error_message error)

let check_execution_replaces_and_clears_committed_state () =
  let context =
    Abi.{
      sequence = 1;
      logical_position = 1;
      output_root = hex_root '6';
      output_prefix_root = hex_root '7';
      committed_target_state_root = Some committed_state_payload_root;
      committed_target_state_payload = Some committed_state_payload;
    }
  in
  let replace_plan =
    execution_plan
      ~session_abi_root:Abi.committed_state_root
      ~program:committed_state_replace_code
      ~capabilities:committed_state_capabilities
      "committed state replace"
  in
  let clear_plan =
    execution_plan
      ~session_abi_root:Abi.committed_state_root
      ~program:committed_state_clear_code
      ~capabilities:committed_state_capabilities
      "committed state clear"
  in
  (match Execution.run ~session_context:context ~plan:replace_plan () with
   | Error error -> failwith (Execution.error_message error)
   | Ok result ->
     check
       "committed-state replaced root"
       (result.Execution.committed_target_state_root
        = Some committed_state_next_payload_root);
     check
       "committed-state replaced payload"
       (result.committed_target_state_payload
        = Some committed_state_next_payload));
  match Execution.run ~session_context:context ~plan:clear_plan () with
  | Error error -> failwith (Execution.error_message error)
  | Ok result ->
    check
      "committed-state cleared root"
      (result.Execution.committed_target_state_root = None);
    check
      "committed-state cleared payload"
      (result.committed_target_state_payload = None)

let check_execution_rejects_bad_post_committed_state () =
  let run_bad program =
    let plan =
      execution_plan
        ~session_abi_root:Abi.committed_state_root
        ~program
        ~capabilities:committed_state_capabilities
        "bad committed state"
    in
    let context =
      Abi.{
        sequence = 1;
        logical_position = 1;
        output_root = hex_root '6';
        output_prefix_root = hex_root '7';
        committed_target_state_root = Some committed_state_payload_root;
        committed_target_state_payload = Some committed_state_payload;
      }
    in
    Execution.run ~session_context:context ~plan ()
  in
  (match run_bad committed_state_non_string_code with
   | Error (Execution.Invalid_committed_target_state _) -> ()
   | Error error -> failwith (Execution.error_message error)
   | Ok _ -> failwith "expected non-string committed-state rejection");
  match run_bad committed_state_missing_blob_code with
  | Error (Execution.Missing_committed_target_state_payload root)
    when String.equal root (hex_root '9') -> ()
  | Error error -> failwith (Execution.error_message error)
  | Ok _ -> failwith "expected missing committed-state blob rejection"

let check_execution_rejects_committed_state_hash_mismatch () =
  let plan =
    execution_plan
      ~session_abi_root:Abi.committed_state_root
      ~program:committed_state_reload_code
      ~capabilities:committed_state_capabilities
      "committed state mismatch"
  in
  let context =
    Abi.{
      sequence = 1;
      logical_position = 1;
      output_root = hex_root '6';
      output_prefix_root = hex_root '7';
      committed_target_state_root = Some (hex_root '8');
      committed_target_state_payload = Some committed_state_payload;
    }
  in
  match Execution.run ~session_context:context ~plan () with
  | Error (Execution.Session_context_mismatch _) -> ()
  | Error error -> failwith (Execution.error_message error)
  | Ok _ -> failwith "expected committed-state hash mismatch rejection"

let starts_with prefix value =
  String.length value >= String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let check_host_float_forbidden () =
  let one = Z.of_int64 (Int64.bits_of_float 1.0) in
  let host_float_code = [|
    VM.JDEST Abi.advance_label;
    VM.LDI (0, VM.VInt Z.zero);
    VM.LDI (1, VM.VInt Z.one);
    VM.LDI (2, VM.VInt one);
    VM.RMSNORM_FP (0, 1, 2);
    VM.STOP;
  |] in
  let requirement =
    Req.{
      requirement with
      capabilities = [capability "tensor.strict-fp" (hex_root 'e')];
    }
  in
  let support = Req.{ support with support_capabilities = requirement.capabilities } in
  match
    Admission.of_inference_code_with_requirement ~support ~requirement host_float_code
  with
  | Error (Admission.Unsafe_error message) ->
    check
      "host-float opcode is forbidden"
      (starts_with "inference opcode RMSNORM_FP" message)
  | _ -> failwith "expected host-float rejection"

let check_opcode_capability_gate () =
  let code = [|
    VM.JDEST Abi.advance_label;
    VM.LDI (0, VM.VString "missing-range");
    VM.FLOAD (1, 0);
    VM.STOP;
  |] in
  let requirement =
    Req.{
      requirement with
      capabilities = [capability "tensor.fixed" (hex_root 'e')];
    }
  in
  let support = Req.{ support with support_capabilities = requirement.capabilities } in
  match
    Admission.of_inference_code_with_requirement ~support ~requirement code
  with
  | Error (Admission.Unsafe_error message) ->
    check
      "missing opcode capability is named"
      (starts_with "inference opcode FLOAD" message)
  | _ -> failwith "expected authenticated-range capability rejection"

let check_committed_state_capability_gate () =
  let code = [|
    VM.JDEST Abi.advance_label;
    VM.LDI (1, VM.VString "state");
    VM.FSTORE (2, 1);
    VM.STOP;
  |] in
  (match
     Admission.of_inference_code_with_requirement ~support ~requirement code
   with
   | Error (Admission.Unsafe_error message) ->
     check
       "missing committed-state capability is named"
       (starts_with "inference opcode FSTORE" message)
   | _ -> failwith "expected committed-state capability rejection");
  let committed_capability =
    capability "session.committed-state" (hex_root '6')
  in
  let requirement =
    Req.{
      requirement with
      capabilities = requirement.capabilities @ [committed_capability];
    }
  in
  let support =
    Req.{ support with support_capabilities = requirement.capabilities }
  in
  (match
     Admission.of_inference_code_with_requirement ~support ~requirement code
   with
   | Ok _ -> ()
   | Error error -> failwith (Admission.error_message error));
  check
    "FSTORE effects include storage_write"
    (Program_effects.(scan code |> names) = ["storage_write"]);
  let stale_source =
    source_with_effects code []
  in
  match
    Admission.decode_inference_program_source
      ~support
      ~requirement
      stale_source
  with
  | Error error ->
    let message = Admission.error_message error in
    check
      "missing FSTORE storage_write effect rejected"
      (starts_with
         "program effect policy: program effect policy mismatch"
         message)
  | Ok _ -> failwith "expected stale FSTORE effect certificate rejection"

let check_policy_frontier_lists_all_violations () =
  let requirement = Req.{ requirement with capabilities = [] } in
  let code = [|
    VM.JDEST Abi.advance_label;
    VM.FLOAD (1, 0);
    VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6);
    VM.RMSNORM_FP (0, 1, 2);
    VM.FHE_ADD (0, 1, 2, 3);
    VM.STOP;
  |] in
  match Policy.violations ~requirement code with
  | [
    Policy.Missing_capability {
      detail = { opcode = "FLOAD"; pc = 1 };
      capability = "storage.authenticated-range";
    };
    Policy.Missing_capability {
      detail = { opcode = "LINEAR_Q1_G128_FP"; pc = 2 };
      capability = "tensor.q1-g128";
    };
    Policy.Forbidden_opcode { opcode = "RMSNORM_FP"; pc = 3 };
    Policy.Forbidden_opcode { opcode = "FHE_ADD"; pc = 4 };
  ] -> ()
  | _ -> failwith "expected complete inference policy frontier"

let check_fhe_forbidden () =
  let cases = [
    "FHE_LOAD_PK", VM.FHE_LOAD_PK (0, 1);
    "FHE_ADD", VM.FHE_ADD (0, 1, 2, 3);
    "FHE_SUB", VM.FHE_SUB (0, 1, 2, 3);
    "FHE_MUL", VM.FHE_MUL (0, 1, 2, 3);
    "FHE_SCALE", VM.FHE_SCALE (0, 1, 2, 3);
    "FHE_DIV_CONST", VM.FHE_DIV_CONST (0, 1, 2, 3);
    "FHE_ADD_CONST", VM.FHE_ADD_CONST (0, 1, 2, 3);
    "FHE_SUB_CONST", VM.FHE_SUB_CONST (0, 1, 2, 3);
    "FHE_VERIFY_ZERO", VM.FHE_VERIFY_ZERO (0, 1, 2, 3);
    "FHE_VERIFY_RANGE", VM.FHE_VERIFY_RANGE (0, 1, 2, 3);
    "FHE_VERIFY_BOUND", VM.FHE_VERIFY_BOUND (0, 1, 2, 3, 4);
    "FHE_COMMIT", VM.FHE_COMMIT (0, 1, 2);
    "FHE_PEDERSEN", VM.FHE_PEDERSEN (0, 1, 2);
    "FHE_SER", VM.FHE_SER (0, 1);
    "FHE_DESER", VM.FHE_DESER (0, 1);
    "FHE_SER_PK", VM.FHE_SER_PK (0, 1);
    "FHE_DESER_PK", VM.FHE_DESER_PK (0, 1);
  ] in
  List.iter
    (fun (name, op) ->
      match
        Admission.of_inference_code_with_requirement
          ~support
          ~requirement
          [| VM.JDEST Abi.advance_label; op; VM.STOP |]
      with
      | Error (Admission.Unsafe_error message) ->
        check
          ("fhe opcode is forbidden: " ^ name)
          (starts_with ("inference opcode " ^ name) message)
      | _ -> failwith ("expected fhe opcode rejection: " ^ name))
    cases

let check_state_surfaces_forbidden () =
  let cases = [
    "SLOAD", VM.SLOAD (0, "k");
    "SSTORE", VM.SSTORE ("k", 0);
    "SDEL", VM.SDEL "k";
    "SLOADK", VM.SLOADK (0, 1);
    "SSTOREK", VM.SSTOREK (0, 1);
    "SDELK", VM.SDELK 0;
    "SLOADN", VM.SLOADN (0, 1, 2);
    "SSTOREN", VM.SSTOREN (0, 1, 2);
    "SKEYS", VM.SKEYS (0, 1, 2);
    "SKEYS_PAGE", VM.SKEYS_PAGE (0, 1, 2, 3, 4);
    "FSTORE", VM.FSTORE (0, 1);
    "OBJECT_MEMBER_COUNT", VM.OBJECT_MEMBER_COUNT (0, 1);
    "OBJECT_HAS_MEMBER", VM.OBJECT_HAS_MEMBER (0, 1, 2);
    "OBJECT_MEMBER_REF_AT", VM.OBJECT_MEMBER_REF_AT (0, 1, 2);
    "OBJECT_TRANSITION_APPLY",
    VM.OBJECT_TRANSITION_APPLY (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    "XCALL", VM.XCALL (0, 1, 2, 3, 0);
    "SPAWN", VM.SPAWN (0, 1);
    "SPAWN2", VM.SPAWN2 (0, 1, 2, 0);
    "TRANSFER", VM.TRANSFER (0, 1, 2);
    "CHECKPOINT", VM.CHECKPOINT;
    "ROLLBACK", VM.ROLLBACK;
    "COMMIT", VM.COMMIT;
    "EMIT", VM.EMIT ("event", []);
  ] in
  List.iter
    (fun (name, op) ->
      match
        Admission.of_inference_code_with_requirement
          ~support
          ~requirement
          [| VM.JDEST Abi.advance_label; op; VM.STOP |]
      with
      | Error (Admission.Unsafe_error message) ->
        check
          ("state opcode is forbidden: " ^ name)
          (starts_with ("inference opcode " ^ name) message)
      | _ -> failwith ("expected state opcode rejection: " ^ name))
    cases

let check_unlisted_opcodes_forbidden () =
  let cases = [
    "CALLER", VM.CALLER 0;
    "GROTH16_VERIFY_BN254", VM.GROTH16_VERIFY_BN254 (0, 1, 2, 3);
    "CALL_INT", VM.CALL_INT (0, 0);
    "SHA256", VM.SHA256 (0, 1);
    "MATMUL", VM.MATMUL (0, 1, 2, 3, 4, 5);
    "VECDOT", VM.VECDOT (0, 1, 2, 3);
    "RELU_INPLACE", VM.RELU_INPLACE (0, 1);
    "LOAD_INT8_BYTES_TO_MEM", VM.LOAD_INT8_BYTES_TO_MEM (0, 1, 2, 3, 4);
    "RESIDUAL_ADD", VM.RESIDUAL_ADD (0, 1, 2);
    "SHIFT_ROUND_INPLACE", VM.SHIFT_ROUND_INPLACE (0, 1, 2);
  ] in
  List.iter
    (fun (name, op) ->
      match
        Admission.of_inference_code_with_requirement
          ~support
          ~requirement
          [| VM.JDEST Abi.advance_label; op; VM.STOP |]
      with
      | Error (Admission.Unsafe_error message) ->
        check
          ("unlisted opcode is forbidden: " ^ name)
          (starts_with ("inference opcode " ^ name) message)
      | _ -> failwith ("expected unlisted opcode rejection: " ^ name))
    cases

let check_matmul_q16_forbidden () =
  let requirement =
    Req.{
      requirement with
      capabilities = [capability "tensor.fixed" (hex_root 'e')];
    }
  in
  let support = Req.{ support with support_capabilities = requirement.capabilities } in
  match
    Admission.of_inference_code_with_requirement
      ~support
      ~requirement
      [|
        VM.JDEST Abi.advance_label;
        VM.MATMUL_Q16 (0, 1, 2, 3, 4, 5);
        VM.STOP;
      |]
  with
  | Error (Admission.Unsafe_error message) ->
    check
      "MATMUL_Q16 is not advertised"
      (starts_with "inference opcode MATMUL_Q16" message)
  | _ -> failwith "expected MATMUL_Q16 rejection"

let check_data_rooted_execution () =
  let left = run "\001\002\003\004" in
  let right = run "\004\003\002\001" in
  check "executed data changes output root" (not (String.equal left right))

let check_request_rooted_execution () =
  let program = input_code () in
  let left = run_result ~program ~input:"left" "\001\002\003\004" in
  let right = run_result ~program ~input:"right" "\001\002\003\004" in
  match left, right with
  | Ok left, Ok right ->
    check "request input changes output root" (not (String.equal left right))
  | Error error, _
  | _, Error error -> failwith error

let check_output_contract () =
  (match run_result ~write_output:false "\001\002\003\004" with
   | Error error ->
     check
       "missing output cell is rejected"
       (starts_with
          "inference execution error: inference output cell"
          error)
   | Ok _ -> failwith "expected missing output cell rejection");
  (match run_result ~code_range_root:"missing-range" "\001\002\003\004" with
   | Error error ->
     check
       "missing authenticated range is rejected"
       (starts_with "inference execution error: inference execution failed" error)
   | Ok _ -> failwith "expected missing authenticated range rejection");
  (match run_result ~max_output_bytes:1 "\001\002\003\004" with
   | Error error ->
     check
       "output limit is enforced"
       (starts_with
          "inference execution error: inference output exceeds limit:"
          error)
   | Ok _ -> failwith "expected output limit rejection");
  (match run_result ~max_scratch_bytes:1 "\001\002\003\004" with
   | Error error ->
     check
       "scratch limit is enforced"
       (starts_with
          "inference execution error: inference scratch exceeds limit:"
          error)
   | Ok _ -> failwith "expected scratch limit rejection")

let check_argmax_output_contract () =
  let argmax_cap = capability "tensor.argmax" (hex_root '6') in
  let left =
    run_payload_result
      ~program:(argmax_output_code [1.0; 5.0; 5.0])
      ~capabilities:[argmax_cap]
      "owner"
  in
  let right =
    run_payload_result
      ~program:(argmax_output_code [1.0; 5.0; 6.0])
      ~capabilities:[argmax_cap]
      "owner"
  in
  match left, right with
  | Ok (left_payload, left), Ok (right_payload, right) ->
    check
      "argmax payload selects first maximum"
      (String.equal left_payload "base=200|length=1|values=int:1");
    check
      "argmax payload selects later maximum"
      (String.equal right_payload "base=200|length=1|values=int:2");
    check "argmax output root shape" (String.length left = 64);
    check "argmax output roots selected index" (not (String.equal left right))
  | Error error, _
  | _, Error error -> failwith error

let () =
  check_host_float_forbidden ();
  check_opcode_capability_gate ();
  check_committed_state_capability_gate ();
  check_policy_frontier_lists_all_violations ();
  check_fhe_forbidden ();
  check_state_surfaces_forbidden ();
  check_unlisted_opcodes_forbidden ();
  check_matmul_q16_forbidden ();
  check_data_rooted_execution ();
  check_request_rooted_execution ();
  check_execution_rejects_v1_context ();
  check_execution_rejects_v2_without_context ();
  check_profiled_execution_rejects_v2_without_context ();
  check_execution_rejects_bad_context_values ();
  check_execution_binds_committed_state_payload ();
  check_profiled_committed_state_equivalence ();
  check_execution_replaces_and_clears_committed_state ();
  check_execution_rejects_bad_post_committed_state ();
  check_execution_rejects_committed_state_hash_mismatch ();
  check_output_contract ();
  check_argmax_output_contract ()
