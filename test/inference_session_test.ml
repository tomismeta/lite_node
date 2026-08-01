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


module Model = Octra_vm.Inference_model
module Plan = Octra_vm.Inference_plan
module Receipt = Octra_vm.Inference_receipt
module Req = Octra_vm.Execution_requirement
module Request = Octra_vm.Inference_request
module Abi = Octra_vm.Inference_session_abi
module Execution = Octra_vm.Inference_execution
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

let capability name capability_root =
  Req.{ name; root = capability_root }

let limits =
  Req.{
    max_model_bytes = 64;
    max_view_bytes = 64;
    max_session_bytes = 768;
    max_scratch_bytes = 128;
    max_output_bytes = 32;
    max_advance_effort = 16;
  }

let requirement =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities = [capability "storage.authenticated-range" (hex_root 'd')];
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

let code = [| VM.JDEST Abi.advance_label; VM.STOP |]

let admitted () =
  Inference_cert.admit ~support ~requirement code

let target ?(requirement = requirement) ?(session_abi_root = Abi.v1_root) admitted =
  Target.{
    program_root = Target.program_root admitted;
    requirement_root = Req.root requirement;
    model_root = hex_root 'e';
    execution_descriptor_root = hex_root '1';
    store_root = hex_root '2';
    session_abi_root;
    entrypoints = [{
      entry_name = Abi.advance_entrypoint;
      entry_label = Abi.advance_label;
    }];
  }

let owner = "session range owner"
let owner_root = sha256 owner

let model target =
  Model.{
    model_root = target.Target.model_root;
    store_root = target.Target.store_root;
    ranges = [{
      owner_root;
      offset = 0;
      length = String.length owner;
      encoding = "octets";
      shape_root = None;
    }];
  }

let request ?(max_output_bytes = 32) ?(max_advance_effort = 16) target =
  Request.{
    schema = Abi.request_schema;
    target_root = Target.root target;
    entrypoint = Abi.advance_entrypoint;
    input_root = sha256 "";
    request_nonce = hex_root '5';
    max_output_bytes;
    max_advance_effort;
  }

let pins model =
  match Store.pin ~limits ~read:(fun _ -> Some owner) model with
  | Ok pins -> pins
  | Error error -> failwith (Store.error_message error)

let plan admitted target request model =
  match
    Plan.create
      ~admitted
      ~target
      ~request
      ~model
      ~pins:(pins model)
      ~input:""
  with
  | Ok plan -> plan
  | Error error -> failwith (Plan.error_message error)

let open_session admitted target request model =
  let plan = plan admitted target request model in
  match Session.open_session ~plan with
  | Ok session -> plan, session
  | Error error -> failwith (Session.error_message error)

let check_lifecycle () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let plan, session = open_session admitted target request model in
  check "initial sequence" (Session.sequence session = 0);
  check "initial position" (Session.logical_position session = 0);
  check
    "no committed target state"
    (Session.committed_target_state_root session = None);
  let open_output_prefix_root = Session.output_prefix_root session in
  match Session.advance ~plan ~expected_sequence:0 session with
  | Error error -> failwith (Session.error_message error)
  | Ok (advanced, receipt) ->
    check "advanced sequence" (Session.sequence advanced = 1);
    check "advanced position" (Session.logical_position advanced = 1);
    check
      "output prefix changed"
      (not
         (String.equal
            (Session.output_prefix_root advanced)
            open_output_prefix_root));
    check "effort committed" (Session.committed_effort advanced > 0);
    check "advance effort delta" (receipt.Receipt.effort_delta > 0);
    check "receipt root" (String.length (Receipt.root receipt) = 64);
    check "candidate root" (String.length (Session.candidate_root advanced) = 64);
    (match Session.advance ~plan ~expected_sequence:1 advanced with
     | Error (Session.Invalid_phase _) -> ()
     | _ -> failwith "expected stateful continuation rejection");
    match Session.finalize ~expected_sequence:1 advanced with
    | Error error -> failwith (Session.error_message error)
    | Ok (finalized, receipt) ->
      check "finalized sequence" (Session.sequence finalized = 2);
      check "finalize effort delta" (receipt.Receipt.effort_delta = 0);
      check "final receipt root" (String.length (Receipt.root receipt) = 64)

let continuation_limits =
  Req.{
    limits with
    max_session_bytes = 1024;
    max_scratch_bytes = 1024;
    max_output_bytes = 512;
    max_advance_effort = 128;
  }

let continuation_requirement =
  Req.{ requirement with limits = continuation_limits }

let continuation_support =
  Req.{ support with support_limits = continuation_limits }

let continuation_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.MLOAD (2, Abi.sequence_cell);
    VM.MSTORE (10, 2);
    VM.MLOAD (2, Abi.logical_position_cell);
    VM.MSTORE (11, 2);
    VM.MLOAD (2, Abi.output_root_cell);
    VM.MSTORE (12, 2);
    VM.MLOAD (2, Abi.output_prefix_root_cell);
    VM.MSTORE (13, 2);
    VM.MLOAD (2, Abi.committed_target_state_root_cell);
    VM.MSTORE (14, 2);
    VM.LDI (0, VM.VInt (Z.of_int 10));
    VM.LDI (1, VM.VInt (Z.of_int 5));
    VM.STOP;
  |]

let committed_state_payload = "state:0"
let committed_state_payload_root = sha256 committed_state_payload

let committed_state_limits =
  Req.{
    continuation_limits with
    max_session_bytes = 4096;
    max_scratch_bytes = 4096;
    max_output_bytes = 512;
    max_advance_effort = 4096;
  }

let committed_state_requirement =
  Req.{
    continuation_requirement with
    capabilities =
      continuation_requirement.capabilities
      @ [capability "session.committed-state" (hex_root '6')];
    limits = committed_state_limits;
  }

let committed_state_support =
  Req.{
    continuation_support with
    support_capabilities = committed_state_requirement.capabilities;
    support_limits = committed_state_limits;
  }

let committed_state_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.MLOAD (20, Abi.sequence_cell);
    VM.LDI (21, VM.VInt Z.zero);
    VM.EQ (22, 20, 21);
    VM.JIF (22, 101);
    VM.MLOAD (5, Abi.committed_target_state_root_cell);
    VM.FLOAD (6, 5);
    VM.MSTORE (30, 6);
    VM.LDI (0, VM.VInt (Z.of_int 30));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
    VM.JDEST 101;
    VM.LDI (5, VM.VString committed_state_payload);
    VM.FSTORE (6, 5);
    VM.MSTORE (Abi.committed_target_state_root_cell, 6);
    VM.MSTORE (30, 5);
    VM.LDI (0, VM.VInt (Z.of_int 30));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let committed_state_bad_cell_code =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (5, VM.VInt Z.one);
    VM.MSTORE (Abi.committed_target_state_root_cell, 5);
    VM.MSTORE (30, 5);
    VM.LDI (0, VM.VInt (Z.of_int 30));
    VM.LDI (1, VM.VInt Z.one);
    VM.STOP;
  |]

let committed_state_plan ?(max_session_bytes = 4096)
    ?(code = committed_state_code) () =
  let limits =
    Req.{ committed_state_limits with max_session_bytes }
  in
  let requirement =
    Req.{ committed_state_requirement with limits }
  in
  let support =
    Req.{
      committed_state_support with
      support_capabilities = requirement.capabilities;
      support_limits = limits;
    }
  in
  let admitted =
    Inference_cert.admit ~support ~requirement code
  in
  let target =
    target
      ~requirement
      ~session_abi_root:Abi.committed_state_root
      admitted
  in
  let request =
    request
      ~max_output_bytes:512
      ~max_advance_effort:4096
      target
  in
  let model = model target in
  plan admitted target request model

let committed_state_advance_result max_session_bytes =
  let plan =
    committed_state_plan ~max_session_bytes ()
  in
  match Session.open_session ~plan with
  | Error error -> `Open_error error
  | Ok session ->
    match Session.advance ~plan ~expected_sequence:0 session with
    | Error error -> `Advance_error (session, error)
    | Ok (advanced, _) -> `Advanced advanced

let check_v2_continuation_lifecycle () =
  let admitted =
    Inference_cert.admit
      ~support:continuation_support
      ~requirement:continuation_requirement
      continuation_code
  in
  let target =
    target
      ~requirement:continuation_requirement
      ~session_abi_root:Abi.v2_root
      admitted
  in
  let request =
    request
      ~max_output_bytes:512
      ~max_advance_effort:128
      target
  in
  let model = model target in
  let plan, session = open_session admitted target request model in
  let open_output_prefix_root = Session.output_prefix_root session in
  let first =
    match Session.advance ~plan ~expected_sequence:0 session with
    | Ok (advanced, receipt) ->
      check "first sequence" (Session.sequence advanced = 1);
      check "first position" (Session.logical_position advanced = 1);
      check
        "first committed state remains absent"
        (Session.committed_target_state_root advanced = None);
      check "first receipt delta" (receipt.Receipt.effort_delta > 0);
      check
        "first output prefix changed"
        (not
           (String.equal
              (Session.output_prefix_root advanced)
              open_output_prefix_root));
      advanced
    | Error error -> failwith (Session.error_message error)
  in
  let profile =
    {
      Execution.clock = (fun () -> 0.0);
      opcode_name = (function
        | VM.MLOAD _ -> "MLOAD"
        | VM.STOP -> "STOP"
        | _ -> "other");
    }
  in
  (match
     Session.advance_profiled
       ~profile
       ~plan
       ~expected_sequence:0
       session
   with
  | Error error -> failwith (Session.error_message error)
  | Ok (profiled, _, execution_profile, _) ->
     check
       "v2 profiled output root"
       (String.equal (Session.output_root profiled) (Session.output_root first));
     check
       "v2 profiled candidate root"
       (String.equal
          (Session.candidate_root profiled)
          (Session.candidate_root first));
     check
       "v2 profiled context phase"
       (List.exists
          (fun (row : Execution.execution_profile) ->
             String.equal row.phase "bind_session_context")
          execution_profile));
  let first_output_root = Session.output_root first in
  check
    "v2 first output payload retained"
    (match Session.output_payload first with
     | Some _ -> true
     | None -> false);
  check
    "v2 first session root vector"
    (String.equal
       (Session.root first)
       "95ffc0a6f5561d15bc1cc48773a2deed45dddf06366b85f9b53668549dee5e90");
  check
    "v2 first output root vector"
    (String.equal
       first_output_root
       "f0a9f45f415c4f07b5bfe7d96d10a5a081fcab489fc3c79d993a6d1cd85c4701");
  let second =
    match Session.advance ~plan ~expected_sequence:1 first with
    | Ok (advanced, receipt) ->
      check "second sequence" (Session.sequence advanced = 2);
      check "second position" (Session.logical_position advanced = 2);
      check
        "second committed state remains absent"
        (Session.committed_target_state_root advanced = None);
      check "second receipt delta" (receipt.Receipt.effort_delta > 0);
      check
        "second output changed"
        (not (String.equal (Session.output_root advanced) first_output_root));
      check
        "candidate is not committed state"
        (Session.committed_target_state_root advanced = None);
      advanced
    | Error error -> failwith (Session.error_message error)
  in
  check
    "v2 second output payload retained"
    (match Session.output_payload second with
     | Some _ -> true
     | None -> false);
  check
    "v2 second session root vector"
    (String.equal
       (Session.root second)
       "5035bb88cca463399860005b9bf63fc8b1b4d5ba172b4a78e84332c684924fdc");
  check
    "v2 second output root vector"
    (String.equal
       (Session.output_root second)
       "068f827975f8fefdb63835e8e960be297c3a8f96e1780cae6e9c125f45ba1f45");
  match Session.finalize ~expected_sequence:2 second with
  | Error error -> failwith (Session.error_message error)
  | Ok (finalized, receipt) ->
    check "v2 finalized sequence" (Session.sequence finalized = 3);
    check "v2 finalize delta" (receipt.Receipt.effort_delta = 0)

let check_committed_state_lifecycle () =
  let admitted =
    Inference_cert.admit
      ~support:committed_state_support
      ~requirement:committed_state_requirement
      committed_state_code
  in
  let target =
    target
      ~requirement:committed_state_requirement
      ~session_abi_root:Abi.committed_state_root
      admitted
  in
  let request =
    request
      ~max_output_bytes:512
      ~max_advance_effort:4096
      target
  in
  let model = model target in
  let plan, session = open_session admitted target request model in
  check
    "committed-state ABI is new"
    (not (String.equal Abi.committed_state_root Abi.v2_root));
  let expected_payload =
    "base=30|length=1|values=string:7:" ^ committed_state_payload
  in
  let first =
    match Session.advance ~plan ~expected_sequence:0 session with
    | Ok (advanced, receipt) ->
      check "first committed sequence" (Session.sequence advanced = 1);
      check
        "first committed root"
        (Session.committed_target_state_root advanced
         = Some committed_state_payload_root);
      check
        "first committed payload"
        (Session.committed_target_state_payload advanced
         = Some committed_state_payload);
      check
        "first committed output"
        (Session.output_payload advanced = Some expected_payload);
      check "first committed receipt" (receipt.Receipt.effort_delta > 0);
      advanced
    | Error error -> failwith (Session.error_message error)
  in
  let second =
    match Session.advance ~plan ~expected_sequence:1 first with
    | Ok (advanced, receipt) ->
      check "second committed sequence" (Session.sequence advanced = 2);
      check
        "second committed root carried"
        (Session.committed_target_state_root advanced
         = Some committed_state_payload_root);
      check
        "second committed payload carried"
        (Session.committed_target_state_payload advanced
         = Some committed_state_payload);
      check
        "second committed output reloaded"
        (Session.output_payload advanced = Some expected_payload);
      check "second committed receipt" (receipt.Receipt.effort_delta > 0);
      advanced
    | Error error -> failwith (Session.error_message error)
  in
  match Session.finalize ~expected_sequence:2 second with
  | Error error -> failwith (Session.error_message error)
  | Ok (finalized, receipt) ->
    check "committed finalized sequence" (Session.sequence finalized = 3);
    check
      "committed finalized root retained"
      (Session.committed_target_state_root finalized
       = Some committed_state_payload_root);
    check
      "committed finalized payload retained"
      (Session.committed_target_state_payload finalized
       = Some committed_state_payload);
    check "committed finalize delta" (receipt.Receipt.effort_delta = 0)

let check_committed_state_session_limit_boundary () =
  let rec first_open_success limit =
    if limit > 4096 then failwith "missing committed-state open threshold"
    else
      match committed_state_advance_result limit with
      | `Open_error _ -> first_open_success (limit + 1)
      | `Advance_error _
      | `Advanced _ -> limit
  in
  let rec first_advance_success limit =
    if limit > 4096 then failwith "missing committed-state success threshold"
    else
      match committed_state_advance_result limit with
      | `Advanced _ -> limit
      | `Open_error _
      | `Advance_error _ -> first_advance_success (limit + 1)
  in
  let open_threshold = first_open_success 1 in
  let threshold = first_advance_success 1 in
  check
    "committed-state payload increases session bound"
    (threshold > open_threshold);
  (match committed_state_advance_result threshold with
   | `Advanced advanced ->
     check
       "committed-state boundary retains payload"
       (Session.committed_target_state_payload advanced
        = Some committed_state_payload)
   | _ -> failwith "expected committed-state boundary success");
  match committed_state_advance_result (threshold - 1) with
  | `Advance_error (_, Session.Session_limit_exceeded _) -> ()
  | `Open_error _ -> failwith "expected advance-time session limit rejection"
  | `Advanced _ -> failwith "expected one-byte session limit rejection"
  | `Advance_error (_, error) -> failwith (Session.error_message error)

let check_committed_state_failed_advance_is_atomic () =
  let plan =
    committed_state_plan
      ~code:committed_state_bad_cell_code
      ()
  in
  let session =
    match Session.open_session ~plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  let prior_root = Session.root session in
  match Session.advance ~plan ~expected_sequence:0 session with
  | Error (Session.Execution_error _) ->
    check
      "failed committed advance root unchanged"
      (String.equal (Session.root session) prior_root);
    check "failed committed sequence unchanged" (Session.sequence session = 0);
    check
      "failed committed payload absent"
      (Session.committed_target_state_payload session = None)
  | Error error -> failwith (Session.error_message error)
  | Ok _ -> failwith "expected committed-state bad-cell rejection"

let check_profiled_advance_equivalence () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let plan, session = open_session admitted target request model in
  let profile =
    {
      Execution.clock = (fun () -> 0.0);
      opcode_name = (function
        | VM.JDEST _ -> "JDEST"
        | VM.STOP -> "STOP"
        | _ -> "other");
    }
  in
  match
    Session.advance ~plan ~expected_sequence:0 session,
    Session.advance_profiled ~profile ~plan ~expected_sequence:0 session
  with
  | Ok (plain, plain_receipt),
    Ok (profiled, profiled_receipt, execution_profile, opcode_profile) ->
    check "profiled sequence" (Session.sequence profiled = Session.sequence plain);
    check
      "profiled position"
      (Session.logical_position profiled = Session.logical_position plain);
    check
      "profiled output root"
      (String.equal (Session.output_root profiled) (Session.output_root plain));
    check
      "profiled output prefix root"
      (String.equal
         (Session.output_prefix_root profiled)
         (Session.output_prefix_root plain));
    check
      "profiled candidate root"
      (String.equal
         (Session.candidate_root profiled)
         (Session.candidate_root plain));
    check
      "profiled effort"
      (Session.committed_effort profiled = Session.committed_effort plain);
    check
      "profiled receipt root"
      (String.equal (Receipt.root profiled_receipt) (Receipt.root plain_receipt));
    check
      "profiled receipt effort"
      (profiled_receipt.Receipt.effort_delta = plain_receipt.effort_delta);
    check
      "profiled execution phases"
      (List.exists
         (fun (row : Execution.execution_profile) ->
           String.equal row.phase "vm_run")
         execution_profile);
    check
      "profiled opcode rows"
      (List.exists
         (fun (row : VM.opcode_profile) -> String.equal row.opcode "STOP")
         opcode_profile)
  | Error error, _ -> failwith (Session.error_message error)
  | _, Error error -> failwith (Session.error_message error)

let check_sequence_mismatch () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let plan, session = open_session admitted target request model in
  match Session.advance ~plan ~expected_sequence:1 session with
  | Error (Session.Bad_sequence (1, 0)) ->
    check "session unchanged" (Session.sequence session = 0)
  | _ -> failwith "expected sequence mismatch"

let check_finalize_phase () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let _, session = open_session admitted target request model in
  match Session.finalize ~expected_sequence:0 session with
  | Error (Session.Invalid_phase _) -> ()
  | _ -> failwith "expected finalize phase rejection"

let check_cancel_terminal () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let _, session = open_session admitted target request model in
  match Session.cancel ~expected_sequence:0 session with
  | Error error -> failwith (Session.error_message error)
  | Ok canceled ->
    (match Session.finalize ~expected_sequence:1 canceled with
     | Error Session.Terminal_session -> ()
     | _ -> failwith "expected terminal session")

let limited_plan max_session_bytes =
  let limits = Req.{ limits with max_session_bytes } in
  let requirement = Req.{ requirement with limits } in
  let support = Req.{ support with support_limits = limits } in
  let admitted =
    Inference_cert.admit ~support ~requirement code
  in
  let target =
    Target.{
      program_root = Target.program_root admitted;
      requirement_root = Req.root requirement;
      model_root = hex_root 'e';
      execution_descriptor_root = hex_root '1';
      store_root = hex_root '2';
      session_abi_root = Abi.v1_root;
      entrypoints = [{
        entry_name = Abi.advance_entrypoint;
        entry_label = Abi.advance_label;
      }];
    }
  in
  let request = request target in
  let model = model target in
  match
    Plan.create
      ~admitted
      ~target
      ~request
      ~model
      ~pins:(pins model)
      ~input:""
  with
  | Ok plan -> plan
  | Error error -> failwith (Plan.error_message error)

let check_session_open_limit () =
  let plan = limited_plan 1 in
  match Session.open_session ~plan with
  | Error (Session.Session_limit_exceeded (_, 1)) -> ()
  | _ -> failwith "expected open session limit rejection"

let check_session_advance_limit () =
  let plan = limited_plan 525 in
  let session =
    match Session.open_session ~plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  match Session.advance ~plan ~expected_sequence:0 session with
  | Error (Session.Session_limit_exceeded (_, 525)) -> ()
  | _ -> failwith "expected advance session limit rejection"

let check_session_finalize_limit () =
  let plan = limited_plan 529 in
  let session =
    match Session.open_session ~plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  let advanced =
    match Session.advance ~plan ~expected_sequence:0 session with
    | Ok (advanced, _) -> advanced
    | Error error -> failwith (Session.error_message error)
  in
  match Session.finalize ~expected_sequence:1 advanced with
  | Error (Session.Session_limit_exceeded (_, 529)) -> ()
  | _ -> failwith "expected finalize session limit rejection"

let check_session_cancel_limit () =
  let plan = limited_plan 525 in
  let session =
    match Session.open_session ~plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  match Session.cancel ~expected_sequence:0 session with
  | Error (Session.Session_limit_exceeded (_, 525)) -> ()
  | _ -> failwith "expected cancel session limit rejection"

let () =
  check_lifecycle ();
  check_v2_continuation_lifecycle ();
  check_committed_state_lifecycle ();
  check_committed_state_session_limit_boundary ();
  check_committed_state_failed_advance_is_atomic ();
  check_profiled_advance_equivalence ();
  check_sequence_mismatch ();
  check_finalize_phase ();
  check_cancel_terminal ();
  check_session_open_limit ();
  check_session_advance_limit ();
  check_session_finalize_limit ();
  check_session_cancel_limit ()
