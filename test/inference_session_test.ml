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
    max_session_bytes = 512;
    max_scratch_bytes = 0;
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

let code = [| VM.JDEST 100; VM.STOP |]

let admitted () =
  Inference_cert.admit ~support ~requirement code

let target admitted =
  Target.{
    program_root = Target.program_root admitted;
    requirement_root = Req.root requirement;
    model_root = hex_root 'e';
    execution_descriptor_root = hex_root '1';
    store_root = hex_root '2';
    session_abi_root = hex_root '3';
    entrypoints = [{ entry_name = "advance"; entry_label = 100 }];
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

let request target =
  Request.{
    schema = 1;
    target_root = Target.root target;
    entrypoint = "advance";
    input_root = sha256 "";
    request_nonce = hex_root '5';
    max_output_bytes = 32;
    max_advance_effort = 16;
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
  match Session.advance ~plan ~expected_sequence:0 session with
  | Error error -> failwith (Session.error_message error)
  | Ok (advanced, receipt) ->
    check "advanced sequence" (Session.sequence advanced = 1);
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
      session_abi_root = hex_root '3';
      entrypoints = [{ entry_name = "advance"; entry_label = 100 }];
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
  let plan = limited_plan 381 in
  let session =
    match Session.open_session ~plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  match Session.advance ~plan ~expected_sequence:0 session with
  | Error (Session.Session_limit_exceeded (_, 381)) -> ()
  | _ -> failwith "expected advance session limit rejection"

let check_session_finalize_limit () =
  let plan = limited_plan 385 in
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
  | Error (Session.Session_limit_exceeded (_, 385)) -> ()
  | _ -> failwith "expected finalize session limit rejection"

let check_session_cancel_limit () =
  let plan = limited_plan 381 in
  let session =
    match Session.open_session ~plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  match Session.cancel ~expected_sequence:0 session with
  | Error (Session.Session_limit_exceeded (_, 381)) -> ()
  | _ -> failwith "expected cancel session limit rejection"

let () =
  check_lifecycle ();
  check_sequence_mismatch ();
  check_finalize_phase ();
  check_cancel_terminal ();
  check_session_open_limit ();
  check_session_advance_limit ();
  check_session_finalize_limit ();
  check_session_cancel_limit ()
