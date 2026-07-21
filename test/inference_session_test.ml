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
module Model = Octra_vm.Inference_model
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
    max_view_bytes = 0;
    max_session_bytes = 64;
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
  match Admission.of_program_with_requirement ~support ~requirement code with
  | Ok admitted -> admitted
  | Error error -> failwith (Admission.error_message error)

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
    ranges = [
      {
        owner_root;
        offset = 0;
        length = String.length owner;
        encoding = "octets";
        shape_root = None;
      };
    ];
  }

let request target =
  Request.{
    schema = 1;
    target_root = Target.root target;
    entrypoint = "advance";
    input_root = hex_root '4';
    request_nonce = hex_root '5';
    max_output_bytes = 32;
    max_advance_effort = 16;
  }

let pins model =
  match Store.pin ~limits ~read:(fun _ -> Some owner) model with
  | Ok pins -> pins
  | Error error -> failwith (Store.error_message error)

let open_session target request model =
  match Session.open_session ~target ~request ~pins:(pins model) with
  | Ok session -> session
  | Error error -> failwith (Session.error_message error)

let check_lifecycle () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let session = open_session target request model in
  check "initial sequence" (session.sequence = 0);
  match
    Session.advance
      ~admitted
      ~target
      ~request
      ~expected_sequence:0
      session
  with
  | Error error -> failwith (Session.error_message error)
  | Ok (advanced, receipt) ->
    check "advanced sequence" (advanced.sequence = 1);
    check "effort committed" (advanced.committed_effort > 0);
    check "receipt root" (String.length (Receipt.root receipt) = 64);
    match Session.finalize ~expected_sequence:1 advanced with
    | Error error -> failwith (Session.error_message error)
    | Ok (finalized, receipt) ->
      check "finalized sequence" (finalized.sequence = 2);
      check "final receipt root" (String.length (Receipt.root receipt) = 64)

let check_sequence_mismatch () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let session = open_session target request model in
  match
    Session.advance
      ~admitted
      ~target
      ~request
      ~expected_sequence:1
      session
  with
  | Error (Session.Bad_sequence (1, 0)) ->
    check "session unchanged" (session.sequence = 0)
  | _ -> failwith "expected sequence mismatch"

let check_cancel_terminal () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let session = open_session target request model in
  match Session.cancel ~expected_sequence:0 session with
  | Error error -> failwith (Session.error_message error)
  | Ok canceled ->
    (match Session.finalize ~expected_sequence:1 canceled with
     | Error Session.Terminal_session -> ()
     | _ -> failwith "expected terminal session")

let () =
  check_lifecycle ();
  check_sequence_mismatch ();
  check_cancel_terminal ()
