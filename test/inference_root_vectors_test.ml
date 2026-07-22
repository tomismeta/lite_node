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
  Req.{ name = name; root = capability_root }

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

let admitted () =
  Inference_cert.admit ~support ~requirement [| VM.JDEST 100; VM.STOP |]

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

let model target =
  let owner = "session range owner" in
  let owner_root = sha256 owner in
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

let open_session admitted target request model =
  let pins =
    match Store.pin ~limits ~read:(fun root ->
      if String.equal root (sha256 "session range owner")
      then Some "session range owner"
      else None) model with
    | Ok pins -> pins
    | Error error -> failwith (Store.error_message error)
  in
  let plan =
    match Plan.create ~admitted ~target ~request ~model ~pins ~input:"" with
    | Ok plan -> plan
    | Error error -> failwith (Plan.error_message error)
  in
  match Session.open_session ~plan with
  | Ok session -> session
  | Error error -> failwith (Session.error_message error)

let check_vectors () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  check
    "program root vector"
    (String.equal
       (Target.program_root admitted)
       "d49827e847610582dec5420dc11fdcbc9dbf91765a27195403f89765934db77e");
  check
    "requirement root vector"
    (String.equal
       (Req.root requirement)
       "50e2dd63cf246f9d30c3f383b293f6aad6ead6282b55fb183c32c1387db9f9b7");
  check
    "target root vector"
    (String.equal
       (Target.root target)
       "adba72e6be085d6685b77aa4cc2bd0ce4519dba4527261a176da03afa4a0ef5c");
  check
    "request root vector"
    (String.equal
       (Request.root request)
       "35470aa3c2da7e284fe675bf59bc5fa4e5ba5b611226a90633c881d671ddd2e8");
  check
    "session root vector"
    (String.equal
       (Session.root (open_session admitted target request model))
       "892fc03d17f77ef2010ae701a0d8b6aff097c9369b563c55646fc91c34451361");
  let receipt = Receipt.{
    target_root = "adba72e6be085d6685b77aa4cc2bd0ce4519dba4527261a176da03afa4a0ef5c";
    request_root = "35470aa3c2da7e284fe675bf59bc5fa4e5ba5b611226a90633c881d671ddd2e8";
    prior_session_root = "892fc03d17f77ef2010ae701a0d8b6aff097c9369b563c55646fc91c34451361";
    next_session_root = "6ec716d43d7d07ee63df75a1fce38d0423c11444523b54ac9249ddec153a5a86";
    prior_sequence = 0;
    next_sequence = 1;
    output_root = hex_root 'f';
    candidate_root = hex_root '0';
    effort_delta = 123;
    completion_status = "advanced";
    consensus_accepted = false;
  } in
  check
    "receipt root vector"
    (String.equal
       (Receipt.root receipt)
       "9e5f1ff8196198ed626f4884ac661d0e87b998d6e32d6208b66bbf4b88299b47")

let () =
  check_vectors ()
