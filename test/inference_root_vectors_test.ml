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

let admitted () =
  match Admission.of_inference_program_with_requirement
          ~support
          ~requirement
          [| VM.JDEST 100; VM.STOP |]
  with
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
       "1fd690bdc8dd34f3eddb0d2470e5c96aa99f652265fa2fd9c0e891674407ed15");
  check
    "target root vector"
    (String.equal
       (Target.root target)
       "9fd1882ba34b8d7437368ef13bf6e2381f601e59e0460960af13d7c455c8e9bc");
  check
    "request root vector"
    (String.equal
       (Request.root request)
       "b38f2736ade58dbdb9e4743687fd314708969fededf201421cf8b7d70f5498ef");
  check
    "session root vector"
    (String.equal
       (Session.root (open_session admitted target request model))
       "ef34b7444f5922f0843c9d9ecf18042b7315bc4ea2eb71b7bf66f044ee7a59ef");
  let receipt = Receipt.{
    target_root = "9fd1882ba34b8d7437368ef13bf6e2381f601e59e0460960af13d7c455c8e9bc";
    request_root = "b38f2736ade58dbdb9e4743687fd314708969fededf201421cf8b7d70f5498ef";
    prior_session_root = "ef34b7444f5922f0843c9d9ecf18042b7315bc4ea2eb71b7bf66f044ee7a59ef";
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
       "0cdfd17910def59c8ddd04146a033deb893fda25e41897f4a46103e240585c8a")

let () =
  check_vectors ()
