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
module Deployment = Octra_vm.Inference_model_deployment
module Plan = Octra_vm.Inference_plan
module Receipt = Octra_vm.Inference_receipt
module Req = Octra_vm.Execution_requirement
module Request = Octra_vm.Inference_request
module Abi = Octra_vm.Inference_session_abi
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

let admitted () =
  Inference_cert.admit
    ~support
    ~requirement
    [| VM.JDEST Abi.advance_label; VM.STOP |]

let target admitted =
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

let request target =
  Request.{
    schema = Abi.request_schema;
    target_root = Target.root target;
    entrypoint = Abi.advance_entrypoint;
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

let deployment target =
  Deployment.{
    model_root = target.Target.model_root;
    store_root = target.Target.store_root;
    tensor_index_root = hex_root '6';
    tokenizer_root = Some (hex_root '7');
    numerical_profile_root = requirement.numerical_root;
    capability_set_root = Deployment.capability_set_root requirement.capabilities;
    default_program_root = Some target.program_root;
  }

let open_session ?deployment admitted target request model =
  let pins =
    match Store.pin ~limits ~read:(fun root ->
      if String.equal root (sha256 "session range owner")
      then Some "session range owner"
      else None) model with
    | Ok pins -> pins
    | Error error -> failwith (Store.error_message error)
  in
  let plan =
    match
      match deployment with
      | None ->
        Plan.create ~admitted ~target ~request ~model ~pins ~input:""
      | Some deployment ->
        Plan.create_with_deployment
          ~deployment
          ~admitted
          ~target
          ~request
          ~model
          ~pins
          ~input:""
    with
    | Ok plan -> plan
    | Error error -> failwith (Plan.error_message error)
  in
  match Session.open_session ~plan with
  | Ok session -> session
  | Error error -> failwith (Session.error_message error)

let advance_session session admitted target request model =
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
  match Session.advance ~plan ~expected_sequence:0 session with
  | Ok value -> value
  | Error error -> failwith (Session.error_message error)

let check_vectors () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target in
  let deployment = deployment target in
  check
    "session ABI root vector"
    (String.equal
       Abi.v1_root
       "be55d94fec70093495473690eb303aa617162e322b76426205ecaa4617d1bb90");
  check
    "program root vector"
    (String.equal
       (Target.program_root admitted)
       "d49827e847610582dec5420dc11fdcbc9dbf91765a27195403f89765934db77e");
  check
    "requirement root vector"
    (String.equal
       (Req.root requirement)
       "fd73c1d4bca4d2277a2cef004b24951f3573b2fdfa3be0da39d156c7f524831d");
  check
    "capability set root vector"
    (String.equal
       (Req.capability_set_root requirement.capabilities)
       "5ab5d64c9522b7124a4bbfb9f6302eed8d016b0a3f642aa0cf30dd39ce57cd3a");
  check
    "target root vector"
    (String.equal
       (Target.root target)
       "b6a6431942fbb39d6fb6f604e7e0e3f538a8aecc079df729dc206bd5038aa5d2");
  check
    "request root vector"
    (String.equal
       (Request.root request)
       "6bf2ad8f48b0d6b96b598690c5a843b66d004f1e60e504092571493fb37417e3");
  check
    "model deployment root vector"
    (String.equal
       (Deployment.root deployment)
       "2a9851bbd4b3088d6a4af958c09b8d58a7ce275d87686fb5b3e287e2eead1106");
  check
    "session root vector"
    (String.equal
       (Session.root (open_session admitted target request model))
       "b3ec62bbd56fbce177675b4ac7f7b7df7d555f9db1bb2fdbdfb772c1c35b2a3a");
  check
    "deployment-bound session root vector"
    (String.equal
       (Session.root (open_session ~deployment admitted target request model))
       "dcb00397858745b916e4d6e460c2bd5b02c591d92cd2caec0b2c7f33b5f2f7db");
  let session = open_session admitted target request model in
  check
    "initial output root vector"
    (String.equal
       (Session.output_root session)
       "49041bfd69f0317c64f68481ede5d2b49089dd733c7ea28a5d75b2651f81a90d");
  check
    "initial output prefix root vector"
    (String.equal
       (Session.output_prefix_root session)
       "b6f9a0aeb15d85732bf6efb815a1c9aeda2a7bbd90246c5d8e967b2ef89864b2");
  let advanced, advance_receipt =
    advance_session session admitted target request model
  in
  check
    "advanced output root vector"
    (String.equal
       (Session.output_root advanced)
       "0578719e90075bd22092cb17d8645febc8317e67809e716a60b2fbbe10c0da99");
  check
    "advanced output prefix root vector"
    (String.equal
       (Session.output_prefix_root advanced)
       "b9cc7d335190d0b445061e07022f6193fa4a3585507702aab42b70ebaaf0bddb");
  check
    "advanced session root vector"
    (String.equal
       (Session.root advanced)
       "e811d07885985659e56fe9b41c2a5c056500528f33a9dbf226cac0b2c26f9e60");
  check
    "advance receipt root vector"
    (String.equal
       (Receipt.root advance_receipt)
       "50af739708a3360c31ce3ba8b138c64c8de5b19a66d57c4da17b51da3f4bab79");
  let receipt = Receipt.{
    target_root = "f7cfe92a4089b13cb7b6ff5eb4f28e39d7dd9c27fbfdddba71aeccd80394bc10";
    request_root = "0a54434081377bdfb3b5b967c60da45f7e2a314d2d610d4f54ce79ecc4793881";
    prior_session_root = "1262abf9cb4a95b07349d79b0646cca8ae09e7df7e12a484476bcbe0358f3d70";
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
       "46c51bbc35dd3e4d63a983ec4f90da5ce7f104db4d7a3d33bcd2cfee636554b4")

let () =
  check_vectors ()
