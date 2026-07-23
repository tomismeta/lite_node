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
module Deployment = Octra_vm.Inference_model_deployment
module Plan = Octra_vm.Inference_plan
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
    max_view_bytes = 16;
    max_session_bytes = 768;
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

let model target encoding =
  Model.{
    model_root = target.Target.model_root;
    store_root = target.Target.store_root;
    ranges = [{
      owner_root = sha256 "range owner";
      offset = 0;
      length = 11;
      encoding;
      shape_root = None;
    }];
  }

let pins model =
  match
    Store.pin
      ~limits
      ~read:(fun root ->
        if String.equal root (sha256 "range owner") then Some "range owner"
        else None)
      model
  with
  | Ok pins -> pins
  | Error error -> failwith (Store.error_message error)

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

let deployment target =
  Deployment.{
    model_root = target.Target.model_root;
    store_root = target.Target.store_root;
    tensor_index_root = hex_root '6';
    tokenizer_root = None;
    numerical_profile_root = requirement.numerical_root;
    capability_set_root = Deployment.capability_set_root requirement.capabilities;
    default_program_root = Some target.program_root;
  }

let make_plan ?deployment admitted target request model =
  match
    match deployment with
    | None ->
      Plan.create
        ~admitted
        ~target
        ~request
        ~model
        ~pins:(pins model)
        ~input:""
    | Some deployment ->
      Plan.create_with_deployment
        ~deployment
        ~admitted
        ~target
        ~request
        ~model
        ~pins:(pins model)
        ~input:""
  with
  | Ok plan -> plan
  | Error error -> failwith (Plan.error_message error)

let check_input_root () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target "octets" in
  match
    Plan.create
      ~admitted
      ~target
      ~request
      ~model
      ~pins:(pins model)
      ~input:"wrong input"
  with
  | Error (Plan.Input_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected input root rejection"

let check_pin_root () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let first_model = model target "octets" in
  let second_model = model target "tensor.q1-g128" in
  match
    Plan.create
      ~admitted
      ~target
      ~request
      ~model:first_model
      ~pins:(pins second_model)
      ~input:""
  with
  | Error (Plan.Pin_ranges_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected model ranges root rejection"

let check_deployment_root () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target "octets" in
  let deployment =
    Deployment.{ (deployment target) with model_root = hex_root '7' }
  in
  match
    Plan.create_with_deployment
      ~deployment
      ~admitted
      ~target
      ~request
      ~model
      ~pins:(pins model)
      ~input:""
  with
  | Error (Plan.Deployment_invalid _) -> ()
  | _ -> failwith "expected deployment root rejection"

let check_session_plan_identity () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let first_plan = make_plan admitted target request (model target "octets") in
  let second_plan = make_plan admitted target request (model target "tensor.q1-g128") in
  let session =
    match Session.open_session ~plan:first_plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  match Session.advance ~plan:second_plan ~expected_sequence:0 session with
  | Error (Session.Model_ranges_root_mismatch (_, _)) ->
    check "session remains open" (Session.sequence session = 0)
  | _ -> failwith "expected session plan identity rejection"

let check_session_plan_deployment_identity () =
  let admitted = admitted () in
  let target = target admitted in
  let request = request target in
  let model = model target "octets" in
  let first_plan =
    make_plan ~deployment:(deployment target) admitted target request model
  in
  let second_plan = make_plan admitted target request model in
  let session =
    match Session.open_session ~plan:first_plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  match Session.advance ~plan:second_plan ~expected_sequence:0 session with
  | Error (Session.Model_deployment_root_mismatch (_, _)) ->
    check "session remains open" (Session.sequence session = 0)
  | _ -> failwith "expected session deployment identity rejection"

let check_generic_admission_rejected () =
  match Admission.of_program [| VM.JDEST Abi.advance_label; VM.STOP |] with
  | Error error -> failwith (Admission.error_message error)
  | Ok generic ->
    let target = target (admitted ()) in
    let request = request target in
    let model = model target "octets" in
    match
      Plan.create
        ~admitted:generic
        ~target
        ~request
        ~model
        ~pins:(pins model)
        ~input:""
    with
    | Error Plan.Missing_requirement -> ()
    | _ -> failwith "expected generic admission rejection"

let () =
  check_input_root ();
  check_pin_root ();
  check_deployment_root ();
  check_session_plan_identity ();
  check_session_plan_deployment_identity ();
  check_generic_admission_rejected ()
