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
module Req = Octra_vm.Execution_requirement
module Request = Octra_vm.Inference_request
module Target = Octra_vm.Inference_target
module VM = Octra_vm.Contract_vm

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

let capability name capability_root =
  Req.{ name; root = capability_root }

let limits =
  Req.{
    max_model_bytes = 100;
    max_view_bytes = 80;
    max_session_bytes = 60;
    max_scratch_bytes = 40;
    max_output_bytes = 20;
    max_advance_effort = 10;
  }

let support =
  Req.{
    support_vm_semantics_root = hex_root 'a';
    support_numerical_roots = [hex_root 'b'];
    support_effort_roots = [hex_root 'c'];
    support_capabilities = [
      capability "tensor.fixed" (hex_root 'd');
      capability "storage.authenticated-range" (hex_root 'e');
    ];
    support_limits = {
      max_model_bytes = 200;
      max_view_bytes = 160;
      max_session_bytes = 120;
      max_scratch_bytes = 80;
      max_output_bytes = 40;
      max_advance_effort = 20;
    };
  }

let requirement =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities = support.support_capabilities;
    limits;
  }

let code = [| VM.JDEST 100; VM.STOP |]

let admitted () =
  match Admission.of_program_with_requirement ~support ~requirement code with
  | Ok admitted -> admitted
  | Error error -> failwith (Admission.error_message error)

let target () =
  let admitted = admitted () in
  Target.{
    program_root = Target.program_root admitted;
    requirement_root = Req.root requirement;
    model_root = hex_root 'f';
    execution_descriptor_root = hex_root '1';
    store_root = hex_root '2';
    session_abi_root = hex_root '3';
    entrypoints = [{ entry_name = "advance"; entry_label = 100 }];
  }

let request () =
  Request.{
    schema = 1;
    target_root = Target.root (target ());
    entrypoint = "advance";
    input_root = hex_root '5';
    request_nonce = hex_root '4';
    max_output_bytes = 20;
    max_advance_effort = 10;
  }

let check_root_fixture () =
  check "request root fixture"
    (String.equal
       (Request.root (request ()))
       "aa3e73c5af4cb387f7bd02cf05d1da9eab329e9d3988cea26a74a2a602274919")

let check_supported () =
  match Request.check ~target:(target ()) ~requirement (request ()) with
  | Ok () -> ()
  | Error error -> failwith (Request.error_message error)

let check_target_root_mismatch () =
  let request = request () in
  let request = Request.{ request with target_root = hex_root '6' } in
  match Request.check ~target:(target ()) ~requirement request with
  | Error (Request.Target_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected target root mismatch"

let check_requirement_root_mismatch () =
  let target = target () in
  let target = Target.{ target with requirement_root = hex_root '6' } in
  let request = request () in
  let request = Request.{ request with target_root = Target.root target } in
  match Request.check ~target ~requirement request with
  | Error (Request.Requirement_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected requirement root mismatch"

let check_missing_entrypoint () =
  let request = request () in
  let request = Request.{ request with entrypoint = "prefill" } in
  match Request.check ~target:(target ()) ~requirement request with
  | Error (Request.Entrypoint_unsupported "prefill") -> ()
  | _ -> failwith "expected unsupported entrypoint"

let check_output_limit () =
  let request = request () in
  let request = Request.{ request with max_output_bytes = 21 } in
  match Request.check ~target:(target ()) ~requirement request with
  | Error (Request.Limit_exceeded ("max_output_bytes", 21, 20)) -> ()
  | _ -> failwith "expected output limit rejection"

let check_effort_limit () =
  let request = request () in
  let request = Request.{ request with max_advance_effort = 11 } in
  match Request.check ~target:(target ()) ~requirement request with
  | Error (Request.Limit_exceeded ("max_advance_effort", 11, 10)) -> ()
  | _ -> failwith "expected effort limit rejection"

let check_bad_schema () =
  let request = request () in
  let request = Request.{ request with schema = 0 } in
  match Request.check ~target:(target ()) ~requirement request with
  | Error (Request.Bad_schema 0) -> ()
  | _ -> failwith "expected bad schema"

let check_bad_name () =
  let request = request () in
  let request = Request.{ request with entrypoint = "Advance" } in
  match Request.check ~target:(target ()) ~requirement request with
  | Error (Request.Bad_name "Advance") -> ()
  | _ -> failwith "expected bad entrypoint name"

let check_bad_limit () =
  let request = request () in
  let request = Request.{ request with max_output_bytes = -1 } in
  match Request.check ~target:(target ()) ~requirement request with
  | Error (Request.Bad_limit ("max_output_bytes", -1)) -> ()
  | _ -> failwith "expected bad request limit"

let () =
  check_root_fixture ();
  check_supported ();
  check_target_root_mismatch ();
  check_requirement_root_mismatch ();
  check_missing_entrypoint ();
  check_output_limit ();
  check_effort_limit ();
  check_bad_schema ();
  check_bad_name ();
  check_bad_limit ()
