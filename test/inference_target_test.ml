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

let support_limits =
  Req.{
    max_model_bytes = 200;
    max_view_bytes = 160;
    max_session_bytes = 120;
    max_scratch_bytes = 80;
    max_output_bytes = 40;
    max_advance_effort = 20;
  }

let requirement =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities = [
      capability "tensor.fixed" (hex_root 'd');
      capability "storage.authenticated-range" (hex_root 'e');
    ];
    limits;
  }

let support =
  Req.{
    support_vm_semantics_root = hex_root 'a';
    support_numerical_roots = [hex_root 'b'];
    support_effort_roots = [hex_root 'c'];
    support_capabilities = requirement.capabilities;
    support_limits;
  }

let code = [| VM.JDEST 100; VM.STOP |]

let admitted () =
  Inference_cert.admit ~support ~requirement code

let target admitted =
  Target.{
    program_root = Target.program_root admitted;
    requirement_root = Req.root requirement;
    model_root = hex_root 'f';
    execution_descriptor_root = hex_root '1';
    store_root = hex_root '2';
    session_abi_root = hex_root '3';
    entrypoints = [
      { entry_name = "advance"; entry_label = 100 };
      { entry_name = "status"; entry_label = 100 };
    ];
  }

let check_root_order () =
  let admitted = admitted () in
  let target = target admitted in
  let left = Target.root target in
  let right =
    Target.root
      Target.{ target with entrypoints = List.rev target.entrypoints }
  in
  check "target root sorts entrypoints" (String.equal left right)

let check_supported () =
  let admitted = admitted () in
  match Target.check ~admitted (target admitted) with
  | Ok () -> ()
  | Error error -> failwith (Target.error_message error)

let check_entry_label () =
  let admitted = admitted () in
  let target = target admitted in
  match Target.entry_label target "advance" with
  | Some 100 -> ()
  | _ -> failwith "missing advance entrypoint"

let check_program_root_mismatch () =
  let admitted = admitted () in
  let target = target admitted in
  let target = Target.{ target with program_root = hex_root '4' } in
  match Target.check ~admitted target with
  | Error (Target.Program_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected program root mismatch"

let check_requirement_root_mismatch () =
  let admitted = admitted () in
  let target = target admitted in
  let target = Target.{ target with requirement_root = hex_root '5' } in
  match Target.check ~admitted target with
  | Error (Target.Requirement_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected requirement root mismatch"

let check_missing_requirement () =
  let expected = target (admitted ()) in
  match Admission.of_program code with
  | Error error -> failwith (Admission.error_message error)
  | Ok admitted ->
    (match Target.check ~admitted expected with
     | Error Target.Missing_requirement -> ()
     | _ -> failwith "expected missing requirement")

let check_uncertified_program () =
  let expected = target (admitted ()) in
  match Admission.of_inference_code_with_requirement ~support ~requirement code with
  | Error error -> failwith (Admission.error_message error)
  | Ok admitted ->
    (match Target.check ~admitted expected with
     | Error Target.Uncertified_program -> ()
     | _ -> failwith "expected uncertified inference program")

let check_missing_entrypoint_label () =
  let admitted = admitted () in
  let target = target admitted in
  let target =
    Target.{
      target with
      entrypoints = [{ entry_name = "advance"; entry_label = 101 }];
    }
  in
  match Target.check ~admitted target with
  | Error (Target.Uncertified_entrypoint ("advance", 101)) -> ()
  | _ -> failwith "expected uncertified entrypoint label"

let check_duplicate_entrypoint () =
  let admitted = admitted () in
  let target = target admitted in
  let target =
    Target.{
      target with
      entrypoints = [
        { entry_name = "advance"; entry_label = 100 };
        { entry_name = "advance"; entry_label = 100 };
      ];
    }
  in
  match Target.check ~admitted target with
  | Error (Target.Duplicate_entrypoint "advance") -> ()
  | _ -> failwith "expected duplicate entrypoint"

let check_bad_entrypoint_name () =
  let admitted = admitted () in
  let target = target admitted in
  let target =
    Target.{
      target with
      entrypoints = [{ entry_name = "Advance"; entry_label = 100 }];
    }
  in
  match Target.check ~admitted target with
  | Error (Target.Bad_name "Advance") -> ()
  | _ -> failwith "expected bad entrypoint name"

let () =
  check_root_order ();
  check_supported ();
  check_entry_label ();
  check_program_root_mismatch ();
  check_requirement_root_mismatch ();
  check_missing_requirement ();
  check_uncertified_program ();
  check_missing_entrypoint_label ();
  check_duplicate_entrypoint ();
  check_bad_entrypoint_name ()
