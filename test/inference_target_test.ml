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
module Abi = Octra_vm.Inference_session_abi
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

let committed_state_capability =
  capability "session.committed-state" (hex_root '6')

let support =
  Req.{
    support_vm_semantics_root = hex_root 'a';
    support_numerical_roots = [hex_root 'b'];
    support_effort_roots = [hex_root 'c'];
    support_capabilities = requirement.capabilities;
    support_limits;
  }

let code = [| VM.JDEST Abi.advance_label; VM.STOP |]

let admitted () =
  Inference_cert.admit ~support ~requirement code

let target ?(session_abi_root = Abi.v1_root) admitted =
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

let check_supported () =
  let admitted = admitted () in
  check
    "checked provenance"
    (Admission.provenance admitted = Admission.Checked_envelope);
  match Target.check ~admitted (target admitted) with
  | Ok () -> ()
  | Error error -> failwith (Target.error_message error)

let check_supported_v2_session_abi () =
  let admitted = admitted () in
  match Target.check ~admitted (target ~session_abi_root:Abi.v2_root admitted) with
  | Ok () -> ()
  | Error error -> failwith (Target.error_message error)

let check_committed_state_session_abi_requires_capability () =
  let admitted = admitted () in
  match
    Target.check
      ~admitted
      (target ~session_abi_root:Abi.committed_state_root admitted)
  with
  | Error (Target.Missing_required_capability "session.committed-state") -> ()
  | _ -> failwith "expected committed-state capability requirement"

let check_committed_state_session_abi_supported_with_capability () =
  let requirement =
    Req.{
      requirement with
      capabilities = requirement.capabilities @ [committed_state_capability];
    }
  in
  let support =
    Req.{ support with support_capabilities = requirement.capabilities }
  in
  let admitted = Inference_cert.admit ~support ~requirement code in
  match
    Target.check
      ~admitted
      Target.{
        (target ~session_abi_root:Abi.committed_state_root admitted) with
        requirement_root = Req.root requirement;
      }
  with
  | Ok () -> ()
  | Error error -> failwith (Target.error_message error)

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

let check_raw_program_rejected () =
  let expected = target (admitted ()) in
  match Admission.of_inference_code_with_requirement ~support ~requirement code with
  | Error error -> failwith (Admission.error_message error)
  | Ok admitted ->
    (match Target.check ~admitted expected with
     | Error (Target.Program_provenance_unsupported Admission.Raw_code) -> ()
     | _ -> failwith "expected raw inference program rejection")

let check_session_abi_root () =
  let admitted = admitted () in
  let target = target admitted in
  let target = Target.{ target with session_abi_root = hex_root '3' } in
  match Target.check ~admitted target with
  | Error (Target.Session_abi_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected session ABI root mismatch"

let check_missing_advance () =
  let admitted = admitted () in
  let target = target admitted in
  let target = Target.{ target with entrypoints = [] } in
  match Target.check ~admitted target with
  | Error Target.Missing_advance_entrypoint -> ()
  | _ -> failwith "expected missing advance entrypoint"

let check_unexpected_entrypoint () =
  let admitted = admitted () in
  let target = target admitted in
  let target =
    Target.{
      target with
      entrypoints = [{ entry_name = "status"; entry_label = Abi.advance_label }];
    }
  in
  match Target.check ~admitted target with
  | Error (Target.Unexpected_entrypoint ("status", _)) -> ()
  | _ -> failwith "expected unexpected entrypoint"

let check_extra_entrypoint () =
  let admitted = admitted () in
  let target = target admitted in
  let target =
    Target.{
      target with
      entrypoints = [
        { entry_name = Abi.advance_entrypoint; entry_label = Abi.advance_label };
        { entry_name = "status"; entry_label = Abi.advance_label };
      ];
    }
  in
  match Target.check ~admitted target with
  | Error (Target.Unexpected_entrypoint ("status", _)) -> ()
  | _ -> failwith "expected extra entrypoint rejection"

let check_advance_label () =
  let admitted = admitted () in
  let target = target admitted in
  let target =
    Target.{
      target with
      entrypoints = [{
        entry_name = Abi.advance_entrypoint;
        entry_label = Abi.advance_label + 1;
      }];
    }
  in
  match Target.check ~admitted target with
  | Error (Target.Advance_label_mismatch (_, _)) -> ()
  | _ -> failwith "expected advance label mismatch"

let check_bad_entrypoint_name () =
  let admitted = admitted () in
  let target = target admitted in
  let target =
    Target.{
      target with
      entrypoints = [{ entry_name = "Advance"; entry_label = Abi.advance_label }];
    }
  in
  match Target.check ~admitted target with
  | Error (Target.Bad_name "Advance") -> ()
  | _ -> failwith "expected bad entrypoint name"

let check_missing_entrypoint_label () =
  let target = target (admitted ()) in
  let admitted =
    Inference_cert.admit ~support ~requirement [| VM.JDEST 101; VM.STOP |]
  in
  let target =
    Target.{ target with program_root = Target.program_root admitted }
  in
  match Target.check ~admitted target with
  | Error (Target.Bad_entrypoint ("advance", label))
    when label = Abi.advance_label -> ()
  | _ -> failwith "expected missing advance label"

let () =
  check_supported ();
  check_supported_v2_session_abi ();
  check_committed_state_session_abi_requires_capability ();
  check_committed_state_session_abi_supported_with_capability ();
  check_program_root_mismatch ();
  check_requirement_root_mismatch ();
  check_missing_requirement ();
  check_raw_program_rejected ();
  check_session_abi_root ();
  check_missing_advance ();
  check_unexpected_entrypoint ();
  check_extra_entrypoint ();
  check_advance_label ();
  check_bad_entrypoint_name ();
  check_missing_entrypoint_label ()
