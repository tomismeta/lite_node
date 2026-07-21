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


module Req = Octra_vm.Execution_requirement
module Admission = Octra_vm.Admission
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

let cap_tensor = capability "tensor.fixed" (hex_root 'd')
let cap_store = capability "storage.authenticated-range" (hex_root 'e')

let requirement =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities = [cap_tensor; cap_store];
    limits;
  }

let support =
  Req.{
    support_vm_semantics_root = hex_root 'a';
    support_numerical_roots = [hex_root 'b'];
    support_effort_roots = [hex_root 'c'];
    support_capabilities = [cap_store; cap_tensor];
    support_limits;
  }

let check_root_order () =
  let left = Req.root requirement in
  let right =
    Req.root
      Req.{ requirement with capabilities = List.rev requirement.capabilities }
  in
  check "requirement root sorts capabilities" (String.equal left right)

let check_supported () =
  match Req.check support requirement with
  | Ok () -> ()
  | Error error -> failwith (Req.error_message error)

let check_support_rollout () =
  let support =
    Req.{
      support with
      support_capabilities =
        capability cap_tensor.Req.name (hex_root 'f')
        :: support.support_capabilities;
    }
  in
  match Req.check support requirement with
  | Ok () -> ()
  | Error error -> failwith (Req.error_message error)

let check_missing_capability () =
  let support = Req.{ support with support_capabilities = [cap_store] } in
  match Req.check support requirement with
  | Error (Req.Capability_unsupported capability) ->
    check "missing capability name"
      (String.equal capability.Req.name cap_tensor.Req.name)
  | _ -> failwith "expected missing capability"

let check_limit () =
  let support =
    Req.{
      support with
      support_limits = { support_limits with max_output_bytes = 10 };
    }
  in
  match Req.check support requirement with
  | Error (Req.Limit_exceeded ("max_output_bytes", 20, 10)) -> ()
  | _ -> failwith "expected output limit rejection"

let check_bad_name () =
  let requirement =
    Req.{
      requirement with
      capabilities = [capability "Tensor.Fixed" (hex_root 'd')];
    }
  in
  match Req.check support requirement with
  | Error (Req.Bad_name "Tensor.Fixed") -> ()
  | _ -> failwith "expected bad capability name"

let check_program_admission () =
  let code = [| VM.STOP |] in
  match Admission.of_program_with_requirement ~support ~requirement code with
  | Ok admitted ->
    (match Admission.requirement admitted with
     | Some admitted_requirement ->
       check "admission stores requirement"
         (String.equal (Req.root admitted_requirement) (Req.root requirement))
     | None -> failwith "missing admitted requirement")
  | Error error -> failwith (Admission.error_message error)

let () =
  check_root_order ();
  check_supported ();
  check_support_rollout ();
  check_missing_capability ();
  check_limit ();
  check_bad_name ();
  check_program_admission ()
