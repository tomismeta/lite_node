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


module Deployment = Octra_vm.Inference_model_deployment
module Abi = Octra_vm.Inference_session_abi
module Req = Octra_vm.Execution_requirement
module Target = Octra_vm.Inference_target

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

let requirement =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities = [
      capability "sequence.delta-rule" (hex_root 'd');
      capability "tensor.q1-g128" (hex_root 'e');
    ];
    limits;
  }

let target =
  Target.{
    program_root = hex_root '1';
    requirement_root = Req.root requirement;
    model_root = hex_root '2';
    execution_descriptor_root = hex_root '3';
    store_root = hex_root '4';
    session_abi_root = Abi.v1_root;
    entrypoints = [];
  }

let deployment
    ?(model_root = target.model_root)
    ?(store_root = target.store_root)
    ?(tensor_index_root = hex_root '5')
    ?tokenizer_root
    ?(numerical_profile_root = requirement.numerical_root)
    ?(capability_set_root =
      Deployment.capability_set_root requirement.capabilities)
    ?default_program_root
    ()
  =
  let capability_set_root_value = capability_set_root in
  Deployment.{
    model_root;
    store_root;
    tensor_index_root;
    tokenizer_root;
    numerical_profile_root;
    capability_set_root = capability_set_root_value;
    default_program_root;
  }

let check_supported () =
  match Deployment.check ~target ~requirement (deployment ()) with
  | Ok () -> ()
  | Error error -> failwith (Deployment.error_message error)

let check_default_program_is_not_authority () =
  let deployment = deployment ~default_program_root:(hex_root '6') () in
  match Deployment.check ~target ~requirement deployment with
  | Ok () -> ()
  | Error error -> failwith (Deployment.error_message error)

let check_capability_order () =
  let left = Deployment.capability_set_root requirement.capabilities in
  let right =
    Deployment.capability_set_root (List.rev requirement.capabilities)
  in
  check "capability set root sorts capabilities" (String.equal left right)

let check_model_root_mismatch () =
  match
    Deployment.check
      ~target
      ~requirement
      (deployment ~model_root:(hex_root '7') ())
  with
  | Error (Deployment.Model_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected model root mismatch"

let check_store_root_mismatch () =
  match
    Deployment.check
      ~target
      ~requirement
      (deployment ~store_root:(hex_root '8') ())
  with
  | Error (Deployment.Store_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected store root mismatch"

let check_numerical_root_mismatch () =
  match
    Deployment.check
      ~target
      ~requirement
      (deployment ~numerical_profile_root:(hex_root '9') ())
  with
  | Error (Deployment.Numerical_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected numerical root mismatch"

let check_capability_set_root_mismatch () =
  match
    Deployment.check
      ~target
      ~requirement
      (deployment ~capability_set_root:(hex_root '0') ())
  with
  | Error (Deployment.Capability_set_root_mismatch (_, _)) -> ()
  | _ -> failwith "expected capability set root mismatch"

let check_bad_tokenizer_root () =
  match
    Deployment.check
      ~target
      ~requirement
      (deployment ~tokenizer_root:"bad" ())
  with
  | Error (Deployment.Bad_root "bad") -> ()
  | _ -> failwith "expected bad tokenizer root"

let check_bad_default_program_root () =
  match
    Deployment.check
      ~target
      ~requirement
      (deployment ~default_program_root:"bad" ())
  with
  | Error (Deployment.Bad_root "bad") -> ()
  | _ -> failwith "expected bad default program root"

let () =
  check_supported ();
  check_default_program_is_not_authority ();
  check_capability_order ();
  check_model_root_mismatch ();
  check_store_root_mismatch ();
  check_numerical_root_mismatch ();
  check_capability_set_root_mismatch ();
  check_bad_tokenizer_root ();
  check_bad_default_program_root ()
