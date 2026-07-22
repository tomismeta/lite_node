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
    max_scratch_bytes = 4096;
    max_output_bytes = 64;
    max_advance_effort = 10000;
  }

let requirement =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities = [
      capability "storage.authenticated-range" (hex_root 'd');
    ];
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

let encoded bytes =
  Base64.encode_exn bytes

let code ?(write_output = true) range_root =
  let output_code = if write_output then [|VM.MSTORE (0, 3)|] else [||] in
  Array.concat
    [
      [|
        VM.JDEST 100;
        VM.LDI (2, VM.VString range_root);
        VM.FLOAD (3, 2);
      |];
      output_code;
      [|
        VM.LDI (0, VM.VInt Z.zero);
        VM.LDI (1, VM.VInt Z.one);
        VM.STOP;
      |];
    ]

let run_result ?(write_output = true) ?(max_output_bytes = 64)
    ?(max_scratch_bytes = 4096)
    ?code_range_root bytes =
  let limits = Req.{ limits with max_scratch_bytes } in
  let requirement = Req.{ requirement with limits } in
  let support = Req.{ support with support_limits = limits } in
  let owner = encoded bytes in
  let owner_root = sha256 owner in
  let range =
    Model.{
      owner_root;
      offset = 0;
      length = String.length owner;
      encoding = "tensor.q1-g128";
      shape_root = None;
    }
  in
  let code_root =
    match code_range_root with
    | Some root -> root
    | None -> Model.range_root range
  in
  let code = code ~write_output code_root in
  let admitted =
    Inference_cert.admit ~support ~requirement code
  in
  let target =
    Target.{
      program_root = Target.program_root admitted;
      requirement_root = Req.root requirement;
      model_root = hex_root 'f';
      execution_descriptor_root = hex_root '1';
      store_root = hex_root '2';
      session_abi_root = hex_root '3';
      entrypoints = [{ entry_name = "advance"; entry_label = 100 }];
    }
  in
  let request =
    Request.{
      schema = 1;
      target_root = Target.root target;
      entrypoint = "advance";
      input_root = sha256 "";
      request_nonce = hex_root '5';
      max_output_bytes;
      max_advance_effort = 10000;
    }
  in
  let model =
    Model.{
      model_root = target.model_root;
      store_root = target.store_root;
      ranges = [range];
    }
  in
  let pins =
    match Store.pin ~limits ~read:(fun root ->
      if String.equal root owner_root then Some owner else None) model with
    | Ok pins -> pins
    | Error error -> failwith (Store.error_message error)
  in
  let plan =
    match Plan.create ~admitted ~target ~request ~model ~pins ~input:"" with
    | Ok plan -> plan
    | Error error -> failwith (Plan.error_message error)
  in
  let session =
    match Session.open_session ~plan with
    | Ok session -> session
    | Error error -> failwith (Session.error_message error)
  in
  match
    Session.advance ~plan ~expected_sequence:0 session
  with
  | Ok (advanced, _) -> Ok (Session.output_root advanced)
  | Error error -> Error (Session.error_message error)

let run bytes =
  match run_result bytes with
  | Ok output_root -> output_root
  | Error error -> failwith error

let starts_with prefix value =
  String.length value >= String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let check_capability_gate () =
  let one = Z.of_int64 (Int64.bits_of_float 1.0) in
  let host_float_code = [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt Z.zero);
    VM.LDI (1, VM.VInt Z.one);
    VM.LDI (2, VM.VInt one);
    VM.RMSNORM_FP (0, 1, 2);
    VM.STOP;
  |] in
  let requirement =
    Req.{
      requirement with
      capabilities = [capability "tensor.strict-fp" (hex_root 'e')];
    }
  in
  let support = Req.{ support with support_capabilities = requirement.capabilities } in
  match
    Admission.of_inference_code_with_requirement ~support ~requirement host_float_code
  with
  | Error (Admission.Unsafe_error _) -> ()
  | _ -> failwith "expected consensus-safe host-float rejection"

let check_opcode_capability_gate () =
  let code = [|
    VM.JDEST 100;
    VM.LDI (0, VM.VString "missing-range");
    VM.FLOAD (1, 0);
    VM.STOP;
  |] in
  let requirement =
    Req.{
      requirement with
      capabilities = [capability "tensor.fixed" (hex_root 'e')];
    }
  in
  let support = Req.{ support with support_capabilities = requirement.capabilities } in
  match
    Admission.of_inference_code_with_requirement ~support ~requirement code
  with
  | Error (Admission.Unsafe_error message) ->
    check
      "missing opcode capability is named"
      (starts_with "inference opcode FLOAD" message)
  | _ -> failwith "expected authenticated-range capability rejection"

let check_fhe_forbidden () =
  let cases = [
    "FHE_LOAD_PK", VM.FHE_LOAD_PK (0, 1);
    "FHE_ADD", VM.FHE_ADD (0, 1, 2, 3);
    "FHE_SUB", VM.FHE_SUB (0, 1, 2, 3);
    "FHE_MUL", VM.FHE_MUL (0, 1, 2, 3);
    "FHE_SCALE", VM.FHE_SCALE (0, 1, 2, 3);
    "FHE_DIV_CONST", VM.FHE_DIV_CONST (0, 1, 2, 3);
    "FHE_ADD_CONST", VM.FHE_ADD_CONST (0, 1, 2, 3);
    "FHE_SUB_CONST", VM.FHE_SUB_CONST (0, 1, 2, 3);
    "FHE_VERIFY_ZERO", VM.FHE_VERIFY_ZERO (0, 1, 2, 3);
    "FHE_VERIFY_RANGE", VM.FHE_VERIFY_RANGE (0, 1, 2, 3);
    "FHE_VERIFY_BOUND", VM.FHE_VERIFY_BOUND (0, 1, 2, 3, 4);
    "FHE_COMMIT", VM.FHE_COMMIT (0, 1, 2);
    "FHE_PEDERSEN", VM.FHE_PEDERSEN (0, 1, 2);
    "FHE_SER", VM.FHE_SER (0, 1);
    "FHE_DESER", VM.FHE_DESER (0, 1);
    "FHE_SER_PK", VM.FHE_SER_PK (0, 1);
    "FHE_DESER_PK", VM.FHE_DESER_PK (0, 1);
  ] in
  List.iter
    (fun (name, op) ->
      match
        Admission.of_inference_code_with_requirement
          ~support
          ~requirement
          [| VM.JDEST 100; op; VM.STOP |]
      with
      | Error (Admission.Unsafe_error message) ->
        check
          ("fhe opcode is forbidden: " ^ name)
          (starts_with ("inference opcode " ^ name) message)
      | _ -> failwith ("expected fhe opcode rejection: " ^ name))
    cases

let check_data_rooted_execution () =
  let left = run "\001\002\003\004" in
  let right = run "\004\003\002\001" in
  check "executed data changes output root" (not (String.equal left right))

let check_output_contract () =
  (match run_result ~write_output:false "\001\002\003\004" with
   | Error error ->
     check
       "missing output cell is rejected"
       (starts_with
          "inference execution error: inference output cell"
          error)
   | Ok _ -> failwith "expected missing output cell rejection");
  (match run_result ~code_range_root:"missing-range" "\001\002\003\004" with
   | Error error ->
     check
       "missing authenticated range is rejected"
       (starts_with "inference execution error: inference execution failed" error)
   | Ok _ -> failwith "expected missing authenticated range rejection");
  (match run_result ~max_output_bytes:1 "\001\002\003\004" with
   | Error error ->
     check
       "output limit is enforced"
       (starts_with
          "inference execution error: inference output exceeds limit:"
          error)
   | Ok _ -> failwith "expected output limit rejection");
  (match run_result ~max_scratch_bytes:1 "\001\002\003\004" with
   | Error error ->
     check
       "scratch limit is enforced"
       (starts_with
          "inference execution error: inference scratch exceeds limit:"
          error)
   | Ok _ -> failwith "expected scratch limit rejection")

let () =
  check_capability_gate ();
  check_opcode_capability_gate ();
  check_fhe_forbidden ();
  check_data_rooted_execution ();
  check_output_contract ()
