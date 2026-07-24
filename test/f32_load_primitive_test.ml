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
module Assembler = Octra_vm.Assembler
module Bytecode = Octra_vm.Bytecode
module Abi = Octra_vm.Inference_session_abi
module Execution = Octra_vm.Inference_execution
module Model = Octra_vm.Inference_model
module Plan = Octra_vm.Inference_plan
module Program_effects = Octra_vm.Program_effects
module Req = Octra_vm.Execution_requirement
module Request = Octra_vm.Inference_request
module Store = Octra_vm.Inference_store
module Target = Octra_vm.Inference_target
module VM = Octra_vm.Contract_vm

let check label condition =
  if not condition then failwith label

let starts_with prefix value =
  String.length value >= String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let hex_root char =
  String.make 64 char

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let put_u32le buffer offset value =
  for byte = 0 to 3 do
    Bytes.set
      buffer
      (offset + byte)
      (Char.chr ((value lsr (byte * 8)) land 0xff))
  done

let put_i64le buffer offset value =
  for byte = 0 to 7 do
    Bytes.set
      buffer
      (offset + byte)
      (Char.chr
         (Int64.to_int
            (Int64.logand
               (Int64.shift_right_logical value (byte * 8))
               0xffL)))
  done

let f32_bytes values =
  let buffer = Bytes.create (List.length values * 4) in
  List.iteri (fun index value -> put_u32le buffer (index * 4) value) values;
  Bytes.to_string buffer

let f64_bytes values =
  let buffer = Bytes.create (List.length values * 8) in
  List.iteri
    (fun index value -> put_i64le buffer (index * 8) value)
    values;
  Bytes.to_string buffer

let output_payload base count state =
  String.concat
    "|"
    [
      "base=" ^ string_of_int base;
      "length=" ^ string_of_int count;
      "values="
      ^ String.concat
	          ","
	          (List.init count (fun index ->
	             match Hashtbl.find_opt state.VM.memory.data (base + index) with
	             | Some (VM.VInt value) -> "int:" ^ Z.to_string value
	             | Some _ -> failwith "unexpected output cell"
	             | None -> ""));
    ]

let output_root ~target_root ~session_abi_root payload =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:output\000"
       ^ target_root ^ "\000" ^ session_abi_root ^ "\000" ^ payload)
    |> to_hex)

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let set_f64_cell state addr value =
  Hashtbl.replace
    state.VM.memory.data
    addr
    (VM.VInt (Z.of_int64 (Int64.bits_of_float value)))

let f64_cell state addr =
  match Hashtbl.find_opt state.VM.memory.data addr with
  | Some (VM.VInt bits) when Z.fits_int64 bits ->
    Int64.float_of_bits (Z.to_int64 bits)
  | _ -> failwith "missing f64 cell"

let f64_bits state addr =
  match Hashtbl.find_opt state.VM.memory.data addr with
  | Some (VM.VInt bits) when Z.fits_int64 bits -> Z.to_int64 bits
  | _ -> failwith "missing f64 cell"

let f32_code =
  [|
    VM.LOAD_F32_LE_FP (0, 1, 2, 3);
    VM.STOP;
  |]

let f64_code =
  [|
    VM.LOAD_F64_LE_FP (0, 1, 2, 3);
    VM.STOP;
  |]

let run ?(dst = 100) ?(offset = 0) ?(count = 4) ?(limit = 1_000_000) data =
  let state =
    VM.create_state
      ~limit
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 dst;
  state.VM.regs.(1) <- VM.VString data;
  set_int_reg state 2 offset;
  set_int_reg state 3 count;
  state, VM.run state f32_code

let run_f64 ?(dst = 100) ?(offset = 0) ?(count = 4) ?(limit = 1_000_000) data =
  let state =
    VM.create_state
      ~limit
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 dst;
  state.VM.regs.(1) <- VM.VString data;
  set_int_reg state 2 offset;
  set_int_reg state 3 count;
  state, VM.run state f64_code

let check_golden_decode () =
  let data = f32_bytes [0x3f800000; 0xc0200000; 0x3f000000; 0x3fc00000] in
  let state, ok = run data in
  check "load f32 succeeds" ok;
  check "cell 0" (f64_cell state 100 = 1.0);
  check "cell 1" (f64_cell state 101 = -2.5);
  check "cell 2" (f64_cell state 102 = 0.5);
  check "cell 3" (f64_cell state 103 = 1.5)

let check_edge_decode () =
  let data =
    f32_bytes [
      0x00000000;
      0x80000000;
      0x00000001;
      0x00800000;
      0x7f7fffff;
    ]
  in
  let state, ok = run ~count:5 data in
  check "edge load succeeds" ok;
  check "positive zero bits" (f64_bits state 100 = Int64.bits_of_float 0.0);
  check "negative zero bits" (f64_bits state 101 = Int64.bits_of_float (-0.0));
  check "min subnormal" (f64_cell state 102 = 2.0 ** -149.0);
  check "min normal" (f64_cell state 103 = 2.0 ** -126.0);
  check "max finite"
    (f64_cell state 104
     = Int64.float_of_bits 0x47efffffe0000000L)

let check_offset_decode () =
  let data = f32_bytes [0x3f800000; 0xc0200000; 0x3f000000] in
  let state, ok = run ~offset:4 ~count:2 data in
  check "offset load succeeds" ok;
  check "offset cell 0" (f64_cell state 100 = -2.5);
  check "offset cell 1" (f64_cell state 101 = 0.5)

let check_f64_golden_decode () =
  let bits =
    [
      Int64.bits_of_float 1.0;
      Int64.bits_of_float (-2.5);
      Int64.bits_of_float 0.5;
      Int64.bits_of_float 1.5;
    ]
  in
  let state, ok = run_f64 (f64_bytes bits) in
  check "load f64 succeeds" ok;
  List.iteri
    (fun index bits ->
      check
        ("f64 cell " ^ string_of_int index)
        (f64_bits state (100 + index) = bits))
    bits

let check_f64_edge_decode () =
  let bits =
    [
      Int64.bits_of_float 0.0;
      Int64.bits_of_float (-0.0);
      Int64.bits_of_float (2.0 ** -1074.0);
      Int64.bits_of_float (2.0 ** -1022.0);
      Int64.bits_of_float max_float;
    ]
  in
  let state, ok = run_f64 ~count:5 (f64_bytes bits) in
  check "edge f64 load succeeds" ok;
  List.iteri
    (fun index bits ->
      check
        ("edge f64 cell " ^ string_of_int index)
        (f64_bits state (100 + index) = bits))
    bits

let check_f64_offset_decode () =
  let bits =
    [
      Int64.bits_of_float 1.0;
      Int64.bits_of_float (-2.5);
      Int64.bits_of_float 0.5;
    ]
  in
  let state, ok = run_f64 ~offset:8 ~count:2 (f64_bytes bits) in
  check "offset f64 load succeeds" ok;
  check "offset f64 cell 0"
    (f64_bits state 100 = Int64.bits_of_float (-2.5));
  check "offset f64 cell 1"
    (f64_bits state 101 = Int64.bits_of_float 0.5)

let check_malformed_reverts () =
  let state, ok = run ~count:1 "\000\000\128" in
  check "truncated f32 reverts" (not ok);
  check "truncated load leaves output empty"
    (not (Hashtbl.mem state.VM.memory.data 100));
  let state, ok = run_f64 ~count:1 "\000\000\000\000\000\000\240" in
  check "truncated f64 reverts" (not ok);
  check "truncated f64 load leaves output empty"
    (not (Hashtbl.mem state.VM.memory.data 100))

let check_nonfinite_reverts_atomically () =
  let state =
    VM.create_state
      ~limit:1_000_000
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 100;
  state.VM.regs.(1) <- VM.VString (f32_bytes [0x3f800000; 0x7f800000]);
  set_int_reg state 2 0;
  set_int_reg state 3 2;
  set_f64_cell state 100 42.0;
  set_f64_cell state 101 43.0;
  check "non-finite f32 reverts" (not (VM.run state f32_code));
  check "first output unchanged" (f64_cell state 100 = 42.0);
  check "second output unchanged" (f64_cell state 101 = 43.0)

let check_nonfinite_cases () =
  List.iter
    (fun (name, bits) ->
      let state, ok = run ~count:1 (f32_bytes [bits]) in
      check (name ^ " reverts") (not ok);
      check (name ^ " leaves output empty")
        (not (Hashtbl.mem state.VM.memory.data 100)))
    [
      "positive infinity", 0x7f800000;
      "negative infinity", 0xff800000;
      "nan", 0x7fc00000;
    ];
  List.iter
    (fun (name, bits) ->
      let state, ok = run_f64 ~count:1 (f64_bytes [bits]) in
      check (name ^ " f64 reverts") (not ok);
      check (name ^ " f64 leaves output empty")
        (not (Hashtbl.mem state.VM.memory.data 100)))
    [
      "positive infinity", 0x7ff0000000000000L;
      "negative infinity", Int64.bits_of_float neg_infinity;
      "nan", 0x7ff8000000000000L;
    ]

let check_invalid_shape_and_effort_reverts () =
  List.iter
    (fun (name, dst, offset, count) ->
      let state, ok =
        run ~dst ~offset ~count (f32_bytes [0x3f800000; 0x40000000])
      in
      check (name ^ " reverts") (not ok);
      check (name ^ " leaves output empty")
        (not (Hashtbl.mem state.VM.memory.data 100)))
    [
      "negative destination", -1, 0, 1;
      "zero count", 100, 0, 0;
      "negative offset", 100, -1, 1;
      "short span", 100, 4, 2;
    ];
  let state, ok = run ~limit:29 (f32_bytes [0x3f800000]) in
  check "effort exhaustion reverts" (not ok);
  check "effort exhaustion leaves output empty"
    (not (Hashtbl.mem state.VM.memory.data 100));
  let state, ok =
    run_f64
      ~dst:131_071
      ~count:1
      (f64_bytes [Int64.bits_of_float 1.0])
  in
  check "max-span f64 load succeeds" ok;
  check "max-span f64 output"
    (f64_bits state 131_071 = Int64.bits_of_float 1.0);
  let state, ok =
    run_f64
      ~count:131_073
      (f64_bytes [Int64.bits_of_float 1.0])
  in
  check "oversized f64 span rejects" (not ok);
  check "oversized f64 span leaves output empty"
    (not (Hashtbl.mem state.VM.memory.data 100));
  let state, ok =
    run_f64 ~limit:30 (f64_bytes [Int64.bits_of_float 1.0])
  in
  check "f64 effort exhaustion reverts" (not ok);
  check "f64 effort exhaustion leaves output empty"
    (not (Hashtbl.mem state.VM.memory.data 100))

let capability name capability_root =
  Req.{ name; root = capability_root }

let limits =
  Req.{
    max_model_bytes = 64;
    max_view_bytes = 64;
    max_session_bytes = 512;
    max_scratch_bytes = 4096;
    max_output_bytes = 512;
    max_advance_effort = 10000;
  }

let requirement capabilities =
  Req.{
    vm_semantics_root = hex_root 'a';
    numerical_root = hex_root 'b';
    effort_root = hex_root 'c';
    capabilities;
    limits;
  }

let support capabilities =
  Req.{
    support_vm_semantics_root = hex_root 'a';
    support_numerical_roots = [hex_root 'b'];
    support_effort_roots = [hex_root 'c'];
    support_capabilities = capabilities;
    support_limits = limits;
  }

let admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt Z.zero);
    VM.LDI (1, VM.VString (f32_bytes [0x3f800000]));
    VM.LDI (2, VM.VInt Z.zero);
    VM.LDI (3, VM.VInt Z.one);
    VM.LOAD_F32_LE_FP (0, 1, 2, 3);
    VM.STOP;
  |]

let f64_admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt Z.zero);
    VM.LDI (1, VM.VString (f64_bytes [Int64.bits_of_float 1.0]));
    VM.LDI (2, VM.VInt Z.zero);
    VM.LDI (3, VM.VInt Z.one);
    VM.LOAD_F64_LE_FP (0, 1, 2, 3);
    VM.STOP;
  |]

let check_capability_gate () =
  let cap = capability "storage.authenticated-range" (hex_root 'd') in
  List.iter
    (fun (name, code) ->
      (match
         Admission.of_inference_code_with_requirement
           ~support:(support [cap])
           ~requirement:(requirement [cap])
           code
       with
       | Ok _ -> ()
       | Error error -> failwith (Admission.error_message error));
      match
        Admission.of_inference_code_with_requirement
          ~support:(support [])
          ~requirement:(requirement [])
          code
      with
      | Error (Admission.Unsafe_error message) ->
        check
          (name ^ " storage capability is named")
          (starts_with
             ("inference opcode " ^ name
              ^ " at pc 5 requires capability storage.authenticated-range")
             message)
      | _ -> failwith ("expected load capability rejection: " ^ name))
    [
      "LOAD_F32_LE_FP", admission_code;
      "LOAD_F64_LE_FP", f64_admission_code;
    ]

let check_generic_admission_rejection () =
  List.iter
    (fun (name, code) ->
      match Admission.of_program code with
      | Error (Admission.Unsafe_error message) ->
        check
          ("generic Program rejects " ^ name)
          (starts_with ("consensus unsafe opcode " ^ name) message)
      | _ -> failwith ("expected generic load rejection: " ^ name))
    [
      "LOAD_F32_LE_FP", admission_code;
      "LOAD_F64_LE_FP", f64_admission_code;
    ]

let check_wire_roundtrip () =
  List.iter
    (fun (name, op) ->
      let raw = Bytecode.encode [| op; VM.STOP |] in
      (match Bytecode.decode raw with
       | Ok [| decoded; VM.STOP |] ->
         check (name ^ " bytecode roundtrip") (decoded = op)
       | Ok _ -> failwith ("unexpected decoded " ^ name ^ " code")
       | Error error -> failwith error);
      let asm = name ^ " r0, r1, r2, r3\nSTOP" in
      check (name ^ " assembler parse")
        (Assembler.parse asm = [| op; VM.STOP |]);
      check (name ^ " assembler emit")
        (String.equal (Assembler.emit [| op; VM.STOP |]) asm))
    [
      "LOAD_F32_LE_FP", VM.LOAD_F32_LE_FP (0, 1, 2, 3);
      "LOAD_F64_LE_FP", VM.LOAD_F64_LE_FP (0, 1, 2, 3);
    ]

let check_effects () =
  check
    "loads write memory"
    (Program_effects.names (Program_effects.scan [|f32_code.(0); f64_code.(0)|])
     = ["memory_write"])

let fload_program range_root =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (4, VM.VString range_root);
    VM.FLOAD (1, 4);
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (2, VM.VInt Z.zero);
    VM.LDI (3, VM.VInt (Z.of_int 2));
    VM.LOAD_F32_LE_FP (0, 1, 2, 3);
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (1, VM.VInt (Z.of_int 2));
    VM.STOP;
  |]

let fload_f64_program range_root =
  [|
    VM.JDEST Abi.advance_label;
    VM.LDI (4, VM.VString range_root);
    VM.FLOAD (1, 4);
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (2, VM.VInt Z.zero);
    VM.LDI (3, VM.VInt (Z.of_int 2));
    VM.LOAD_F64_LE_FP (0, 1, 2, 3);
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (1, VM.VInt (Z.of_int 2));
    VM.STOP;
  |]

let check_fload_session () =
  let owner = f32_bytes [0x3f800000; 0xc0200000] in
  let owner_root = sha256 owner in
  let range =
    Model.{
      owner_root;
      offset = 0;
      length = String.length owner;
      encoding = "tensor.f32le";
      shape_root = None;
    }
  in
  let range_root = Model.range_root range in
  let cap = capability "storage.authenticated-range" (hex_root 'd') in
  let requirement = requirement [cap] in
  let support = support [cap] in
  let code = fload_program range_root in
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
      session_abi_root = Abi.v1_root;
      entrypoints = [{
        entry_name = Abi.advance_entrypoint;
        entry_label = Abi.advance_label;
      }];
    }
  in
  let request =
    Request.{
      schema = Abi.request_schema;
      target_root = Target.root target;
      entrypoint = Abi.advance_entrypoint;
      input_root = sha256 "";
      request_nonce = hex_root '5';
      max_output_bytes = 128;
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
    match
      Store.pin
        ~limits:requirement.limits
        ~read:(fun root -> if String.equal root owner_root then Some owner else None)
        model
    with
    | Ok pins -> pins
    | Error error -> failwith (Store.error_message error)
  in
  let plan =
    match Plan.create ~admitted ~target ~request ~model ~pins ~input:"" with
    | Ok plan -> plan
    | Error error -> failwith (Plan.error_message error)
  in
  match Execution.run ~plan () with
  | Ok result ->
    let state, ok = run ~count:2 owner in
    check "expected local decode succeeds" ok;
    let expected =
      output_root
        ~target_root:(Target.root target)
        ~session_abi_root:Abi.v1_root
        (output_payload 100 2 state)
    in
    check "fload f32 session root" (String.equal result.Execution.output_root expected)
  | Error error -> failwith (Execution.error_message error)

let check_fload_f64_session () =
  let owner =
    f64_bytes [Int64.bits_of_float 1.0; Int64.bits_of_float (-2.5)]
  in
  let owner_root = sha256 owner in
  let range =
    Model.{
      owner_root;
      offset = 0;
      length = String.length owner;
      encoding = "tensor.f64le";
      shape_root = None;
    }
  in
  let range_root = Model.range_root range in
  let cap = capability "storage.authenticated-range" (hex_root 'd') in
  let requirement = requirement [cap] in
  let support = support [cap] in
  let code = fload_f64_program range_root in
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
      session_abi_root = Abi.v1_root;
      entrypoints = [{
        entry_name = Abi.advance_entrypoint;
        entry_label = Abi.advance_label;
      }];
    }
  in
  let request =
    Request.{
      schema = Abi.request_schema;
      target_root = Target.root target;
      entrypoint = Abi.advance_entrypoint;
      input_root = sha256 "";
      request_nonce = hex_root '5';
      max_output_bytes = 128;
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
    match
      Store.pin
        ~limits:requirement.limits
        ~read:(fun root -> if String.equal root owner_root then Some owner else None)
        model
    with
    | Ok pins -> pins
    | Error error -> failwith (Store.error_message error)
  in
  let plan =
    match Plan.create ~admitted ~target ~request ~model ~pins ~input:"" with
    | Ok plan -> plan
    | Error error -> failwith (Plan.error_message error)
  in
  match Execution.run ~plan () with
  | Ok result ->
    let state, ok = run_f64 ~count:2 owner in
    check "expected local f64 decode succeeds" ok;
    let expected =
      output_root
        ~target_root:(Target.root target)
        ~session_abi_root:Abi.v1_root
        (output_payload 100 2 state)
    in
    check "fload f64 session root" (String.equal result.Execution.output_root expected)
  | Error error -> failwith (Execution.error_message error)

let check_f64_chunk_composition () =
  let bits =
    [
      Int64.bits_of_float 1.0;
      Int64.bits_of_float (-2.0);
      Int64.bits_of_float 3.0;
      Int64.bits_of_float (-4.0);
    ]
  in
  let data = f64_bytes bits in
  let state =
    VM.create_state
      ~limit:1_000_000
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 100;
  state.VM.regs.(1) <- VM.VString data;
  set_int_reg state 2 0;
  set_int_reg state 3 2;
  check
    "chunked f64 load succeeds"
    (VM.run state
       [|
         VM.LOAD_F64_LE_FP (0, 1, 2, 3);
         VM.LDI (0, VM.VInt (Z.of_int 102));
         VM.LDI (2, VM.VInt (Z.of_int 16));
         VM.LDI (3, VM.VInt (Z.of_int 2));
         VM.LOAD_F64_LE_FP (0, 1, 2, 3);
       |]);
  List.iteri
    (fun index bits ->
      check
        ("chunked f64 cell " ^ string_of_int index)
        (f64_bits state (100 + index) = bits))
    bits

let () =
  check_golden_decode ();
  check_edge_decode ();
  check_offset_decode ();
  check_f64_golden_decode ();
  check_f64_edge_decode ();
  check_f64_offset_decode ();
  check_malformed_reverts ();
  check_nonfinite_reverts_atomically ();
  check_nonfinite_cases ();
  check_invalid_shape_and_effort_reverts ();
  check_capability_gate ();
  check_generic_admission_rejection ();
  check_wire_roundtrip ();
  check_effects ();
  check_fload_session ();
  check_fload_f64_session ();
  check_f64_chunk_composition ()
