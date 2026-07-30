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
module Fp64 = Octra_vm.Inference_fp64
module Store = Octra_vm.Inference_store
module Target = Octra_vm.Inference_target
module VM = Octra_vm.Contract_vm

let check label condition =
  if not condition then failwith label

let hex_root char =
  String.make 64 char

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | c -> failwith (Printf.sprintf "bad hex: %c" c)

let bytes_of_hex value =
  let value =
    value
    |> String.to_seq
    |> Seq.filter (function ' ' | '\n' | '\r' | '\t' -> false | _ -> true)
    |> String.of_seq
  in
  if String.length value mod 2 <> 0 then failwith "odd hex length";
  String.init (String.length value / 2) (fun index ->
    let hi = hex_value value.[index * 2] in
    let lo = hex_value value.[index * 2 + 1] in
    Char.chr ((hi lsl 4) lor lo))

let put_u64le buffer offset value =
  for byte = 0 to 7 do
    Bytes.set
      buffer
      (offset + byte)
      (Char.chr
         (Int64.to_int
            (Int64.logand
               0xffL
               (Int64.shift_right_logical value (byte * 8)))))
  done

let f64_bytes values =
  let buffer = Bytes.create (List.length values * 8) in
  List.iteri
    (fun index value ->
      put_u64le buffer (index * 8) (Int64.bits_of_float value))
    values;
  Bytes.to_string buffer

let input_values =
  List.init 2 (fun row ->
    List.init 256 (fun col ->
      float_of_int ((col mod 17) - 8) *. float_of_int (row + 1) /. 9.0))
  |> List.concat

let q1_owner =
  bytes_of_hex
    "003c218c50420a318410462821851842082300380825a4104308618412528821\
     84304209004084146208218c50420a31841046282185003e218610c20825a410\
     43086184125288210034104218a184146208218c50420a318410004284114a48\
     218610c20825a41043086184"

let expected_output =
  bytes_of_hex
    "5a55555555550d406d1cc7711cc715c01cc7711cc77120c05a55555555551d40\
     6d1cc7711cc725c01cc7711cc77130c0"

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let set_f64_memory state base values =
  List.iteri
    (fun index value ->
      Hashtbl.replace
        state.VM.memory.data
        (base + index)
        (VM.VInt (Z.of_int64 (Int64.bits_of_float value))))
    values

let output_bytes state base count =
  let buffer = Bytes.create (count * 8) in
  for index = 0 to count - 1 do
    let bits =
      match Hashtbl.find_opt state.VM.memory.data (base + index) with
      | Some (VM.VInt value) when Z.fits_int64 value -> Z.to_int64 value
      | _ -> failwith "missing output cell"
    in
    put_u64le buffer (index * 8) bits
  done;
  Bytes.to_string buffer

let set_output_cell state base value =
  Hashtbl.replace
    state.VM.memory.data
    base
    (VM.VInt (Z.of_int64 (Int64.bits_of_float value)))

let q1_code =
  [|
    VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6);
    VM.STOP;
  |]

let q1_state
    ?(q1 = q1_owner)
    ?(input = input_values)
    ?(dst = 10000)
    ?(lhs = 20000)
    ?(off = 0)
    ?(limit = 1_000_000)
    () =
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
  set_int_reg state 1 lhs;
  state.VM.regs.(2) <- VM.VString q1;
  set_int_reg state 3 off;
  set_int_reg state 4 2;
  set_int_reg state 5 256;
  set_int_reg state 6 3;
  set_f64_memory state lhs input;
  state, dst, lhs

let q1_block scale sign_bytes =
  scale ^ sign_bytes

let one_block_state ?(input = List.init 128 (fun _ -> 1.0)) q1 =
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
  set_int_reg state 0 10000;
  set_int_reg state 1 20000;
  state.VM.regs.(2) <- VM.VString q1;
  set_int_reg state 3 0;
  set_int_reg state 4 1;
  set_int_reg state 5 128;
  set_int_reg state 6 1;
  set_f64_memory state 20000 input;
  state

let fp16_bytes bits =
  String.init 2 (fun index ->
    Char.chr ((bits lsr (index * 8)) land 0xff))

let expected_fp16 bits =
  let sign = if bits land 0x8000 = 0 then 1.0 else -1.0 in
  let exponent = (bits lsr 10) land 0x1f in
  let fraction = bits land 0x03ff in
  match exponent, fraction with
  | 31, _ -> None
  | 0, 0 -> Some (Int64.bits_of_float (sign *. 0.0))
  | 0, _ ->
    Some (Int64.bits_of_float (sign *. ldexp (float_of_int fraction) (-24)))
  | _ ->
    Some
      (Int64.bits_of_float
         (sign *. ldexp (1024.0 +. float_of_int fraction) (exponent - 25)))

let check_fp16_scale_decode_exhaustive () =
  let finite = ref 0 in
  let rejected = ref 0 in
  for bits = 0 to 0xffff do
    match expected_fp16 bits, VM.fp16_le_to_fp64 (fp16_bytes bits) 0 with
    | None, None -> incr rejected
    | Some expected, Some observed
      when Int64.equal expected (Int64.bits_of_float observed) ->
      incr finite
    | _ ->
      failwith
        (Printf.sprintf "fp16 scale decode mismatch for 0x%04x" bits)
  done;
  check "fp16 finite encoding count" (!finite = 63488);
  check "fp16 rejected encoding count" (!rejected = 2048)

let expect_bits label actual expected =
  match actual with
  | Some bits -> check label (Int64.equal bits expected)
  | None -> failwith (label ^ " returned non-finite")

let check_fp64_core_edges () =
  let bits value = Int64.bits_of_float value in
  expect_bits "fp64 add min-subnormal"
    (Fp64.add 1L 1L)
    2L;
  expect_bits "fp64 mul underflow tie to even"
    (Fp64.mul 1L (bits 0.5))
    0L;
  expect_bits "fp64 mul finite"
    (Fp64.mul (bits 1.5) (bits 2.0))
    (bits 3.0);
  expect_bits "fp64 positive zero plus negative zero"
    (Fp64.add 0L Int64.min_int)
    0L;
  expect_bits "fp64 negative zero plus negative zero"
    (Fp64.add Int64.min_int Int64.min_int)
    Int64.min_int;
  check "fp64 add overflow rejects"
    (Fp64.add 0x7fefffffffffffffL 0x7fefffffffffffffL = None);
  check "fp64 mul overflow rejects"
    (Fp64.mul 0x7fefffffffffffffL (bits 2.0) = None)

let check_golden_fixture () =
  let input = f64_bytes input_values in
  check
    "input fixture sha"
    (String.equal
       (sha256 input)
       "7c0a594504cdff24a4869c837082443200a552263c209821610033b1c88d3bec");
  check
    "q1 owner fixture sha"
    (String.equal
       (sha256 q1_owner)
       "57f9dadd168580d1b39660f54e5a21b480a70bc3017a90e3892f7c74f2895785");
  let state, dst, _ = q1_state () in
  check "q1 primitive runs" (VM.run state q1_code);
  let output = output_bytes state dst 6 in
  check "q1 output bytes" (String.equal output expected_output);
  check
    "q1 output sha"
    (String.equal
       (sha256 output)
       "43411283d083bd6e959bca6aa7edbebc55d8ad251510d992bd52046ac71d9d22")

let check_sign_and_scale_edges () =
  let scale_one = "\000\060" in
  let scale_minus_one = "\000\188" in
  let scale_zero = "\000\000" in
  let scale_negative_zero = "\000\128" in
  let scale_min_subnormal = "\001\000" in
  let scale_negative_min_subnormal = "\001\128" in
  let scale_one_and_half = "\000\062" in
  let scale_max_finite = "\255\123" in
  List.iter
    (fun (name, q1, expected) ->
      let state = one_block_state q1 in
      check (name ^ " runs") (VM.run state q1_code);
      check
        (name ^ " output")
        (output_bytes state 10000 1 = f64_bytes [expected]))
    [
      "all positive signs",
      q1_block scale_one (String.make 16 '\255'),
      128.0;
      "all negative signs",
      q1_block scale_one (String.make 16 '\000'),
      -128.0;
      "negative scale",
      q1_block scale_minus_one (String.make 16 '\255'),
      -128.0;
      "balanced signs",
      q1_block scale_one (String.make 8 '\255' ^ String.make 8 '\000'),
      0.0;
      "zero scale",
      q1_block scale_zero (String.make 16 '\255'),
      0.0;
      "negative zero scale",
      q1_block scale_negative_zero (String.make 16 '\255'),
      0.0;
      "positive min-subnormal scale",
      q1_block scale_min_subnormal (String.make 16 '\255'),
      ldexp 1.0 (-17);
      "negative min-subnormal scale",
      q1_block scale_negative_min_subnormal (String.make 16 '\255'),
      ~-. (ldexp 1.0 (-17));
      "fractional normal scale",
      q1_block scale_one_and_half (String.make 16 '\255'),
      192.0;
      "positive max-finite scale",
      q1_block scale_max_finite (String.make 16 '\255'),
      8384512.0;
    ]

let check_accumulation_order_stress () =
  let input =
    1e16 :: 1.0 :: -1e16 :: 0.25 :: List.init 124 (fun _ -> 0.0)
  in
  let state =
    one_block_state
      ~input
      (q1_block "\000\060" (String.make 16 '\255'))
  in
  check "q1 accumulation-order stress runs" (VM.run state q1_code);
  check
    "q1 accumulation-order stress output"
    (output_bytes state 10000 1 = f64_bytes [0.25]);
  check
    "q1 accumulation-order stress distinguishes reassociation"
    (f64_bytes [0.25] <> f64_bytes [1.25])

let check_output_overflow_reverts () =
  let scale_one = "\000\060" in
  let state =
    one_block_state
      ~input:(List.init 128 (fun _ -> max_float))
      (q1_block scale_one (String.make 16 '\255'))
  in
  set_output_cell state 10000 42.0;
  check "q1 output overflow rejects" (not (VM.run state q1_code));
  check
    "q1 output overflow keeps output"
    (output_bytes state 10000 1 = f64_bytes [42.0])

let check_profiled_run_equivalence () =
  let state, dst, _ = q1_state () in
  let profiled, profile =
    VM.run_profiled
      ~clock:(fun () -> 0.0)
      ~opcode_name:(function
        | VM.LINEAR_Q1_G128_FP _ -> "LINEAR_Q1_G128_FP"
        | VM.STOP -> "STOP"
        | _ -> "other")
      state
      q1_code
  in
  check "profiled q1 run succeeds" profiled;
  check
    "profiled q1 output bytes"
    (output_bytes state dst 6 = expected_output);
  check
    "profiled q1 records opcode"
    (List.exists
       (fun (row : VM.opcode_profile) ->
         String.equal row.opcode "LINEAR_Q1_G128_FP"
         && row.count = 1
         && row.effort_used > 0)
       profile)

let check_invalid_input_reverts () =
  let state, dst, lhs = q1_state () in
  set_output_cell state dst 42.0;
  Hashtbl.remove state.VM.memory.data (lhs + 17);
  check "missing lhs reverts" (not (VM.run state q1_code));
  check
    "missing lhs does not overwrite output"
    (output_bytes state dst 1 = f64_bytes [42.0]);
  let state, dst, lhs = q1_state () in
  set_output_cell state dst 42.0;
  Hashtbl.replace state.VM.memory.data (lhs + 17) (VM.VString "bad");
  check "bad lhs type reverts" (not (VM.run state q1_code));
  check
    "bad lhs does not overwrite output"
    (output_bytes state dst 1 = f64_bytes [42.0]);
  let state, dst, lhs = q1_state () in
  set_output_cell state dst 42.0;
  Hashtbl.replace
    state.VM.memory.data
    (lhs + 17)
    (VM.VInt (Z.of_int64 (Int64.bits_of_float (0.0 /. 0.0))));
  check "non-finite lhs reverts" (not (VM.run state q1_code));
  check
    "non-finite lhs does not overwrite output"
    (output_bytes state dst 1 = f64_bytes [42.0])

let check_bad_q1_reverts () =
  let q1 = Bytes.of_string q1_owner in
  Bytes.set q1 0 '\000';
  Bytes.set q1 1 '\124';
  let state, dst, _ = q1_state ~q1:(Bytes.to_string q1) () in
  set_output_cell state dst 42.0;
  check "non-finite q1 scale reverts" (not (VM.run state q1_code));
  check
    "bad q1 does not overwrite output"
    (output_bytes state dst 1 = f64_bytes [42.0]);
  List.iter
    (fun (name, scale) ->
      let state = one_block_state (q1_block scale (String.make 16 '\255')) in
      set_output_cell state 10000 42.0;
      check (name ^ " rejects") (not (VM.run state q1_code));
      check
        (name ^ " keeps output")
        (output_bytes state 10000 1 = f64_bytes [42.0]))
    [
      "q1 positive infinity scale",
      "\000\124";
      "q1 negative infinity scale",
      "\000\252";
      "q1 nan scale",
      "\000\126";
    ];
  let state, _, _ = q1_state ~q1:(String.sub q1_owner 0 17) () in
  check "short q1 reverts" (not (VM.run state q1_code))

let check_shape_and_effort_revert () =
  let state, dst, _ = q1_state () in
  set_output_cell state dst 42.0;
  set_int_reg state 5 255;
  check "unaligned k reverts" (not (VM.run state q1_code));
  check
    "unaligned k does not overwrite output"
    (output_bytes state dst 1 = f64_bytes [42.0]);
  let state, dst, _ = q1_state ~limit:200 () in
  set_output_cell state dst 42.0;
  check "effort exhaustion reverts" (not (VM.run state q1_code));
  check
    "effort exhaustion does not overwrite output"
    (output_bytes state dst 1 = f64_bytes [42.0])

let check_offset_and_overlap () =
  let state, dst, _ = q1_state ~q1:("\255" ^ q1_owner) ~off:1 () in
  check "nonzero offset runs" (VM.run state q1_code);
  check
    "nonzero offset output"
    (output_bytes state dst 6 = expected_output);
  let state, dst, _ = q1_state ~dst:20000 () in
  check "overlap runs from snapshot" (VM.run state q1_code);
  check
    "overlap output"
    (output_bytes state dst 6 = expected_output)

let check_effects () =
  let names = Program_effects.(scan q1_code |> names) in
  check
    "q1 effects"
    (names = ["memory_read"; "memory_write"])

let capability name capability_root =
  Req.{ name; root = capability_root }

let limits =
  Req.{
    max_model_bytes = 1024;
    max_view_bytes = 1024;
    max_session_bytes = 1024;
    max_scratch_bytes = 8192;
    max_output_bytes = 1024;
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

let inference_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt Z.zero);
    VM.LDI (1, VM.VInt Z.zero);
    VM.LDI (2, VM.VString q1_owner);
    VM.LDI (3, VM.VInt Z.zero);
    VM.LDI (4, VM.VInt (Z.of_int 2));
    VM.LDI (5, VM.VInt (Z.of_int 256));
    VM.LDI (6, VM.VInt (Z.of_int 3));
    VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6);
    VM.STOP;
  |]

let starts_with prefix value =
  String.length value >= String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let check_capability_gate () =
  let cap = capability "tensor.q1-g128" (hex_root 'd') in
  let admitted =
    Admission.of_inference_code_with_requirement
      ~support:(support [cap])
      ~requirement:(requirement [cap])
      inference_code
  in
  (match admitted with
   | Ok _ -> ()
   | Error error -> failwith (Admission.error_message error));
  let missing =
    Admission.of_inference_code_with_requirement
      ~support:(support [])
      ~requirement:(requirement [])
      inference_code
  in
  match missing with
  | Error (Admission.Unsafe_error message) ->
    check
      "q1 capability is named"
      (starts_with
         "inference opcode LINEAR_Q1_G128_FP at pc 8 requires capability tensor.q1-g128"
         message)
  | _ -> failwith "expected q1 capability rejection"

let check_wire_roundtrip () =
  let raw = Bytecode.encode q1_code in
  match Bytecode.decode raw with
  | Ok decoded ->
    check
      "bytecode roundtrip"
      (decoded = q1_code);
    let asm = Assembler.emit q1_code in
    check
      "assembler emits q1"
      (String.equal
         asm
         "LINEAR_Q1_G128_FP r0, r1, r2, r3, r4, r5, r6\nSTOP");
    check "assembler parses q1" (Assembler.parse asm = q1_code)
  | Error error -> failwith error

let check_generic_admission_rejection () =
  let check_rejection label = function
    | Error (Admission.Unsafe_error message) ->
      check
        label
        (starts_with "consensus unsafe opcode LINEAR_Q1_G128_FP" message)
    | _ -> failwith ("expected " ^ label)
  in
  check_rejection "legacy rejects q1" (Admission.of_code q1_code);
  check_rejection "program rejects q1" (Admission.of_program q1_code)

let output_payload_from_bytes base raw =
  let count = String.length raw / 8 in
  let values =
    List.init count (fun index ->
      let bits = ref 0L in
      for byte = 0 to 7 do
        bits :=
          Int64.logor
            !bits
            (Int64.shift_left
               (Int64.of_int (Char.code raw.[(index * 8) + byte]))
               (byte * 8))
      done;
      "int:" ^ Z.to_string (Z.of_int64 !bits))
  in
  String.concat
    "|"
    [
      "base=" ^ string_of_int base;
      "length=" ^ string_of_int count;
      "values=" ^ String.concat "," values;
    ]

let inference_output_root ~target_root ~session_abi_root payload =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:output\000"
       ^ target_root ^ "\000" ^ session_abi_root ^ "\000" ^ payload)
    |> to_hex)

let simple_q1 =
  "\000\060" ^ String.make 16 '\255'

let simple_program range_root =
  let one = VM.VInt (Z.of_int64 (Int64.bits_of_float 1.0)) in
  Array.concat [
    [|
      VM.JDEST Abi.advance_label;
    |];
    Array.concat
      (List.init 128 (fun index ->
         [|
           VM.LDI (10, one);
           VM.MSTORE (20000 + index, 10);
         |]));
    [|
      VM.LDI (2, VM.VString range_root);
      VM.FLOAD (2, 2);
      VM.LDI (0, VM.VInt (Z.of_int 10000));
      VM.LDI (1, VM.VInt (Z.of_int 20000));
      VM.LDI (3, VM.VInt Z.zero);
      VM.LDI (4, VM.VInt Z.one);
      VM.LDI (5, VM.VInt (Z.of_int 128));
      VM.LDI (6, VM.VInt Z.one);
      VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6);
      VM.LDI (0, VM.VInt (Z.of_int 10000));
      VM.LDI (1, VM.VInt Z.one);
      VM.STOP;
    |];
  ]

let check_fload_session () =
  let owner_root = sha256 simple_q1 in
  let range =
    Model.{
      owner_root;
      offset = 0;
      length = String.length simple_q1;
      encoding = "tensor.q1-g128";
      shape_root = None;
    }
  in
  let range_root = Model.range_root range in
  let cap_store = capability "storage.authenticated-range" (hex_root 'd') in
  let cap_q1 = capability "tensor.q1-g128" (hex_root 'e') in
  let requirement = requirement [cap_store; cap_q1] in
  let support = support [cap_store; cap_q1] in
  let code = simple_program range_root in
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
        ~read:(fun root -> if String.equal root owner_root then Some simple_q1 else None)
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
  let expected_payload = output_payload_from_bytes 10000 (f64_bytes [128.0]) in
  let expected_root =
    inference_output_root
      ~target_root:(Target.root target)
      ~session_abi_root:Abi.v1_root
      expected_payload
  in
  match Execution.run ~plan () with
  | Ok result ->
    check "fload q1 session root" (String.equal result.Execution.output_root expected_root)
  | Error error -> failwith (Execution.error_message error)

let () =
  check_golden_fixture ();
  check_fp16_scale_decode_exhaustive ();
  check_fp64_core_edges ();
  check_sign_and_scale_edges ();
  check_accumulation_order_stress ();
  check_output_overflow_reverts ();
  check_profiled_run_equivalence ();
  check_invalid_input_reverts ();
  check_bad_q1_reverts ();
  check_shape_and_effort_revert ();
  check_offset_and_overlap ();
  check_effects ();
  check_capability_gate ();
  check_wire_roundtrip ();
  check_generic_admission_rejection ();
  check_fload_session ()
