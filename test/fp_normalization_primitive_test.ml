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
module Lang = Octra_vm.Oct_lang
module Gen = Octra_vm.Oct_gen
module Program_effects = Octra_vm.Program_effects
module Req = Octra_vm.Execution_requirement
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

let hex_value = function
  | '0'..'9' as c -> Char.code c - Char.code '0'
  | 'a'..'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A'..'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> failwith "invalid hex"

let bytes_of_hex hex =
  let compact =
    String.to_seq hex
    |> Seq.filter (function '\n' | '\r' | '\t' | ' ' -> false | _ -> true)
    |> String.of_seq
  in
  let len = String.length compact in
  if len mod 2 <> 0 then failwith "odd hex length";
  String.init (len / 2) (fun i ->
    Char.chr ((hex_value compact.[i * 2] lsl 4)
              lor hex_value compact.[(i * 2) + 1]))

let int64_le bytes offset =
  let value = ref 0L in
  for byte = 0 to 7 do
    value :=
      Int64.logor
        !value
        (Int64.shift_left
           (Int64.of_int (Char.code bytes.[offset + byte]))
           (byte * 8))
  done;
  !value

let put_int64_le buffer index value =
  for byte = 0 to 7 do
    Bytes.set
      buffer
      ((index * 8) + byte)
      (Char.chr
         (Int64.to_int
            (Int64.logand
               (Int64.shift_right_logical value (byte * 8))
               0xffL)))
  done

let f64_bytes values =
  let buffer = Bytes.create (List.length values * 8) in
  List.iteri
    (fun index value -> put_int64_le buffer index (Int64.bits_of_float value))
    values;
  Bytes.to_string buffer

let fixture_bits bytes =
  if String.length bytes mod 8 <> 0 then failwith "invalid f64 fixture";
  List.init (String.length bytes / 8) (fun i -> int64_le bytes (i * 8))

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let set_i64_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int64 value)

let set_f64_bits state addr bits =
  Hashtbl.replace state.VM.memory.data addr (VM.VInt (Z.of_int64 bits))

let f64_bits state addr =
  match Hashtbl.find_opt state.VM.memory.data addr with
  | Some (VM.VInt bits) when Z.fits_int64 bits -> Z.to_int64 bits
  | _ -> failwith "missing f64 cell"

let set_fixture state addr bytes =
  List.iteri
    (fun index bits -> set_f64_bits state (addr + index) bits)
    (fixture_bits bytes)

let check_cells label state base expected =
  List.iteri
    (fun index bits ->
      check
        (label ^ " output " ^ string_of_int index)
        (f64_bits state (base + index) = bits))
    expected

let raw_cells state base count =
  let raw = Bytes.create (count * 8) in
  for index = 0 to count - 1 do
    put_int64_le raw index (f64_bits state (base + index))
  done;
  Bytes.to_string raw

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
       let len = in_channel_length input in
       really_input_string input len)

let getenv_opt name =
  try Some (Sys.getenv name) with Not_found -> None

let model_epsilon_bits = 4517329193108106637L
let epsilon_1e_5_bits = 4532020583610935537L

let single_input =
  bytes_of_hex
    "000000000000f03f00000000000000c0000000000000084000000000000010c0"

let row_batched_input =
  bytes_of_hex
    "000000000000f03f00000000000000c0000000000000084000000000000010c0\
     000000000000d03f000000000000e03f000000000000e8bf000000000000f43f"

let gamma =
  bytes_of_hex
    "000000000000e03f000000000000f83f000000000000f0bf0000000000000040"

let single_expected_model_epsilon =
  bytes_of_hex
    "9c467d2c975ec73ff5f45d61f186f1bff5f45d61f186f1bf9c467d2c975e07c0"

let row_batched_expected_model_epsilon =
  bytes_of_hex
    "9c467d2c975ec73ff5f45d61f186f1bff5f45d61f186f1bf9c467d2c975e07c0\
     69b5ba35137fc43f1e1098d09cbeee3f1e1098d09cbeee3fc3622903d89e0940"

let single_expected_epsilon_1e_5 =
  bytes_of_hex
    "289c3e41965ec73f1ef5eeb0f086f1bf1ef5eeb0f086f1bf289c3e41965e07c0"

let signed_zero_subnormal_input =
  bytes_of_hex
    "0000000000000000000000000000008001000000000000000100000000000080"

let signed_zero_subnormal_gamma =
  bytes_of_hex
    "000000000000f03f000000000000f03f000000000000f03f000000000000f03f"

let min_subnormal_epsilon_input =
  bytes_of_hex
    "0000000000000000000000000000008001000000000000000100000000000080"

let min_subnormal_epsilon_expected =
  bytes_of_hex
    "00000000000000000000000000000080000000000000601e000000000000609e"

let l2_single_input =
  bytes_of_hex
    "0000000000000840000000000000104000000000000000000000000000000000"

let l2_single_expected_model_epsilon =
  bytes_of_hex
    "bfeec12c3333e33fa99302919999e93f00000000000000000000000000000000"

let l2_row_batched_input =
  bytes_of_hex
    "0000000000000840000000000000104000000000000000000000000000000000\
     000000000000f03f000000000000004000000000000000400000000000000000"

let l2_row_batched_expected_model_epsilon =
  bytes_of_hex
    "bfeec12c3333e33fa99302919999e93f00000000000000000000000000000000\
     8d0073415555d53f8d0073415555e53f8d0073415555e53f0000000000000000"

type rms_fixture = {
  name : string;
  count : int;
  epsilon_bits : int64;
  input : string;
  gamma : string;
  expected : string;
}

let single_model_epsilon = {
  name = "single vector model epsilon";
  count = 4;
  epsilon_bits = model_epsilon_bits;
  input = single_input;
  gamma;
  expected = single_expected_model_epsilon;
}

let single_epsilon_1e_5 = {
  name = "single vector epsilon 1e-5";
  count = 4;
  epsilon_bits = epsilon_1e_5_bits;
  input = single_input;
  gamma;
  expected = single_expected_epsilon_1e_5;
}

let signed_zero_subnormal = {
  name = "signed zero and subnormal";
  count = 4;
  epsilon_bits = Int64.bits_of_float 1.0;
  input = signed_zero_subnormal_input;
  gamma = signed_zero_subnormal_gamma;
  expected = signed_zero_subnormal_input;
}

let min_subnormal_epsilon = {
  name = "minimum subnormal epsilon";
  count = 4;
  epsilon_bits = 1L;
  input = min_subnormal_epsilon_input;
  gamma = signed_zero_subnormal_gamma;
  expected = min_subnormal_epsilon_expected;
}

let rms_op =
  VM.RMSNORM_FP_EPS (0, 1, 2, 3)

let rms_code = [|rms_op; VM.STOP|]
let rms_op_only = [|rms_op|]

let make_rms_state ?(limit = 1_000_000) ?(strict_values = false) ?(addr = 100)
    ?(gamma_base = 200) fixture =
  let state =
    VM.create_state
      ~limit
      ~strict_values
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 addr;
  set_int_reg state 1 fixture.count;
  set_int_reg state 2 gamma_base;
  set_i64_reg state 3 fixture.epsilon_bits;
  set_fixture state addr fixture.input;
  set_fixture state gamma_base fixture.gamma;
  state

let check_rms_golden () =
  List.iter
    (fun fixture ->
      let state = make_rms_state fixture in
      check (fixture.name ^ " succeeds") (VM.run state rms_code);
      check_cells fixture.name state 100 (fixture_bits fixture.expected))
    [single_model_epsilon; single_epsilon_1e_5]

let check_rms_signed_zero_and_subnormal () =
  let state = make_rms_state signed_zero_subnormal in
  check "rms signed zero and subnormal succeeds" (VM.run state rms_code);
  check_cells
    "rms signed zero and subnormal"
    state
    100
    (fixture_bits signed_zero_subnormal.expected)

let check_rms_min_subnormal_epsilon () =
  let state = make_rms_state min_subnormal_epsilon in
  check "rms minimum subnormal epsilon succeeds" (VM.run state rms_code);
  check_cells
    "rms minimum subnormal epsilon"
    state
    100
    (fixture_bits min_subnormal_epsilon.expected)

let check_rms_row_composition () =
  let state =
    make_rms_state
      {
        single_model_epsilon with
        name = "row composed";
        input = row_batched_input;
      }
  in
  let code =
    [|
      VM.LDI (0, VM.VInt (Z.of_int 100));
      rms_op;
      VM.LDI (0, VM.VInt (Z.of_int 104));
      rms_op;
      VM.STOP;
    |]
  in
  check "rms row composition succeeds" (VM.run state code);
  check_cells
    "rms row composition"
    state
    100
    (fixture_bits row_batched_expected_model_epsilon)

let check_rms_missing_and_nonfinite_revert () =
  let state = make_rms_state single_model_epsilon in
  Hashtbl.remove state.VM.memory.data 101;
  check "rms missing input rejects" (not (VM.run state rms_code));
  check "rms missing input keeps first cell"
    (f64_bits state 100 = List.hd (fixture_bits single_input));
  check "rms missing input keeps empty"
    (not (Hashtbl.mem state.VM.memory.data 101));
  let state = make_rms_state single_model_epsilon in
  Hashtbl.remove state.VM.memory.data 200;
  check "rms missing gamma rejects" (not (VM.run state rms_code));
  check_cells "rms missing gamma keeps input" state 100 (fixture_bits single_input);
  let state = make_rms_state single_model_epsilon in
  set_f64_bits state 100 0x7ff0000000000000L;
  check "rms nonfinite input rejects" (not (VM.run state rms_code));
  check "rms nonfinite input keeps input"
    (f64_bits state 100 = 0x7ff0000000000000L);
  let state = make_rms_state single_model_epsilon in
  set_f64_bits state 200 0x7ff8000000000000L;
  check "rms nonfinite gamma rejects" (not (VM.run state rms_code));
  check_cells "rms nonfinite gamma keeps input" state 100 (fixture_bits single_input)

let check_rms_invalid_epsilon_reverts () =
  List.iter
    (fun (name, set_epsilon) ->
      let state = make_rms_state single_model_epsilon in
      set_epsilon state;
      check (name ^ " rejects") (not (VM.run state rms_code));
      check_cells
        (name ^ " keeps input")
        state
        100
        (fixture_bits single_input))
    [
      "zero epsilon",
      (fun state -> set_i64_reg state 3 (Int64.bits_of_float 0.0));
      "negative zero epsilon",
      (fun state -> set_i64_reg state 3 (Int64.bits_of_float (-0.0)));
      "negative epsilon",
      (fun state -> set_i64_reg state 3 (Int64.bits_of_float (-1.0)));
      "infinite epsilon", (fun state -> set_i64_reg state 3 0x7ff0000000000000L);
      "nan epsilon", (fun state -> set_i64_reg state 3 0x7ff8000000000000L);
      "missing epsilon",
      (fun state -> state.VM.regs.(3) <- VM.VString "missing-epsilon");
      "unsigned epsilon carrier",
      (fun state -> state.VM.regs.(3) <- VM.VU64 (Z.of_int64 model_epsilon_bits));
      "oversized epsilon carrier",
      (fun state -> state.VM.regs.(3) <- VM.VInt (Z.shift_left Z.one 80));
    ]

let check_rms_invalid_shape_alias_and_effort () =
  List.iter
    (fun (name, update) ->
      let state = make_rms_state single_model_epsilon in
      update state;
      check (name ^ " rejects") (not (VM.run state rms_code));
      check_cells
        (name ^ " keeps input")
        state
        100
        (fixture_bits single_input))
    [
      "zero count", (fun state -> set_int_reg state 1 0);
      "oversized count", (fun state -> set_int_reg state 1 1_048_577);
      "address overflow", (fun state -> set_int_reg state 0 max_int);
      "gamma overflow", (fun state -> set_int_reg state 2 max_int);
      "input gamma alias", (fun state -> set_int_reg state 2 100);
      "partial input gamma overlap", (fun state -> set_int_reg state 2 101);
    ];
  let exact_effort = 50 + (single_model_epsilon.count * 4) in
  let state = make_rms_state ~limit:(exact_effort - 1) single_model_epsilon in
  check "one-under rms effort rejects" (not (VM.run state rms_op_only));
  check_cells
    "one-under rms effort keeps input"
    state
    100
    (fixture_bits single_input);
  let state = make_rms_state ~limit:exact_effort single_model_epsilon in
  check "exact rms effort succeeds" (VM.run state rms_op_only);
  check_cells
    "exact rms effort"
    state
    100
    (fixture_bits single_expected_model_epsilon)

let check_rms_overflow_reverts () =
  let fixture = {
    single_model_epsilon with
    input = f64_bytes [1e154; 0.0; 0.0; 0.0];
    gamma = f64_bytes [max_float; 1.0; 1.0; 1.0];
  } in
  let state = make_rms_state fixture in
  check "rms output overflow rejects" (not (VM.run state rms_code));
  check_cells
    "rms output overflow keeps input"
    state
    100
    (fixture_bits fixture.input);
  let fixture = {
    single_model_epsilon with
    input = f64_bytes [max_float; max_float; 0.0; 0.0];
  } in
  let state = make_rms_state fixture in
  check "rms sum overflow rejects" (not (VM.run state rms_code));
  check_cells
    "rms sum overflow keeps input"
    state
    100
    (fixture_bits fixture.input)

let check_rms_strict_operands () =
  let state = make_rms_state ~strict_values:true single_model_epsilon in
  state.VM.regs.(2) <- VM.VString "not-gamma-address";
  check "rms strict operands reject" (not (VM.run state rms_code))

type l2_fixture = {
  l2_name : string;
  l2_count : int;
  l2_epsilon_bits : int64;
  l2_input : string;
  l2_expected : string;
}

let l2_single_model_epsilon = {
  l2_name = "single vector l2 model epsilon";
  l2_count = 4;
  l2_epsilon_bits = model_epsilon_bits;
  l2_input = l2_single_input;
  l2_expected = l2_single_expected_model_epsilon;
}

let l2_op =
  VM.L2NORM_FP (0, 1, 2)

let l2_code = [|l2_op; VM.STOP|]
let l2_op_only = [|l2_op|]

let make_l2_state ?(limit = 1_000_000) ?(strict_values = false) ?(addr = 100)
    fixture =
  let state =
    VM.create_state
      ~limit
      ~strict_values
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 addr;
  set_int_reg state 1 fixture.l2_count;
  set_i64_reg state 2 fixture.l2_epsilon_bits;
  set_fixture state addr fixture.l2_input;
  state

let check_l2_golden () =
  let state = make_l2_state l2_single_model_epsilon in
  check "l2 succeeds" (VM.run state l2_code);
  check_cells
    l2_single_model_epsilon.l2_name
    state
    100
    (fixture_bits l2_single_model_epsilon.l2_expected)

let check_l2_row_composition () =
  let state =
    make_l2_state
      {
        l2_single_model_epsilon with
        l2_name = "l2 row composed";
        l2_input = l2_row_batched_input;
      }
  in
  let code =
    [|
      VM.LDI (0, VM.VInt (Z.of_int 100));
      l2_op;
      VM.LDI (0, VM.VInt (Z.of_int 104));
      l2_op;
      VM.STOP;
    |]
  in
  check "l2 row composition succeeds" (VM.run state code);
  check_cells
    "l2 row composition"
    state
    100
    (fixture_bits l2_row_batched_expected_model_epsilon)

let check_l2_missing_nonfinite_and_invalid_epsilon () =
  let original = fixture_bits l2_single_input in
  let state = make_l2_state l2_single_model_epsilon in
  Hashtbl.remove state.VM.memory.data 101;
  check "l2 missing input rejects" (not (VM.run state l2_code));
  check "l2 missing input keeps first cell"
    (f64_bits state 100 = List.hd original);
  let state = make_l2_state l2_single_model_epsilon in
  set_f64_bits state 100 0x7ff0000000000000L;
  check "l2 nonfinite input rejects" (not (VM.run state l2_code));
  check "l2 nonfinite input keeps input"
    (f64_bits state 100 = 0x7ff0000000000000L);
  List.iter
    (fun (name, set_epsilon) ->
      let state = make_l2_state l2_single_model_epsilon in
      set_epsilon state;
      check (name ^ " rejects") (not (VM.run state l2_code));
      check_cells (name ^ " keeps input") state 100 original)
    [
      "l2 zero epsilon",
      (fun state -> set_i64_reg state 2 (Int64.bits_of_float 0.0));
      "l2 negative epsilon",
      (fun state -> set_i64_reg state 2 (Int64.bits_of_float (-1.0)));
      "l2 infinite epsilon", (fun state -> set_i64_reg state 2 0x7ff0000000000000L);
      "l2 nan epsilon", (fun state -> set_i64_reg state 2 0x7ff8000000000000L);
      "l2 missing epsilon",
      (fun state -> state.VM.regs.(2) <- VM.VString "missing-epsilon");
    ]

let check_l2_invalid_shape_and_effort () =
  let original = fixture_bits l2_single_input in
  List.iter
    (fun (name, update) ->
      let state = make_l2_state l2_single_model_epsilon in
      update state;
      check (name ^ " rejects") (not (VM.run state l2_code));
      check_cells (name ^ " keeps input") state 100 original)
    [
      "l2 zero count", (fun state -> set_int_reg state 1 0);
      "l2 oversized count", (fun state -> set_int_reg state 1 1_048_577);
      "l2 address overflow", (fun state -> set_int_reg state 0 max_int);
    ];
  let exact_effort = 40 + (l2_single_model_epsilon.l2_count * 3) in
  let state = make_l2_state ~limit:(exact_effort - 1) l2_single_model_epsilon in
  check "one-under l2 effort rejects" (not (VM.run state l2_op_only));
  check_cells "one-under l2 effort keeps input" state 100 original;
  let state = make_l2_state ~limit:exact_effort l2_single_model_epsilon in
  check "exact l2 effort succeeds" (VM.run state l2_op_only);
  check_cells
    "exact l2 effort"
    state
    100
    (fixture_bits l2_single_model_epsilon.l2_expected)

let check_l2_overflow_reverts () =
  let fixture = {
    l2_single_model_epsilon with
    l2_input = f64_bytes [max_float; max_float; 0.0; 0.0];
  } in
  let state = make_l2_state fixture in
  check "l2 sum overflow rejects" (not (VM.run state l2_code));
  check_cells
    "l2 sum overflow keeps input"
    state
    100
    (fixture_bits fixture.l2_input)

let check_l2_strict_operands () =
  let state = make_l2_state ~strict_values:true l2_single_model_epsilon in
  state.VM.regs.(2) <- VM.VU64 (Z.of_int64 model_epsilon_bits);
  check "l2 strict operands reject" (not (VM.run state l2_code))

let mul_op =
  VM.ELEMWISE_MUL_FP (0, 1, 2)

let mul_code = [|mul_op; VM.STOP|]
let mul_op_only = [|mul_op|]

let make_mul_state ?(limit = 1_000_000) ?(strict_values = false) ?(dst = 100)
    ?(src = 200) ?(count = 3) () =
  let state =
    VM.create_state
      ~limit
      ~strict_values
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 dst;
  set_int_reg state 1 src;
  set_int_reg state 2 count;
  List.iteri
    (fun index value -> set_f64_bits state (dst + index) (Int64.bits_of_float value))
    [2.0; -3.0; 4.0];
  if dst <> src then
    List.iteri
      (fun index value -> set_f64_bits state (src + index) (Int64.bits_of_float value))
      [0.5; -2.0; -0.25];
  state

let check_mul_golden () =
  let state = make_mul_state () in
  check "elemwise mul succeeds" (VM.run state mul_code);
  check_cells
    "elemwise mul"
    state
    100
    (List.map Int64.bits_of_float [1.0; 6.0; -1.0])

let check_mul_in_place_alias () =
  let state = make_mul_state ~dst:100 ~src:100 () in
  check "elemwise mul in-place alias succeeds" (VM.run state mul_code);
  check_cells
    "elemwise mul in-place alias"
    state
    100
    (List.map Int64.bits_of_float [4.0; 9.0; 16.0])

let check_mul_rejections () =
  let state = make_mul_state () in
  Hashtbl.remove state.VM.memory.data 201;
  check "elemwise mul missing source rejects" (not (VM.run state mul_code));
  check_cells
    "elemwise mul missing source keeps output"
    state
    100
    (List.map Int64.bits_of_float [2.0; -3.0; 4.0]);
  let state = make_mul_state () in
  Hashtbl.remove state.VM.memory.data 101;
  check "elemwise mul missing dest rejects" (not (VM.run state mul_code));
  check "elemwise mul missing dest keeps empty"
    (not (Hashtbl.mem state.VM.memory.data 101));
  let state = make_mul_state () in
  set_f64_bits state 200 0x7ff0000000000000L;
  check "elemwise mul nonfinite source rejects" (not (VM.run state mul_code));
  check_cells
    "elemwise mul nonfinite source keeps output"
    state
    100
    (List.map Int64.bits_of_float [2.0; -3.0; 4.0]);
  let state = make_mul_state ~dst:101 ~src:100 () in
  List.iteri
    (fun index value -> set_f64_bits state (101 + index) (Int64.bits_of_float value))
    [42.0; 43.0; 44.0];
  check "elemwise mul partial overlap rejects" (not (VM.run state mul_code));
  check_cells
    "elemwise mul partial overlap keeps output"
    state
    101
    (List.map Int64.bits_of_float [42.0; 43.0; 44.0]);
  let state = make_mul_state ~count:0 () in
  check "elemwise mul zero count rejects" (not (VM.run state mul_code));
  check_cells
    "elemwise mul zero count keeps output"
    state
    100
    (List.map Int64.bits_of_float [2.0; -3.0; 4.0]);
  let exact_effort = 10 + (3 * 3) in
  let state = make_mul_state ~limit:(exact_effort - 1) () in
  check "elemwise mul one-under effort rejects" (not (VM.run state mul_op_only));
  check_cells
    "elemwise mul one-under effort keeps output"
    state
    100
    (List.map Int64.bits_of_float [2.0; -3.0; 4.0]);
  let state = make_mul_state ~limit:exact_effort () in
  check "elemwise mul exact effort succeeds" (VM.run state mul_op_only);
  check_cells
    "elemwise mul exact effort"
    state
    100
    (List.map Int64.bits_of_float [1.0; 6.0; -1.0])

let check_mul_strict_operands () =
  let state = make_mul_state ~strict_values:true () in
  state.VM.regs.(1) <- VM.VString "not-source-address";
  check "elemwise mul strict operands reject" (not (VM.run state mul_code))

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

let rms_admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (1, VM.VInt (Z.of_int 4));
    VM.LDI (2, VM.VInt (Z.of_int 200));
    VM.LDI (3, VM.VInt (Z.of_int64 model_epsilon_bits));
    rms_op;
    VM.STOP;
  |]

let mul_admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (1, VM.VInt (Z.of_int 200));
    VM.LDI (2, VM.VInt (Z.of_int 3));
    mul_op;
    VM.STOP;
  |]

let l2_admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (1, VM.VInt (Z.of_int 4));
    VM.LDI (2, VM.VInt (Z.of_int64 model_epsilon_bits));
    l2_op;
    VM.STOP;
  |]

let check_capability_gate () =
  let cap = capability "tensor.strict-fp" (hex_root 'd') in
  List.iter
    (fun (name, pc, code) ->
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
          (name ^ " capability is named")
          (starts_with
             ("inference opcode " ^ name ^ " at pc " ^ string_of_int pc
              ^ " requires capability tensor.strict-fp")
             message)
      | _ -> failwith ("expected strict-fp capability rejection: " ^ name))
    [
      "RMSNORM_FP_EPS", 5, rms_admission_code;
      "L2NORM_FP", 4, l2_admission_code;
      "ELEMWISE_MUL_FP", 4, mul_admission_code;
    ];
  match
    Admission.of_inference_code_with_requirement
      ~support:(support [cap])
      ~requirement:(requirement [cap])
      [|VM.JDEST 100; VM.RMSNORM_FP (0, 1, 2); VM.STOP|]
  with
  | Error (Admission.Unsafe_error message) ->
    check "fixed epsilon rmsnorm remains forbidden"
      (starts_with "inference opcode RMSNORM_FP" message)
  | _ -> failwith "expected fixed epsilon rmsnorm rejection"

let check_epsilon_type_flow () =
  let cap = capability "tensor.strict-fp" (hex_root 'd') in
  let code =
    Array.copy rms_admission_code
  in
  code.(4) <- VM.LDI (3, VM.VU64 (Z.of_int64 model_epsilon_bits));
  (match
     Admission.of_inference_code_with_requirement
       ~support:(support [cap])
       ~requirement:(requirement [cap])
       code
   with
   | Error (Admission.Verify_error message) ->
     check
       "unsigned epsilon carrier rejected by type flow"
       (starts_with
          "Program type flow: expected int in r3 at pc 5, got u64"
          message)
   | _ -> failwith "expected epsilon type-flow rejection");
  let code =
    Array.copy l2_admission_code
  in
  code.(3) <- VM.LDI (2, VM.VU64 (Z.of_int64 model_epsilon_bits));
  match
    Admission.of_inference_code_with_requirement
      ~support:(support [cap])
      ~requirement:(requirement [cap])
      code
  with
  | Error (Admission.Verify_error message) ->
    check
      "l2 unsigned epsilon carrier rejected by type flow"
      (starts_with
         "Program type flow: expected int in r2 at pc 4, got u64"
         message)
  | _ -> failwith "expected l2 epsilon type-flow rejection"

let check_generic_admission_rejection () =
  let check_rejection name label = function
    | Error (Admission.Unsafe_error message) ->
      check
        label
        (starts_with ("consensus unsafe opcode " ^ name) message)
    | _ -> failwith ("expected " ^ label)
  in
  List.iter
    (fun (name, code) ->
      check_rejection name ("legacy rejects " ^ name) (Admission.of_code code);
      check_rejection name ("program rejects " ^ name) (Admission.of_program code))
    [
      "RMSNORM_FP_EPS", rms_admission_code;
      "L2NORM_FP", l2_admission_code;
      "ELEMWISE_MUL_FP", mul_admission_code;
    ]

let compiler_contract declaration name args =
  let open Lang in
  let fn =
    {
      fn_name = "advance";
      fn_params = [];
      fn_ret = TVoid;
      fn_view = false;
      fn_pure = false;
      fn_payable = false;
      fn_nonreentrant = false;
      fn_vis = Public;
      fn_body = [SExpr (ECall (name, args))];
    }
  in
  {
    declaration;
    name = "Fp";
    imports = [];
    structs = [];
    enums = [];
    consts = [];
    invariants_decl = [];
    state = [];
    events = [];
    errors = [];
    interfaces = [];
    implements = [];
    ctor = None;
    funcs = [fn];
  }

let code_contains predicate code =
  Array.exists predicate code

let int_args count =
  let open Lang in
  List.init count (fun index -> EInt (Z.of_int index))

let check_compiler_surface () =
  List.iter
    (fun (name, args, expected) ->
      let program_code =
        Gen.generate (compiler_contract Lang.ProgramDecl name (int_args args))
      in
      check
        (name ^ " compiler emits opcode in Program")
        (code_contains expected program_code);
      match Gen.generate (compiler_contract Lang.ContractDecl name (int_args args)) with
      | _ -> failwith (name ^ " should reject outside Program")
      | exception Gen.GenError (message, _) ->
        check
          (name ^ " compiler rejection")
          (String.equal message (name ^ " is available only in Program")))
    [
      "rmsnorm_fp_eps", 4, (function VM.RMSNORM_FP_EPS _ -> true | _ -> false);
      "l2norm_fp", 3, (function VM.L2NORM_FP _ -> true | _ -> false);
      "elemwise_mul_fp", 3, (function VM.ELEMWISE_MUL_FP _ -> true | _ -> false);
    ]

let check_wire_roundtrip () =
  List.iter
    (fun (asm, op) ->
      let raw = Bytecode.encode [|op; VM.STOP|] in
      (match Bytecode.decode raw with
       | Ok [|decoded; VM.STOP|] ->
         check (asm ^ " bytecode roundtrip") (decoded = op)
       | Ok _ -> failwith "unexpected decoded normalization code"
       | Error error -> failwith error);
      check (asm ^ " assembler parse")
        (Assembler.parse (asm ^ "\nSTOP") = [|op; VM.STOP|]);
      check (asm ^ " assembler emit")
        (String.equal (Assembler.emit [|op; VM.STOP|]) (asm ^ "\nSTOP")))
    [
      "RMSNORM_FP_EPS r0, r1, r2, r3", rms_op;
      "L2NORM_FP r0, r1, r2", l2_op;
      "ELEMWISE_MUL_FP r0, r1, r2", mul_op;
    ]

let check_effects () =
  let effects =
    Program_effects.names
      (Program_effects.scan [|rms_op; l2_op; mul_op|])
  in
  check "normalization effects" (effects = ["memory_read"; "memory_write"])

let check_full_binding_if_available () =
  match getenv_opt "OCTRA_RMSNORM_EPS_EVIDENCE" with
  | None -> ()
  | Some base ->
    let input =
      read_file (Filename.concat base "binding/input-recurrent-output.f64le")
    in
    let gamma =
      read_file (Filename.concat base "binding/gamma.f64le")
    in
    let expected =
      read_file (Filename.concat base "binding/expected-rmsnorm-output.f64le")
    in
    let fixture = {
      single_model_epsilon with
      count = 128;
      input;
      gamma;
      expected;
    } in
    let state =
      make_rms_state ~addr:20_000 ~gamma_base:40_000 fixture
    in
    for row = 0 to 47 do
      state.VM.pc <- 0;
      set_int_reg state 0 (20_000 + (row * 128));
      check
        ("bonsai rmsnorm row " ^ string_of_int row)
        (VM.run state rms_op_only)
    done;
    check
      "bonsai rmsnorm binding sha"
      (String.equal
         (sha256 (raw_cells state 20_000 (48 * 128)))
         "f2d9ad1d096f2e5f38f652cc5aacbfde0d1239ec0e9f4a79d2e45747793f69ab")

let () =
  check_rms_golden ();
  check_rms_signed_zero_and_subnormal ();
  check_rms_min_subnormal_epsilon ();
  check_rms_row_composition ();
  check_rms_missing_and_nonfinite_revert ();
  check_rms_invalid_epsilon_reverts ();
  check_rms_invalid_shape_alias_and_effort ();
  check_rms_overflow_reverts ();
  check_rms_strict_operands ();
  check_l2_golden ();
  check_l2_row_composition ();
  check_l2_missing_nonfinite_and_invalid_epsilon ();
  check_l2_invalid_shape_and_effort ();
  check_l2_overflow_reverts ();
  check_l2_strict_operands ();
  check_mul_golden ();
  check_mul_in_place_alias ();
  check_mul_rejections ();
  check_mul_strict_operands ();
  check_capability_gate ();
  check_epsilon_type_flow ();
  check_generic_admission_rejection ();
  check_compiler_surface ();
  check_wire_roundtrip ();
  check_effects ();
  check_full_binding_if_available ()
