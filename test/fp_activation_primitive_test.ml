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

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let int_reg state reg =
  match state.VM.regs.(reg) with
  | VM.VInt value when Z.fits_int value -> Z.to_int value
  | _ -> failwith "missing int register"

let set_f64_bits state addr bits =
  Hashtbl.replace state.VM.memory.data addr (VM.VInt (Z.of_int64 bits))

let f64_bits state addr =
  match Hashtbl.find_opt state.VM.memory.data addr with
  | Some (VM.VInt bits) when Z.fits_int64 bits -> Z.to_int64 bits
  | _ -> failwith "missing f64 cell"

let set_fixture state addr bytes =
  if String.length bytes mod 8 <> 0 then failwith "invalid f64 fixture";
  for i = 0 to (String.length bytes / 8) - 1 do
    set_f64_bits state (addr + i) (int64_le bytes (i * 8))
  done

let fixture_bits bytes =
  if String.length bytes mod 8 <> 0 then failwith "invalid f64 fixture";
  List.init (String.length bytes / 8) (fun i -> int64_le bytes (i * 8))

let finite_f64_bits bits =
  match classify_float (Int64.float_of_bits bits) with
  | FP_nan
  | FP_infinite -> false
  | FP_normal
  | FP_subnormal
  | FP_zero -> true

let run_activation op input =
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
  let count = String.length input / 8 in
  set_int_reg state 0 100;
  set_int_reg state 1 count;
  set_fixture state 100 input;
  state, VM.run state [|op; VM.STOP|]

let sigmoid_input =
  bytes_of_hex
    "00000000000008c0000000000000f0bf0000000000000000000000000000f03f\
     0000000000000840"

let sigmoid_output =
  bytes_of_hex
    "4654903c3448a83f86ba54145636d13f000000000000e03fbda2d5f5d464e73f\
     bdfa36bc7c7bee3f"

let softplus_input =
  bytes_of_hex
    "00000000000010c0000000000000f0bf0000000000000000000000000000f03f\
     0000000000001040"

let softplus_output =
  bytes_of_hex
    "53b6530be595923f25c1bebf7a0cd43fef39fafe422ee63f49b0efaf1e03f53f\
     b6530be595121040"

let silu_output =
  bytes_of_hex
    "343f6c2d2736c2bf86ba54145636d1bf0000000000000000bda2d5f5d464e73f\
     0e3c298d9ddc0640"

let check_golden label op input expected =
  let state, ok = run_activation op input in
  check (label ^ " succeeds") ok;
  List.iteri
    (fun index bits ->
      check
        (label ^ " output " ^ string_of_int index)
        (f64_bits state (100 + index) = bits))
    (fixture_bits expected)

let check_missing_cell_reverts_atomically () =
  List.iter
    (fun (name, op) ->
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
      set_int_reg state 1 2;
      set_f64_bits state 100 (Int64.bits_of_float 2.0);
      set_f64_bits state 101 (Int64.bits_of_float 9.0);
      Hashtbl.remove state.VM.memory.data 101;
      check (name ^ " missing input reverts")
        (not (VM.run state [|op; VM.STOP|]));
      check (name ^ " first input unchanged")
        (f64_bits state 100 = Int64.bits_of_float 2.0);
      check (name ^ " second input remains empty")
        (not (Hashtbl.mem state.VM.memory.data 101)))
    [
      "SIGMOID_FP", VM.SIGMOID_FP (0, 1);
      "SILU_FP", VM.SILU_FP (0, 1);
    ]

let check_nonfinite_reverts_atomically () =
  List.iter
    (fun (name, op) ->
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
      set_int_reg state 1 2;
      set_f64_bits state 100 0x7ff0000000000000L;
      set_f64_bits state 101 (Int64.bits_of_float 9.0);
      check (name ^ " non-finite input reverts")
        (not (VM.run state [|op; VM.STOP|]));
      check (name ^ " non-finite input unchanged")
        (f64_bits state 100 = 0x7ff0000000000000L);
      check (name ^ " following input unchanged")
        (f64_bits state 101 = Int64.bits_of_float 9.0))
    [
      "SIGMOID_FP", VM.SIGMOID_FP (0, 1);
      "SOFTPLUS_FP", VM.SOFTPLUS_FP (0, 1);
      "SILU_FP", VM.SILU_FP (0, 1);
    ]

let check_strict_operands () =
  let state =
    VM.create_state
      ~limit:1_000_000
      ~strict_values:true
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  state.VM.regs.(0) <- VM.VString "not-an-address";
  set_int_reg state 1 1;
  set_f64_bits state 100 (Int64.bits_of_float 1.0);
  check "silu strict operand reverts"
    (not (VM.run state [|VM.SILU_FP (0, 1); VM.STOP|]));
  check "silu strict operand leaves memory unchanged"
    (f64_bits state 100 = Int64.bits_of_float 1.0);
  let state =
    VM.create_state
      ~limit:1_000_000
      ~strict_values:true
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg state 0 100;
  state.VM.regs.(1) <- VM.VString "not-an-address";
  set_int_reg state 2 1;
  set_f64_bits state 100 (Int64.bits_of_float 1.0);
  check "residual strict operand reverts"
    (not (VM.run state [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]));
  check "residual strict operand leaves memory unchanged"
    (f64_bits state 100 = Int64.bits_of_float 1.0);
  let state =
    VM.create_state
      ~limit:1_000_000
      ~strict_values:true
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  state.VM.regs.(0) <- VM.VString "not-an-address";
  set_int_reg state 1 1;
  set_f64_bits state 100 (Int64.bits_of_float 1.0);
  check "argmax strict operand reverts"
    (not (VM.run state [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]));
  check "argmax strict operand leaves destination" (int_reg state 2 = 0)

let check_invalid_shape_and_effort_reverts () =
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
  set_int_reg state 1 0;
  set_f64_bits state 100 (Int64.bits_of_float 1.0);
  check "zero count activation reverts"
    (not (VM.run state [|VM.SIGMOID_FP (0, 1); VM.STOP|]));
  check "zero count leaves input unchanged"
    (f64_bits state 100 = Int64.bits_of_float 1.0);
  let exhausted =
    VM.create_state
      ~limit:19
      ~caller:"caller"
      ~origin:"origin"
      ~address:"contract"
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  set_int_reg exhausted 0 100;
  set_int_reg exhausted 1 1;
  set_f64_bits exhausted 100 (Int64.bits_of_float 1.0);
  check "activation effort exhaustion reverts"
    (not (VM.run exhausted [|VM.SIGMOID_FP (0, 1); VM.STOP|]));
  check "effort exhaustion leaves input unchanged"
    (f64_bits exhausted 100 = Int64.bits_of_float 1.0)

let fresh_state ?(limit = 1_000_000) () =
  VM.create_state
    ~limit
    ~caller:"caller"
    ~origin:"origin"
    ~address:"contract"
    ~value:Z.zero
    ~storage:(Hashtbl.create 0)
    ()

let set_f64_values state base values =
  List.iteri
    (fun index value ->
      set_f64_bits state (base + index) (Int64.bits_of_float value))
    values

let check_activation_large_magnitude_edges () =
  let sigmoid = fresh_state () in
  set_int_reg sigmoid 0 100;
  set_int_reg sigmoid 1 2;
  set_f64_values sigmoid 100 [max_float; -. max_float];
  check "sigmoid large magnitude succeeds"
    (VM.run sigmoid [|VM.SIGMOID_FP (0, 1); VM.STOP|]);
  check
    "sigmoid large positive saturates"
    (f64_bits sigmoid 100 = Int64.bits_of_float 1.0);
  check
    "sigmoid large negative saturates"
    (f64_bits sigmoid 101 = Int64.bits_of_float 0.0);
  let silu = fresh_state () in
  set_int_reg silu 0 100;
  set_int_reg silu 1 2;
  set_f64_values silu 100 [max_float; -. max_float];
  check "silu large magnitude succeeds"
    (VM.run silu [|VM.SILU_FP (0, 1); VM.STOP|]);
  check
    "silu large positive preserves magnitude"
    (f64_bits silu 100 = Int64.bits_of_float max_float);
  check
    "silu large negative saturates to negative zero"
    (f64_bits silu 101 = Int64.bits_of_float (-0.0))

let check_activation_zero_subnormal_edges () =
  let pos_min_subnormal = 0x0000000000000001L in
  let neg_min_subnormal = 0x8000000000000001L in
  let sigmoid = fresh_state () in
  set_int_reg sigmoid 0 100;
  set_int_reg sigmoid 1 4;
  set_f64_bits sigmoid 100 (Int64.bits_of_float 0.0);
  set_f64_bits sigmoid 101 (Int64.bits_of_float (-0.0));
  set_f64_bits sigmoid 102 pos_min_subnormal;
  set_f64_bits sigmoid 103 neg_min_subnormal;
  check "sigmoid zero/subnormal succeeds"
    (VM.run sigmoid [|VM.SIGMOID_FP (0, 1); VM.STOP|]);
  check
    "sigmoid positive zero"
    (f64_bits sigmoid 100 = Int64.bits_of_float 0.5);
  check
    "sigmoid negative zero"
    (f64_bits sigmoid 101 = Int64.bits_of_float 0.5);
  check "sigmoid positive subnormal finite" (finite_f64_bits (f64_bits sigmoid 102));
  check "sigmoid negative subnormal finite" (finite_f64_bits (f64_bits sigmoid 103));
  let silu = fresh_state () in
  set_int_reg silu 0 100;
  set_int_reg silu 1 4;
  set_f64_bits silu 100 (Int64.bits_of_float 0.0);
  set_f64_bits silu 101 (Int64.bits_of_float (-0.0));
  set_f64_bits silu 102 pos_min_subnormal;
  set_f64_bits silu 103 neg_min_subnormal;
  check "silu zero/subnormal succeeds"
    (VM.run silu [|VM.SILU_FP (0, 1); VM.STOP|]);
  check "silu positive zero" (f64_bits silu 100 = Int64.bits_of_float 0.0);
  check "silu negative zero" (f64_bits silu 101 = Int64.bits_of_float (-0.0));
  check "silu positive subnormal finite" (finite_f64_bits (f64_bits silu 102));
  check "silu negative subnormal finite" (finite_f64_bits (f64_bits silu 103))

let check_activation_effort_accounting () =
  List.iter
    (fun (name, op) ->
      let exact = fresh_state ~limit:26 () in
      set_int_reg exact 0 100;
      set_int_reg exact 1 2;
      set_f64_values exact 100 [1.0; -1.0];
      check (name ^ " exact effort succeeds") (VM.run exact [|op|]);
      check (name ^ " exact effort charged") (exact.VM.effort_used = 26);
      let one_under = fresh_state ~limit:25 () in
      set_int_reg one_under 0 100;
      set_int_reg one_under 1 2;
      set_f64_values one_under 100 [1.0; -1.0];
      check (name ^ " one-under effort reverts") (not (VM.run one_under [|op|]));
      check
        (name ^ " one-under effort leaves input")
        (f64_bits one_under 100 = Int64.bits_of_float 1.0))
    [
      "sigmoid", VM.SIGMOID_FP (0, 1);
      "silu", VM.SILU_FP (0, 1);
    ]

let check_residual_add () =
  let state = fresh_state () in
  set_int_reg state 0 100;
  set_int_reg state 1 200;
  set_int_reg state 2 3;
  set_f64_values state 100 [1.5; -2.0; 0.25];
  set_f64_values state 200 [0.5; 3.0; -4.0];
  check "residual add succeeds"
    (VM.run state [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]);
  List.iteri
    (fun index expected ->
      check
        ("residual add output " ^ string_of_int index)
        (f64_bits state (100 + index) = Int64.bits_of_float expected))
    [2.0; 1.0; -3.75];
  let same = fresh_state () in
  set_int_reg same 0 100;
  set_int_reg same 1 100;
  set_int_reg same 2 2;
  set_f64_values same 100 [1.25; -3.5];
  check "residual add same range succeeds"
    (VM.run same [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]);
  check "residual add same range doubles first"
    (f64_bits same 100 = Int64.bits_of_float 2.5);
  check "residual add same range doubles second"
    (f64_bits same 101 = Int64.bits_of_float (-7.0))

let check_residual_reverts_atomically () =
  let missing = fresh_state () in
  set_int_reg missing 0 100;
  set_int_reg missing 1 200;
  set_int_reg missing 2 2;
  set_f64_values missing 100 [7.0; 8.0];
  set_f64_bits missing 200 (Int64.bits_of_float 1.0);
  check "residual missing source reverts"
    (not (VM.run missing [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]));
  check "residual missing source leaves first dst"
    (f64_bits missing 100 = Int64.bits_of_float 7.0);
  check "residual missing source leaves second dst"
    (f64_bits missing 101 = Int64.bits_of_float 8.0);
  let nonfinite = fresh_state () in
  set_int_reg nonfinite 0 100;
  set_int_reg nonfinite 1 200;
  set_int_reg nonfinite 2 2;
  set_f64_values nonfinite 100 [7.0; 8.0];
  set_f64_bits nonfinite 200 0x7ff0000000000000L;
  set_f64_bits nonfinite 201 (Int64.bits_of_float 1.0);
  check "residual non-finite source reverts"
    (not (VM.run nonfinite [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]));
  check "residual non-finite source leaves first dst"
    (f64_bits nonfinite 100 = Int64.bits_of_float 7.0);
  check "residual non-finite source leaves second dst"
    (f64_bits nonfinite 101 = Int64.bits_of_float 8.0);
  let missing_dst = fresh_state () in
  set_int_reg missing_dst 0 100;
  set_int_reg missing_dst 1 200;
  set_int_reg missing_dst 2 2;
  set_f64_bits missing_dst 101 (Int64.bits_of_float 8.0);
  set_f64_values missing_dst 200 [1.0; 2.0];
  check "residual missing dst reverts"
    (not (VM.run missing_dst [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]));
  check "residual missing dst leaves second dst"
    (f64_bits missing_dst 101 = Int64.bits_of_float 8.0);
  let nonfinite_dst = fresh_state () in
  set_int_reg nonfinite_dst 0 100;
  set_int_reg nonfinite_dst 1 200;
  set_int_reg nonfinite_dst 2 2;
  set_f64_bits nonfinite_dst 100 0x7ff0000000000000L;
  set_f64_bits nonfinite_dst 101 (Int64.bits_of_float 8.0);
  set_f64_values nonfinite_dst 200 [1.0; 2.0];
  check "residual non-finite dst reverts"
    (not (VM.run nonfinite_dst [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]));
  check "residual non-finite dst leaves first dst"
    (f64_bits nonfinite_dst 100 = 0x7ff0000000000000L);
  check "residual non-finite dst leaves second dst"
    (f64_bits nonfinite_dst 101 = Int64.bits_of_float 8.0);
  let overflow = fresh_state () in
  set_int_reg overflow 0 100;
  set_int_reg overflow 1 200;
  set_int_reg overflow 2 2;
  set_f64_values overflow 100 [max_float; 8.0];
  set_f64_values overflow 200 [max_float; 2.0];
  check "residual output overflow reverts"
    (not (VM.run overflow [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]));
  check "residual output overflow leaves first dst"
    (f64_bits overflow 100 = Int64.bits_of_float max_float);
  check "residual output overflow leaves second dst"
    (f64_bits overflow 101 = Int64.bits_of_float 8.0);
  let overlap = fresh_state () in
  set_int_reg overlap 0 100;
  set_int_reg overlap 1 101;
  set_int_reg overlap 2 2;
  set_f64_values overlap 100 [1.0; 2.0; 3.0];
  check "residual partial overlap reverts"
    (not (VM.run overlap [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]));
  check "residual partial overlap leaves first"
    (f64_bits overlap 100 = Int64.bits_of_float 1.0);
  check "residual partial overlap leaves second"
    (f64_bits overlap 101 = Int64.bits_of_float 2.0);
  let exhausted = fresh_state ~limit:19 () in
  set_int_reg exhausted 0 100;
  set_int_reg exhausted 1 200;
  set_int_reg exhausted 2 10;
  set_f64_values exhausted 100 (List.init 10 float_of_int);
  set_f64_values exhausted 200 (List.init 10 (fun _ -> 1.0));
  check "residual effort exhaustion reverts"
    (not (VM.run exhausted [|VM.RESIDUAL_ADD_FP (0, 1, 2); VM.STOP|]));
  check "residual effort exhaustion leaves dst"
    (f64_bits exhausted 100 = Int64.bits_of_float 0.0)

let check_residual_effort () =
  let exact = fresh_state ~limit:30 () in
  set_int_reg exact 0 100;
  set_int_reg exact 1 200;
  set_int_reg exact 2 10;
  set_f64_values exact 100 (List.init 10 float_of_int);
  set_f64_values exact 200 (List.init 10 (fun _ -> 1.0));
  check "residual exact effort succeeds"
    (VM.run exact [|VM.RESIDUAL_ADD_FP (0, 1, 2)|]);
  check "residual exact effort charged"
    (exact.VM.effort_used = 30);
  let one_under = fresh_state ~limit:29 () in
  set_int_reg one_under 0 100;
  set_int_reg one_under 1 200;
  set_int_reg one_under 2 10;
  set_f64_values one_under 100 (List.init 10 float_of_int);
  set_f64_values one_under 200 (List.init 10 (fun _ -> 1.0));
  check "residual one-under effort reverts"
    (not (VM.run one_under [|VM.RESIDUAL_ADD_FP (0, 1, 2)|]));
  check "residual one-under effort leaves dst"
    (f64_bits one_under 100 = Int64.bits_of_float 0.0)

let check_argmax_fp () =
  let state = fresh_state () in
  set_int_reg state 0 100;
  set_int_reg state 1 4;
  set_f64_values state 100 [1.0; 3.0; 3.0; -1.0];
  check "argmax succeeds"
    (VM.run state [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]);
  check "argmax chooses first maximum" (int_reg state 2 = 1);
  let negative = fresh_state () in
  set_int_reg negative 0 100;
  set_int_reg negative 1 3;
  set_f64_values negative 100 [-7.0; -2.0; -3.0];
  check "argmax all-negative succeeds"
    (VM.run negative [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]);
  check "argmax all-negative chooses maximum" (int_reg negative 2 = 1);
  let signed_zero = fresh_state () in
  set_int_reg signed_zero 0 100;
  set_int_reg signed_zero 1 2;
  set_f64_bits signed_zero 100 (Int64.bits_of_float (-0.0));
  set_f64_bits signed_zero 101 (Int64.bits_of_float 0.0);
  check "argmax signed-zero tie succeeds"
    (VM.run signed_zero [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]);
  check "argmax signed-zero tie chooses first" (int_reg signed_zero 2 = 0);
  let subnormal = fresh_state () in
  set_int_reg subnormal 0 100;
  set_int_reg subnormal 1 4;
  set_f64_bits subnormal 100 (Int64.logor Int64.min_int 1L);
  set_f64_bits subnormal 101 Int64.min_int;
  set_f64_bits subnormal 102 1L;
  set_f64_bits subnormal 103 0L;
  check "argmax subnormal ordering succeeds"
    (VM.run subnormal [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]);
  check "argmax subnormal ordering chooses positive min" (int_reg subnormal 2 = 2);
  let alias = fresh_state () in
  set_int_reg alias 0 100;
  set_int_reg alias 1 2;
  set_f64_values alias 100 [1.0; 2.0];
  check "argmax destination alias succeeds"
    (VM.run alias [|VM.ARGMAX_FP (0, 0, 1); VM.STOP|]);
  check "argmax destination alias writes index" (int_reg alias 0 = 1);
  let missing = fresh_state () in
  set_int_reg missing 0 100;
  set_int_reg missing 1 2;
  set_f64_bits missing 100 (Int64.bits_of_float 1.0);
  check "argmax missing input reverts"
    (not (VM.run missing [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]));
  check "argmax missing input leaves destination" (int_reg missing 2 = 0);
  let nonfinite = fresh_state () in
  set_int_reg nonfinite 0 100;
  set_int_reg nonfinite 1 2;
  set_f64_bits nonfinite 100 0x7ff8000000000000L;
  set_f64_bits nonfinite 101 (Int64.bits_of_float 1.0);
  check "argmax non-finite input reverts"
    (not (VM.run nonfinite [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]));
  check "argmax non-finite input leaves destination" (int_reg nonfinite 2 = 0);
  let infinity = fresh_state () in
  set_int_reg infinity 0 100;
  set_int_reg infinity 1 2;
  set_f64_bits infinity 100 0x7ff0000000000000L;
  set_f64_bits infinity 101 0xfff0000000000000L;
  check "argmax infinity input reverts"
    (not (VM.run infinity [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]));
  check "argmax infinity input leaves destination" (int_reg infinity 2 = 0);
  let zero = fresh_state () in
  set_int_reg zero 0 100;
  set_int_reg zero 1 0;
  check "argmax zero length reverts"
    (not (VM.run zero [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]));
  let bad_addr = fresh_state () in
  set_int_reg bad_addr 0 (-1);
  set_int_reg bad_addr 1 1;
  check "argmax negative address reverts"
    (not (VM.run bad_addr [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]));
  check "argmax negative address leaves destination" (int_reg bad_addr 2 = 0);
  let too_large = fresh_state () in
  set_int_reg too_large 0 100;
  set_int_reg too_large 1 1_048_577;
  check "argmax over-limit length reverts"
    (not (VM.run too_large [|VM.ARGMAX_FP (2, 0, 1); VM.STOP|]));
  check "argmax over-limit length leaves destination" (int_reg too_large 2 = 0);
  let exact = fresh_state ~limit:10 () in
  set_int_reg exact 0 100;
  set_int_reg exact 1 10;
  set_f64_values exact 100 (List.init 10 float_of_int);
  check "argmax exact effort succeeds"
    (VM.run exact [|VM.ARGMAX_FP (2, 0, 1)|]);
  check "argmax exact effort charged" (exact.VM.effort_used = 10);
  let one_under = fresh_state ~limit:9 () in
  set_int_reg one_under 0 100;
  set_int_reg one_under 1 10;
  set_f64_values one_under 100 (List.init 10 float_of_int);
  check "argmax one-under effort reverts"
    (not (VM.run one_under [|VM.ARGMAX_FP (2, 0, 1)|]));
  check "argmax one-under effort leaves destination" (int_reg one_under 2 = 0)

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

let admission_code op =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (1, VM.VInt Z.one);
    VM.LDI (2, VM.VInt Z.one);
    op;
    VM.STOP;
  |]

let check_capability_gate () =
  let cap = capability "tensor.strict-fp" (hex_root 'd') in
  List.iter
    (fun (name, op) ->
      (match
         Admission.of_inference_code_with_requirement
           ~support:(support [cap])
           ~requirement:(requirement [cap])
           (admission_code op)
       with
       | Ok _ -> ()
       | Error error -> failwith (Admission.error_message error));
      match
        Admission.of_inference_code_with_requirement
          ~support:(support [])
          ~requirement:(requirement [])
          (admission_code op)
      with
      | Error (Admission.Unsafe_error message) ->
        check
          (name ^ " capability is named")
          (starts_with
             ("inference opcode " ^ name
              ^ " at pc 4 requires capability tensor.strict-fp")
             message)
      | _ -> failwith ("expected activation capability rejection: " ^ name))
    [
      "SIGMOID_FP", VM.SIGMOID_FP (0, 1);
      "SOFTPLUS_FP", VM.SOFTPLUS_FP (0, 1);
      "SILU_FP", VM.SILU_FP (0, 1);
      "RESIDUAL_ADD_FP", VM.RESIDUAL_ADD_FP (0, 1, 2);
    ]

let check_argmax_capability_gate () =
  let cap = capability "tensor.argmax" (hex_root 'd') in
  (match
     Admission.of_inference_code_with_requirement
       ~support:(support [cap])
       ~requirement:(requirement [cap])
       (admission_code (VM.ARGMAX_FP (0, 1, 2)))
   with
   | Ok _ -> ()
   | Error error -> failwith (Admission.error_message error));
  (match
     Admission.of_inference_code_with_requirement
       ~support:(support [])
       ~requirement:(requirement [])
       (admission_code (VM.ARGMAX_FP (0, 1, 2)))
   with
   | Error (Admission.Unsafe_error message) ->
     check
       "argmax capability is named"
       (starts_with
          "inference opcode ARGMAX_FP at pc 4 requires capability tensor.argmax"
          message)
   | _ -> failwith "expected argmax capability rejection");
  let fp_cap = capability "tensor.strict-fp" (hex_root 'e') in
  match
    Admission.of_inference_code_with_requirement
      ~support:(support [fp_cap])
      ~requirement:(requirement [fp_cap])
      (admission_code (VM.ARGMAX_FP (0, 1, 2)))
  with
  | Error (Admission.Unsafe_error message) ->
    check
      "argmax is separate from strict fp"
      (starts_with
         "inference opcode ARGMAX_FP at pc 4 requires capability tensor.argmax"
         message)
  | _ -> failwith "expected argmax strict-fp rejection"

let check_existing_fp_family_remains_forbidden () =
  let cap = capability "tensor.strict-fp" (hex_root 'd') in
  let cases = [
    "EXP_LUT", VM.EXP_LUT (0, 1);
    "SOFTMAX_INPLACE", VM.SOFTMAX_INPLACE (0, 1);
    "LAYERNORM_INPLACE", VM.LAYERNORM_INPLACE (0, 1, 2, 3);
    "RMSNORM_INPLACE", VM.RMSNORM_INPLACE (0, 1, 2);
    "SILU_INPLACE", VM.SILU_INPLACE (0, 1);
    "ROPE_APPLY", VM.ROPE_APPLY (0, 1, 2, 3);
    "MATMUL_FP", VM.MATMUL_FP (0, 1, 2, 3, 4, 5);
    "RMSNORM_FP", VM.RMSNORM_FP (0, 1, 2);
    "ROPE_APPLY_FP", VM.ROPE_APPLY_FP (0, 1, 2, 3);
    "LOAD_INT8_FP", VM.LOAD_INT8_FP (0, 1, 2, 3, 4);
    "VECDOT_FP", VM.VECDOT_FP (0, 1, 2, 3);
    "ARGMAX_FP", VM.ARGMAX_FP (0, 1, 2);
    "ATTENTION_KV_FP", VM.ATTENTION_KV_FP (0, 1, 2, 3, 4, 5, 6, 7);
    "APPEND_VEC_FP", VM.APPEND_VEC_FP (0, 1, 2, 3);
  ] in
  List.iter
    (fun (name, op) ->
      match
        Admission.of_inference_code_with_requirement
          ~support:(support [cap])
          ~requirement:(requirement [cap])
          [| VM.JDEST 100; op; VM.STOP |]
      with
      | Error (Admission.Unsafe_error message) ->
        check (name ^ " remains forbidden")
          (starts_with ("inference opcode " ^ name) message)
      | _ -> failwith ("expected fp opcode rejection: " ^ name))
    cases

let check_generic_admission_rejection () =
  let check_rejection name label = function
    | Error (Admission.Unsafe_error message) ->
      check
        label
        (starts_with ("consensus unsafe opcode " ^ name) message)
    | _ -> failwith ("expected " ^ label)
  in
  List.iter
    (fun (name, op) ->
      let code = admission_code op in
      check_rejection name ("legacy rejects " ^ name) (Admission.of_code code);
      check_rejection name ("program rejects " ^ name) (Admission.of_program code))
    [
      "SIGMOID_FP", VM.SIGMOID_FP (0, 1);
      "SOFTPLUS_FP", VM.SOFTPLUS_FP (0, 1);
      "SILU_FP", VM.SILU_FP (0, 1);
      "RESIDUAL_ADD_FP", VM.RESIDUAL_ADD_FP (0, 1, 2);
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
    name = "Activation";
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

let check_compiler_surface () =
  List.iter
    (fun (name, args, expected) ->
      let program_code =
        Gen.generate (compiler_contract Lang.ProgramDecl name args)
      in
      check
        (name ^ " compiler emits opcode in Program")
        (code_contains expected program_code);
      match Gen.generate (compiler_contract Lang.ContractDecl name args) with
      | _ -> failwith (name ^ " should reject outside Program")
      | exception Gen.GenError (message, _) ->
        check
          (name ^ " compiler rejection")
          (String.equal message (name ^ " is available only in Program")))
    [
      "sigmoid_fp", [Lang.EInt Z.zero; Lang.EInt Z.one],
      (function VM.SIGMOID_FP _ -> true | _ -> false);
      "softplus_fp", [Lang.EInt Z.zero; Lang.EInt Z.one],
      (function VM.SOFTPLUS_FP _ -> true | _ -> false);
      "silu_fp", [Lang.EInt Z.zero; Lang.EInt Z.one],
      (function VM.SILU_FP _ -> true | _ -> false);
      "residual_add_fp", [Lang.EInt Z.zero; Lang.EInt Z.one; Lang.EInt Z.one],
      (function VM.RESIDUAL_ADD_FP _ -> true | _ -> false);
      "argmax_fp", [Lang.EInt Z.zero; Lang.EInt Z.one],
      (function VM.ARGMAX_FP _ -> true | _ -> false);
    ]

let check_wire_roundtrip () =
  List.iter
    (fun (asm, op) ->
      let raw = Bytecode.encode [|op; VM.STOP|] in
      (match Bytecode.decode raw with
       | Ok [|decoded; VM.STOP|] ->
         check (asm ^ " bytecode roundtrip") (decoded = op)
       | Ok _ -> failwith "unexpected decoded activation code"
       | Error error -> failwith error);
      check (asm ^ " assembler parse")
        (Assembler.parse (asm ^ "\nSTOP") = [|op; VM.STOP|]);
      check (asm ^ " assembler emit")
        (String.equal (Assembler.emit [|op; VM.STOP|]) (asm ^ "\nSTOP")))
    [
      "SIGMOID_FP r0, r1", VM.SIGMOID_FP (0, 1);
      "SOFTPLUS_FP r0, r1", VM.SOFTPLUS_FP (0, 1);
      "SILU_FP r0, r1", VM.SILU_FP (0, 1);
      "RESIDUAL_ADD_FP r0, r1, r2", VM.RESIDUAL_ADD_FP (0, 1, 2);
      "ARGMAX_FP r0, r1, r2", VM.ARGMAX_FP (0, 1, 2);
    ]

let check_effects () =
  let effects =
    Program_effects.names
      (Program_effects.scan
         [|VM.SIGMOID_FP (0, 1); VM.SOFTPLUS_FP (0, 1);
           VM.SILU_FP (0, 1); VM.RESIDUAL_ADD_FP (0, 1, 2)|])
  in
  check "activation effects" (effects = ["memory_read"; "memory_write"]);
  let effects =
    Program_effects.names
      (Program_effects.scan [|VM.RESIDUAL_ADD_FP (0, 1, 2)|])
  in
  check "residual effects" (effects = ["memory_read"; "memory_write"]);
  let effects =
    Program_effects.names
      (Program_effects.scan [|VM.ARGMAX_FP (0, 1, 2)|])
  in
  check "argmax effects" (effects = ["memory_read"])

let () =
  check_golden "sigmoid" (VM.SIGMOID_FP (0, 1)) sigmoid_input sigmoid_output;
  check_golden "softplus" (VM.SOFTPLUS_FP (0, 1)) softplus_input softplus_output;
  check_golden "silu" (VM.SILU_FP (0, 1)) sigmoid_input silu_output;
  check_missing_cell_reverts_atomically ();
  check_nonfinite_reverts_atomically ();
  check_strict_operands ();
  check_invalid_shape_and_effort_reverts ();
  check_activation_large_magnitude_edges ();
  check_activation_zero_subnormal_edges ();
  check_activation_effort_accounting ();
  check_residual_add ();
  check_residual_reverts_atomically ();
  check_residual_effort ();
  check_argmax_fp ();
  check_capability_gate ();
  check_argmax_capability_gate ();
  check_existing_fp_family_remains_forbidden ();
  check_generic_admission_rejection ();
  check_compiler_surface ();
  check_wire_roundtrip ();
  check_effects ()
