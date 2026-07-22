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
    "4554903c3448a83f86ba54145636d13f000000000000e03fbda2d5f5d464e73f\
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
    (f64_bits state 100 = Int64.bits_of_float 1.0)

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
              ^ " at pc 3 requires capability tensor.strict-fp")
             message)
      | _ -> failwith ("expected activation capability rejection: " ^ name))
    [
      "SIGMOID_FP", VM.SIGMOID_FP (0, 1);
      "SOFTPLUS_FP", VM.SOFTPLUS_FP (0, 1);
      "SILU_FP", VM.SILU_FP (0, 1);
    ]

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
    "ELEMWISE_MUL_FP", VM.ELEMWISE_MUL_FP (0, 1, 2);
    "RESIDUAL_ADD_FP", VM.RESIDUAL_ADD_FP (0, 1, 2);
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
    ]

let compiler_contract declaration name =
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
      fn_body = [
        SExpr (ECall (name, [EInt Z.zero; EInt Z.one]));
      ];
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
    (fun (name, expected) ->
      let program_code = Gen.generate (compiler_contract Lang.ProgramDecl name) in
      check
        (name ^ " compiler emits opcode in Program")
        (code_contains expected program_code);
      match Gen.generate (compiler_contract Lang.ContractDecl name) with
      | _ -> failwith (name ^ " should reject outside Program")
      | exception Gen.GenError (message, _) ->
        check
          (name ^ " compiler rejection")
          (String.equal message (name ^ " is available only in Program")))
    [
      "sigmoid_fp", (function VM.SIGMOID_FP _ -> true | _ -> false);
      "softplus_fp", (function VM.SOFTPLUS_FP _ -> true | _ -> false);
      "silu_fp", (function VM.SILU_FP _ -> true | _ -> false);
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
    ]

let check_effects () =
  let effects =
    Program_effects.names
      (Program_effects.scan
         [|VM.SIGMOID_FP (0, 1); VM.SOFTPLUS_FP (0, 1); VM.SILU_FP (0, 1)|])
  in
  check "activation effects" (effects = ["memory_read"; "memory_write"])

let () =
  check_golden "sigmoid" (VM.SIGMOID_FP (0, 1)) sigmoid_input sigmoid_output;
  check_golden "softplus" (VM.SOFTPLUS_FP (0, 1)) softplus_input softplus_output;
  check_golden "silu" (VM.SILU_FP (0, 1)) sigmoid_input silu_output;
  check_missing_cell_reverts_atomically ();
  check_nonfinite_reverts_atomically ();
  check_strict_operands ();
  check_invalid_shape_and_effort_reverts ();
  check_capability_gate ();
  check_existing_fp_family_remains_forbidden ();
  check_generic_admission_rejection ();
  check_compiler_surface ();
  check_wire_roundtrip ();
  check_effects ()
