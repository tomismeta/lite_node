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

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let set_f64 state addr value =
  Hashtbl.replace
    state.VM.memory.data
    addr
    (VM.VInt (Z.of_int64 (Int64.bits_of_float value)))

let f64_bits state addr =
  match Hashtbl.find_opt state.VM.memory.data addr with
  | Some (VM.VInt bits) when Z.fits_int64 bits -> Z.to_int64 bits
  | _ -> failwith ("missing f64 cell " ^ string_of_int addr)

let state ?(limit = 1_000_000) ?(strict_values = false) () =
  VM.create_state
    ~limit
    ~strict_values
    ~caller:"caller"
    ~origin:"origin"
    ~address:"contract"
    ~value:Z.zero
    ~storage:(Hashtbl.create 0)
    ()

let set_values st base values =
  List.iteri (fun index value -> set_f64 st (base + index) value) values

let set_output_sentinel ?(count = 4) ?(dst = 300) st =
  for index = 0 to count - 1 do
    set_f64 st (dst + index) (42. +. float_of_int index)
  done

let check_output_sentinel ?(count = 4) ?(dst = 300) label st =
  for index = 0 to count - 1 do
    check
      (label ^ " preserves output " ^ string_of_int index)
      (f64_bits st (dst + index)
       = Int64.bits_of_float (42. +. float_of_int index))
  done

let softmax_op =
  VM.SOFTMAX_FP (0, 1, 2)

let weighted_sum_op =
  VM.ATTENTION_WEIGHTED_SUM_FP (0, 1, 2, 3, 4)

let set_softmax_regs st ?(dst = 300) ?(scores = 100)
    ?(count = 3) () =
  set_int_reg st 0 dst;
  set_int_reg st 1 scores;
  set_int_reg st 2 count

let set_weighted_sum_regs st ?(dst = 300) ?(probs = 100) ?(value = 200)
    ?(key_count = 2) ?(head_dim = 2) () =
  set_int_reg st 0 dst;
  set_int_reg st 1 probs;
  set_int_reg st 2 value;
  set_int_reg st 3 key_count;
  set_int_reg st 4 head_dim

let check_cells label st base expected =
  List.iteri
    (fun index value ->
      check
        (label ^ " cell " ^ string_of_int index)
        (f64_bits st (base + index) = Int64.bits_of_float value))
    expected

let check_softmax_equal_scores () =
  let st = state () in
  set_softmax_regs st ~count:2 ();
  set_values st 100 [0.; 0.];
  check "softmax equal scores runs" (VM.run st [|softmax_op; VM.STOP|]);
  check_cells "softmax equal scores" st 300 [0.5; 0.5]

let check_softmax_exact_inplace () =
  let st = state () in
  set_softmax_regs st ~dst:100 ~scores:100 ~count:2 ();
  set_values st 100 [0.; 0.];
  check "softmax exact inplace runs" (VM.run st [|softmax_op; VM.STOP|]);
  check_cells "softmax exact inplace" st 100 [0.5; 0.5]

let check_weighted_sum_golden () =
  let st = state () in
  set_weighted_sum_regs st ();
  set_values st 100 [0.25; 0.75];
  set_values st 200 [2.; 4.; 6.; 8.];
  check "weighted sum runs" (VM.run st [|weighted_sum_op; VM.STOP|]);
  check_cells "weighted sum" st 300 [5.; 7.]

let check_tail_reverts_atomically () =
  let cases = [
    "softmax missing score", 300, 3, (fun () ->
      let st = state () in
      set_softmax_regs st ~count:3 ();
      set_values st 100 [0.; 0.];
      set_output_sentinel ~count:3 st;
      st, VM.run st [|softmax_op; VM.STOP|]);
    "softmax nonfinite score", 300, 3, (fun () ->
      let st = state () in
      set_softmax_regs st ~count:3 ();
      set_values st 100 [0.; infinity; 0.];
      set_output_sentinel ~count:3 st;
      st, VM.run st [|softmax_op; VM.STOP|]);
    "softmax partial overlap", 101, 3, (fun () ->
      let st = state () in
      set_softmax_regs st ~dst:101 ~scores:100 ~count:3 ();
      set_values st 100 [0.; 0.; 0.];
      set_output_sentinel ~dst:101 ~count:3 st;
      st, VM.run st [|softmax_op; VM.STOP|]);
    "softmax bad count", 300, 3, (fun () ->
      let st = state () in
      set_softmax_regs st ~count:0 ();
      set_values st 100 [0.; 0.; 0.];
      set_output_sentinel ~count:3 st;
      st, VM.run st [|softmax_op; VM.STOP|]);
    "weighted sum missing value", 300, 2, (fun () ->
      let st = state () in
      set_weighted_sum_regs st ();
      set_values st 100 [0.25; 0.75];
      set_values st 200 [2.; 4.; 6.];
      set_output_sentinel ~count:2 st;
      st, VM.run st [|weighted_sum_op; VM.STOP|]);
    "weighted sum nonfinite prob", 300, 2, (fun () ->
      let st = state () in
      set_weighted_sum_regs st ();
      set_values st 100 [0.25; nan];
      set_values st 200 [2.; 4.; 6.; 8.];
      set_output_sentinel ~count:2 st;
      st, VM.run st [|weighted_sum_op; VM.STOP|]);
    "weighted sum nonfinite value", 300, 2, (fun () ->
      let st = state () in
      set_weighted_sum_regs st ();
      set_values st 100 [0.25; 0.75];
      set_values st 200 [2.; infinity; 6.; 8.];
      set_output_sentinel ~count:2 st;
      st, VM.run st [|weighted_sum_op; VM.STOP|]);
    "weighted sum overflow", 300, 2, (fun () ->
      let st = state () in
      set_weighted_sum_regs st ();
      set_values st 100 [max_float; max_float];
      set_values st 200 [max_float; max_float; max_float; max_float];
      set_output_sentinel ~count:2 st;
      st, VM.run st [|weighted_sum_op; VM.STOP|]);
    "weighted sum overlaps probs", 101, 2, (fun () ->
      let st = state () in
      set_weighted_sum_regs st ~dst:101 ();
      set_values st 100 [0.25; 0.75];
      set_values st 200 [2.; 4.; 6.; 8.];
      set_output_sentinel ~dst:101 ~count:2 st;
      st, VM.run st [|weighted_sum_op; VM.STOP|]);
    "weighted sum overlaps value", 202, 2, (fun () ->
      let st = state () in
      set_weighted_sum_regs st ~dst:202 ();
      set_values st 100 [0.25; 0.75];
      set_values st 200 [2.; 4.; 6.; 8.];
      set_output_sentinel ~dst:202 ~count:2 st;
      st, VM.run st [|weighted_sum_op; VM.STOP|]);
  ] in
  List.iter
    (fun (name, dst, count, make) ->
      let st, ok = make () in
      check (name ^ " rejects") (not ok);
      check_output_sentinel ~dst ~count name st)
    cases

let check_effort () =
  let st = state ~limit:1_000_000 () in
  set_softmax_regs st ~count:2 ();
  set_values st 100 [0.; 0.];
  check "softmax ample effort runs" (VM.run st [|softmax_op; VM.STOP|]);
  let st = state ~limit:100 () in
  set_softmax_regs st ~count:2 ();
  set_values st 100 [0.; 0.];
  check "softmax low effort rejects" (not (VM.run st [|softmax_op; VM.STOP|]));
  let st = state ~limit:1_000_000 () in
  set_weighted_sum_regs st ();
  set_values st 100 [0.25; 0.75];
  set_values st 200 [2.; 4.; 6.; 8.];
  check "weighted sum ample effort runs" (VM.run st [|weighted_sum_op; VM.STOP|]);
  let st = state ~limit:100 () in
  set_weighted_sum_regs st ();
  set_values st 100 [0.25; 0.75];
  set_values st 200 [2.; 4.; 6.; 8.];
  check "weighted sum low effort rejects"
    (not (VM.run st [|weighted_sum_op; VM.STOP|]))

let capability name capability_root =
  Req.{ name = name; root = capability_root }

let limits =
  Req.{
    max_model_bytes = 512;
    max_view_bytes = 512;
    max_session_bytes = 512;
    max_scratch_bytes = 512;
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

let softmax_admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt (Z.of_int 300));
    VM.LDI (1, VM.VInt (Z.of_int 100));
    VM.LDI (2, VM.VInt (Z.of_int 3));
    softmax_op;
    VM.STOP;
  |]

let weighted_sum_admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt (Z.of_int 300));
    VM.LDI (1, VM.VInt (Z.of_int 100));
    VM.LDI (2, VM.VInt (Z.of_int 200));
    VM.LDI (3, VM.VInt (Z.of_int 2));
    VM.LDI (4, VM.VInt (Z.of_int 2));
    weighted_sum_op;
    VM.STOP;
  |]

let check_capability_gate () =
  let cap = capability "tensor.attention" (hex_root 'd') in
  List.iter
    (fun code ->
      match
        Admission.of_inference_code_with_requirement
          ~support:(support [cap])
          ~requirement:(requirement [cap])
          code
      with
      | Ok _ -> ()
      | Error error -> failwith (Admission.error_message error))
    [softmax_admission_code; weighted_sum_admission_code];
  let check_missing expected code =
    match
      Admission.of_inference_code_with_requirement
        ~support:(support [])
        ~requirement:(requirement [])
        code
    with
    | Error (Admission.Unsafe_error message) ->
      check expected (starts_with expected message)
    | _ -> failwith ("expected " ^ expected)
  in
  check_missing
    "inference opcode SOFTMAX_FP at pc 4 requires capability tensor.attention"
    softmax_admission_code;
  check_missing
    "inference opcode ATTENTION_WEIGHTED_SUM_FP at pc 6 requires capability tensor.attention"
    weighted_sum_admission_code

let check_generic_admission_rejection () =
  let check_rejection name label = function
    | Error (Admission.Unsafe_error message) ->
      check
        label
        (starts_with ("consensus unsafe opcode " ^ name) message)
    | _ -> failwith ("expected " ^ label)
  in
  check_rejection
    "SOFTMAX_FP"
    "legacy rejects attention softmax"
    (Admission.of_code softmax_admission_code);
  check_rejection
    "ATTENTION_WEIGHTED_SUM_FP"
    "legacy rejects attention weighted sum"
    (Admission.of_code weighted_sum_admission_code);
  check_rejection
    "SOFTMAX_FP"
    "program rejects attention softmax"
    (Admission.of_program softmax_admission_code);
  check_rejection
    "ATTENTION_WEIGHTED_SUM_FP"
    "program rejects attention weighted sum"
    (Admission.of_program weighted_sum_admission_code)

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
    name = "Tail";
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
  let softmax_code =
    Gen.generate
      (compiler_contract
         Lang.ProgramDecl
         "softmax_fp"
         (int_args 3))
  in
  check
    "compiler emits attention softmax in Program"
    (code_contains
       (function VM.SOFTMAX_FP _ -> true | _ -> false)
       softmax_code);
  let weighted_sum_code =
    Gen.generate
      (compiler_contract
         Lang.ProgramDecl
         "attention_weighted_sum_fp"
         (int_args 5))
  in
  check
    "compiler emits attention weighted sum in Program"
    (code_contains
       (function VM.ATTENTION_WEIGHTED_SUM_FP _ -> true | _ -> false)
       weighted_sum_code);
  (match
     Gen.generate
       (compiler_contract
          Lang.ContractDecl
          "softmax_fp"
          (int_args 3))
   with
   | _ -> failwith "softmax_fp should reject outside Program"
   | exception Gen.GenError (message, _) ->
     check
       "compiler softmax rejection"
       (String.equal message
          "softmax_fp is available only in Program"));
  match
    Gen.generate
      (compiler_contract
         Lang.ContractDecl
         "attention_weighted_sum_fp"
         (int_args 5))
  with
  | _ -> failwith "attention_weighted_sum_fp should reject outside Program"
  | exception Gen.GenError (message, _) ->
    check
      "compiler weighted sum rejection"
      (String.equal message
         "attention_weighted_sum_fp is available only in Program")

let check_wire_roundtrip () =
  let softmax_asm =
    "SOFTMAX_FP r0, r1, r2"
  in
  let weighted_sum_asm =
    "ATTENTION_WEIGHTED_SUM_FP r0, r1, r2, r3, r4"
  in
  let code = [|softmax_op; weighted_sum_op; VM.STOP|] in
  (match Bytecode.decode (Bytecode.encode code) with
   | Ok decoded -> check "bytecode roundtrip" (decoded = code)
   | Error error -> failwith error);
  check
    "assembler parse softmax"
    (Assembler.parse (softmax_asm ^ "\nSTOP") = [|softmax_op; VM.STOP|]);
  check
    "assembler parse weighted sum"
    (Assembler.parse (weighted_sum_asm ^ "\nSTOP")
     = [|weighted_sum_op; VM.STOP|]);
  check
    "assembler emit"
    (String.equal
       (Assembler.emit code)
       (softmax_asm ^ "\n" ^ weighted_sum_asm ^ "\nSTOP"))

let check_effects () =
  let effects =
    Program_effects.names
      (Program_effects.scan [|softmax_op; weighted_sum_op|])
  in
  check
    "attention tail effects"
    (effects = ["memory_read"; "memory_write"])

let () =
  check_softmax_equal_scores ();
  check_softmax_exact_inplace ();
  check_weighted_sum_golden ();
  check_tail_reverts_atomically ();
  check_effort ();
  check_capability_gate ();
  check_generic_admission_rejection ();
  check_compiler_surface ();
  check_wire_roundtrip ();
  check_effects ()
