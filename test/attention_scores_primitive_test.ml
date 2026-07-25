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

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
       let len = in_channel_length input in
       really_input_string input len)

let getenv_opt name =
  try Some (Sys.getenv name) with Not_found -> None

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

let set_f64 state addr value =
  Hashtbl.replace
    state.VM.memory.data
    addr
    (VM.VInt (Z.of_int64 (Int64.bits_of_float value)))

let set_f64_bits state addr bits =
  Hashtbl.replace state.VM.memory.data addr (VM.VInt (Z.of_int64 bits))

let f64_bits state addr =
  match Hashtbl.find_opt state.VM.memory.data addr with
  | Some (VM.VInt bits) when Z.fits_int64 bits -> Z.to_int64 bits
  | _ -> failwith ("missing f64 cell " ^ string_of_int addr)

let set_f64_bytes state addr bytes =
  if String.length bytes mod 8 <> 0 then failwith "invalid f64 fixture";
  for index = 0 to (String.length bytes / 8) - 1 do
    set_f64_bits state (addr + index) (int64_le bytes (index * 8))
  done

let raw_cells state base count =
  let raw = Bytes.create (count * 8) in
  for index = 0 to count - 1 do
    put_int64_le raw index (f64_bits state (base + index))
  done;
  Bytes.to_string raw

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

let op =
  VM.ATTENTION_SCORES_FP (0, 1, 2, 3, 4)

let set_regs st ?(dst = 300) ?(query = 100) ?(key = 200)
    ?(key_count = 2) ?(head_dim = 4) () =
  set_int_reg st 0 dst;
  set_int_reg st 1 query;
  set_int_reg st 2 key;
  set_int_reg st 3 key_count;
  set_int_reg st 4 head_dim

let set_values st base values =
  List.iteri (fun index value -> set_f64 st (base + index) value) values

let make_state ?limit ?strict_values ?dst ?query ?key ?key_count ?head_dim
    query_values key_values =
  let st = state ?limit ?strict_values () in
  let query_addr = Option.value query ~default:100 in
  let key_addr = Option.value key ~default:200 in
  set_regs st ?dst ~query:query_addr ~key:key_addr ?key_count ?head_dim ();
  set_values st query_addr query_values;
  set_values st key_addr key_values;
  st

let run ?limit ?strict_values ?dst ?query ?key ?key_count ?head_dim
    query_values key_values =
  let st =
    make_state
      ?limit
      ?strict_values
      ?dst
      ?query
      ?key
      ?key_count
      ?head_dim
      query_values
      key_values
  in
  st, VM.run st [|op; VM.STOP|]

let check_scores label st expected =
  List.iteri
    (fun index value ->
      check
        (label ^ " score " ^ string_of_int index)
        (f64_bits st (300 + index) = Int64.bits_of_float value))
    expected

let set_output_sentinel ?(dst = 300) st =
  set_f64 st dst 42.;
  set_f64 st (dst + 1) (-17.)

let check_output_sentinel ?(dst = 300) label st =
  check (label ^ " preserves output 0")
    (f64_bits st dst = Int64.bits_of_float 42.);
  check (label ^ " preserves output 1")
    (f64_bits st (dst + 1) = Int64.bits_of_float (-17.))

let check_golden () =
  let st, ok =
    run
      [1.; 2.; 3.; 4.]
      [2.; 0.; -1.; 1.; 0.5; 0.25; 0.; -0.5]
  in
  check "attention scores runs" ok;
  check_scores "attention scores" st [1.5; -0.5]

let check_query_key_aliasing () =
  let st = state () in
  set_regs st ~query:100 ~key:100 ();
  set_values st 100 [1.; 2.; 3.; 4.; 0.5; 0.25; 0.; -0.5];
  check "exact q/key alias runs" (VM.run st [|op; VM.STOP|]);
  check_scores "exact q/key alias" st [15.; -0.5];
  let st = state () in
  set_regs st ~query:102 ~key:100 ();
  set_values st 100 [10.; 20.; 1.; 2.; 3.; 4.; 7.; 8.];
  check "partial q/key alias runs" (VM.run st [|op; VM.STOP|]);
  check_scores "partial q/key alias" st [30.5; 32.]

let check_reverts_atomically () =
  let cases = [
    "missing query cell", 300, (fun () ->
      let st = state () in
      set_regs st ();
      set_values st 100 [1.; 2.; 3.];
      set_values st 200 [2.; 0.; -1.; 1.; 0.5; 0.25; 0.; -0.5];
      set_output_sentinel st;
      st, VM.run st [|op; VM.STOP|]);
    "missing key cell", 300, (fun () ->
      let st = state () in
      set_regs st ();
      set_values st 100 [1.; 2.; 3.; 4.];
      set_values st 200 [2.; 0.; -1.; 1.; 0.5; 0.25; 0.];
      set_output_sentinel st;
      st, VM.run st [|op; VM.STOP|]);
    "nonfinite query", 300, (fun () ->
      let st =
        make_state [1.; nan; 3.; 4.]
          [2.; 0.; -1.; 1.; 0.5; 0.25; 0.; -0.5]
      in
      set_output_sentinel st;
      st, VM.run st [|op; VM.STOP|]);
    "nonfinite key", 300, (fun () ->
      let st =
        make_state [1.; 2.; 3.; 4.]
          [2.; 0.; infinity; 1.; 0.5; 0.25; 0.; -0.5]
      in
      set_output_sentinel st;
      st, VM.run st [|op; VM.STOP|]);
    "overflow accumulator", 300, (fun () ->
      let st =
        make_state
          [max_float; max_float; max_float; max_float]
          [max_float; max_float; max_float; max_float;
           0.5; 0.25; 0.; -0.5]
      in
      set_output_sentinel st;
      st, VM.run st [|op; VM.STOP|]);
    "output overlaps query", 101, (fun () ->
      let st =
        make_state ~dst:101 [1.; 2.; 3.; 4.]
          [2.; 0.; -1.; 1.; 0.5; 0.25; 0.; -0.5]
      in
      set_output_sentinel ~dst:101 st;
      st, VM.run st [|op; VM.STOP|]);
    "output overlaps key", 204, (fun () ->
      let st =
        make_state ~dst:204 [1.; 2.; 3.; 4.]
          [2.; 0.; -1.; 1.; 0.5; 0.25; 0.; -0.5]
      in
      set_output_sentinel ~dst:204 st;
      st, VM.run st [|op; VM.STOP|]);
    "bad key count", 300, (fun () ->
      let st =
        make_state ~key_count:0 [1.; 2.; 3.; 4.]
          [2.; 0.; -1.; 1.; 0.5; 0.25; 0.; -0.5]
      in
      set_output_sentinel st;
      st, VM.run st [|op; VM.STOP|]);
    "bad head dim", 300, (fun () ->
      let st =
        make_state ~head_dim:0 [1.; 2.; 3.; 4.]
          [2.; 0.; -1.; 1.; 0.5; 0.25; 0.; -0.5]
      in
      set_output_sentinel st;
      st, VM.run st [|op; VM.STOP|]);
  ] in
  List.iter
    (fun (name, dst, make) ->
      let st, ok = make () in
      check (name ^ " rejects") (not ok);
      check_output_sentinel ~dst name st)
    cases

let check_effort () =
  let query = [1.; 2.; 3.; 4.] in
  let key = [2.; 0.; -1.; 1.; 0.5; 0.25; 0.; -0.5] in
  let _, ample = run ~limit:1_000_000 query key in
  let _, low = run ~limit:100 query key in
  check "ample effort runs" ample;
  check "low effort rejects" (not low)

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

let admission_code =
  [|
    VM.JDEST 100;
    VM.LDI (0, VM.VInt (Z.of_int 300));
    VM.LDI (1, VM.VInt (Z.of_int 100));
    VM.LDI (2, VM.VInt (Z.of_int 200));
    VM.LDI (3, VM.VInt (Z.of_int 2));
    VM.LDI (4, VM.VInt (Z.of_int 4));
    op;
    VM.STOP;
  |]

let check_capability_gate () =
  let cap = capability "tensor.attention" (hex_root 'd') in
  (match
     Admission.of_inference_code_with_requirement
       ~support:(support [cap])
       ~requirement:(requirement [cap])
       admission_code
   with
   | Ok _ -> ()
   | Error error -> failwith (Admission.error_message error));
  match
    Admission.of_inference_code_with_requirement
      ~support:(support [])
      ~requirement:(requirement [])
      admission_code
  with
  | Error (Admission.Unsafe_error message) ->
    check
      "attention capability is named"
      (starts_with
         "inference opcode ATTENTION_SCORES_FP at pc 6 requires capability tensor.attention"
         message)
  | _ -> failwith "expected attention capability rejection"

let check_generic_admission_rejection () =
  let check_rejection name label = function
    | Error (Admission.Unsafe_error message) ->
      check
        label
        (starts_with ("consensus unsafe opcode " ^ name) message)
    | _ -> failwith ("expected " ^ label)
  in
  check_rejection
    "ATTENTION_SCORES_FP"
    "legacy rejects attention scores"
    (Admission.of_code admission_code);
  check_rejection
    "ATTENTION_SCORES_FP"
    "program rejects attention scores"
    (Admission.of_program admission_code)

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
    name = "Scores";
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
  let program_code =
    Gen.generate
      (compiler_contract
         Lang.ProgramDecl
         "attention_scores_fp"
         (int_args 5))
  in
  check
    "compiler emits attention scores in Program"
    (code_contains
       (function VM.ATTENTION_SCORES_FP _ -> true | _ -> false)
       program_code);
  match
    Gen.generate
      (compiler_contract
         Lang.ContractDecl
         "attention_scores_fp"
         (int_args 5))
  with
  | _ -> failwith "attention_scores_fp should reject outside Program"
  | exception Gen.GenError (message, _) ->
    check
      "compiler rejection"
      (String.equal message
         "attention_scores_fp is available only in Program")

let check_wire_roundtrip () =
  let asm = "ATTENTION_SCORES_FP r0, r1, r2, r3, r4" in
  let raw = Bytecode.encode [|op; VM.STOP|] in
  (match Bytecode.decode raw with
   | Ok [|decoded; VM.STOP|] ->
     check "bytecode roundtrip" (decoded = op)
   | Ok _ -> failwith "unexpected decoded attention scores code"
   | Error error -> failwith error);
  check "assembler parse" (Assembler.parse (asm ^ "\nSTOP") = [|op; VM.STOP|]);
  check "assembler emit"
    (String.equal (Assembler.emit [|op; VM.STOP|]) (asm ^ "\nSTOP"))

let check_effects () =
  let effects =
    Program_effects.names
      (Program_effects.scan [|op|])
  in
  check "attention scores effects" (effects = ["memory_read"; "memory_write"])

let check_artifact_fixture_if_available () =
  match getenv_opt "OCTRA_ATTENTION_FRONTIER_EVIDENCE" with
  | None -> ()
  | Some base ->
    let q =
      read_file (Filename.concat base "imrope/q-head0-pos1-output.f64le")
    in
    let k0 =
      read_file (Filename.concat base "imrope/k-head0-pos0-output.f64le")
    in
    let k1 =
      read_file (Filename.concat base "imrope/k-head0-pos1-output.f64le")
    in
    let expected =
      read_file
        (Filename.concat base "contracts/attention-scores-head0-pos1.f64le")
    in
    let st = state ~limit:1_000_000 () in
    set_regs st ~dst:2000 ~query:100 ~key:1000 ~key_count:2 ~head_dim:256 ();
    set_f64_bytes st 100 q;
    set_f64_bytes st 1000 (k0 ^ k1);
    check "artifact score fixture runs" (VM.run st [|op; VM.STOP|]);
    let observed = raw_cells st 2000 2 in
    check "artifact score fixture bytes match" (String.equal observed expected)

let () =
  check_golden ();
  check_query_key_aliasing ();
  check_reverts_atomically ();
  check_effort ();
  check_capability_gate ();
  check_generic_admission_rejection ();
  check_compiler_surface ();
  check_wire_roundtrip ();
  check_effects ();
  check_artifact_fixture_if_available ()
