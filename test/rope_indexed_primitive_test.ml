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

let set_f64_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int64 (Int64.bits_of_float value))

let set_f64 state addr value =
  Hashtbl.replace
    state.VM.memory.data
    addr
    (VM.VInt (Z.of_int64 (Int64.bits_of_float value)))

let set_f64_bits state addr bits =
  Hashtbl.replace state.VM.memory.data addr (VM.VInt (Z.of_int64 bits))

let set_int_cell state addr value =
  Hashtbl.replace state.VM.memory.data addr (VM.VInt (Z.of_int value))

let set_z_cell state addr value =
  Hashtbl.replace state.VM.memory.data addr (VM.VInt value)

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

let set_regs st ?(addr = 100) ?(count = 6) ?(head_dim = 6)
    ?(rot_dim = 4) ?(positions = 200) ?(base = 10_000_000.0) () =
  set_int_reg st 0 addr;
  set_int_reg st 1 count;
  set_int_reg st 2 head_dim;
  set_int_reg st 3 rot_dim;
  set_int_reg st 4 positions;
  set_f64_reg st 5 base

let set_positions st ?(base = 200) positions =
  List.iteri (fun index value -> set_int_cell st (base + index) value) positions

let op =
  VM.ROPE_APPLY_INDEXED_FP (0, 1, 2, 3, 4, 5)

let run ?limit ?strict_values ?addr ?count ?head_dim ?rot_dim ?positions ?base
    input pos =
  let st = state ?limit ?strict_values () in
  let addr = Option.value addr ~default:100 in
  let positions_base = Option.value positions ~default:200 in
  set_regs st
    ~addr
    ?count
    ?head_dim
    ?rot_dim
    ~positions:positions_base
    ?base
    ();
  List.iteri (fun index value -> set_f64 st (addr + index) value) input;
  set_positions st ~base:positions_base pos;
  st, VM.run st [|op; VM.STOP|]

let expected_values ~head_dim ~rot_dim ~base positions input =
  let values = Array.of_list input in
  let output = Array.copy values in
  let pairs = rot_dim / 2 in
  let heads = Array.length values / head_dim in
  let nf = float_of_int rot_dim in
  for head = 0 to heads - 1 do
    let base_addr = head * head_dim in
    for i = 0 to pairs - 1 do
      let theta =
        float_of_int (List.nth positions i)
        /. (base ** ((2.0 *. float_of_int i) /. nf))
      in
      let c = cos theta in
      let s = sin theta in
      let left_index = base_addr + i in
      let right_index = base_addr + i + pairs in
      let left = values.(left_index) in
      let right = values.(right_index) in
      output.(left_index) <- (left *. c) -. (right *. s);
      output.(right_index) <- (left *. s) +. (right *. c)
    done
  done;
  Array.to_list output

let check_bits label st base expected =
  List.iteri
    (fun index value ->
      check
        (label ^ " cell " ^ string_of_int index)
        (f64_bits st (base + index) = Int64.bits_of_float value))
    expected

let check_position_vector_golden () =
  let input = [1.; 2.; 3.; 4.; 5.; 6.] in
  let st, ok = run input [1; 2] in
  check "rope indexed golden runs" ok;
  check_bits
    "rope indexed golden"
    st
    100
    (expected_values
       ~head_dim:6
       ~rot_dim:4
       ~base:10_000_000.0
       [1; 2]
       input)

let check_zero_position_keeps_bits () =
  let input = [-0.0; 2.; 3.; -4.; 5.; -6.] in
  let st, ok = run input [0; 0] in
  check "zero position runs" ok;
  List.iteri
    (fun index value ->
      check
        ("zero position cell " ^ string_of_int index)
        (f64_bits st (100 + index) = Int64.bits_of_float value))
    input

let check_multi_head_tail () =
  let input = [1.; 2.; 3.; 4.; 5.; 6.; -1.; -2.; -3.; -4.; -5.; -6.] in
  let st, ok =
    run
      ~count:12
      ~head_dim:6
      ~rot_dim:4
      input
      [1; 2]
  in
  check "multi-head runs" ok;
  check_bits
    "multi-head"
    st
    100
    (expected_values
       ~head_dim:6
       ~rot_dim:4
       ~base:10_000_000.0
       [1; 2]
       input);
  check "first head tail preserved" (f64_bits st 104 = Int64.bits_of_float 5.);
  check "second head tail preserved" (f64_bits st 110 = Int64.bits_of_float (-5.))

let check_reverts_atomically () =
  let cases = [
    "missing position", (fun () ->
      let st, ok =
        run [1.; 2.; 3.; 4.; 5.; 6.] [1]
      in
      st, ok);
    "overlap", (fun () ->
      let st, ok =
        run ~positions:102 [1.; 2.; 3.; 4.; 5.; 6.] [1; 2]
      in
      st, ok);
    "invalid base", (fun () ->
      let st, ok =
        run ~base:1.0 [1.; 2.; 3.; 4.; 5.; 6.] [1; 2]
      in
      st, ok);
    "bad count", (fun () ->
      let st, ok =
        run ~count:5 [1.; 2.; 3.; 4.; 5.; 6.] [1; 2]
      in
      st, ok);
    "odd rotary dimension", (fun () ->
      let st, ok =
        run ~rot_dim:3 [1.; 2.; 3.; 4.; 5.; 6.] [1]
      in
      st, ok);
    "rotary dimension overflow", (fun () ->
      let st, ok =
        run ~rot_dim:8 [1.; 2.; 3.; 4.; 5.; 6.] [1; 2; 3; 4]
      in
      st, ok);
    "nonfinite input", (fun () ->
      let st, ok =
        run [1.; nan; 3.; 4.; 5.; 6.] [1; 2]
      in
      st, ok);
    "unsafe position", (fun () ->
      let st = state () in
      set_regs st ();
      List.iteri
        (fun index value -> set_f64 st (100 + index) value)
        [1.; 2.; 3.; 4.; 5.; 6.];
      set_positions st [1; 2];
      set_z_cell st 200 (Z.of_string "9007199254740992");
      st, VM.run st [|op; VM.STOP|]);
  ] in
  List.iter
    (fun (name, make) ->
      let st, ok = make () in
      check (name ^ " rejects") (not ok);
      check (name ^ " leaves first cell")
        (f64_bits st 100 = Int64.bits_of_float 1.))
    cases

let check_effort () =
  let input = [1.; 2.; 3.; 4.; 5.; 6.] in
  let _, ample = run ~limit:1_000_000 input [1; 2] in
  let _, low = run ~limit:121 input [1; 2] in
  let wide_tail =
    List.init 1024 (fun index -> float_of_int (index + 1))
  in
  let _, underpriced_tail =
    run
      ~limit:108
      ~count:1024
      ~head_dim:1024
      ~rot_dim:2
      wide_tail
      [1]
  in
  check "ample effort runs" ample;
  check "low effort rejects" (not low);
  check "wide tail is charged" (not underpriced_tail)

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
    VM.LDI (0, VM.VInt (Z.of_int 100));
    VM.LDI (1, VM.VInt (Z.of_int 6));
    VM.LDI (2, VM.VInt (Z.of_int 6));
    VM.LDI (3, VM.VInt (Z.of_int 4));
    VM.LDI (4, VM.VInt (Z.of_int 200));
    VM.LDI (5, VM.VInt (Z.of_int64 (Int64.bits_of_float 10_000_000.0)));
    op;
    VM.STOP;
  |]

let check_capability_gate () =
  let cap = capability "tensor.rope-indexed" (hex_root 'd') in
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
      "rope indexed capability is named"
      (starts_with
         "inference opcode ROPE_APPLY_INDEXED_FP at pc 7 requires capability tensor.rope-indexed"
         message)
  | _ -> failwith "expected rope-indexed capability rejection"

let check_generic_admission_rejection () =
  let check_rejection name label = function
    | Error (Admission.Unsafe_error message) ->
      check
        label
        (starts_with ("consensus unsafe opcode " ^ name) message)
    | _ -> failwith ("expected " ^ label)
  in
  check_rejection
    "ROPE_APPLY_INDEXED_FP"
    "legacy rejects rope indexed"
    (Admission.of_code admission_code);
  check_rejection
    "ROPE_APPLY_INDEXED_FP"
    "program rejects rope indexed"
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
    name = "Rope";
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
         "rope_apply_indexed_fp"
         (int_args 6))
  in
  check
    "compiler emits rope indexed in Program"
    (code_contains
       (function VM.ROPE_APPLY_INDEXED_FP _ -> true | _ -> false)
       program_code);
  match
    Gen.generate
      (compiler_contract
         Lang.ContractDecl
         "rope_apply_indexed_fp"
         (int_args 6))
  with
  | _ -> failwith "rope_apply_indexed_fp should reject outside Program"
  | exception Gen.GenError (message, _) ->
    check
      "compiler rejection"
      (String.equal message
         "rope_apply_indexed_fp is available only in Program")

let check_wire_roundtrip () =
  let asm = "ROPE_APPLY_INDEXED_FP r0, r1, r2, r3, r4, r5" in
  let raw = Bytecode.encode [|op; VM.STOP|] in
  (match Bytecode.decode raw with
   | Ok [|decoded; VM.STOP|] ->
     check "bytecode roundtrip" (decoded = op)
   | Ok _ -> failwith "unexpected decoded rope indexed code"
   | Error error -> failwith error);
  check "assembler parse" (Assembler.parse (asm ^ "\nSTOP") = [|op; VM.STOP|]);
  check "assembler emit" (String.equal (Assembler.emit [|op; VM.STOP|]) (asm ^ "\nSTOP"))

let check_effects () =
  let effects =
    Program_effects.names
      (Program_effects.scan [|op|])
  in
  check "rope indexed effects" (effects = ["memory_read"; "memory_write"])

let check_artifact_fixture_if_available () =
  match getenv_opt "OCTRA_ROPE_INDEXED_EVIDENCE" with
  | None -> ()
  | Some base ->
    let input =
      read_file (Filename.concat base "q-head0-pos1-input.f64le")
    in
    let expected =
      read_file (Filename.concat base "q-head0-pos1-output.f64le")
    in
    let st = state ~limit:1_000_000 () in
    set_regs st
      ~addr:100
      ~count:256
      ~head_dim:256
      ~rot_dim:64
      ~positions:1000
      ~base:10_000_000.0
      ();
    set_f64_bytes st 100 input;
    set_positions st ~base:1000 (List.init 32 (fun _ -> 1));
    check "artifact fixture runs" (VM.run st [|op; VM.STOP|]);
    let observed = raw_cells st 100 256 in
    check "artifact fixture bytes match" (String.equal observed expected)

let () =
  check_position_vector_golden ();
  check_zero_position_keeps_bits ();
  check_multi_head_tail ();
  check_reverts_atomically ();
  check_effort ();
  check_capability_gate ();
  check_generic_admission_rejection ();
  check_compiler_surface ();
  check_wire_roundtrip ();
  check_effects ();
  check_artifact_fixture_if_available ()
