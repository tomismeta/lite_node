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
module Gen = Octra_vm.Oct_gen
module Lang = Octra_vm.Oct_lang
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

let fixture_bits bytes =
  if String.length bytes mod 8 <> 0 then failwith "invalid f64 fixture";
  List.init (String.length bytes / 8) (fun i -> int64_le bytes (i * 8))

let set_int_reg state reg value =
  state.VM.regs.(reg) <- VM.VInt (Z.of_int value)

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

let ssm_input =
  bytes_of_hex
    "000000000000f03f00000000000000c0000000000000e03f0000000000000840\
     000000000000f8bf00000000000000400000000000000000000000000000e0bf"

let ssm_kernel =
  bytes_of_hex
    "000000000000d03f000000000000e0bf000000000000e83f000000000000f0bf\
     000000000000e03f000000000000d03f"

(* Host-local SILU fixture bits from another platform's libm are intentionally
   not pinned: SILU_FP still uses host exp and is local_only / non-consensus.
   Compose expected silu cells via the same opcode path on this host. *)
let ssm_conv_expected =
  List.map
    Int64.bits_of_float
    [0.25; 2.0; -0.375; -4.0; 0.125; -1.0; 1.125; 2.25]

let host_local_silu_bits conv_bits =
  let n = List.length conv_bits in
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
  List.iteri
    (fun index bits -> set_f64_bits state (300 + index) bits)
    conv_bits;
  set_int_reg state 0 300;
  set_int_reg state 6 n;
  check
    "host-local silu reference run"
    (VM.run state [|VM.SILU_FP (0, 6); VM.STOP|]);
  List.init n (fun index -> f64_bits state (300 + index))

let make_conv_state ?(dst = 300) ?(input_base = 100) ?(kernel_base = 200)
    ?(timesteps = 4) ?(channels = 2) ?(width = 3) ?(limit = 1_000_000)
    input kernel =
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
  set_int_reg state 1 input_base;
  set_int_reg state 2 kernel_base;
  set_int_reg state 3 timesteps;
  set_int_reg state 4 channels;
  set_int_reg state 5 width;
  set_fixture state input_base input;
  set_fixture state kernel_base kernel;
  state

let run_conv ?(dst = 300) ?(input_base = 100) ?(kernel_base = 200)
    ?(timesteps = 4) ?(channels = 2) ?(width = 3) ?(limit = 1_000_000)
    input kernel code =
  let state =
    make_conv_state
      ~dst
      ~input_base
      ~kernel_base
      ~timesteps
      ~channels
      ~width
      ~limit
      input
      kernel
  in
  state, VM.run state code

let conv_op =
  VM.CAUSAL_DEPTHWISE_CONV1D_FP (0, 1, 2, 3, 4, 5)

let conv_code = [|conv_op; VM.STOP|]

let composed_code =
  [|
    conv_op;
    VM.LDI (6, VM.VInt (Z.of_int 8));
    VM.SILU_FP (0, 6);
    VM.STOP;
  |]

let check_golden_conv () =
  let state, ok = run_conv ssm_input ssm_kernel conv_code in
  check "conv succeeds" ok;
  check_cells "conv" state 300 ssm_conv_expected;
  (* Composed SILU is host-local (local_only profile); golden is same-host
     SILU_FP on the deterministic conv cells — not a cross-platform libm pin. *)
  let silu_expected = host_local_silu_bits ssm_conv_expected in
  let state, ok = run_conv ssm_input ssm_kernel composed_code in
  check "conv silu succeeds" ok;
  check_cells "conv silu" state 300 silu_expected

let f64_bytes values =
  let buffer = Bytes.create (List.length values * 8) in
  List.iteri
    (fun index value ->
      let bits = Int64.bits_of_float value in
      for byte = 0 to 7 do
        Bytes.set
          buffer
          ((index * 8) + byte)
          (Char.chr
             (Int64.to_int
                (Int64.logand
                   (Int64.shift_right_logical bits (byte * 8))
                   0xffL)))
      done)
    values;
  Bytes.to_string buffer

let check_shape_variants () =
  let input = f64_bytes [1.0; 2.0] in
  let kernel = f64_bytes [10.0; 20.0; 30.0] in
  let state, ok =
    run_conv ~timesteps:2 ~channels:1 ~width:3 input kernel conv_code
  in
  check "k greater than timesteps succeeds" ok;
  check_cells
    "k greater than timesteps"
    state
    300
    (List.map Int64.bits_of_float [10.0; 40.0]);
  let input = f64_bytes [2.0; 3.0] in
  let kernel = f64_bytes [5.0; 7.0] in
  let state, ok =
    run_conv ~timesteps:1 ~channels:2 ~width:1 input kernel conv_code
  in
  check "channel isolation succeeds" ok;
  check_cells
    "channel isolation"
    state
    300
    (List.map Int64.bits_of_float [10.0; 21.0])

let check_aliasing () =
  let state, ok = run_conv ~dst:100 ssm_input ssm_kernel conv_code in
  check "conv input alias succeeds" ok;
  check_cells "conv input alias" state 100 ssm_conv_expected;
  let state, ok = run_conv ~dst:200 ssm_input ssm_kernel conv_code in
  check "conv kernel alias succeeds" ok;
  check_cells "conv kernel alias" state 200 ssm_conv_expected

let check_missing_and_nonfinite_revert_atomically () =
  let state = make_conv_state ssm_input ssm_kernel in
  set_f64_bits state 300 (Int64.bits_of_float 42.0);
  Hashtbl.remove state.VM.memory.data 101;
  check "missing input reverts" (not (VM.run state conv_code));
  check "missing input leaves dst unchanged"
    (f64_bits state 300 = Int64.bits_of_float 42.0);
  let state = make_conv_state ssm_input ssm_kernel in
  set_f64_bits state 300 (Int64.bits_of_float 42.0);
  set_f64_bits state 200 0x7ff0000000000000L;
  check "non-finite kernel reverts" (not (VM.run state conv_code));
  check "non-finite kernel leaves dst unchanged"
    (f64_bits state 300 = Int64.bits_of_float 42.0)

let check_invalid_shape_and_effort_revert () =
  let state = make_conv_state ssm_input ssm_kernel in
  set_f64_bits state 300 (Int64.bits_of_float 42.0);
  set_int_reg state 3 0;
  check "zero timesteps reverts" (not (VM.run state conv_code));
  check "zero timesteps leaves dst unchanged"
    (f64_bits state 300 = Int64.bits_of_float 42.0);
  let state = make_conv_state ssm_input ssm_kernel in
  set_f64_bits state 300 (Int64.bits_of_float 42.0);
  set_int_reg state 0 (-1);
  check "negative dst reverts" (not (VM.run state conv_code));
  check "negative dst leaves positive cell unchanged"
    (f64_bits state 300 = Int64.bits_of_float 42.0);
  let state = make_conv_state ~limit:123 ssm_input ssm_kernel in
  set_f64_bits state 300 (Int64.bits_of_float 42.0);
  check "effort exhaustion reverts" (not (VM.run state conv_code));
  check "effort exhaustion leaves dst unchanged"
    (f64_bits state 300 = Int64.bits_of_float 42.0)

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
    VM.LDI (0, VM.VInt (Z.of_int 300));
    VM.LDI (1, VM.VInt (Z.of_int 100));
    VM.LDI (2, VM.VInt (Z.of_int 200));
    VM.LDI (3, VM.VInt (Z.of_int 4));
    VM.LDI (4, VM.VInt (Z.of_int 2));
    VM.LDI (5, VM.VInt (Z.of_int 3));
    conv_op;
    VM.LDI (6, VM.VInt (Z.of_int 8));
    VM.SILU_FP (0, 6);
    VM.STOP;
  |]

let check_capability_gate () =
  let conv_cap = capability "sequence.causal-convolution" (hex_root 'd') in
  let fp_cap = capability "tensor.strict-fp" (hex_root 'e') in
  (match
     Admission.of_inference_code_with_requirement
       ~support:(support [conv_cap; fp_cap])
       ~requirement:(requirement [conv_cap; fp_cap])
       admission_code
   with
   | Ok _ -> ()
   | Error error -> failwith (Admission.error_message error));
  (match
     Admission.of_inference_code_with_requirement
       ~support:(support [fp_cap])
       ~requirement:(requirement [fp_cap])
       admission_code
   with
   | Error (Admission.Unsafe_error message) ->
     check
       "conv capability is named"
       (starts_with
          "inference opcode CAUSAL_DEPTHWISE_CONV1D_FP"
          message)
   | _ -> failwith "expected conv capability rejection");
  match
    Admission.of_inference_code_with_requirement
      ~support:(support [conv_cap])
      ~requirement:(requirement [conv_cap])
      admission_code
  with
  | Error (Admission.Unsafe_error message) ->
    check "silu capability is named"
      (starts_with "inference opcode SILU_FP" message)
  | _ -> failwith "expected silu capability rejection"

let check_generic_admission_rejection () =
  let check_rejection label = function
    | Error (Admission.Unsafe_error message) ->
      check
        label
        (starts_with "consensus unsafe opcode CAUSAL_DEPTHWISE_CONV1D_FP" message)
    | _ -> failwith ("expected " ^ label)
  in
  check_rejection "legacy rejects conv" (Admission.of_code [|conv_op; VM.STOP|]);
  check_rejection "program rejects conv" (Admission.of_program [|conv_op; VM.STOP|])

let check_wire_roundtrip () =
  let raw = Bytecode.encode [|conv_op; VM.STOP|] in
  (match Bytecode.decode raw with
   | Ok [|decoded; VM.STOP|] -> check "conv bytecode roundtrip" (decoded = conv_op)
   | Ok _ -> failwith "unexpected decoded conv code"
   | Error error -> failwith error);
  let asm = "CAUSAL_DEPTHWISE_CONV1D_FP r0, r1, r2, r3, r4, r5\nSTOP" in
  check "conv assembler parse" (Assembler.parse asm = [|conv_op; VM.STOP|]);
  check "conv assembler emit"
    (String.equal (Assembler.emit [|conv_op; VM.STOP|]) asm)

let compiler_contract declaration =
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
        SExpr
          (ECall
             ("causal_depthwise_conv1d_fp",
              [EInt Z.zero; EInt Z.one; EInt (Z.of_int 2);
               EInt (Z.of_int 3); EInt (Z.of_int 4); EInt (Z.of_int 5)]));
      ];
    }
  in
  {
    declaration;
    name = "Conv";
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

let check_compiler_surface () =
  let program_code = Gen.generate (compiler_contract Lang.ProgramDecl) in
  check
    "compiler emits conv"
    (Array.exists
       (function VM.CAUSAL_DEPTHWISE_CONV1D_FP _ -> true | _ -> false)
       program_code);
  match Gen.generate (compiler_contract Lang.ContractDecl) with
  | _ -> failwith "conv should reject outside Program"
  | exception Gen.GenError (message, _) ->
    check
      "conv compiler rejection"
      (String.equal
         message
         "causal_depthwise_conv1d_fp is available only in Program")

let check_effects () =
  let effects =
    Program_effects.names (Program_effects.scan [|conv_op; VM.SILU_FP (0, 6)|])
  in
  check "conv effects" (effects = ["memory_read"; "memory_write"])

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

let store_cells base raw =
  fixture_bits raw
  |> List.mapi (fun index bits ->
    [|
      VM.LDI (6, VM.VInt (Z.of_int64 bits));
      VM.MSTORE (base + index, 6);
    |])
  |> Array.concat

let session_code =
  Array.concat
    [
      [|VM.JDEST Abi.advance_label|];
      store_cells 100 ssm_input;
      store_cells 200 ssm_kernel;
      [|
        VM.LDI (0, VM.VInt (Z.of_int 300));
        VM.LDI (1, VM.VInt (Z.of_int 100));
        VM.LDI (2, VM.VInt (Z.of_int 200));
        VM.LDI (3, VM.VInt (Z.of_int 4));
        VM.LDI (4, VM.VInt (Z.of_int 2));
        VM.LDI (5, VM.VInt (Z.of_int 3));
        conv_op;
        VM.LDI (6, VM.VInt (Z.of_int 8));
        VM.SILU_FP (0, 6);
        VM.LDI (0, VM.VInt (Z.of_int 300));
        VM.LDI (1, VM.VInt (Z.of_int 8));
        VM.STOP;
      |];
    ]

let check_session_execution () =
  let conv_cap = capability "sequence.causal-convolution" (hex_root 'd') in
  let fp_cap = capability "tensor.strict-fp" (hex_root 'e') in
  let requirement = requirement [conv_cap; fp_cap] in
  let support = support [conv_cap; fp_cap] in
  let admitted = Inference_cert.admit ~support ~requirement session_code in
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
  let owner = "range owner" in
  let owner_root = sha256 owner in
  let model =
    Model.{
      model_root = target.model_root;
      store_root = target.store_root;
      ranges = [{
        owner_root;
        offset = 0;
        length = String.length owner;
        encoding = "octets";
        shape_root = None;
      }];
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
  let request =
    Request.{
      schema = Abi.request_schema;
      target_root = Target.root target;
      entrypoint = Abi.advance_entrypoint;
      input_root = sha256 "";
      request_nonce = hex_root '5';
      max_output_bytes = 512;
      max_advance_effort = 10000;
    }
  in
  let plan =
    match Plan.create ~admitted ~target ~request ~model ~pins ~input:"" with
    | Ok plan -> plan
    | Error error -> failwith (Plan.error_message error)
  in
  match Execution.run ~plan () with
  | Ok result ->
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
    check "local session code succeeds" (VM.run state session_code);
    let expected =
      output_root
        ~target_root:(Target.root target)
        ~session_abi_root:Abi.v1_root
        (output_payload 300 8 state)
    in
    check "session output root" (String.equal result.Execution.output_root expected);
    check_cells
      "session local"
      state
      300
      (host_local_silu_bits ssm_conv_expected)
  | Error error -> failwith (Execution.error_message error)

let () =
  check_golden_conv ();
  check_shape_variants ();
  check_aliasing ();
  check_missing_and_nonfinite_revert_atomically ();
  check_invalid_shape_and_effort_revert ();
  check_capability_gate ();
  check_generic_admission_rejection ();
  check_wire_roundtrip ();
  check_compiler_surface ();
  check_effects ();
  check_session_execution ()
