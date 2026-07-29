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


module VM = Octra_vm.Contract_vm
module Policy = Octra_vm.Opcode_policy

let check label condition =
  if not condition then failwith label

let check_info label expected_class expected_name op =
  let info = Policy.describe op in
  check (label ^ ": name") (String.equal info.Policy.name expected_name);
  check (label ^ ": class") (info.Policy.admission_class = expected_class)

let check_host_float label expected_name op =
  check_info label Policy.Consensus_unsafe expected_name op;
  check (label ^ ": host")
    (Policy.host_float_opcode op = Some expected_name);
  check (label ^ ": program-only")
    (Policy.program_only_opcode op = None);
  check (label ^ ": uses host float")
    (Policy.uses_host_float op)

let check_program_only label expected_name op =
  check_info label Policy.Program_only expected_name op;
  check (label ^ ": host")
    (Policy.host_float_opcode op = None);
  check (label ^ ": program-only")
    (Policy.program_only_opcode op = Some expected_name);
  check (label ^ ": uses host float")
    (not (Policy.uses_host_float op))

let check_profiled label expected_name op =
  check_info label Policy.Consensus_unsafe expected_name op;
  check (label ^ ": host")
    (Policy.host_float_opcode op = Some expected_name);
  check (label ^ ": program-only")
    (Policy.program_only_opcode op = None);
  check (label ^ ": unsafe")
    (Policy.uses_host_float op)

let check_legacy_and_program label expected_name op =
  check_info label Policy.Legacy_and_program expected_name op;
  check (label ^ ": host")
    (Policy.host_float_opcode op = None);
  check (label ^ ": program-only")
    (Policy.program_only_opcode op = None);
  check (label ^ ": uses host float")
    (not (Policy.uses_host_float op))

let host_float_cases = [
  ("exp", "EXP_LUT", VM.EXP_LUT (0, 1));
  ("softmax", "SOFTMAX_INPLACE", VM.SOFTMAX_INPLACE (0, 1));
  ("layernorm", "LAYERNORM_INPLACE", VM.LAYERNORM_INPLACE (0, 1, 2, 3));
  ("rmsnorm", "RMSNORM_INPLACE", VM.RMSNORM_INPLACE (0, 1, 2));
  ("silu", "SILU_INPLACE", VM.SILU_INPLACE (0, 1));
  ("rope", "ROPE_APPLY", VM.ROPE_APPLY (0, 1, 2, 3));
  ("matmul-fp", "MATMUL_FP", VM.MATMUL_FP (0, 1, 2, 3, 4, 5));
  ("rmsnorm-fp", "RMSNORM_FP", VM.RMSNORM_FP (0, 1, 2));
  ("rmsnorm-fp-eps", "RMSNORM_FP_EPS", VM.RMSNORM_FP_EPS (0, 1, 2, 3));
  ("l2norm-fp", "L2NORM_FP", VM.L2NORM_FP (0, 1, 2));
  ("silu-fp", "SILU_FP", VM.SILU_FP (0, 1));
  ("mul-fp", "ELEMWISE_MUL_FP", VM.ELEMWISE_MUL_FP (0, 1, 2));
  ("residual-fp", "RESIDUAL_ADD_FP", VM.RESIDUAL_ADD_FP (0, 1, 2));
  ("rope-fp", "ROPE_APPLY_FP", VM.ROPE_APPLY_FP (0, 1, 2, 3));
  ("rope-indexed-fp", "ROPE_APPLY_INDEXED_FP",
   VM.ROPE_APPLY_INDEXED_FP (0, 1, 2, 3, 4, 5));
  ("load-int8-fp", "LOAD_INT8_FP", VM.LOAD_INT8_FP (0, 1, 2, 3, 4));
  ("vecdot-fp", "VECDOT_FP", VM.VECDOT_FP (0, 1, 2, 3));
  ("argmax-fp", "ARGMAX_FP", VM.ARGMAX_FP (0, 1, 2));
  ("attention-scores-fp", "ATTENTION_SCORES_FP",
   VM.ATTENTION_SCORES_FP (0, 1, 2, 3, 4));
  ("softmax-fp", "SOFTMAX_FP", VM.SOFTMAX_FP (0, 1, 2));
  ("attention-weighted-sum-fp", "ATTENTION_WEIGHTED_SUM_FP",
   VM.ATTENTION_WEIGHTED_SUM_FP (0, 1, 2, 3, 4));
  ("attention-fp", "ATTENTION_KV_FP",
   VM.ATTENTION_KV_FP (0, 1, 2, 3, 4, 5, 6, 7));
  ("append-fp", "APPEND_VEC_FP", VM.APPEND_VEC_FP (0, 1, 2, 3));
  ("linear-q1-g128", "LINEAR_Q1_G128_FP",
   VM.LINEAR_Q1_G128_FP (0, 1, 2, 3, 4, 5, 6));
  ("sigmoid-fp", "SIGMOID_FP", VM.SIGMOID_FP (0, 1));
  ("softplus-fp", "SOFTPLUS_FP", VM.SOFTPLUS_FP (0, 1));
  ("causal-depthwise-conv1d-fp", "CAUSAL_DEPTHWISE_CONV1D_FP",
   VM.CAUSAL_DEPTHWISE_CONV1D_FP (0, 1, 2, 3, 4, 5));
  ("gated-delta-rule-fp", "GATED_DELTA_RULE_FP",
   VM.GATED_DELTA_RULE_FP
     (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13));
]

let profiled_cases = [
  ("load-f32-le", "LOAD_F32_LE_FP", VM.LOAD_F32_LE_FP (0, 1, 2, 3));
  ("load-f64-le", "LOAD_F64_LE_FP", VM.LOAD_F64_LE_FP (0, 1, 2, 3));
]

let program_only_cases = [
  ("exp-q16", "EXP_Q16", VM.EXP_Q16 (0, 1));
  ("softmax-q16", "SOFTMAX_Q16_INPLACE", VM.SOFTMAX_Q16_INPLACE (0, 1));
  ("layernorm-q16", "LAYERNORM_Q16_INPLACE",
   VM.LAYERNORM_Q16_INPLACE (0, 1, 2, 3));
  ("rmsnorm-q16", "RMSNORM_Q16_INPLACE", VM.RMSNORM_Q16_INPLACE (0, 1, 2));
  ("silu-q16", "SILU_Q16_INPLACE", VM.SILU_Q16_INPLACE (0, 1));
  ("rope-q16", "ROPE_APPLY_Q16", VM.ROPE_APPLY_Q16 (0, 1, 2, 3));
  ("attention-q16", "ATTENTION_KV_Q16",
   VM.ATTENTION_KV_Q16 (0, 1, 2, 3, 4, 5, 6, 7));
  ("vecdot-q16", "VECDOT_Q16", VM.VECDOT_Q16 (0, 1, 2, 3));
  ("mul-q16", "ELEMWISE_MUL_Q16", VM.ELEMWISE_MUL_Q16 (0, 1, 2));
  ("residual-q16", "RESIDUAL_ADD_Q16", VM.RESIDUAL_ADD_Q16 (0, 1, 2));
  ("load-int8-q16", "LOAD_INT8_Q16", VM.LOAD_INT8_Q16 (0, 1, 2, 3, 4));
  ("append-q16", "APPEND_VEC_Q16", VM.APPEND_VEC_Q16 (0, 1, 2, 3));
  ("argmax-q16", "ARGMAX_Q16", VM.ARGMAX_Q16 (0, 1, 2));
]

let legacy_and_program_cases = [
  ("stop", "STOP", VM.STOP);
  ("add", "ADD", VM.ADD (0, 1, 2));
  ("storage-read", "SLOAD", VM.SLOAD (0, "k"));
  ("fhe", "FHE_ADD", VM.FHE_ADD (0, 1, 2, 3));
  ("blob-bytes", "LOAD_INT8_BYTES_TO_MEM",
   VM.LOAD_INT8_BYTES_TO_MEM (0, 1, 2, 3, 4));
  ("blob-b64", "LOAD_INT8_B64_TO_MEM",
   VM.LOAD_INT8_B64_TO_MEM (0, 1, 2, 3, 4));
  ("matmul-q16-outlier", "MATMUL_Q16", VM.MATMUL_Q16 (0, 1, 2, 3, 4, 5));
  ("shift-round-outlier", "SHIFT_ROUND_INPLACE", VM.SHIFT_ROUND_INPLACE (0, 1, 2));
]

let check_legacy_error () =
  let code = [|
    VM.EXP_Q16 (0, 1);
    VM.ATTENTION_KV_FP (0, 1, 2, 3, 4, 5, 6, 7);
  |] in
  match Policy.legacy_error code with
  | Some (Policy.Program_only hit) ->
    check "legacy error keeps Program-only precedence"
      (hit.Policy.pc = 0 && String.equal hit.Policy.opcode "EXP_Q16")
  | _ ->
    failwith "legacy error keeps Program-only precedence"

let check_first_host_float () =
  let code = [|
    VM.EXP_Q16 (0, 1);
    VM.ATTENTION_KV_FP (0, 1, 2, 3, 4, 5, 6, 7);
  |] in
  match Policy.first_host_float code with
  | Some hit ->
    check "first host float skips Program-only opcodes"
      (hit.Policy.pc = 1 && String.equal hit.Policy.opcode "ATTENTION_KV_FP")
  | _ ->
    failwith "first host float skips Program-only opcodes"

let () =
  List.iter
    (fun (label, expected_name, op) ->
      check_host_float label expected_name op)
    host_float_cases;
  List.iter
    (fun (label, expected_name, op) ->
      check_profiled label expected_name op)
    profiled_cases;
  List.iter
    (fun (label, expected_name, op) ->
      check_program_only label expected_name op)
    program_only_cases;
  List.iter
    (fun (label, expected_name, op) ->
      check_legacy_and_program label expected_name op)
    legacy_and_program_cases;
  check_legacy_error ();
  check_first_host_float ()
