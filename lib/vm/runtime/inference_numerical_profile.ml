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

type consensus_status =
  | Local_only
  | Consensus_candidate
  | Consensus_ready

type t = {
  name : string;
  consensus_status : consensus_status;
  summary : string;
  required_actions : string list;
}

type status_counts = {
  local_only : int;
  consensus_candidate : int;
  consensus_ready : int;
  unknown : int;
}

type root_binding_counts = {
  matched : int;
  unbound : int;
  unavailable : int;
}

type error =
  | Unknown_profile of string
  | Unsupported_opcode_profile of {
      opcode : string;
      profile : string;
      expected : string;
    }

let status_string = function
  | Local_only -> "local_only"
  | Consensus_candidate -> "consensus_candidate"
  | Consensus_ready -> "consensus_ready"

let empty_status_counts = {
  local_only = 0;
  consensus_candidate = 0;
  consensus_ready = 0;
  unknown = 0;
}

let empty_root_binding_counts = {
  matched = 0;
  unbound = 0;
  unavailable = 0;
}

let gate_status = function
  | `Assoc fields ->
    (match List.assoc_opt "consensus_status" fields with
     | Some (`String value) -> Some value
     | _ -> Some "unknown")
  | _ -> Some "unknown"

let add_gate_status counts gate =
  match gate_status gate with
  | Some "local_only" ->
    { counts with local_only = counts.local_only + 1 }
  | Some "consensus_candidate" ->
    { counts with consensus_candidate = counts.consensus_candidate + 1 }
  | Some "consensus_ready" ->
    { counts with consensus_ready = counts.consensus_ready + 1 }
  | Some _ ->
    { counts with unknown = counts.unknown + 1 }
  | None -> counts

let status_counts_of_json_gates gates =
  List.fold_left add_gate_status empty_status_counts gates

let classified_gate_count counts =
  counts.local_only
  + counts.consensus_candidate
  + counts.consensus_ready
  + counts.unknown

let status_counts_are_consensus_ready counts =
  counts.consensus_ready > 0
  && counts.local_only = 0
  && counts.consensus_candidate = 0
  && counts.unknown = 0

let consensus_ready ~profile_gate_count ~unprofiled_count counts =
  profile_gate_count > 0
  && classified_gate_count counts = profile_gate_count
  && unprofiled_count = 0
  && status_counts_are_consensus_ready counts

let add_if condition value values =
  if condition then value :: values else values

let consensus_ready_blockers ~profile_gate_count ~unprofiled_count counts =
  let classified_count = classified_gate_count counts in
  []
  |> add_if
       (classified_count <> profile_gate_count)
       "unclassified_profile_gates"
  |> add_if (counts.unknown > 0) "unknown_profile_gates"
  |> add_if (counts.consensus_candidate > 0) "consensus_candidate_profile_gates"
  |> add_if (counts.local_only > 0) "local_only_profile_gates"
  |> add_if (unprofiled_count > 0) "unprofiled_profile_gates"
  |> add_if (profile_gate_count = 0) "no_profile_gates"

let root_binding_status = function
  | `Assoc fields ->
    (match List.assoc_opt "status" fields with
     | Some (`String "matched") -> "matched"
     | Some (`String "unbound") -> "unbound"
     | Some (`String "unavailable") -> "unavailable"
     | _ -> "unavailable")
  | _ -> "unavailable"

let add_root_binding_status counts binding =
  match root_binding_status binding with
  | "matched" -> { counts with matched = counts.matched + 1 }
  | "unbound" -> { counts with unbound = counts.unbound + 1 }
  | _ -> { counts with unavailable = counts.unavailable + 1 }

let root_binding_counts_of_json bindings =
  List.fold_left add_root_binding_status empty_root_binding_counts bindings

let root_bindings_are_consensus_ready counts =
  counts.unbound = 0 && counts.unavailable = 0

let root_binding_blockers counts =
  []
  |> add_if (counts.unavailable > 0) "unavailable_profile_roots"
  |> add_if (counts.unbound > 0) "unbound_profile_roots"

let root_binding_counts_json counts =
  `Assoc [
    "matched", `Int counts.matched;
    "unbound", `Int counts.unbound;
    "unavailable", `Int counts.unavailable;
  ]

let status_counts_json counts =
  `Assoc [
    "local_only", `Int counts.local_only;
    "consensus_candidate", `Int counts.consensus_candidate;
    "consensus_ready", `Int counts.consensus_ready;
    "unknown", `Int counts.unknown;
  ]

let error_message = function
  | Unknown_profile profile -> "unknown numerical profile: " ^ profile
  | Unsupported_opcode_profile { opcode; profile; expected } ->
    Printf.sprintf
      "profile %s is not implemented for opcode %s; expected %s"
      profile
      opcode
      expected

let host_fp_actions = [
  "bind exact reduction order, rounding, non-finite, signed-zero, subnormal, overflow, and aliasing semantics";
  "replace host math with deterministic software arithmetic or qualify this profile as local-only";
  "pass cross-platform conformance before validator admission";
]

let of_name = function
  | "host-fp-local-candidate" as name ->
    Ok {
      name;
      consensus_status = Local_only;
      summary =
        "native host floating point accepted only for local inference proof execution";
      required_actions = host_fp_actions;
    }
  | "byte-ingress-exact" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "little-endian finite floating-point byte-ingress profile";
      required_actions = [
        "pin little-endian f32/f64 bit interpretation and finite rejection";
        "bind source bytes through authenticated ranges and storage_read effects";
        "preserve decode atomicity before exposing loaded cells to arithmetic kernels";
      ];
    }
  | "q16-exact" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "integer Q16 fixed-point profile candidate";
      required_actions = [
        "prove model quality and token-order preservation for each admitted primitive";
        "reject use where explicit epsilon or precision requirements cannot be represented";
      ];
    }
  | "q32-exact" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "integer Q32 fixed-point profile candidate";
      required_actions = [
        "define exact scaling, rounding, overflow, and saturation behavior";
        "prove model quality and token-order preservation";
      ];
    }
  | "soft-fp-exact" as name ->
    Ok {
      name;
      consensus_status = Consensus_candidate;
      summary = "software-defined floating-point profile candidate";
      required_actions = [
        "implement software sqrt, exp, log, sin, cos, and arithmetic where used";
        "pin bit-exact scalar oracle and cross-platform conformance";
      ];
    }
  | profile -> Error (Unknown_profile profile)

let current_runtime_profile ~opcode =
  match opcode with
  | "LOAD_F32_LE_FP"
  | "LOAD_F64_LE_FP" ->
    Some "byte-ingress-exact"
  | "LINEAR_Q1_G128_FP"
  | "RMSNORM_FP_EPS"
  | "L2NORM_FP"
  | "SOFTMAX_FP"
  | "GATED_DELTA_RULE_FP"
  | "CAUSAL_DEPTHWISE_CONV1D_FP"
  | "ARGMAX_FP"
  | "ROPE_APPLY_INDEXED_FP"
  | "ATTENTION_SCORES_FP"
  | "ATTENTION_WEIGHTED_SUM_FP"
  | "SIGMOID_FP"
  | "SOFTPLUS_FP"
  | "SILU_FP"
  | "ELEMWISE_MUL_FP"
  | "RESIDUAL_ADD_FP" ->
    Some "host-fp-local-candidate"
  | _ -> None

let validate_for_opcode ~opcode ~profile =
  match of_name profile, current_runtime_profile ~opcode with
  | Error error, _ -> Error error
  | Ok parsed, None -> Ok parsed
  | Ok parsed, Some expected when String.equal parsed.name expected -> Ok parsed
  | Ok parsed, Some expected ->
    Error (Unsupported_opcode_profile { opcode; profile = parsed.name; expected })

let local_semantics ~opcode =
  match opcode with
  | "LOAD_F32_LE_FP" ->
    [
      "source bytes are read from a string/bytes register";
      "offset is byte-based, non-negative, and must cover count little-endian f32 cells";
      "finite IEEE-754 binary32 values are decoded into binary64 memory cells";
      "NaN and infinity are rejected before writeback";
      "outputs are written only after the complete finite decode buffer is computed";
    ]
  | "LOAD_F64_LE_FP" ->
    [
      "source bytes are read from a string/bytes register";
      "offset is byte-based, non-negative, and must cover count little-endian f64 cells";
      "finite IEEE-754 binary64 bit patterns are copied into binary64 memory cells";
      "NaN and infinity are rejected before writeback";
      "outputs are written only after the complete finite decode buffer is computed";
    ]
  | "LINEAR_Q1_G128_FP" ->
    [
      "Q1-G128 blocks are 18 bytes: little-endian binary16 scale followed by 128 sign bits";
      "binary16 scale decode uses integer bit fields and binary exponent shifts";
      "finite binary16 scales include zero, signed zero, subnormal, normal, and max-finite values";
      "sign bit 1 maps to +1.0 and sign bit 0 maps to -1.0";
      "loop order is row, column, block, item with a deterministic finite binary64 accumulator";
      "input cells, decoded scales, and final outputs must be finite before writeback";
      "destination cells are written only after the full output buffer is computed";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "epsilon is read from an integer register as binary64 bits and must be finite and positive";
      "minimum positive subnormal epsilon is accepted by the current host-fp profile";
      "input and gamma cells are finite binary64 values and their ranges must not overlap";
      "sum of squares is accumulated left-to-right with deterministic finite binary64 multiply/add";
      "inverse RMS is computed as 1.0 / sqrt((sum_sq / count) + epsilon)";
      "output multiply uses deterministic finite binary64 multiply";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "L2NORM_FP" ->
    [
      "epsilon is read from an integer register as binary64 bits and must be finite and positive";
      "input cells are finite binary64 values";
      "sum of squares is accumulated left-to-right in native binary64";
      "inverse norm is computed as 1.0 / sqrt(sum_sq + epsilon)";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "SOFTMAX_FP" ->
    [
      "score cells are finite binary64 values";
      "maximum score is selected left-to-right before exponentiation";
      "exp is applied to each score minus the selected maximum score";
      "probabilities are divided by the native binary64 sum of exponentials";
      "destination may equal scores exactly, but partial overlap is rejected";
    ]
  | "SIGMOID_FP" ->
    [
      "input cells are finite binary64 values and are updated in place";
      "sigmoid is computed as 1.0 / (1.0 + exp(-x)) using native binary64";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "SOFTPLUS_FP" ->
    [
      "input cells are finite binary64 values and are updated in place";
      "positive inputs use x + log1p(exp(-x)) and other inputs use log1p(exp(x))";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "SILU_FP" ->
    [
      "input cells are finite binary64 values and are updated in place";
      "SiLU is computed as x * (1.0 / (1.0 + exp(-x))) using native binary64";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "ELEMWISE_MUL_FP" ->
    [
      "destination and source cells are finite binary64 values";
      "destination may equal source exactly, but partial overlap is rejected";
      "each output cell is computed with native binary64 multiplication";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "RESIDUAL_ADD_FP" ->
    [
      "destination and source cells are finite binary64 values";
      "destination may equal source exactly, but partial overlap is rejected";
      "each output cell is computed with native binary64 addition";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "query, key, value, decay, beta, and recurrent-state cells are finite binary64 values";
      "value heads map to query and key heads by modulo";
      "state decay uses exp(log_decay) and scale uses 1.0 / sqrt(key_dim)";
      "loop order is timestep, value head, value row, key column";
      "output and next-state cells are written only after both buffers are finite";
    ]
  | "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    [
      "input and kernel cells are finite binary64 values";
      "timesteps, channels, and width must be positive";
      "each output cell uses causal depthwise indexing over matching channels";
      "accumulation is left-to-right by kernel index using native binary64";
      "input and kernel ranges are snapshotted before output writeback";
      "outputs are written only after the complete finite output tensor is computed";
    ]
  | "ARGMAX_FP" ->
    [
      "input cells are finite binary64 values";
      "comparison uses native binary64 greater-than";
      "ties keep the lowest zero-based index, including signed-zero ties";
      "the selected index is written only after the full input span is read";
    ]
  | "ROPE_APPLY_INDEXED_FP" ->
    [
      "input cells are finite binary64 values and are updated in place";
      "base is read as binary64, must be finite, and must be greater than 1.0";
      "position cells must be exact signed integers within the binary64-safe integer range";
      "theta uses native binary64 exponentiation before native cos and sin";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | "ATTENTION_SCORES_FP" ->
    [
      "query and key cells are finite binary64 values";
      "each score is a dot product accumulated left-to-right by head dimension";
      "scale is computed as 1.0 / sqrt(head_dim) using native binary64";
      "each output score is computed with native binary64 multiplication and addition";
      "outputs are written only after the complete finite score vector is computed";
    ]
  | "ATTENTION_WEIGHTED_SUM_FP" ->
    [
      "probability and value cells are finite binary64 values";
      "each output dimension is accumulated left-to-right by key index";
      "probabilities are consumed as provided and are not renormalized";
      "each weighted value is computed with native binary64 multiplication and addition";
      "outputs are written only after the complete finite output vector is computed";
    ]
  | _ -> []

let consensus_obligations ~opcode =
  match opcode with
  | "LOAD_F32_LE_FP" ->
    [
      "pin IEEE-754 binary32 to binary64 widening for zero, signed zero, subnormal, normal, and max-finite values";
      "define non-finite rejection, offset/count bounds, writeback atomicity, and effort";
      "bind source bytes to authenticated range roots when loaded through FLOAD";
      "pass independent cross-platform conformance for f32 little-endian ingress edge vectors";
    ]
  | "LOAD_F64_LE_FP" ->
    [
      "pin IEEE-754 binary64 bit preservation for zero, signed zero, subnormal, normal, and max-finite values";
      "define non-finite rejection, offset/count bounds, writeback atomicity, and effort";
      "bind source bytes to authenticated range roots when loaded through FLOAD";
      "pass independent cross-platform conformance for f64 little-endian ingress edge vectors";
    ]
  | "LINEAR_Q1_G128_FP" ->
    [
      "pin binary16 scale decode for zero, signed zero, subnormal, normal, NaN, and infinity";
      "qualify deterministic binary64 multiply/add rounding and accumulator behavior";
      "define exact output encoding, overflow policy, and writeback atomicity";
      "pass independent cross-platform conformance for scale, sign, and accumulation edge vectors";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "replace or qualify native binary64 division and sqrt";
      "qualify deterministic binary64 reduction and output multiplication";
      "pin signed-zero, subnormal, overflow, underflow, and non-finite behavior";
      "define exact epsilon-bit interpretation and range-overlap rejection";
      "pass independent cross-platform conformance for reduction and sqrt edge vectors";
    ]
  | "L2NORM_FP" ->
    [
      "replace or qualify native binary64 reduction, division, multiplication, and sqrt";
      "pin epsilon-bit interpretation, signed-zero, subnormal, overflow, and non-finite behavior";
      "define exact output encoding and in-place writeback atomicity";
      "pass independent cross-platform conformance for inverse-norm edge vectors";
    ]
  | "SOFTMAX_FP" ->
    [
      "replace or qualify native binary64 exp, division, summation, and comparison behavior";
      "pin max-subtract semantics, ties, underflow, overflow, and non-finite rejection";
      "define exact output encoding and overlap/writeback atomicity";
      "pass independent cross-platform conformance for probability and ordering edge vectors";
    ]
  | "SIGMOID_FP" ->
    [
      "replace or qualify native binary64 exp, division, and addition";
      "pin saturation, signed-zero, subnormal, overflow, and non-finite behavior";
      "define in-place writeback atomicity and effort";
      "pass independent cross-platform conformance for sigmoid edge vectors";
    ]
  | "SOFTPLUS_FP" ->
    [
      "replace or qualify native binary64 exp, log1p, addition, and branch behavior";
      "pin positive/negative branch boundary, signed-zero, subnormal, overflow, and non-finite behavior";
      "define in-place writeback atomicity and effort";
      "pass independent cross-platform conformance for softplus edge vectors";
    ]
  | "SILU_FP" ->
    [
      "replace or qualify native binary64 exp, division, multiplication, and addition";
      "pin sigmoid reuse, signed-zero, subnormal, overflow, and non-finite behavior";
      "define in-place writeback atomicity and effort";
      "pass independent cross-platform conformance for SiLU edge vectors";
    ]
  | "ELEMWISE_MUL_FP" ->
    [
      "replace or qualify native binary64 multiplication";
      "pin signed-zero, subnormal, overflow, and non-finite behavior";
      "define same-range aliasing, partial-overlap rejection, writeback atomicity, and effort";
      "pass independent cross-platform conformance for elementwise multiply edge vectors";
    ]
  | "RESIDUAL_ADD_FP" ->
    [
      "replace or qualify native binary64 addition";
      "pin signed-zero, subnormal, overflow, and non-finite behavior";
      "define same-range aliasing, partial-overlap rejection, writeback atomicity, and effort";
      "pass independent cross-platform conformance for residual add edge vectors";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "replace or qualify native binary64 exp, sqrt, dot-product, and recurrence behavior";
      "pin head mapping, decay order, beta application, state update order, and scaling";
      "define exact output plus next-state encoding, alias rejection, effort, and atomicity";
      "pass independent cross-platform conformance for recurrent state-transition edge vectors";
    ]
  | "CAUSAL_DEPTHWISE_CONV1D_FP" ->
    [
      "replace or qualify native binary64 multiplication and addition";
      "pin causal depthwise indexing, kernel-order accumulation, finite rejection, and effort";
      "define input/kernel snapshot behavior, output encoding, alias handling, and writeback atomicity";
      "pass independent cross-platform conformance for causal-convolution edge vectors";
    ]
  | "ARGMAX_FP" ->
    [
      "define the exact binary64 comparison relation for signed zero and all finite values";
      "reject non-finite logits before selection and preserve destination on rejection";
      "pin first-maximum tie behavior, selected-index encoding, and effort";
      "prove ordering preservation before mapping fixed-point logits to token authority";
    ]
  | "ROPE_APPLY_INDEXED_FP" ->
    [
      "replace or qualify native binary64 exponentiation, cos, sin, multiply, and add/subtract";
      "pin exact position-cell interpretation, base handling, rotary dimension validation, and zero-position behavior";
      "define in-place writeback atomicity, tail preservation, finite rejection, and effort";
      "pass independent cross-platform conformance for indexed rotary edge vectors";
    ]
  | "ATTENTION_SCORES_FP" ->
    [
      "replace or qualify native binary64 multiplication, addition, sqrt, and division";
      "pin scale semantics, finite rejection, score accumulation order, output atomicity, and effort";
      "define exact output encoding for cancellation, signed-zero, subnormal, and overflow cases";
      "pass independent cross-platform conformance for attention-score edge vectors";
    ]
  | "ATTENTION_WEIGHTED_SUM_FP" ->
    [
      "replace or qualify native binary64 multiplication and addition";
      "pin weighted-sum accumulation order, finite rejection, overlap rejection, writeback atomicity, and effort";
      "define exact output encoding for cancellation, signed-zero, subnormal, and overflow cases";
      "pass independent cross-platform conformance for weighted-sum edge vectors";
    ]
  | _ -> [
      "write primitive-specific deterministic math obligations before promotion";
    ]

let consensus_blocker_codes ~opcode =
  match opcode with
  | "LOAD_F32_LE_FP"
  | "LOAD_F64_LE_FP" ->
    [
      "float_byte_decode";
      "finite_rejection";
      "authenticated_range_binding";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "LINEAR_Q1_G128_FP" ->
    [
      "binary16_scale_decode";
      "q1_sign_mapping";
      "fp64_mul_add_conformance";
      "accumulation_order";
      "finite_overflow_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "epsilon_bit_interpretation";
      "fp64_reduction_conformance";
      "host_fp_sqrt";
      "host_fp_divide";
      "fp64_output_multiply_conformance";
      "signed_zero_subnormal_policy";
      "alias_rejection";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "L2NORM_FP" ->
    [
      "epsilon_bit_interpretation";
      "host_fp_reduction";
      "host_fp_sqrt";
      "host_fp_divide";
      "signed_zero_subnormal_policy";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | "SOFTMAX_FP" ->
    [
      "host_fp_comparison";
      "host_fp_exp";
      "host_fp_reduction";
      "host_fp_divide";
      "probability_ordering";
      "overlap_policy";
      "cross_platform_conformance";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "host_fp_recurrence";
      "state_transition_order";
      "host_fp_dot_product";
      "host_fp_exp_sqrt";
      "alias_rejection";
      "atomic_writeback";
      "cross_platform_conformance";
    ]
  | opcode ->
    match current_runtime_profile ~opcode with
    | Some "host-fp-local-candidate" ->
      [
        "host_fp_arithmetic";
        "edge_value_policy";
        "atomic_writeback";
        "cross_platform_conformance";
      ]
    | Some "byte-ingress-exact" ->
      [
        "float_byte_decode";
        "finite_rejection";
        "authenticated_range_binding";
        "atomic_writeback";
        "cross_platform_conformance";
      ]
    | _ -> []

let arithmetic_domain ~profile ~opcode =
  match profile.name, opcode with
  | "byte-ingress-exact", _ -> "ieee754-little-endian-byte-ingress"
  | "q16-exact", _ -> "integer-q16"
  | "q32-exact", _ -> "integer-q32"
  | "soft-fp-exact", _ -> "software-defined-floating-point"
  | "host-fp-local-candidate", "LINEAR_Q1_G128_FP" ->
    "q1-g128-binary16-scale-deterministic-binary64-accumulator"
  | "host-fp-local-candidate", "RMSNORM_FP_EPS" ->
    "deterministic-binary64-reduction-host-sqrt-divide"
  | "host-fp-local-candidate", _ -> "native-binary64-host-floating-point"
  | name, _ -> name

let rounding_mode ~profile ~opcode =
  match profile.name, opcode with
  | "host-fp-local-candidate", "LINEAR_Q1_G128_FP" ->
    "ieee754-roundTiesToEven"
  | ("q16-exact" | "q32-exact"), _ -> "integer-profile-defined"
  | "soft-fp-exact", _ -> "software-profile-defined"
  | "byte-ingress-exact", _ -> "exact-byte-decode"
  | "host-fp-local-candidate", _ -> "host-runtime-native"
  | _ -> "profile-defined"

let operation_sequence ~opcode =
  match opcode with
  | "LINEAR_Q1_G128_FP" ->
    [
      "decode_q1_g128_scale_fp16_le";
      "decode_q1_g128_sign_bits_lsb0";
      "iterate_row_col_block_item";
      "multiply_lhs_sign_scale";
      "accumulate_left_to_right";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "read_epsilon_binary64_bits";
      "snapshot_input_and_gamma";
      "sum_squares_left_to_right";
      "divide_by_count";
      "add_epsilon";
      "sqrt";
      "reciprocal";
      "multiply_input_inverse_gamma";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "L2NORM_FP" ->
    [
      "read_epsilon_binary64_bits";
      "snapshot_input";
      "sum_squares_left_to_right";
      "add_epsilon";
      "sqrt";
      "reciprocal";
      "multiply_input_inverse";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "SOFTMAX_FP" ->
    [
      "snapshot_scores";
      "select_max_left_to_right";
      "subtract_max";
      "exp_each_score";
      "sum_exponentials_left_to_right";
      "divide_each_exponential_by_sum";
      "finite_output_check";
      "atomic_output_writeback";
    ]
  | "GATED_DELTA_RULE_FP" ->
    [
      "snapshot_operands_and_state";
      "iterate_timestep_value_head_row_column";
      "compute_decay";
      "apply_state_decay";
      "compute_memory_dot";
      "apply_beta_delta";
      "update_state";
      "compute_output_dot";
      "apply_query_scale";
      "finite_output_and_state_check";
      "atomic_output_and_state_writeback";
    ]
  | _ ->
    [
      "primitive_defined_snapshot";
      "primitive_defined_compute";
      "finite_output_check";
      "atomic_writeback";
    ]

let edge_value_policy ~opcode =
  match opcode with
  | "LINEAR_Q1_G128_FP" ->
    [
      "reject_nonfinite_binary16_scale";
      "accept_finite_binary16_zero_signed_zero_subnormal_normal_max";
      "reject_missing_or_nonfinite_lhs";
      "reject_nonfinite_output";
      "preserve_destination_on_reject";
    ]
  | "RMSNORM_FP_EPS" | "L2NORM_FP" ->
    [
      "epsilon_must_decode_to_finite_positive_binary64";
      "reject_missing_or_nonfinite_operands";
      "reject_nonfinite_reduction_inverse_or_output";
      "preserve_destination_on_reject";
    ]
  | _ ->
    [
      "reject_missing_or_nonfinite_operands";
      "reject_nonfinite_output";
      "preserve_destination_on_reject";
    ]

let oracle_vector_root ~opcode =
  let vectors =
    match opcode with
    | "LINEAR_Q1_G128_FP" ->
      [
        "golden_q1_g128";
        "sign_and_scale_edges";
        "accumulation_order_stress";
        "overflow_revert";
        "invalid_lhs_revert";
        "invalid_scale_revert";
        "shape_effort_revert";
        "offset_snapshot_overlap";
      ]
    | "RMSNORM_FP_EPS" ->
      [
        "model_epsilon";
        "epsilon_1e_5";
        "signed_zero_subnormal";
        "minimum_subnormal_epsilon";
        "reduction_order_stress";
        "row_composition";
        "invalid_epsilon_revert";
        "alias_effort_revert";
        "overflow_revert";
      ]
    | _ -> ["profile_gate_only"]
  in
  let payload =
    `Assoc [
      "schema", `String "octra.inference.oracle-vectors.v1";
      "opcode", `String opcode;
      "vectors", `List (List.map (fun value -> `String value) vectors);
    ]
    |> Yojson.Safe.to_string
  in
  Digestif.SHA256.(
    digest_string ("octra:inference:oracle-vectors\000" ^ payload) |> to_hex)

let contract_json_for_opcode ~opcode profile =
  `Assoc [
    "schema", `String "octra.inference.numerical-contract.v1";
    "profile_name", `String profile.name;
    "opcode", `String opcode;
    "arithmetic_domain", `String (arithmetic_domain ~profile ~opcode);
    "rounding_mode", `String (rounding_mode ~profile ~opcode);
    "operation_sequence",
    `List (List.map (fun value -> `String value) (operation_sequence ~opcode));
    "edge_value_policy",
    `List (List.map (fun value -> `String value) (edge_value_policy ~opcode));
    "overflow_policy", `String "reject_nonfinite_before_writeback";
    "writeback_policy", `String "atomic_after_successful_full_output";
    "oracle_vector_root", `String (oracle_vector_root ~opcode);
  ]

let root_for_opcode ~opcode profile =
  let payload =
    Yojson.Safe.to_string (contract_json_for_opcode ~opcode profile)
  in
  Digestif.SHA256.(
    digest_string ("octra:inference:numerical-contract\000" ^ payload) |> to_hex)

let profile_set_root_for_opcodes ~opcodes profile =
  let entries =
    opcodes
    |> List.sort_uniq String.compare
    |> List.map (fun opcode ->
      `Assoc [
        "opcode", `String opcode;
        "profile_root", `String (root_for_opcode ~opcode profile);
      ])
  in
  let payload =
    `Assoc [
      "schema", `String "octra.inference.numerical-profile-set.v1";
      "entries", `List entries;
    ]
    |> Yojson.Safe.to_string
  in
  Digestif.SHA256.(
    digest_string ("octra:inference:numerical-profile-set\000" ^ payload)
    |> to_hex)

let profile_gate_core_json ~opcode profile =
  `Assoc [
    "name", `String profile.name;
    "consensus_status", `String (status_string profile.consensus_status);
    "summary", `String profile.summary;
    "required_actions",
    `List (List.map (fun action -> `String action) profile.required_actions);
    "opcode", `String opcode;
    "local_semantics",
    `List
      (List.map
         (fun value -> `String value)
         (local_semantics ~opcode));
    "consensus_obligations",
    `List
      (List.map
         (fun value -> `String value)
         (consensus_obligations ~opcode));
    "consensus_blocker_codes",
    `List
      (List.map
         (fun value -> `String value)
         (consensus_blocker_codes ~opcode));
  ]

let profile_root_of_json = function
  | `Assoc fields ->
    (match List.assoc_opt "profile_root" fields with
     | Some (`String root) -> Some root
     | _ -> None)
  | _ -> None

let root_binding_json ~numerical_profile_root profile_gate =
  let profile_root = profile_root_of_json profile_gate in
  let status =
    match profile_root with
    | Some root when String.equal root numerical_profile_root -> "matched"
    | Some _ -> "unbound"
    | None -> "unavailable"
  in
  `Assoc [
    "status", `String status;
    "numerical_profile_root", `String numerical_profile_root;
    "profile_root",
    (match profile_root with
     | None -> `Null
     | Some root -> `String root);
  ]

let unavailable_root_binding_json =
  `Assoc [
    "status", `String "unavailable";
    "numerical_profile_root", `Null;
    "profile_root", `Null;
  ]

let to_json profile =
  `Assoc [
    "name", `String profile.name;
    "consensus_status", `String (status_string profile.consensus_status);
    "summary", `String profile.summary;
    "required_actions",
    `List (List.map (fun action -> `String action) profile.required_actions);
  ]

let to_json_for_opcode ~opcode profile =
  match profile_gate_core_json ~opcode profile with
  | `Assoc fields ->
    `Assoc
      (fields
       @ [
         "profile_contract", contract_json_for_opcode ~opcode profile;
         "profile_root", `String (root_for_opcode ~opcode profile);
       ])
  | value -> value
