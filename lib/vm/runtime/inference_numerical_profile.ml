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
  | "LINEAR_Q1_G128_FP" ->
    [
      "Q1-G128 blocks are 18 bytes: little-endian binary16 scale followed by 128 sign bits";
      "finite binary16 scales include zero, signed zero, subnormal, normal, and max-finite values";
      "sign bit 1 maps to +1.0 and sign bit 0 maps to -1.0";
      "loop order is row, column, block, item with a native binary64 accumulator";
      "input cells, decoded scales, and final outputs must be finite before writeback";
      "destination cells are written only after the full output buffer is computed";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "epsilon is read from an integer register as binary64 bits and must be finite and positive";
      "minimum positive subnormal epsilon is accepted by the current host-fp profile";
      "input and gamma cells are finite binary64 values and their ranges must not overlap";
      "sum of squares is accumulated left-to-right in native binary64";
      "inverse RMS is computed as 1.0 / sqrt((sum_sq / count) + epsilon)";
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
  | "LINEAR_Q1_G128_FP" ->
    [
      "pin binary16 scale decode for zero, signed zero, subnormal, normal, NaN, and infinity";
      "replace or qualify native binary64 multiply/add rounding and accumulator behavior";
      "define exact output encoding, overflow policy, and writeback atomicity";
      "pass independent cross-platform conformance for scale, sign, and accumulation edge vectors";
    ]
  | "RMSNORM_FP_EPS" ->
    [
      "replace or qualify native binary64 reduction, division, multiplication, and sqrt";
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

let to_json profile =
  `Assoc [
    "name", `String profile.name;
    "consensus_status", `String (status_string profile.consensus_status);
    "summary", `String profile.summary;
    "required_actions",
    `List (List.map (fun action -> `String action) profile.required_actions);
  ]

let to_json_for_opcode ~opcode profile =
  match to_json profile with
  | `Assoc fields ->
    `Assoc
      (fields
       @ [
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
       ])
  | value -> value
