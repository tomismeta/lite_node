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

let softmax_exp_replacement_plan ~case_name ~outputs =
  `Assoc [
    "schema", `String "octra.inference.deterministic-replacement-plan.v1";
    "diagnostic_only", `Bool true;
    "authority", `String "none";
    "opcode", `String "SOFTMAX_FP";
    "case", `String case_name;
    "current_profile", `String "host-fp-exp-local-candidate";
    "current_classification", `String "host_transcendental_portability_gap";
    "status", `String "required_before_consensus";
    "consensus_admission_status", `String "blocked";
    "validator_admission_blocker", `String "host_transcendental_exp";
    "punitive_math_status", `String "required_before_consensus";
    "consensus_action",
    `String "qualified_protocol_owned_exp_required_before_consensus";
    "preferred_replacement",
    `String "protocol-owned software exp over finite nonpositive binary64 inputs";
    "formal_explanation",
    `Assoc [
      "status", `String "formally_explained";
      "claim",
      `String
        "SOFTMAX_FP is locally executable but not consensus-admissible while shifted-score exponentials use host-native exp.";
      "evidence",
      `List [
        `String
          "The wide-1024-stable-tail fixture completes VM execution and differs by one output f64 bit on the known local host.";
        `String
          "All non-exp steps are already ordered by the VM contract: max selection, score subtraction, exponential accumulation, division, and output encoding.";
        `String
          "The remaining unconstrained step is exp over finite nonpositive shifted scores.";
      ];
      "non_resolution",
      `List [
        `String "do not rebaseline expected bytes to a local host exp result";
        `String "do not treat single-platform agreement as validator readiness";
        `String "do not promote host-fp-exp-local-candidate to consensus-safe";
      ];
    ];
    "blocked_surface",
    `List [
      `String "native host exp";
      `String "max-shifted score exponentials";
      `String "wide-tail probability bit patterns";
    ];
    "required_semantics",
    `List [
      `String "snapshot finite scores before writeback";
      `String "select max left-to-right";
      `String "subtract max with deterministic binary64 semantics";
      `String "reject positive shifted exp input";
      `String "compute exp with protocol-owned software semantics";
      `String "accumulate exponentials left-to-right";
      `String "reject nonpositive or nonfinite exponential sum";
      `String "divide each exponential by the sum with deterministic binary64 semantics";
      `String "encode each output as one little-endian binary64 value";
      `String "preserve destination on validation or arithmetic rejection";
    ];
    "required_punitive_vectors",
    `List [
      `String "equal_scores";
      `String "wide_uniform_tail_1024";
      `String "near_underflow_shift";
      `String "subnormal_probability_tail";
      `String "positive_shifted_exp_reject";
      `String "nonfinite_score_reject";
      `String "partial_overlap_reject";
      `String "low_effort_reject";
    ];
    "acceptance_gates",
    `List [
      `String "new_numerical_profile_root_bound";
      `String "new_vm_semantics_root_bound";
      `String "strict_effort_matches";
      `String "p0_plus_softmax_outputs_match";
      `String "cross_platform_matrix_matches";
    ];
    "fallbacks",
    `List [
      `String "keep SOFTMAX_FP local-only for proof/demo mode";
      `String "admit a narrower fixed-point softmax only after token-order evidence";
    ];
    "observed_outputs", `List outputs;
  ]

let p0_plus_replacement_plan ~opcode ~case_name ~classification ~outputs =
  match opcode, classification with
  | "SOFTMAX_FP", "host_transcendental_portability_gap" ->
    Some (softmax_exp_replacement_plan ~case_name ~outputs)
  | _ -> None
