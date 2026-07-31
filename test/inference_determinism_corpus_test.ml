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

module Corpus = Octra_vm.Inference_determinism_corpus

let check label condition =
  if not condition then failwith label

let operation schedule_operation opcode recommendation =
  `Assoc [
    "schedule_operation", `String schedule_operation;
    "current_litenode_opcode", `String opcode;
    "recommendation", `String recommendation;
  ]

let qualification_summary =
  `Assoc [
    "type", `String "determinism_qualification_corpus";
    "status", `String "emitted";
    "fixture_count", `Int 5;
    "failure_case_count", `Int 2;
    "schedule_cutpoint_count", `Int 32;
    "recommendation_summary",
    `Assoc [
      "selected_token_changes_under_q16_16_candidate", `Int 0;
      "first_divergent_cutpoint",
      `String "fixtures:expected-final-norm.f64le";
    ];
  ]

let operation_mapping =
  `Assoc [
    "operations",
    `List [
      operation
        "q1_g128_projection"
        "LINEAR_Q1_G128_FP"
        "deterministic_software_fp_required";
      operation
        "rmsnorm_explicit_epsilon"
        "RMSNORM_FP_EPS"
        "deterministic_software_fp_required";
      operation
        "l2norm_explicit_epsilon"
        "L2NORM_FP"
        "deterministic_software_fp_required";
      operation
        "softmax"
        "SOFTMAX_FP"
        "deterministic_software_fp_required";
      operation
        "gated_delta_rule"
        "GATED_DELTA_RULE_FP"
        "deterministic_software_fp_required";
    ];
  ]

let qualification_fixture primitive =
  `Assoc [
    "primitive", `String primitive;
    "case", `String "ordinary";
    "root", `String (String.make 64 'a');
  ]

let qualification_fixture_corpus =
  `Assoc [
    "case_count", `Int 5;
    "cases",
    `List [
      qualification_fixture "q1_g128_projection";
      qualification_fixture "rmsnorm_fp_eps";
      qualification_fixture "l2norm_fp_eps";
      qualification_fixture "softmax_fp";
      qualification_fixture "gated_delta_rule_fp";
    ];
  ]

let qualification_failure_cases =
  `List [
    `Assoc ["case", `String "nonfinite_input_nan"];
    `Assoc ["case", `String "negative_epsilon"];
  ]

let ingestion_summary =
  `Assoc [
    "type", `String "determinism_ingestion_corpus";
    "status", `String "emitted";
    "fixture_count", `Int 5;
    "failure_case_count", `Int 35;
    "differential_cutpoint_count", `Int 48;
    "logit_cutpoint_count", `Int 2;
  ]

let ingestion_fixture opcode primitive =
  `Assoc [
    "opcode", `String opcode;
    "primitive", `String primitive;
    "failure_case_count", `Int 7;
    "fixture_root", `String (String.make 64 'b');
  ]

let ingestion_fixture_pack =
  `Assoc [
    "type", `String "p0_litenode_ingestion_fixture_pack";
    "status", `String "emitted";
    "fixture_count", `Int 5;
    "fixtures",
    `List [
      ingestion_fixture "LINEAR_Q1_G128_FP" "linear_q1_0_g128_fp";
      ingestion_fixture "RMSNORM_FP_EPS" "rmsnorm_fp_eps";
      ingestion_fixture "L2NORM_FP" "l2norm_fp";
      ingestion_fixture "SOFTMAX_FP" "softmax_fp";
      ingestion_fixture "GATED_DELTA_RULE_FP" "gated_delta_rule_fp";
    ];
  ]

let failure_group =
  `List [
    `Assoc ["case", `String "nonfinite_input_nan"];
    `Assoc ["case", `String "nonfinite_input_infinity"];
    `Assoc ["case", `String "insufficient_input_bytes"];
    `Assoc ["case", `String "output_input_aliasing"];
    `Assoc ["case", `String "shape"];
    `Assoc ["case", `String "overflow"];
    `Assoc ["case", `String "insufficient_effort"];
  ]

let ingestion_failure_cases =
  `Assoc [
    "LINEAR_Q1_G128_FP", failure_group;
    "RMSNORM_FP_EPS", failure_group;
    "L2NORM_FP", failure_group;
    "SOFTMAX_FP", failure_group;
    "GATED_DELTA_RULE_FP", failure_group;
  ]

let differential_summary =
  `Assoc [
    "type", `String "ingestion_differential_reports";
    "reports",
    `List [
      `Assoc [
        "candidate", `String "signed_q16_16_round_to_nearest_saturating";
        "selected_token_changes", `Int 1;
        "top_k_order_changes", `Int 1;
      ];
      `Assoc [
        "candidate", `String "signed_q32_32_round_to_nearest_saturating";
        "selected_token_changes", `Int 0;
        "top_k_order_changes", `Int 0;
      ];
    ];
  ]

let check_accepts_qualification_corpus () =
  match
    Corpus.of_json
      ~summary:qualification_summary
      ~operation_mapping
      ~fixture_corpus:qualification_fixture_corpus
      ~failure_cases:qualification_failure_cases
      ()
  with
  | Error error -> failwith (Corpus.error_message error)
  | Ok corpus ->
    check "corpus type"
      (String.equal corpus.Corpus.corpus_type "determinism_qualification_corpus");
    check "operation count" (corpus.operation_count = Some 5);
    check "fixture count" (corpus.fixture_count = 5);
    check "failure case count" (corpus.failure_case_count = 2);
    check "p0 worklist" (List.length corpus.p0_worklist = 5);
    check
      "selected token changes"
      (corpus.selected_token_changes_under_q16_16_candidate = Some 0);
    check
      "first divergence"
      (corpus.first_divergent_cutpoint =
       Some "fixtures:expected-final-norm.f64le")

let check_accepts_ingestion_corpus () =
  match
    Corpus.of_json
      ~summary:ingestion_summary
      ~differential_summary
      ~fixture_corpus:ingestion_fixture_pack
      ~failure_cases:ingestion_failure_cases
      ()
  with
  | Error error -> failwith (Corpus.error_message error)
  | Ok corpus ->
    check "corpus type"
      (String.equal corpus.Corpus.corpus_type "determinism_ingestion_corpus");
    check "operation count omitted" (corpus.operation_count = None);
    check "fixture count" (corpus.fixture_count = 5);
    check "failure case count" (corpus.failure_case_count = 35);
    check "cutpoint count" (corpus.cutpoint_count = 48);
    check "logit cutpoint count" (corpus.logit_cutpoint_count = Some 2);
    check
      "q16 selected token changes"
      (corpus.selected_token_changes_under_q16_16_candidate = Some 1);
    check "differential reports" (List.length corpus.differential_reports = 2);
    check "p0 worklist" (List.length corpus.p0_worklist = 5)

let check_rejects_missing_p0_fixture () =
  let fixture_corpus =
    `Assoc [
      "case_count", `Int 4;
      "cases",
      `List [
        qualification_fixture "q1_g128_projection";
        qualification_fixture "rmsnorm_fp_eps";
        qualification_fixture "l2norm_fp_eps";
        qualification_fixture "softmax_fp";
      ];
    ]
  in
  let summary = `Assoc [
    "type", `String "determinism_qualification_corpus";
    "status", `String "emitted";
    "fixture_count", `Int 4;
    "failure_case_count", `Int 2;
    "bonsai_cutpoint_count", `Int 32;
  ] in
  match
    Corpus.of_json
      ~summary
      ~operation_mapping
      ~fixture_corpus
      ~failure_cases:qualification_failure_cases
      ()
  with
  | Error (Corpus.Corpus_error message) ->
    check
      "missing gated fixture"
      (String.equal message "missing P0 fixture primitive: gated_delta_rule_fp")
  | Error error -> failwith (Corpus.error_message error)
  | Ok _ -> failwith "expected missing fixture rejection"

let () =
  check_accepts_qualification_corpus ();
  check_accepts_ingestion_corpus ();
  check_rejects_missing_p0_fixture ()
