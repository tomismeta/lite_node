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

let summary =
  `Assoc [
    "type", `String "determinism_qualification_corpus";
    "status", `String "emitted";
    "fixture_count", `Int 5;
    "failure_case_count", `Int 2;
    "bonsai_cutpoint_count", `Int 32;
    "recommendation_summary",
    `Assoc [
      "selected_token_changes_under_q16_16_candidate", `Int 0;
      "first_divergent_cutpoint",
      `String "fixtures:expected-final-norm.f64le";
    ];
  ]

let operation schedule_operation opcode recommendation =
  `Assoc [
    "schedule_operation", `String schedule_operation;
    "current_litenode_opcode", `String opcode;
    "recommendation", `String recommendation;
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

let fixture primitive =
  `Assoc [
    "primitive", `String primitive;
    "case", `String "ordinary";
    "root", `String (String.make 64 'a');
  ]

let fixture_corpus =
  `Assoc [
    "case_count", `Int 5;
    "cases",
    `List [
      fixture "q1_g128_projection";
      fixture "rmsnorm_fp_eps";
      fixture "l2norm_fp_eps";
      fixture "softmax_fp";
      fixture "gated_delta_rule_fp";
    ];
  ]

let failure_cases =
  `List [
    `Assoc ["case", `String "nonfinite_input_nan"];
    `Assoc ["case", `String "negative_epsilon"];
  ]

let check_accepts_p0_corpus () =
  match
    Corpus.of_json
      ~summary
      ~operation_mapping
      ~fixture_corpus
      ~failure_cases
  with
  | Error error -> failwith (Corpus.error_message error)
  | Ok corpus ->
    check "operation count" (corpus.Corpus.operation_count = 5);
    check "fixture count" (corpus.Corpus.fixture_count = 5);
    check "failure case count" (corpus.Corpus.failure_case_count = 2);
    check "p0 worklist" (List.length corpus.Corpus.p0_worklist = 5);
    check
      "selected token changes"
      (corpus.Corpus.selected_token_changes_under_q16_16_candidate = Some 0);
    check
      "first divergence"
      (corpus.Corpus.first_divergent_cutpoint =
       Some "fixtures:expected-final-norm.f64le")

let check_rejects_missing_p0_fixture () =
  let fixture_corpus =
    `Assoc [
      "case_count", `Int 4;
      "cases",
      `List [
        fixture "q1_g128_projection";
        fixture "rmsnorm_fp_eps";
        fixture "l2norm_fp_eps";
        fixture "softmax_fp";
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
      ~failure_cases
  with
  | Error (Corpus.Corpus_error message) ->
    check
      "missing gated fixture"
      (String.equal message "missing P0 fixture primitive: gated_delta_rule_fp")
  | Error error -> failwith (Corpus.error_message error)
  | Ok _ -> failwith "expected missing fixture rejection"

let () =
  check_accepts_p0_corpus ();
  check_rejects_missing_p0_fixture ()
