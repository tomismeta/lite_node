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


let request_schema = 1
let advance_entrypoint = "advance"
let advance_label = 100
let input_root_cell = 1000
let sequence_cell = 1001
let logical_position_cell = 1002
let output_root_cell = 1003
let output_prefix_root_cell = 1004
let committed_target_state_root_cell = 1005
let output_base_register = 0
let output_count_register = 1

let v1_json =
  `Assoc [
    "advance_entrypoint", `String advance_entrypoint;
    "advance_label", `Int advance_label;
    "candidate_state", `String "memory";
    "input_root_cell", `Int input_root_cell;
    "output_base_register", `Int output_base_register;
    "output_count_register", `Int output_count_register;
    "request_schema", `Int request_schema;
  ]

let v1_root =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:session-abi\000" ^ Yojson.Safe.to_string v1_json)
    |> to_hex)

type continuation_context = {
  sequence : int;
  logical_position : int;
  output_root : string;
  output_prefix_root : string;
  committed_target_state_root : string option;
  committed_target_state_payload : string option;
}

let v2_json =
  `Assoc [
    "advance_entrypoint", `String advance_entrypoint;
    "advance_label", `Int advance_label;
    "candidate_state", `String "memory";
    "input_root_cell", `Int input_root_cell;
    "continuation_cells",
    `Assoc [
      "sequence",
      `Assoc [
        "cell", `Int sequence_cell;
        "value_encoding", `String "VInt";
        "semantics", `String "nonnegative pre-advance sequence";
      ];
      "logical_position",
      `Assoc [
        "cell", `Int logical_position_cell;
        "value_encoding", `String "VInt";
        "semantics", `String "nonnegative pre-advance logical position";
      ];
      "output_root",
      `Assoc [
        "cell", `Int output_root_cell;
        "value_encoding", `String "VString";
        "string_encoding", `String "64-lowercase-hex";
      ];
      "output_prefix_root",
      `Assoc [
        "cell", `Int output_prefix_root_cell;
        "value_encoding", `String "VString";
        "string_encoding", `String "64-lowercase-hex";
      ];
      "committed_target_state_root",
      `Assoc [
        "cell", `Int committed_target_state_root_cell;
        "value_encoding", `String "VString";
        "string_encoding", `String "64-lowercase-hex-or-empty";
        "none_encoding", `String "";
      ];
    ];
    "output_base_register", `Int output_base_register;
    "output_count_register", `Int output_count_register;
    "request_schema", `Int request_schema;
  ]

let v2_root =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:session-abi\000" ^ Yojson.Safe.to_string v2_json)
    |> to_hex)

let committed_state_json =
  `Assoc [
    "advance_entrypoint", `String advance_entrypoint;
    "advance_label", `Int advance_label;
    "candidate_state", `String "memory";
    "input_root_cell", `Int input_root_cell;
    "continuation_cells",
    `Assoc [
      "sequence",
      `Assoc [
        "cell", `Int sequence_cell;
        "value_encoding", `String "VInt";
        "semantics", `String "nonnegative pre-advance sequence";
      ];
      "logical_position",
      `Assoc [
        "cell", `Int logical_position_cell;
        "value_encoding", `String "VInt";
        "semantics", `String "nonnegative pre-advance logical position";
      ];
      "output_root",
      `Assoc [
        "cell", `Int output_root_cell;
        "value_encoding", `String "VString";
        "string_encoding", `String "64-lowercase-hex";
      ];
      "output_prefix_root",
      `Assoc [
        "cell", `Int output_prefix_root_cell;
        "value_encoding", `String "VString";
        "string_encoding", `String "64-lowercase-hex";
      ];
      "committed_target_state_root",
      `Assoc [
        "cell", `Int committed_target_state_root_cell;
        "value_encoding", `String "VString";
        "string_encoding", `String "64-lowercase-hex-or-empty";
        "none_encoding", `String "";
        "root_hash", `String "sha256-raw";
        "payload_binding", `String "blob-keyed-by-root";
        "capability", `String "session.committed-state";
      ];
    ];
    "committed_target_state_payload",
    `Assoc [
      "transport", `String "resident-blob";
      "root_cell", `Int committed_target_state_root_cell;
      "root_hash", `String "sha256-raw";
      "payload_binding", `String "blob-keyed-by-root";
      "retention", `String "session";
      "bytes_count_against", `String "max_session_bytes";
      "capability", `String "session.committed-state";
    ];
    "output_base_register", `Int output_base_register;
    "output_count_register", `Int output_count_register;
    "request_schema", `Int request_schema;
  ]

let committed_state_root =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:session-abi\000"
       ^ Yojson.Safe.to_string committed_state_json)
    |> to_hex)

let resident_lifecycle_schema =
  "octra.inference.resident-session-lifecycle.v1"

let resident_lifecycle_json =
  `Assoc [
    "schema", `String resident_lifecycle_schema;
    "product_lifecycle",
    `List [
      `String "open_session";
      `String "prefill";
      `String "decode";
      `String "finalize";
    ];
    "runtime_lifecycle",
    `List [
      `String "open_session";
      `String "advance_session";
      `String "finalize_session";
    ];
    "referenced_session_abi_roots",
    `Assoc [
      "progress_cells", `String v2_root;
      "committed_state_transport", `String committed_state_root;
    ];
    "transition_phase_contract",
    `Assoc [
      "allowed_phases", `List [`String "prefill"; `String "decode"];
      "phase_order",
      `String "exactly-one-prefill-followed-by-zero-or-more-decode";
      "decode_steps",
      `String "must equal the number of decode transitions";
    ];
    "uniform_identity",
    `List [
      `String "target_root";
      `String "request_root";
      `String "model_ranges_root";
      `String "model_deployment_root";
      `String "session_abi_root";
    ];
    "state_transport",
    `Assoc [
      "progress_cells", `String "session-abi-v2";
      "committed_target_state_payload", `String "session-abi-committed-state";
      "committed_target_state_payload_root",
      `String "sha256-raw-bound-to-root-cell";
      "vm_memory_residency",
      `String "fresh-vm-state-per-advance";
      "resident_state_residency",
      `String "host-session-rooted-transport";
    ];
    "failure_semantics",
    `List [
      `String "identity_mismatch_rejects_before_execution";
      `String "declaration_mismatch_rejects_before_execution";
      `String "failed_advance_is_atomic";
      `String "finalize_requires_advanced_session";
    ];
    "output_contracts",
    `Assoc [
      "selected_index",
      `Assoc [
        "authority", `String "vm-emitted-selected-index";
        "payload_encoding",
        `String "base=<r0>|length=1|values=int:<token-id>";
        "value_rule", `String "single-nonnegative-integer";
        "tie_policy", `String "primitive-specific-profile";
        "not_bound_by_this_contract",
        `List [
          `String "vocabulary_bounds";
          `String "argmax_algorithm_correctness";
        ];
      ];
    ];
    "execution_contracts",
    `Assoc [
      "lifecycle_only",
      `Assoc [
        "authority", `String "transition-lifecycle-only";
        "binding",
        `String
          "transition may bind resident lifecycle/request/deployment roots without claiming graph execution";
      ];
      "graph_real",
      `Assoc [
        "authority", `String "executed-generic-vm-inference-compute-evidence";
        "binding",
        `String
          "transition must contain at least one admitted inference compute opcode, run with opcode timing, report at least one executed inference compute opcode, and may require a minimum instruction count";
        "not_bound_by_this_contract",
        `List [
          `String "model-family-semantics";
          `String "full-model-completeness";
          `String "tensor-layout-semantics";
        ];
      ];
    ];
    "prior_state_contracts",
    `Assoc [
      "previous_selected_index_u64le",
      `Assoc [
        "authority", `String "decode-prior-state-inclusion";
        "source", `String "prior committed target-state payload";
        "encoding", `String "little-endian-u64";
        "binding",
        `String
          "selected_index emitted by a previous decode transition must appear at the declared byte offset before the next decode";
        "not_bound_by_this_contract",
        `List [
          `String "vocabulary_bounds";
          `String "argmax_algorithm_correctness";
          `String "tokenizer_semantics";
          `String "decode_program_consumption";
          `String "autoregressive_feedback_semantics";
        ];
      ];
    ];
    "claim_boundary",
    `String
      "resident local candidate session shape; consensus authority comes from profile and conformance roots";
  ]

let resident_lifecycle_root =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:resident-session-lifecycle\000"
       ^ Yojson.Safe.to_string resident_lifecycle_json)
    |> to_hex)

let supported_roots = [v1_root; v2_root; committed_state_root]

let supported_root root =
  List.exists (String.equal root) supported_roots

let continuation_supported root =
  String.equal root v2_root || String.equal root committed_state_root

let committed_state_supported root =
  String.equal root committed_state_root

let supported_root_message =
  String.concat "," supported_roots
