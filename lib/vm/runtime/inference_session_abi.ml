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

let supported_roots = [v1_root; v2_root]

let supported_root root =
  List.exists (String.equal root) supported_roots

let continuation_supported root =
  String.equal root v2_root

let supported_root_message =
  String.concat "," supported_roots
