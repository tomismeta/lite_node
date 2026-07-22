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
