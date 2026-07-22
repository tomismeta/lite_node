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


val request_schema : int
val advance_entrypoint : string
val advance_label : int
val input_root_cell : int
val output_base_register : int
val output_count_register : int
val v1_json : Yojson.Safe.t
val v1_root : string
