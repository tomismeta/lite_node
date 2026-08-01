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
val sequence_cell : int
val logical_position_cell : int
val output_root_cell : int
val output_prefix_root_cell : int
val committed_target_state_root_cell : int
val output_base_register : int
val output_count_register : int

type continuation_context = {
  sequence : int;
  logical_position : int;
  output_root : string;
  output_prefix_root : string;
  committed_target_state_root : string option;
}

val v1_json : Yojson.Safe.t
val v1_root : string
val v2_json : Yojson.Safe.t
val v2_root : string
val supported_roots : string list
val supported_root : string -> bool
val continuation_supported : string -> bool
val supported_root_message : string
