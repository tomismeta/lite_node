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

type p0_status = {
  schedule_operation : string;
  current_litenode_opcode : string;
  recommendation : string;
  next_action : string;
}

type t = {
  operation_count : int;
  fixture_count : int;
  failure_case_count : int;
  bonsai_cutpoint_count : int;
  selected_token_changes_under_q16_16_candidate : int option;
  first_divergent_cutpoint : string option;
  p0_worklist : p0_status list;
}

type error =
  | Json_error of string
  | Corpus_error of string

val of_json :
  summary:Yojson.Safe.t ->
  operation_mapping:Yojson.Safe.t ->
  fixture_corpus:Yojson.Safe.t ->
  failure_cases:Yojson.Safe.t ->
  (t, error) result

val to_json : t -> Yojson.Safe.t
val error_message : error -> string
