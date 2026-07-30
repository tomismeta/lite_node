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

type differential_status = {
  candidate : string;
  selected_token_changes : int;
  top_k_order_changes : int;
}

type t = {
  corpus_type : string;
  operation_count : int option;
  fixture_count : int;
  failure_case_count : int;
  cutpoint_count : int;
  logit_cutpoint_count : int option;
  selected_token_changes_under_q16_16_candidate : int option;
  first_divergent_cutpoint : string option;
  differential_reports : differential_status list;
  p0_worklist : p0_status list;
}

type error =
  | Json_error of string
  | Corpus_error of string

val of_json :
  ?operation_mapping:Yojson.Safe.t ->
  ?differential_summary:Yojson.Safe.t ->
  summary:Yojson.Safe.t ->
  fixture_corpus:Yojson.Safe.t ->
  failure_cases:Yojson.Safe.t ->
  unit ->
  (t, error) result

val to_json : t -> Yojson.Safe.t
val error_message : error -> string
