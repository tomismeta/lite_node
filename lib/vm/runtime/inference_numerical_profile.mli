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

type consensus_status =
  | Local_only
  | Consensus_candidate
  | Consensus_ready

type t = {
  name : string;
  consensus_status : consensus_status;
  summary : string;
  required_actions : string list;
}

type status_counts = {
  local_only : int;
  consensus_candidate : int;
  consensus_ready : int;
  unknown : int;
}

type root_binding_counts = {
  matched : int;
  unbound : int;
  unavailable : int;
}

type root_binding_classification_counts = {
  none : int;
  profile_root_mismatch : int;
  profile_root_unavailable : int;
  root_binding_unknown : int;
}

type error =
  | Unknown_profile of string
  | Unsupported_opcode_profile of {
      opcode : string;
      profile : string;
      expected : string;
    }

val error_message : error -> string
val of_name : string -> (t, error) result
val status_string : consensus_status -> string
val current_runtime_opcodes : string list
val current_runtime_profile : opcode:string -> string option
val current_runtime_profile_gate : opcode:string -> Yojson.Safe.t option
val current_runtime_profile_catalog_json :
  opcodes:string list -> Yojson.Safe.t
val validate_for_opcode : opcode:string -> profile:string -> (t, error) result
val to_json : t -> Yojson.Safe.t
val contract_json_for_opcode : opcode:string -> t -> Yojson.Safe.t
val root_for_opcode : opcode:string -> t -> string
val profile_set_root_for_opcodes : opcodes:string list -> t -> string
val root_binding_json :
  numerical_profile_root:string -> Yojson.Safe.t -> Yojson.Safe.t
val unavailable_root_binding_json : Yojson.Safe.t
val to_json_for_opcode : opcode:string -> t -> Yojson.Safe.t
val status_counts_of_json_gates : Yojson.Safe.t list -> status_counts
val classified_gate_count : status_counts -> int
val status_counts_are_consensus_candidate : status_counts -> bool
val status_counts_are_consensus_ready : status_counts -> bool
val consensus_candidate :
  profile_gate_count:int -> unprofiled_count:int -> status_counts -> bool
val consensus_ready :
  profile_gate_count:int -> unprofiled_count:int -> status_counts -> bool
val consensus_candidate_blockers :
  profile_gate_count:int -> unprofiled_count:int -> status_counts -> string list
val consensus_ready_blockers :
  profile_gate_count:int -> unprofiled_count:int -> status_counts -> string list
val root_binding_counts_of_json : Yojson.Safe.t list -> root_binding_counts
val root_binding_classification_counts_of_json :
  Yojson.Safe.t list -> root_binding_classification_counts
val root_bindings_are_consensus_ready : root_binding_counts -> bool
val root_binding_blockers : root_binding_counts -> string list
val root_bindings_required_pass :
  required:bool -> root_binding_counts -> bool
val validator_readiness_accepted :
  execution_ready:bool ->
  failure_cases_ready:bool ->
  effort_ready:bool ->
  profile_ready:bool ->
  roots_ready:bool ->
  cross_platform_ready:bool ->
  bool
val root_binding_gate_json :
  required:bool -> root_binding_counts -> Yojson.Safe.t
val root_binding_counts_json : root_binding_counts -> Yojson.Safe.t
val root_binding_classification_counts_json :
  root_binding_classification_counts -> Yojson.Safe.t
val status_counts_json : status_counts -> Yojson.Safe.t
val profile_root_catalog_json : Yojson.Safe.t list -> Yojson.Safe.t
val profile_catalog_root_json : Yojson.Safe.t list -> Yojson.Safe.t
val profile_root_binding_catalog_json : Yojson.Safe.t list -> Yojson.Safe.t
val consensus_blocker_catalog_json : Yojson.Safe.t list -> Yojson.Safe.t
val consensus_blocker_class_counts_json : Yojson.Safe.t list -> Yojson.Safe.t
val transcendental_dependency_catalog_json :
  opcodes:string list -> Yojson.Safe.t
val transcendental_dependency_catalog_root :
  Yojson.Safe.t -> string
