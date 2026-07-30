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
val current_runtime_profile : opcode:string -> string option
val validate_for_opcode : opcode:string -> profile:string -> (t, error) result
val to_json : t -> Yojson.Safe.t
val root_for_opcode : opcode:string -> t -> string
val to_json_for_opcode : opcode:string -> t -> Yojson.Safe.t
val status_counts_of_json_gates : Yojson.Safe.t list -> status_counts
val classified_gate_count : status_counts -> int
val status_counts_are_consensus_ready : status_counts -> bool
val consensus_ready :
  profile_gate_count:int -> unprofiled_count:int -> status_counts -> bool
val consensus_ready_blockers :
  profile_gate_count:int -> unprofiled_count:int -> status_counts -> string list
val status_counts_json : status_counts -> Yojson.Safe.t
