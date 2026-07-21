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


type t = {
  target_root : string;
  request_root : string;
  prior_session_root : string;
  next_session_root : string;
  prior_sequence : int;
  next_sequence : int;
  output_root : string;
  candidate_root : string;
  effort_delta : int;
  completion_status : string;
  consensus_accepted : bool;
}

val root : t -> string
val to_json : t -> Yojson.Safe.t
