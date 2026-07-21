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
  committed_effort : int;
  completion_status : string;
  consensus_accepted : bool;
}

let to_json receipt =
  `Assoc [
    "target_root", `String receipt.target_root;
    "request_root", `String receipt.request_root;
    "prior_session_root", `String receipt.prior_session_root;
    "next_session_root", `String receipt.next_session_root;
    "prior_sequence", `Int receipt.prior_sequence;
    "next_sequence", `Int receipt.next_sequence;
    "output_root", `String receipt.output_root;
    "candidate_root", `String receipt.candidate_root;
    "committed_effort", `Int receipt.committed_effort;
    "completion_status", `String receipt.completion_status;
    "consensus_accepted", `Bool receipt.consensus_accepted;
  ]

let root receipt =
  let payload = Yojson.Safe.to_string (to_json receipt) in
  Digestif.SHA256.(
    digest_string ("octra:inference:receipt\000" ^ payload) |> to_hex)
