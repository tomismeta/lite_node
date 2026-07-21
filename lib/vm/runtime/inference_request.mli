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
  schema : int;
  target_root : string;
  entrypoint : string;
  input_root : string;
  request_nonce : string;
  max_output_bytes : int;
  max_advance_effort : int;
}

type error =
  | Bad_schema of int
  | Bad_root of string
  | Bad_name of string
  | Bad_limit of string * int
  | Target_root_mismatch of string * string
  | Requirement_root_mismatch of string * string
  | Entrypoint_unsupported of string
  | Limit_exceeded of string * int * int

val root : t -> string
val check :
  target:Inference_target.t ->
  requirement:Execution_requirement.t ->
  t ->
  (unit, error) result
val error_message : error -> string
