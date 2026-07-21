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


type entrypoint = {
  entry_name : string;
  entry_label : int;
}

type t = {
  program_root : string;
  requirement_root : string;
  model_root : string;
  execution_descriptor_root : string;
  store_root : string;
  session_abi_root : string;
  entrypoints : entrypoint list;
}

type error =
  | Bad_root of string
  | Bad_name of string
  | Bad_entrypoint of string * int
  | Duplicate_entrypoint of string
  | Program_root_mismatch of string * string
  | Missing_requirement
  | Requirement_root_mismatch of string * string

val program_root : Admission.t -> string
val root : t -> string
val entry_label : t -> string -> int option
val check : admitted:Admission.t -> t -> (unit, error) result
val error_message : error -> string
