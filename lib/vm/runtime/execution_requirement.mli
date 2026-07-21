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


type capability = {
  name : string;
  root : string;
}

type limits = {
  max_model_bytes : int;
  max_view_bytes : int;
  max_session_bytes : int;
  max_scratch_bytes : int;
  max_output_bytes : int;
  max_advance_effort : int;
}

type t = {
  vm_semantics_root : string;
  numerical_root : string;
  effort_root : string;
  capabilities : capability list;
  limits : limits;
}

type support = {
  support_vm_semantics_root : string;
  support_numerical_roots : string list;
  support_effort_roots : string list;
  support_capabilities : capability list;
  support_limits : limits;
}

type error =
  | Bad_root of string
  | Bad_name of string
  | Bad_limit of string * int
  | Duplicate_capability of string
  | Vm_semantics_mismatch
  | Numerical_root_unsupported of string
  | Effort_root_unsupported of string
  | Capability_unsupported of capability
  | Limit_exceeded of string * int * int

val root : t -> string
val check : support -> t -> (unit, error) result
val error_message : error -> string
