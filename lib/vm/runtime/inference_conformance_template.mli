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

type memory_access =
  | Read
  | Write
  | Read_write

type register_binding = {
  register : int;
  name : string;
  kind : string;
  value : string;
}

type memory_binding = {
  name : string;
  path : string option;
  root : string;
  byte_length : int option;
  sha256 : string option;
  encoding : string;
  base : int;
  cells : int;
  access : memory_access;
}

type expected_span = {
  span_name : string;
  base : int;
  cells : int;
  output_root : string option;
  output_sha256 : string option;
}

type failure_case = {
  case_name : string;
  expected : string;
  mutations : string list;
  unchanged_spans : string list;
}

type t = {
  opcode : string;
  primitive : string;
  profile : string;
  vm_semantics_root : string;
  numerical_profile_root : string;
  expected_effort : int;
  effects : string list;
  registers : register_binding list;
  memory : memory_binding list;
  expected : expected_span list;
  failure_cases : failure_case list;
}

type error =
  | Json_error of string
  | Template_error of string

val of_json : Yojson.Safe.t -> (t, error) result
val to_json : t -> Yojson.Safe.t
val error_message : error -> string
val p0_opcodes : string list
