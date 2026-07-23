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
  model_root : string;
  store_root : string;
  tensor_index_root : string;
  tokenizer_root : string option;
  numerical_profile_root : string;
  capability_set_root : string;
  default_program_root : string option;
}

type error =
  | Bad_root of string
  | Model_root_mismatch of string * string
  | Store_root_mismatch of string * string
  | Numerical_root_mismatch of string * string
  | Capability_set_root_mismatch of string * string

val capability_set_root : Execution_requirement.capability list -> string
val root : t -> string
val check :
  target:Inference_target.t ->
  requirement:Execution_requirement.t ->
  t ->
  (unit, error) result
val error_message : error -> string
