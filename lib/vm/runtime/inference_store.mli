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


type pinned_range = {
  range_root : string;
  owner_root : string;
  offset : int;
  length : int;
  encoding : string;
  shape_root : string option;
  bytes : string;
}

type pin_set = {
  model_root : string;
  store_root : string;
  model_ranges_root : string;
  ranges : pinned_range list;
}

type error =
  | Missing_owner of string
  | Owner_root_mismatch of string * string
  | Range_out_of_bounds of string * int * int * int
  | Range_overflow of int * int
  | Model_limit_exceeded of int * int

val pin :
  limits:Execution_requirement.limits ->
  read:(string -> string option) ->
  Inference_model.t ->
  (pin_set, error) result

val error_message : error -> string
