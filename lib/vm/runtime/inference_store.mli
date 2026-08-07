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

type pin_set

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

val pin_streamed :
  limits:Execution_requirement.limits ->
  read_span:(Inference_model.range -> (string * string) option) ->
  Inference_model.t ->
  (pin_set, error) result

val model_root : pin_set -> string
val store_root : pin_set -> string
val model_ranges_root : pin_set -> string
val ranges : pin_set -> pinned_range list

val error_message : error -> string
