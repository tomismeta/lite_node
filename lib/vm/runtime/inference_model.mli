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


type range = {
  owner_root : string;
  offset : int;
  length : int;
  encoding : string;
  shape_root : string option;
}

type t = {
  model_root : string;
  store_root : string;
  ranges : range list;
}

type error =
  | Bad_root of string
  | Bad_name of string
  | Bad_range of string * int
  | Range_overflow of int * int
  | Empty_ranges
  | Duplicate_range of string
  | Model_root_mismatch of string * string
  | Store_root_mismatch of string * string

val range_root : range -> string
val root : t -> string
val check : target:Inference_target.t -> t -> (unit, error) result
val error_message : error -> string
