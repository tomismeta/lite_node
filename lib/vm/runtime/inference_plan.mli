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


type t

type error =
  | Target_invalid of string
  | Request_invalid of string
  | Model_invalid of string
  | Pin_model_root_mismatch of string * string
  | Pin_store_root_mismatch of string * string
  | Pin_ranges_root_mismatch of string * string
  | Input_root_mismatch of string * string
  | Input_limit_exceeded of int * int
  | Missing_requirement

val create :
  admitted:Admission.t ->
  target:Inference_target.t ->
  request:Inference_request.t ->
  model:Inference_model.t ->
  pins:Inference_store.pin_set ->
  input:string ->
  (t, error) result

val admitted : t -> Admission.t
val target : t -> Inference_target.t
val request : t -> Inference_request.t
val model : t -> Inference_model.t
val pins : t -> Inference_store.pin_set
val input : t -> string
val model_ranges_root : t -> string
val error_message : error -> string
