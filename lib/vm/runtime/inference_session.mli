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


type phase =
  | Open
  | Advanced
  | Finalized
  | Canceled

type t = {
  target_root : string;
  request_root : string;
  sequence : int;
  phase : phase;
  output_root : string;
  committed_effort : int;
}

type error =
  | Bad_sequence of int * int
  | Terminal_session
  | Target_root_mismatch of string * string
  | Request_root_mismatch of string * string
  | Pin_root_mismatch of string * string
  | Entrypoint_unsupported of string
  | Entrypoint_missing of int
  | Execution_failed
  | Effort_exceeded of int * int
  | Effort_overflow of int * int

val root : t -> string

val open_session :
  target:Inference_target.t ->
  request:Inference_request.t ->
  pins:Inference_store.pin_set ->
  (t, error) result

val advance :
  admitted:Admission.t ->
  target:Inference_target.t ->
  request:Inference_request.t ->
  expected_sequence:int ->
  t ->
  (t * Inference_receipt.t, error) result

val finalize :
  expected_sequence:int ->
  t ->
  (t * Inference_receipt.t, error) result

val cancel : expected_sequence:int -> t -> (t, error) result
val phase_name : phase -> string
val error_message : error -> string
