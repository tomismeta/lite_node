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

type t

type error =
  | Bad_sequence of int * int
  | Terminal_session
  | Invalid_phase of string
  | Target_root_mismatch of string * string
  | Request_root_mismatch of string * string
  | Model_ranges_root_mismatch of string * string
  | Entrypoint_unsupported of string
  | Entrypoint_missing of int
  | Execution_error of string
  | Execution_failed
  | Effort_exceeded of int * int
  | Effort_overflow of int * int

val root : t -> string

val open_session :
  plan:Inference_plan.t ->
  (t, error) result

val advance :
  plan:Inference_plan.t ->
  expected_sequence:int ->
  t ->
  (t * Inference_receipt.t, error) result

val finalize :
  expected_sequence:int ->
  t ->
  (t * Inference_receipt.t, error) result

val cancel : expected_sequence:int -> t -> (t, error) result
val sequence : t -> int
val phase : t -> phase
val output_root : t -> string
val candidate_root : t -> string
val committed_effort : t -> int
val phase_name : phase -> string
val error_message : error -> string
