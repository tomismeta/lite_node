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


type result = {
  effort_used : int;
  output_root : string;
  candidate_root : string;
}

type error =
  | Entrypoint_unsupported of string
  | Entrypoint_missing of int
  | Opaque_value
  | Invalid_output of string
  | Missing_output_cell of int
  | Output_limit_exceeded of int * int
  | Execution_failed

val run :
  plan:Inference_plan.t ->
  unit ->
  (result, error) Stdlib.result

val error_message : error -> string
