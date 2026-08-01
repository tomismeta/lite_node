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


type execution_profile = {
  phase : string;
  microseconds : int;
}

type result = {
  effort_used : int;
  output_root : string;
  candidate_root : string;
}

type profile_config = {
  clock : unit -> float;
  opcode_name : Contract_vm.instr -> string;
}

type profile = {
  execution_profile : execution_profile list;
  opcode_profile : Contract_vm.opcode_profile list;
}

type profiled_result = {
  result : result;
  profile : profile;
}

type error =
  | Entrypoint_unsupported of string
  | Entrypoint_missing of int
  | Session_context_mismatch of string
  | Opaque_value
  | Invalid_output of string
  | Missing_output_cell of int
  | Output_limit_exceeded of int * int
  | Scratch_limit_exceeded of int * int
  | Execution_failed

val run :
  ?session_context:Inference_session_abi.continuation_context ->
  plan:Inference_plan.t ->
  unit ->
  (result, error) Stdlib.result

val run_profiled :
  ?session_context:Inference_session_abi.continuation_context ->
  profile:profile_config ->
  plan:Inference_plan.t ->
  unit ->
  (profiled_result, error) Stdlib.result

val error_message : error -> string
