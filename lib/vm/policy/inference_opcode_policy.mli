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

type detail = {
  pc : int;
  opcode : string;
}

type violation =
  | Missing_capability of {
      detail : detail;
      capability : string;
    }
  | Forbidden_opcode of detail

val first_violation :
  requirement:Execution_requirement.t ->
  Contract_vm.instr array ->
  violation option

val violations :
  requirement:Execution_requirement.t ->
  Contract_vm.instr array ->
  violation list

val error_message : violation -> string
