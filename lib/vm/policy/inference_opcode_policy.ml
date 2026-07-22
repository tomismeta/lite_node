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

type opcode_class =
  | Allowed
  | Requires of string
  | Forbidden

let has_capability name requirement =
  List.exists
    (fun capability -> String.equal capability.Execution_requirement.name name)
    requirement.Execution_requirement.capabilities

let opcode_class = function
  | Contract_vm.FHE_LOAD_PK _
  | Contract_vm.FHE_ADD _
  | Contract_vm.FHE_SUB _
  | Contract_vm.FHE_MUL _
  | Contract_vm.FHE_SCALE _
  | Contract_vm.FHE_DIV_CONST _
  | Contract_vm.FHE_ADD_CONST _
  | Contract_vm.FHE_SUB_CONST _
  | Contract_vm.FHE_VERIFY_ZERO _
  | Contract_vm.FHE_VERIFY_RANGE _
  | Contract_vm.FHE_VERIFY_BOUND _
  | Contract_vm.FHE_COMMIT _
  | Contract_vm.FHE_PEDERSEN _
  | Contract_vm.FHE_SER _
  | Contract_vm.FHE_DESER _
  | Contract_vm.FHE_SER_PK _
  | Contract_vm.FHE_DESER_PK _ -> Forbidden
  | Contract_vm.FLOAD _ -> Requires "storage.authenticated-range"
  | Contract_vm.EXP_Q16 _
  | Contract_vm.SOFTMAX_Q16_INPLACE _
  | Contract_vm.LAYERNORM_Q16_INPLACE _
  | Contract_vm.RMSNORM_Q16_INPLACE _
  | Contract_vm.SILU_Q16_INPLACE _
  | Contract_vm.ROPE_APPLY_Q16 _
  | Contract_vm.ATTENTION_KV_Q16 _
  | Contract_vm.VECDOT_Q16 _
  | Contract_vm.ELEMWISE_MUL_Q16 _
  | Contract_vm.RESIDUAL_ADD_Q16 _
  | Contract_vm.LOAD_INT8_Q16 _
  | Contract_vm.APPEND_VEC_Q16 _
  | Contract_vm.ARGMAX_Q16 _
  | Contract_vm.MATMUL_Q16 _ -> Requires "tensor.fixed"
  | op when Opcode_policy.uses_host_float op -> Requires "tensor.strict-fp"
  | _ -> Allowed

let first_violation ~requirement code =
  let violation = ref None in
  Array.iteri
    (fun pc op ->
      match !violation, opcode_class op with
      | None, Forbidden ->
        violation := Some (Forbidden_opcode {
          pc;
          opcode = Opcode_policy.opcode_name op;
        })
      | None, Requires capability
        when not (has_capability capability requirement) ->
        violation := Some (Missing_capability {
          detail = {
            pc;
            opcode = Opcode_policy.opcode_name op;
          };
          capability;
        })
      | _ -> ())
    code;
  !violation

let error_message = function
  | Missing_capability { detail; capability } ->
    Printf.sprintf
      "inference opcode %s at pc %d requires capability %s"
      detail.opcode
      detail.pc
      capability
  | Forbidden_opcode detail ->
    Printf.sprintf
      "inference opcode %s at pc %d is forbidden in plain inference"
      detail.opcode
      detail.pc
