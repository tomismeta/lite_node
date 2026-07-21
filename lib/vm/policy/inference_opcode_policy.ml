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

type missing = {
  pc : int;
  opcode : string;
  capability : string;
}

let has_capability name requirement =
  List.exists
    (fun capability -> String.equal capability.Execution_requirement.name name)
    requirement.Execution_requirement.capabilities

let required_capability = function
  | Contract_vm.FLOAD _ -> Some "storage.authenticated-range"
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
  | Contract_vm.MATMUL_Q16 _ -> Some "tensor.fixed"
  | op when Opcode_policy.uses_host_float op -> Some "tensor.strict-fp"
  | _ -> None

let first_missing ~requirement code =
  let missing = ref None in
  Array.iteri
    (fun pc op ->
      match !missing, required_capability op with
      | None, Some capability
        when not (has_capability capability requirement) ->
        missing := Some {
          pc;
          opcode = Opcode_policy.opcode_name op;
          capability;
        }
      | _ -> ())
    code;
  !missing

let error_message missing =
  Printf.sprintf
    "inference opcode %s at pc %d requires capability %s"
    missing.opcode
    missing.pc
    missing.capability
