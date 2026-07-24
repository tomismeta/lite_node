(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effect =
  | Memory_read
  | Memory_write
  | Storage_read
  | Storage_write
  | Call
  | Deploy
  | Transfer
  | Emit
  | Fhe
  | Journal

type t = effect list

let of_instr = function
  | Contract_vm.MLOAD _ | Contract_vm.MLOADR _ -> [Memory_read]
  | Contract_vm.MSTORE _ | Contract_vm.MSTORER _ -> [Memory_write]
  | Contract_vm.LINEAR_Q1_G128_FP _ -> [Memory_read; Memory_write]
  | Contract_vm.LOAD_F32_LE_FP _ -> [Memory_write]
  | Contract_vm.SIGMOID_FP _
  | Contract_vm.SOFTPLUS_FP _
  | Contract_vm.SILU_FP _
  | Contract_vm.RMSNORM_FP_EPS _
  | Contract_vm.L2NORM_FP _
  | Contract_vm.ELEMWISE_MUL_FP _
  | Contract_vm.RESIDUAL_ADD_FP _
  | Contract_vm.CAUSAL_DEPTHWISE_CONV1D_FP _
  | Contract_vm.GATED_DELTA_RULE_FP _ -> [Memory_read; Memory_write]
  | Contract_vm.ARGMAX_FP _ -> [Memory_read]
  | Contract_vm.SLOAD _
  | Contract_vm.SLOADK _
  | Contract_vm.SKEYS _
  | Contract_vm.SKEYS_PAGE _
  | Contract_vm.SLOADN _
  | Contract_vm.FLOAD _
  | Contract_vm.OBJECT_MEMBER_COUNT _
  | Contract_vm.OBJECT_HAS_MEMBER _
  | Contract_vm.OBJECT_MEMBER_REF_AT _ -> [Storage_read]
  | Contract_vm.SSTORE _
  | Contract_vm.SSTOREK _
  | Contract_vm.SDEL _
  | Contract_vm.SDELK _
  | Contract_vm.SSTOREN _
  | Contract_vm.FSTORE _
  | Contract_vm.OBJECT_TRANSITION_APPLY _ -> [Storage_write]
  | Contract_vm.XCALL _ | Contract_vm.CALL_INT _ -> [Call]
  | Contract_vm.SPAWN _ | Contract_vm.SPAWN2 _ -> [Deploy]
  | Contract_vm.TRANSFER _ -> [Transfer]
  | Contract_vm.EMIT _ -> [Emit]
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
  | Contract_vm.GROTH16_VERIFY_BN254 _
  | Contract_vm.FHE_COMMIT _
  | Contract_vm.FHE_PEDERSEN _
  | Contract_vm.FHE_SER _
  | Contract_vm.FHE_DESER _
  | Contract_vm.FHE_SER_PK _
  | Contract_vm.FHE_DESER_PK _ -> [Fhe]
  | Contract_vm.CHECKPOINT
  | Contract_vm.ROLLBACK
  | Contract_vm.COMMIT -> [Journal]
  | _ -> []

let add effect effects =
  if List.mem effect effects then effects else effects @ [effect]

let scan code =
  Array.fold_left
    (fun effects instr ->
      List.fold_left
        (fun effects effect -> add effect effects)
        effects
        (of_instr instr))
    [] code

let names effects =
  List.map (function
    | Memory_read -> "memory_read"
    | Memory_write -> "memory_write"
    | Storage_read -> "storage_read"
    | Storage_write -> "storage_write"
    | Call -> "call"
    | Deploy -> "deploy"
    | Transfer -> "transfer"
    | Emit -> "emit"
    | Fhe -> "fhe"
    | Journal -> "journal") effects
