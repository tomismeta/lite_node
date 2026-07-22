(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Contract_vm

type admission_class =
  | Legacy_and_program
  | Program_only
  | Consensus_unsafe

type info = {
  name : string;
  admission_class : admission_class;
}

type host_float_hit = {
  pc : int;
  opcode : string;
}

let opcode_name = function
  | VM.ADD _ -> "ADD"
  | VM.SUB _ -> "SUB"
  | VM.MUL _ -> "MUL"
  | VM.DIV _ -> "DIV"
  | VM.MOD _ -> "MOD"
  | VM.NEG _ -> "NEG"
  | VM.ABS _ -> "ABS"
  | VM.EQ _ -> "EQ"
  | VM.LT _ -> "LT"
  | VM.GT _ -> "GT"
  | VM.NEQ _ -> "NEQ"
  | VM.LDI _ -> "LDI"
  | VM.MOV _ -> "MOV"
  | VM.SLOAD _ -> "SLOAD"
  | VM.SSTORE _ -> "SSTORE"
  | VM.SDEL _ -> "SDEL"
  | VM.SLOADK _ -> "SLOADK"
  | VM.SSTOREK _ -> "SSTOREK"
  | VM.SDELK _ -> "SDELK"
  | VM.MLOAD _ -> "MLOAD"
  | VM.MSTORE _ -> "MSTORE"
  | VM.JMP _ -> "JMP"
  | VM.JIF _ -> "JIF"
  | VM.JDEST _ -> "JDEST"
  | VM.STOP -> "STOP"
  | VM.REVERT -> "REVERT"
  | VM.CALLER _ -> "CALLER"
  | VM.ORIGIN _ -> "ORIGIN"
  | VM.SELF _ -> "SELF"
  | VM.EPOCH _ -> "EPOCH"
  | VM.EPOCH_TIME _ -> "EPOCH_TIME"
  | VM.VALUE _ -> "VALUE"
  | VM.BALANCE _ -> "BALANCE"
  | VM.TREEHASH _ -> "TREEHASH"
  | VM.NODEID _ -> "NODEID"
  | VM.TXHASH _ -> "TXHASH"
  | VM.XCALL _ -> "XCALL"
  | VM.SPAWN _ -> "SPAWN"
  | VM.SPAWN2 _ -> "SPAWN2"
  | VM.TRANSFER _ -> "TRANSFER"
  | VM.CHECKPOINT -> "CHECKPOINT"
  | VM.ROLLBACK -> "ROLLBACK"
  | VM.COMMIT -> "COMMIT"
  | VM.EMIT _ -> "EMIT"
  | VM.CONCAT _ -> "CONCAT"
  | VM.STRLEN _ -> "STRLEN"
  | VM.ASSERT _ -> "ASSERT"
  | VM.EFFORT _ -> "EFFORT"
  | VM.NOP -> "NOP"
  | VM.FHE_LOAD_PK _ -> "FHE_LOAD_PK"
  | VM.FHE_ADD _ -> "FHE_ADD"
  | VM.FHE_SUB _ -> "FHE_SUB"
  | VM.FHE_MUL _ -> "FHE_MUL"
  | VM.FHE_SCALE _ -> "FHE_SCALE"
  | VM.FHE_DIV_CONST _ -> "FHE_DIV_CONST"
  | VM.FHE_ADD_CONST _ -> "FHE_ADD_CONST"
  | VM.FHE_SUB_CONST _ -> "FHE_SUB_CONST"
  | VM.FHE_VERIFY_ZERO _ -> "FHE_VERIFY_ZERO"
  | VM.FHE_VERIFY_RANGE _ -> "FHE_VERIFY_RANGE"
  | VM.FHE_VERIFY_BOUND _ -> "FHE_VERIFY_BOUND"
  | VM.GROTH16_VERIFY_BN254 _ -> "GROTH16_VERIFY_BN254"
  | VM.FHE_COMMIT _ -> "FHE_COMMIT"
  | VM.FHE_PEDERSEN _ -> "FHE_PEDERSEN"
  | VM.FHE_SER _ -> "FHE_SER"
  | VM.FHE_DESER _ -> "FHE_DESER"
  | VM.FHE_SER_PK _ -> "FHE_SER_PK"
  | VM.FHE_DESER_PK _ -> "FHE_DESER_PK"
  | VM.CALL_INT _ -> "CALL_INT"
  | VM.MLOADR _ -> "MLOADR"
  | VM.MSTORER _ -> "MSTORER"
  | VM.PARSE_INTS _ -> "PARSE_INTS"
  | VM.ISADDR _ -> "ISADDR"
  | VM.ISHEX _ -> "ISHEX"
  | VM.STATE_PATH_KEY _ -> "STATE_PATH_KEY"
  | VM.OBJECT_MEMBER_COUNT _ -> "OBJECT_MEMBER_COUNT"
  | VM.OBJECT_HAS_MEMBER _ -> "OBJECT_HAS_MEMBER"
  | VM.OBJECT_MEMBER_REF_AT _ -> "OBJECT_MEMBER_REF_AT"
  | VM.OBJECT_TRANSITION_APPLY _ -> "OBJECT_TRANSITION_APPLY"
  | VM.ASSERT_ADDR _ -> "ASSERT_ADDR"
  | VM.SUBSTR _ -> "SUBSTR"
  | VM.INDEXOF _ -> "INDEXOF"
  | VM.SHA256 _ -> "SHA256"
  | VM.KECCAK256 _ -> "KECCAK256"
  | VM.ED25519_OK _ -> "ED25519_OK"
  | VM.BITAND _ -> "BITAND"
  | VM.BITOR _ -> "BITOR"
  | VM.BITXOR _ -> "BITXOR"
  | VM.BITSHL _ -> "BITSHL"
  | VM.BITSHR _ -> "BITSHR"
  | VM.SKEYS _ -> "SKEYS"
  | VM.SKEYS_PAGE _ -> "SKEYS_PAGE"
  | VM.SLOADN _ -> "SLOADN"
  | VM.SSTOREN _ -> "SSTOREN"
  | VM.FSTORE _ -> "FSTORE"
  | VM.FLOAD _ -> "FLOAD"
  | VM.MATMUL _ -> "MATMUL"
  | VM.VECDOT _ -> "VECDOT"
  | VM.VECDOT_Q16 _ -> "VECDOT_Q16"
  | VM.EXP_LUT _ -> "EXP_LUT"
  | VM.EXP_Q16 _ -> "EXP_Q16"
  | VM.SOFTMAX_INPLACE _ -> "SOFTMAX_INPLACE"
  | VM.SOFTMAX_Q16_INPLACE _ -> "SOFTMAX_Q16_INPLACE"
  | VM.LAYERNORM_INPLACE _ -> "LAYERNORM_INPLACE"
  | VM.LAYERNORM_Q16_INPLACE _ -> "LAYERNORM_Q16_INPLACE"
  | VM.RELU_INPLACE _ -> "RELU_INPLACE"
  | VM.RMSNORM_INPLACE _ -> "RMSNORM_INPLACE"
  | VM.RMSNORM_Q16_INPLACE _ -> "RMSNORM_Q16_INPLACE"
  | VM.SILU_INPLACE _ -> "SILU_INPLACE"
  | VM.SILU_Q16_INPLACE _ -> "SILU_Q16_INPLACE"
  | VM.ELEMWISE_MUL_INPLACE _ -> "ELEMWISE_MUL_INPLACE"
  | VM.ELEMWISE_MUL_Q16 _ -> "ELEMWISE_MUL_Q16"
  | VM.LOAD_INT8_BYTES_TO_MEM _ -> "LOAD_INT8_BYTES_TO_MEM"
  | VM.RESIDUAL_ADD _ -> "RESIDUAL_ADD"
  | VM.RESIDUAL_ADD_Q16 _ -> "RESIDUAL_ADD_Q16"
  | VM.ROPE_APPLY _ -> "ROPE_APPLY"
  | VM.ROPE_APPLY_Q16 _ -> "ROPE_APPLY_Q16"
  | VM.LOAD_INT8_B64_TO_MEM _ -> "LOAD_INT8_B64_TO_MEM"
  | VM.LOAD_INT8_Q16 _ -> "LOAD_INT8_Q16"
  | VM.APPEND_VEC_Q16 _ -> "APPEND_VEC_Q16"
  | VM.ARGMAX_Q16 _ -> "ARGMAX_Q16"
  | VM.MATMUL_Q16 _ -> "MATMUL_Q16"
  | VM.LINEAR_Q1_G128_FP _ -> "LINEAR_Q1_G128_FP"
  | VM.LOAD_F32_LE_FP _ -> "LOAD_F32_LE_FP"
  | VM.SIGMOID_FP _ -> "SIGMOID_FP"
  | VM.SOFTPLUS_FP _ -> "SOFTPLUS_FP"
  | VM.CAUSAL_DEPTHWISE_CONV1D_FP _ -> "CAUSAL_DEPTHWISE_CONV1D_FP"
  | VM.SHIFT_ROUND_INPLACE _ -> "SHIFT_ROUND_INPLACE"
  | VM.MATMUL_FP _ -> "MATMUL_FP"
  | VM.RMSNORM_FP _ -> "RMSNORM_FP"
  | VM.SILU_FP _ -> "SILU_FP"
  | VM.ELEMWISE_MUL_FP _ -> "ELEMWISE_MUL_FP"
  | VM.RESIDUAL_ADD_FP _ -> "RESIDUAL_ADD_FP"
  | VM.ROPE_APPLY_FP _ -> "ROPE_APPLY_FP"
  | VM.LOAD_INT8_FP _ -> "LOAD_INT8_FP"
  | VM.VECDOT_FP _ -> "VECDOT_FP"
  | VM.ARGMAX_FP _ -> "ARGMAX_FP"
  | VM.ATTENTION_KV_FP _ -> "ATTENTION_KV_FP"
  | VM.ATTENTION_KV_Q16 _ -> "ATTENTION_KV_Q16"
  | VM.APPEND_VEC_FP _ -> "APPEND_VEC_FP"

let admission_class = function
  | VM.EXP_LUT _
  | VM.SOFTMAX_INPLACE _
  | VM.LAYERNORM_INPLACE _
  | VM.RMSNORM_INPLACE _
  | VM.SILU_INPLACE _
  | VM.ROPE_APPLY _
  | VM.MATMUL_FP _
  | VM.RMSNORM_FP _
  | VM.SILU_FP _
  | VM.ELEMWISE_MUL_FP _
  | VM.RESIDUAL_ADD_FP _
  | VM.ROPE_APPLY_FP _
  | VM.LOAD_INT8_FP _
  | VM.VECDOT_FP _
  | VM.ARGMAX_FP _
  | VM.ATTENTION_KV_FP _
  | VM.APPEND_VEC_FP _
  | VM.LINEAR_Q1_G128_FP _
  | VM.LOAD_F32_LE_FP _
  | VM.SIGMOID_FP _
  | VM.SOFTPLUS_FP _
  | VM.CAUSAL_DEPTHWISE_CONV1D_FP _ -> Consensus_unsafe
  | VM.EXP_Q16 _
  | VM.SOFTMAX_Q16_INPLACE _
  | VM.LAYERNORM_Q16_INPLACE _
  | VM.RMSNORM_Q16_INPLACE _
  | VM.SILU_Q16_INPLACE _
  | VM.ROPE_APPLY_Q16 _
  | VM.ATTENTION_KV_Q16 _
  | VM.VECDOT_Q16 _
  | VM.ELEMWISE_MUL_Q16 _
  | VM.RESIDUAL_ADD_Q16 _
  | VM.LOAD_INT8_Q16 _
  | VM.APPEND_VEC_Q16 _
  | VM.ARGMAX_Q16 _ -> Program_only
  | VM.ADD _
  | VM.SUB _
  | VM.MUL _
  | VM.DIV _
  | VM.MOD _
  | VM.NEG _
  | VM.ABS _
  | VM.EQ _
  | VM.LT _
  | VM.GT _
  | VM.NEQ _
  | VM.LDI _
  | VM.MOV _
  | VM.SLOAD _
  | VM.SSTORE _
  | VM.SDEL _
  | VM.SLOADK _
  | VM.SSTOREK _
  | VM.SDELK _
  | VM.MLOAD _
  | VM.MSTORE _
  | VM.JMP _
  | VM.JIF _
  | VM.JDEST _
  | VM.STOP
  | VM.REVERT
  | VM.CALLER _
  | VM.ORIGIN _
  | VM.SELF _
  | VM.EPOCH _
  | VM.EPOCH_TIME _
  | VM.VALUE _
  | VM.BALANCE _
  | VM.TREEHASH _
  | VM.NODEID _
  | VM.TXHASH _
  | VM.XCALL _
  | VM.SPAWN _
  | VM.SPAWN2 _
  | VM.TRANSFER _
  | VM.CHECKPOINT
  | VM.ROLLBACK
  | VM.COMMIT
  | VM.EMIT _
  | VM.CONCAT _
  | VM.STRLEN _
  | VM.ASSERT _
  | VM.EFFORT _
  | VM.NOP
  | VM.FHE_LOAD_PK _
  | VM.FHE_ADD _
  | VM.FHE_SUB _
  | VM.FHE_MUL _
  | VM.FHE_SCALE _
  | VM.FHE_DIV_CONST _
  | VM.FHE_ADD_CONST _
  | VM.FHE_SUB_CONST _
  | VM.FHE_VERIFY_ZERO _
  | VM.FHE_VERIFY_RANGE _
  | VM.FHE_VERIFY_BOUND _
  | VM.GROTH16_VERIFY_BN254 _
  | VM.FHE_COMMIT _
  | VM.FHE_PEDERSEN _
  | VM.FHE_SER _
  | VM.FHE_DESER _
  | VM.FHE_SER_PK _
  | VM.FHE_DESER_PK _
  | VM.CALL_INT _
  | VM.MLOADR _
  | VM.MSTORER _
  | VM.PARSE_INTS _
  | VM.ISADDR _
  | VM.ISHEX _
  | VM.STATE_PATH_KEY _
  | VM.OBJECT_MEMBER_COUNT _
  | VM.OBJECT_HAS_MEMBER _
  | VM.OBJECT_MEMBER_REF_AT _
  | VM.OBJECT_TRANSITION_APPLY _
  | VM.ASSERT_ADDR _
  | VM.SUBSTR _
  | VM.INDEXOF _
  | VM.SHA256 _
  | VM.KECCAK256 _
  | VM.ED25519_OK _
  | VM.BITAND _
  | VM.BITOR _
  | VM.BITXOR _
  | VM.BITSHL _
  | VM.BITSHR _
  | VM.SKEYS _
  | VM.SKEYS_PAGE _
  | VM.SLOADN _
  | VM.SSTOREN _
  | VM.FSTORE _
  | VM.FLOAD _
  | VM.MATMUL _
  | VM.VECDOT _
  | VM.RELU_INPLACE _
  | VM.ELEMWISE_MUL_INPLACE _
  | VM.LOAD_INT8_BYTES_TO_MEM _
  | VM.RESIDUAL_ADD _
  | VM.LOAD_INT8_B64_TO_MEM _
  | VM.MATMUL_Q16 _
  | VM.SHIFT_ROUND_INPLACE _ -> Legacy_and_program

let describe op = {
  name = opcode_name op;
  admission_class = admission_class op;
}

let host_float_opcode op =
  match describe op with
  | { name; admission_class = Consensus_unsafe } -> Some name
  | _ -> None

let program_only_opcode op =
  match describe op with
  | { name; admission_class = Program_only } -> Some name
  | _ -> None

let uses_host_float op =
  Option.is_some (host_float_opcode op)

let first_host_float code =
  let hit = ref None in
  Array.iteri
    (fun pc op ->
      match !hit, host_float_opcode op with
      | None, Some opcode -> hit := Some { pc; opcode }
      | _ -> ())
    code;
  !hit

let consensus_safe code =
  Option.is_none (first_host_float code)

let require_consensus_safe code =
  match first_host_float code with
  | None -> Ok ()
  | Some hit -> Error hit

let first_program_only code =
  let hit = ref None in
  Array.iteri
    (fun pc op ->
      match !hit, program_only_opcode op with
      | None, Some opcode -> hit := Some { pc; opcode }
      | _ -> ())
    code;
  !hit

let require_legacy_safe code =
  match first_program_only code with
  | Some hit -> Error hit
  | None -> require_consensus_safe code

type legacy_error =
  | Program_only of host_float_hit
  | Consensus_unsafe of host_float_hit

let legacy_error code =
  match first_program_only code with
  | Some hit -> Some (Program_only hit)
  | None -> Option.map (fun hit -> Consensus_unsafe hit) (first_host_float code)

let require_program_safe = require_consensus_safe

let error_message hit =
  Printf.sprintf "consensus unsafe opcode %s at pc %d" hit.opcode hit.pc

let program_only_error_message hit =
  Printf.sprintf "Program-only opcode %s at pc %d" hit.opcode hit.pc
