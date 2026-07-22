# VM Semantic Inventory

Status: observational audit. No runtime behavior is changed by this document.

Baseline: `octra-labs/lite_node` `main` at
`2a5803bf62b33d542a5d7adab64a1133e9c0340c`.

## Purpose

This document records the current VM shape before inference runtime work begins.
It is an input to the Phase 1 build boundary and the Phase 2 execution-profile
foundation. Git identifies the audited source; this document does not introduce
a separate VM version.

The inventory is deliberately descriptive. It does not redefine existing
opcodes, repair current inconsistencies, or allocate new instructions. The
architecture and contracts documents remain authoritative for the intended
inference runtime.

The audit asks one question: which current semantic facts must remain visible
when a VM operation is admitted, executed, metered, tested, and rooted?

## Existing Character

The VM follows the same direct style as the rest of LiteNode:

- algebraic variants describe values, instructions, effects, and errors;
- execution is explicit pattern matching over instructions;
- small pure modules handle cost, policy, effects, and type flow;
- state and dependencies are records passed to functions;
- `.mli` files are used where a small boundary is already useful; and
- there is no registry, class hierarchy, dynamic plugin system, or generated
  instruction framework.

Inference work should preserve that character. A total classification can be a
plain match returning a small record. A conformance check can compare existing
pure functions. Neither requires a new abstraction system.

## Semantic Surfaces

Current opcode facts are distributed across these source surfaces:

| Surface | Current responsibility |
| --- | --- |
| `runtime/contract_vm.ml` | Values, instructions, state, execution, base effort, verifier |
| `runtime/fixed_q16.ml` | Deterministic Q16 scalar and vector helpers |
| `runtime/cost.ml` | Checked host-`int` addition and products |
| `wire/bytecode.ml` | Binary opcode allocation, encoding, and decoding |
| `wire/assembler.ml` | Text parsing and instruction rendering |
| `compiler/oct_gen.ml` | Source builtin to instruction lowering |
| `policy/opcode_policy.ml` | Host-float and Program-only rejection |
| `policy/opcode_inventory.ml` | Host-float reporting |
| `runtime/program_effects.ml` | Coarse effect scan |
| `runtime/program_type_flow.ml` | Typed Program-profile flow checks |
| `runtime/admission.ml` | Verifier, policy, effects, certificate, and type-flow composition |

No one surface is currently a complete opcode specification. Correctness is the
agreement of these matches plus the execution body.

## Values And State

### Values

`Contract_vm.v` has eleven constructors:

- signed arbitrary-precision integer, boolean, string, and bytes;
- 32-byte values and bounded unsigned 64-, 128-, and 256-bit integers;
- addresses; and
- PVAC ciphertext and public-key values.

Q16 cells and FP64 bit patterns both use `VInt`. Their numeric interpretation is
selected by the executing opcode, not represented by a distinct VM value.
`VECDOT_FP` likewise returns FP64 bits in a `VInt` register.

This representation is compatible with one semantic memory address space, but
typed range access must preserve the contextual interpretation. Generic
`MLOAD` and `MSTORE` can observe or replace the same cell.

### State domains

The current execution state separates:

| Domain | Representation | Failure or rollback behavior |
| --- | --- | --- |
| Registers | Fixed array of 64 values | Saved around internal calls |
| Memory | Sparse `int` to `v` table plus high-water size | Not part of journal rollback |
| Contract storage | String table | Writes recorded in the undo journal |
| Blobs | State-local SHA-256 to string table | Not recorded in the undo journal |
| Events | Shared event list | Managed by surrounding execution paths |
| Effort | Used and limit as host `int` | Charged before execution and within shapes |
| Context | Explicit callbacks and chain values | Supplied by the execution caller |

Generic missing-memory reads return integer zero unless strict values are
enabled. Q16 and FP helpers also treat missing cells as zero. Q16 range readers
reject a present value outside Q16 range; FP readers treat a present non-`VInt`
value as positive zero.

Memory bounds are not yet uniform:

- register-addressed generic memory limits an index to `16_777_216`;
- `valid_mem_span` limits a Q16 range length to `131_072`, but not its end to the
  generic-memory ceiling;
- several legacy operations allow a length up to `1_048_576` without a common
  address-span check; and
- FP operations use their own shape limits and generally do not use
  `valid_mem_span`.

## Instruction Families

The VM currently defines 138 instruction constructors. They form these broad
families:

| Family | Representative instructions |
| --- | --- |
| Scalar and comparison | `ADD`, `DIV`, `EQ`, `BITXOR` |
| Values and text | `LDI`, `MOV`, `CONCAT`, `SUBSTR` |
| Control flow | `JMP`, `JIF`, `CALL_INT`, `STOP`, `REVERT` |
| Memory and storage | `MLOAD`, `MSTORE`, `SLOAD`, `SSTORE`, batched storage |
| Chain context | `CALLER`, `EPOCH`, `BALANCE`, `TREEHASH` |
| Calls and effects | `XCALL`, `SPAWN`, `TRANSFER`, `EMIT` |
| Journal | `CHECKPOINT`, `ROLLBACK`, `COMMIT` |
| Cryptography | Hashing, Ed25519, Groth16, and PVAC operations |
| Object transitions | Object inspection and transition application |
| Blob data | `FSTORE`, `FLOAD` |
| Tensor compute | Legacy, Q16, and FP operation families |

Inference extends the last two families and composes the existing memory,
control-flow, cryptographic, and effort machinery. It does not require another
instruction type or execution loop.

## Current Compute Matrix

The current inference-adjacent surface contains two blob instructions and 45
compute instructions. The labels below describe present admission behavior, not
the intended future numerical profiles.

| Group | Count | Legacy admission | Typed Program admission |
| --- | ---: | --- | --- |
| State-local blobs | 2 | Allowed | Unsupported by type flow |
| Legacy integer or fixed | 7 | Allowed | Unsupported by type flow |
| Host-assisted fixed | 6 | Rejected as host float | Rejected as host float |
| Typed Q16 | 13 | Rejected as Program-only | Accepted by type flow |
| Q1-G128 proof | 1 | Rejected as host/profiled float | Accepted by type flow |
| F32 ingress proof | 1 | Rejected as profiled ingress | Accepted by type flow |
| Strict-FP activation proof | 3 | Rejected as host/profiled float | Accepted by type flow |
| Causal depthwise proof | 1 | Rejected as host/profiled float | Accepted by type flow |
| Delta-rule proof | 1 | Rejected as host/profiled float | Accepted by type flow |
| Unclassified Q16 | 2 | Allowed | Unsupported by type flow |
| FP | 10 | Rejected as host float | Rejected as host float |

The groups contain:

- State-local blobs: `FSTORE` and `FLOAD`.
- Legacy integer or fixed: `MATMUL`, `VECDOT`, `RELU_INPLACE`,
  `ELEMWISE_MUL_INPLACE`, `LOAD_INT8_BYTES_TO_MEM`, `RESIDUAL_ADD`, and
  `LOAD_INT8_B64_TO_MEM`.
- Host-assisted fixed: `EXP_LUT`, `SOFTMAX_INPLACE`, `LAYERNORM_INPLACE`,
  `RMSNORM_INPLACE`, `SILU_INPLACE`, and `ROPE_APPLY`.
- Typed Q16: `EXP_Q16`, `SOFTMAX_Q16_INPLACE`, `LAYERNORM_Q16_INPLACE`,
  `RMSNORM_Q16_INPLACE`, `SILU_Q16_INPLACE`, `ROPE_APPLY_Q16`,
  `ATTENTION_KV_Q16`, `VECDOT_Q16`, `ELEMWISE_MUL_Q16`,
  `RESIDUAL_ADD_Q16`, `LOAD_INT8_Q16`, `APPEND_VEC_Q16`, and `ARGMAX_Q16`.
- Q1-G128 proof: `LINEAR_Q1_G128_FP`.
- F32 ingress proof: `LOAD_F32_LE_FP`.
- Strict-FP activation proof: `SIGMOID_FP`, `SOFTPLUS_FP`, and `SILU_FP`.
- Causal depthwise proof: `CAUSAL_DEPTHWISE_CONV1D_FP`.
- Delta-rule proof: `GATED_DELTA_RULE_FP`.
- Unclassified Q16: `MATMUL_Q16` and `SHIFT_ROUND_INPLACE`.
- FP: `MATMUL_FP`, `RMSNORM_FP`, `ELEMWISE_MUL_FP`, `RESIDUAL_ADD_FP`,
  `ROPE_APPLY_FP`, `LOAD_INT8_FP`, `VECDOT_FP`, `ARGMAX_FP`,
  `ATTENTION_KV_FP`, and `APPEND_VEC_FP`.

"Allowed" means the current opcode policy does not reject the instruction. The
normal verifier and execution checks still apply.

The supporting surfaces have different coverage:

- bytecode encoding, decoding, and register verification cover all 47
  inference-adjacent instructions;
- `oct_gen.ml` can lower builtins to all 47 instructions;
- the assembler renderer covers all 47, while its parser covers blobs, the
  13 Typed Q16 instructions, Q1-G128 proof opcode, F32 ingress proof opcode,
  the three strict-FP activation proof opcodes, the causal depthwise proof
  opcode, and the delta-rule proof opcode;
- strict runtime operand checking covers the Typed Q16 group, the Q1-G128 proof
  opcode, the F32 ingress proof opcode, the strict-FP activation proof opcodes,
  the causal depthwise proof opcode, the delta-rule proof opcode, and selected
  data loaders, but not the complete compute surface;
- Program type flow covers the 13 Typed Q16 instructions, the Q1-G128 proof
  opcode, the F32 ingress proof opcode, the strict-FP activation proof opcodes,
  the causal depthwise proof opcode, and the delta-rule proof opcode; and
- the effect scan assigns memory read/write to the Q1-G128 proof opcode and
  strict-FP activation proof opcodes, memory read/write to the causal depthwise
  proof opcode and delta-rule proof opcode, and memory write to the F32 ingress
  proof opcode, but still assigns no memory or blob effect to the older tensor
  instructions.

## Per-Instruction Closure

An accepted semantic operation needs one factual answer in every row below.
These facts may remain implemented by small functions in their owning modules;
they do not need a central dynamic registry.

| Fact | Required answer |
| --- | --- |
| Identity | Constructor, stable text name, binary encoding, and semantic root input |
| Operands | Register roles, accepted value kinds, and validated shape fields |
| Result | Register or memory writes and their value interpretation |
| Memory | Read and write ranges, missing-cell behavior, and maximum extent |
| Aliasing | Permitted overlap and whether sources are snapshotted |
| Numerics | Exact scalar definition or bounded profile and non-finite rules |
| Effects | Memory, storage, blob, call, cryptographic, and session effects |
| Failure | Validation order, candidate writes, rollback, and error disposition |
| Effort | Checked analytic formula over admitted shape metadata |
| Admission | Required capabilities, numerical profile, and execution domain |
| Toolchain | Bytecode, assembler, compiler, verifier, and type-flow coverage |
| Conformance | Scalar vectors, malformed inputs, boundaries, aliases, and effort |

The `vm_semantics_root` must bind the protocol-visible rows. Compiler spellings,
prepared layouts, native kernels, and local diagnostics are not VM semantics.

The current local harness consumes declared support roots. Runtime-derived
support roots should wait until the discrepancy register below is closed and
the executable inventories cover opcode semantics, numerical profiles,
capability semantics, and effort schedules.

## Discrepancy Register

### Must close before semantic roots

1. **Policy and typed admission are not a total matrix.**
   `MATMUL_Q16` and `SHIFT_ROUND_INPLACE` are neither Program-only nor supported
   by Program type flow. They are therefore admitted through the legacy path
   while the otherwise stricter Q16 family is admitted through the Program
   path. Host-float and Program-only names are also maintained as separate
   matches.

2. **The FP data-loader cache can affect semantics and effort.**
   `LOAD_INT8_FP` keys decoded data by source length and a small hash of at most
   the first 32 characters. Distinct sources can share that key. A hit reuses
   the stored decoded string and receives a different dynamic charge. Cache
   state must not select data or logical effort.

3. **Memory bounds and value interpretation vary by operation family.**
   Generic, legacy, Q16, and FP operations do not share one checked range
   boundary. FP64 and Q16 values are contextual `VInt` cells, and missing or
   mistyped cells can become zero through different helpers. Rooted semantics
   need one explicit rule per typed access.

4. **Memory failure is not generally candidate-based.**
   The contract journal covers storage, not VM memory or blobs. Several
   operations write in place. A later exception marks the run reverted but does
   not itself restore those state domains. Session execution cannot rely on
   discarding a whole contract state as its atomicity mechanism.

5. **Attention has two materially different current boundaries.**
   Q16 attention snapshots validated ranges through `valid_mem_span`; FP
   attention reads sparse memory directly. Q16 key/value size is also limited by
   the common `131_072`-cell range bound. The capability audit must either state
   one broad, bounded attention contract or replace it with smaller primitives.

### Must close before profiled admission

6. **Effects are still partial for the older tensor surface.**
   `Program_effects` now reports memory access for the Q1-G128 proof opcode,
   F32 ingress proof opcode, strict-FP activation proof opcodes, and causal
   depthwise proof opcode. Older tensor and blob instructions still do not all
   contribute memory or blob effects, so capability matching and receipts cannot
   infer those facts from the current scan unless the admitted profile is fully
   enumerated.

7. **Strict operands and Program type flow are intentionally partial today.**
   Their wildcard or unsupported cases are safe only while policy prevents the
   affected operations from entering that profile. New admission must prove
   total coverage instead of relying on match ordering across modules.

8. **Logical effort is a host `int` split between base and execution bodies.**
   Checked helpers prevent ordinary host-`int` overflow, but formulas and shape
   limits remain embedded in execution cases. This is not yet the canonical
   unsigned analytic schedule required by the contracts document.

9. **Numerical class and admission decision are coupled.**
   `host_float_opcode` both identifies a numerical implementation and drives
   consensus rejection. Future exact and bounded FP profiles need separate
   numerical facts while retaining current admission behavior by default.

10. **Text assembly is not round-trip complete.**
    The renderer emits legacy, unclassified Q16, and FP instructions that the
    parser does not accept. Binary bytecode is complete, so this is a tooling
    discrepancy rather than a reason to alter wire allocation.

## Architectural Disposition

The audit supports the accepted architecture:

- keep one VM, bytecode, verifier, and execution loop;
- keep model interpretation and model names outside LiteNode;
- retain existing wire allocation and opcode names unless compatibility work
  proves a change necessary;
- make current decisions explicit before changing any decision;
- place typed range access and scalar semantics below generic operations;
- keep kernels, prepared views, and cache state outside semantic identity; and
- add profiled admission by composing existing verifier, policy, effect, and
  type-flow functions.

There is no need for a broader ontology refactor. The useful ontology is the
small set already present: values, instructions, effects, policies, profiles,
effort, and state transitions. The work is to make their relationships total.

## First Implementation Delta

The next code change should be behavior-neutral and confined to policy and
conformance:

1. represent numerical class and current admission class in one small opcode
   classification;
2. derive the existing host-float and Program-only queries from that match;
3. add table-driven checks that preserve every current admission decision,
   including the two unclassified Q16 instructions;
4. check that every classified compute instruction has bytecode, verifier,
   compiler, strict-operand, effect, and type-flow dispositions; and
5. report discrepancies without repairing them in the same change.

That delta creates an auditable boundary with minimal merge impact. Subsequent
changes can correct one discrepancy at a time, with compatibility decisions and
conformance vectors visible in review.
