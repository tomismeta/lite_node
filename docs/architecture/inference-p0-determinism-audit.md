# Inference P0 Determinism Audit

Status: implementation audit, 2026-07-30.

This audit is scoped to the five P0 primitives in the determinism ingestion
corpus:

- `LINEAR_Q1_G128_FP`
- `RMSNORM_FP_EPS`
- `L2NORM_FP`
- `SOFTMAX_FP`
- `GATED_DELTA_RULE_FP`

The current VM surface is model-neutral and useful for local candidate
execution. This audit does not claim validator-grade deterministic math. The
remaining work is to turn each primitive into a punitive numerical contract
with a scalar oracle and VM conformance gate.

## Current Runtime Read

| Opcode | Current implementation surface | Determinism risk | Required hardening |
| --- | --- | --- | --- |
| `LINEAR_Q1_G128_FP` | `contract_vm.ml` decodes Q1-G128 blocks, binary16-like scales, sign bits, and accumulates into native `float`. | Native binary64 multiplication/addition and scale decode are local-profile behavior unless the exact decode, accumulation, rounding, and finite policy are contract-bound. | Pin byte layout, scale decode, sign mapping, loop order, accumulator profile, finite rejection, aliasing, and output encoding. |
| `RMSNORM_FP_EPS` | Reads explicit epsilon bits, computes sum of squares, `sqrt`, and gamma multiply using native `float`. | `sqrt`, reduction behavior, signed-zero/subnormal treatment, and multiplication order are not yet consensus-owned. | Define exact epsilon input, reduction order, inverse-root implementation, gamma read/order, non-finite policy, and rollback behavior. |
| `L2NORM_FP` | Same native reduction/sqrt shape as RMSNorm without gamma. | Same as RMSNorm; smaller surface but still depends on native `sqrt` and reduction semantics. | Define exact inverse-norm semantics, edge vectors near zero, signed-zero/subnormal behavior, and failure atomicity. |
| `SOFTMAX_FP` | Uses max-subtraction, native `exp`, sum, and division over native `float`. | Highest math risk in this set because `exp`, sum, and division can change probabilities and token/order behavior. | Replace or pin `exp`, sum, division, tie behavior, overflow/underflow, in-place behavior, and finite-result rules. |
| `GATED_DELTA_RULE_FP` | Stateful recurrence with native `exp`, `sqrt`, repeated dot products, decay, state update, and output writeback. | Highest state risk: small numeric drift compounds; output and next-state rollback must be all-or-nothing. | Pin layout, head mapping, loop nesting, decay math, scale math, state update order, aliasing, effort, and atomicity. |

## Blocking Semantic Question

`GATED_DELTA_RULE_FP` must be reconciled before its corpus fixture can qualify
the opcode. The current LiteNode implementation computes a correction term:

```text
decay = exp(log_decay)
state *= decay
memory[row] = sum(state[row,col] * k[col])
delta[row] = (v[row] - memory[row]) * beta
state[row,col] += k[col] * delta[row]
output[row] = sum(state[row,col] * q[col]) / sqrt(key_dim)
q_head = head mod q_heads
k_head = head mod k_heads
```

Any producer oracle that instead describes `state = state * decay + beta * v *
k` followed by `output = state * q` is not the same operation. The next
producer handoff must state which formula is authoritative and identify the
first divergent term if LiteNode is wrong.

## Existing Positive Controls

The current branch already has useful controls:

- opcode capability gates under `inference_opcode_policy.ml`;
- generic program policy still rejects these inference FP opcodes outside the
  inference path;
- fixed loop order in the OCaml implementation;
- finite-output checks for the P0 primitives;
- aliasing and span checks in the major writeback paths;
- failure-path tests for individual primitives; and
- an ingestion checker for the producer-side determinism corpus.

These are necessary controls. They are not sufficient for consensus
determinism because native `float`, `sqrt`, and `exp` are still active in the
P0 path.

## Runner Shape

The LiteNode-side conformance runner should advance in three steps:

1. Validate external template shape with
   `tools/inference_conformance_check.exe`.
2. Compile each accepted template into a direct VM state/program invocation.
3. Compare VM output bytes/root and failure atomicity against the independent
   oracle emitted by `octra-inference`.

The template checker is intentionally diagnostic-only. It rejects drift before
execution, but it does not execute math. The companion positive runner executes
the current P0 templates directly through `Contract_vm` and compares each output
span byte-for-byte:

```text
tools/inference_conformance_run.exe \
  --template-index <p0-vm-execution-templates.cjson>
```

The first accepted producer indexes are:

```text
/home/exedev/evidence/octra-inference/determinism-ingestion-corpus-vm-templates-20260730-004544/p0-vm-execution-templates.cjson
/home/exedev/evidence/octra-inference/determinism-ingestion-corpus-vm-templates-corrected-20260730-011945/p0-vm-execution-templates.cjson
```

Both indexes currently execute all five positive P0 templates and match every
declared output span. The saved LiteNode reports live beside the indexes as
`litenode-positive-execution-report.cjson`.

Observed VM effort does not match producer `expected_effort` yet. Treat that
field as a producer estimate until `octra-inference` either emits exact
`Contract_vm` effort or renames the field to make the estimate boundary
unambiguous. `--strict-effort` is available when exact effort becomes part of
the contract.

Required template schema:

```json
{
  "type": "litenode_vm_conformance_template",
  "schema": 1,
  "opcode": "RMSNORM_FP_EPS",
  "primitive": "rmsnorm_fp_eps",
  "profile": "host-fp-local-candidate",
  "vm_semantics_root": "<vm-semantics-root>",
  "numerical_profile_root": "<numerical-profile-root>",
  "expected_effort": 16,
  "effects": ["memory_read", "memory_write"],
  "registers": [
    {"register": 0, "name": "addr", "kind": "address", "value": "100"}
  ],
  "memory": [
    {
      "name": "input",
      "path": "fixtures/input.f64le.bin",
      "root": "<sha256-or-root>",
      "byte_length": 32,
      "sha256": "<sha256>",
      "encoding": "f64le",
      "base": 100,
      "cells": 4,
      "access": "read_write"
    }
  ],
  "expected": {
    "spans": [
      {
        "name": "output",
        "base": 100,
        "cells": 4,
        "output_root": "<root>",
        "output_sha256": null
      }
    ]
  },
  "failure_cases": [
    {
      "case": "nonfinite_input_nan",
      "expected": "reject_before_write",
      "mutations": ["input[0]=nan"],
      "unchanged_spans": ["output"]
    }
  ]
}
```

The schema is deliberately generic. There are no model names, tokenizer
assumptions, HuggingFace assumptions, or Bonsai/Qwen-specific fields.

## Do Not Do Yet

- Do not broaden P0 before these five templates execute.
- Do not claim devnet readiness from host-FP local candidate roots.
- Do not optimize kernels before conformance identifies the exact numerical
  profile.
- Do not migrate everything to Q16.16; the ingestion corpus already shows a
  near-tie token-selection failure for Q16.16.
- Do not add model-specific fast paths to the VM.

## Next Acceptance Gate

The next useful LiteNode gate is:

```text
inference_conformance_check --template-dir <p0-template-dir>
```

For the producer-emitted index format:

```text
inference_conformance_check --template-index <p0-vm-execution-templates.cjson>
```

Acceptance means only:

- all five P0 opcodes are represented;
- every template declares memory/register ABI;
- fixture files are relative paths and match declared byte length and SHA-256;
- expected output identity is present;
- `GATED_DELTA_RULE_FP` declares both output and next-state spans;
- required effects match the opcode class; and
- failure cases are present.

After that, LiteNode can wire execution without negotiating schema again.

## Current Positive Execution Gate

Status on 2026-07-30:

| Opcode | VM run | Output spans | Producer effort vs observed effort |
| --- | --- | --- | --- |
| `LINEAR_Q1_G128_FP` | accepted | matched | `256` vs `201` |
| `RMSNORM_FP_EPS` | accepted | matched | `32` vs `67` |
| `L2NORM_FP` | accepted | matched | `24` vs `53` |
| `SOFTMAX_FP` | accepted | matched | `64` vs `133` |
| `GATED_DELTA_RULE_FP` | accepted | matched | `96` vs `222` |

This is a positive arithmetic ingestion gate, not a validator-grade
determinism claim. It proves the producer fixtures now agree with the current
LiteNode VM implementation for the five P0 positive cases. The next gap is
failure/atomicity execution over the declared mutations, followed by replacing
host-FP semantics where protocol determinism requires it.
