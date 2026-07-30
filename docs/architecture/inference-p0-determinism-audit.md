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
- conformance templates reject unknown or over-claimed numerical profiles;
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
/home/exedev/evidence/octra-inference/determinism-ingestion-corpus-effort-authority-20260730-014647/p0-vm-execution-templates.cjson
```

These indexes currently execute all five positive P0 templates and match every
declared output span. The saved LiteNode reports live beside the indexes as
`litenode-positive-execution-report.cjson`.

Failure/atomicity mode is available for definitive rejection cases:

```text
tools/inference_conformance_run.exe \
  --template-index <p0-vm-execution-templates.cjson> \
  --include-failures
```

The saved reports live beside the indexes as
`litenode-failure-atomicity-report.cjson`.

The effort-authority artifact emits exact `Contract_vm` effort for this corpus,
so `--strict-effort` passes there. Earlier template artifacts remain useful
historical fixtures, but their `expected_effort` values are estimates.

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

## Schema Acceptance Gate

The LiteNode schema gate is:

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

Producer-index reports include diagnostic `profile_gates`, using a declared
template profile when present and the opcode's current runtime profile when the
template is profile-less.
If a referenced template also declares `opcode` or `primitive`, those fields
must match the index entry so profile diagnostics cannot be mislabeled.

After that, LiteNode can execute templates without negotiating schema again.

## Numerical Profile Gate

The conformance template parser now treats the numerical profile as a gate, not
as free-form metadata. The current runtime profile for the P0 FP templates is:

```text
host-fp-local-candidate
```

That profile is accepted only as local candidate execution. It is reported as
`local_only`, with required actions to bind exact arithmetic, replace or qualify
host math, and pass cross-platform conformance before validator admission.
Conformance JSON includes:

```text
profile_gate.name
profile_gate.consensus_status
profile_gate.local_semantics
profile_gate.consensus_obligations
```

`local_semantics` records what the current VM does today.
`consensus_obligations` records what must become protocol-owned before the
opcode can be promoted out of `local_only`.
All five P0 opcodes report primitive-specific local semantics and consensus
obligations; none fall back to a generic promotion checklist.

Templates that declare an unknown profile are rejected. Templates that claim a
profile the current runtime does not implement, such as `soft-fp-exact` for
`RMSNORM_FP_EPS`, are also rejected. That prevents producer artifacts from
silently upgrading a host-FP fixture into a consensus-candidate claim.

The same check applies to producer VM execution templates when they include a
`profile` field. Older accepted templates without that field remain readable,
but execution reports attach the opcode's current runtime profile as an implicit
diagnostic `profile_gate`. Any declared profile is still enforced before
execution.

Initial P0 focus:

| Opcode | Current accepted profile | Consensus status | Pinned locally | Why not ready |
| --- | --- | --- | --- | --- |
| `LINEAR_Q1_G128_FP` | `host-fp-local-candidate` | `local_only` | Q1 scale/sign edge vectors, binary16 zero/signed-zero/subnormal/max-finite scale vectors, NaN/infinity scale rejection, finite rejection, overflow rollback, effort floor, output atomicity, and destination/lhs snapshot behavior. | Uses native binary64 multiplication/addition after Q1 scale/sign decode. |
| `RMSNORM_FP_EPS` | `host-fp-local-candidate` | `local_only` | Explicit epsilon bits, minimum-subnormal epsilon, row composition, signed-zero/subnormal acceptance, finite rejection, alias rejection, effort floor, and output atomicity. | Uses native binary64 reduction, division, multiplication, and `sqrt`. |

The next acceptable status change for either opcode requires a runtime
implementation change and matching scalar-oracle corpus, not just a new string
in the producer template.

## Current Positive Execution Gate

Status on 2026-07-30, using the effort-authority artifact:

| Opcode | VM run | Output spans | Expected effort | Observed effort | Strict effort |
| --- | --- | --- | ---: | ---: | --- |
| `LINEAR_Q1_G128_FP` | accepted | matched | `201` | `201` | matched |
| `RMSNORM_FP_EPS` | accepted | matched | `67` | `67` | matched |
| `L2NORM_FP` | accepted | matched | `53` | `53` | matched |
| `SOFTMAX_FP` | accepted | matched | `133` | `133` | matched |
| `GATED_DELTA_RULE_FP` | accepted | matched | `222` | `222` | matched |

This is a positive arithmetic ingestion gate, not a validator-grade
determinism claim. It proves the producer fixtures now agree with the current
LiteNode VM implementation for the five P0 positive cases. The next gap is
full policy resolution for observational failure cases, followed by replacing
host-FP semantics where protocol determinism requires it.

Failure/atomicity status:

| Opcode | Declared failure cases | Counted rejection cases | Counted cases accepted |
| --- | ---: | ---: | ---: |
| `LINEAR_Q1_G128_FP` | 7 | 6 | 6 |
| `RMSNORM_FP_EPS` | 7 | 5 | 5 |
| `L2NORM_FP` | 7 | 5 | 5 |
| `SOFTMAX_FP` | 7 | 5 | 5 |
| `GATED_DELTA_RULE_FP` | 7 | 6 | 6 |

Uncounted cases are intentionally observational today:

- `output_input_aliasing`, because some operations are in-place or safe-copy
  candidates rather than unconditional rejections;
- `finite_square_overflow`, because the deterministic overflow profile still
  needs an explicit protocol decision; and
- `overflow_without_max_subtract`, because that validates stable softmax
  behavior rather than reject-before-write behavior.

## P0-Plus Execution Gate

Status on 2026-07-30:

```text
/home/exedev/evidence/octra-inference/determinism-p0-plus-corpus-20260730-022653/p0-plus-fixture-pack.cjson
/home/exedev/evidence/octra-inference/determinism-p0-plus-topk-boundary-20260730-025306/p0-plus-fixture-pack.cjson
```

LiteNode executes this pack with:

```text
tools/inference_conformance_run.exe \
  --p0-plus-pack <p0-plus-fixture-pack.cjson>
```

Current result:

| Surface | Fixture cases | VM result |
| --- | ---: | --- |
| `SOFTMAX_FP` | 2 | accepted/matched |
| `ATTENTION_SCORES_FP` | 1 | accepted/matched |
| `ATTENTION_WEIGHTED_SUM_FP` | 1 | accepted/matched |
| `ROPE_APPLY_INDEXED_FP` | 2 | accepted/matched |
| `ARGMAX_FP` | 2 | accepted/matched for selected index |
| `RMSNORM_FP_EPS -> LINEAR_Q1_G128_FP -> ARGMAX_FP` | 1 | accepted/matched |

The saved report is:

```text
/home/exedev/evidence/octra-inference/determinism-p0-plus-corpus-20260730-022653/litenode-p0-plus-execution-report.cjson
/home/exedev/evidence/octra-inference/determinism-p0-plus-topk-boundary-20260730-025306/litenode-p0-plus-execution-report.cjson
```

The P0-plus pack is still diagnostic and model-neutral. It adds coverage for
attention math, indexed RoPE, argmax tie behavior, and the final logits-tail
composition. `ARGMAX_FP` only returns the selected index, so producer top-k
ordering manifests are preserved as `producer_only` evidence. The explicit
top-k boundary is:

```text
/home/exedev/evidence/octra-inference/determinism-p0-plus-topk-boundary-20260730-025306/p0-plus-topk-boundary.cjson
```

It declares `topk_vm_authority = false` and `topk_opcode_proposal =
not_emitted`. A future `TOPK_FP` must be a separate primitive/spec; it must not
be inferred from `ARGMAX_FP` fixtures.
