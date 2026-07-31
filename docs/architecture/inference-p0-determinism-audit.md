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
| `LINEAR_Q1_G128_FP` | `contract_vm.ml` decodes Q1-G128 blocks, exact little-endian binary16 scale bits, sign bits, and accumulates with LiteNode's finite binary64 core. | Scale/sign interpretation and accumulator order are now profile-described and tested locally; independent cross-platform oracle qualification is still required before consensus promotion. | Pin byte layout, scale decode, sign mapping, loop order, accumulator profile, finite rejection, aliasing, and output encoding. |
| `RMSNORM_FP_EPS` | Reads explicit epsilon bits; sum of squares, count division, epsilon addition, inverse-root `sqrt`, reciprocal division, and output multiply use LiteNode's finite binary64 core. | Independent cross-platform oracle qualification is still required before consensus promotion. | Define exact epsilon input, reduction order, inverse-root implementation, gamma read/order, non-finite policy, and rollback behavior. |
| `L2NORM_FP` | Same deterministic reduction, inverse-root `sqrt`, and output multiply shape as RMSNorm without gamma. | Same as RMSNorm; smaller surface but still needs independent inverse-root qualification. | Define exact inverse-norm semantics, edge vectors near zero, signed-zero/subnormal behavior, and failure atomicity. |
| `SOFTMAX_FP` | Uses deterministic finite binary64 left-to-right max selection, score shift, nonpositive shifted-score gate, exponential sum, and probability division; `exp` remains native. | Highest math risk in this set because native `exp` can change probabilities and token/order behavior. The local gate now constrains host `exp` inputs to shifted scores that compare `<= +0.0`. | Replace or pin `exp`, tie behavior, overflow/underflow, in-place behavior, and finite-result rules. |
| `GATED_DELTA_RULE_FP` | Stateful recurrence with native decay `exp` behind a finite nonpositive `log_decay` gate; integer key-dimension conversion, query-scale `sqrt`, reciprocal division, recurrence dot products, beta-delta updates, state updates, and output scaling multiply use LiteNode's finite binary64 core. | Highest state risk: small numeric drift compounds; decay math and recurrence ordering are not yet consensus-owned. The local gate rejects positive finite `log_decay` before state mutation. | Pin layout, head mapping, loop nesting, decay math, scale math, state update order, aliasing, software-fp effort, and atomicity. |

The software-fp effort charge is intentionally unchanged in this local-only
hardening slice. Consensus promotion must recalibrate the charge for
deterministic add/mul/div work before treating the profile as a validator cost
contract.

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
- L2Norm accepts the current normalization-profile minimum-positive-subnormal
  epsilon path over signed-zero/subnormal inputs;
- aliasing and span checks in the major writeback paths;
- failure-path tests for individual primitives, including gated-delta output
  and next-state rollback on missing input, nonfinite decay, invalid shape,
  insufficient effort, and product overflow; and
- an ingestion checker for the producer-side determinism corpus.

These are necessary controls. They are not sufficient for consensus
determinism because native `exp` is still active in the P0 path, and the
finite binary64 core still needs independent cross-platform oracle
qualification.

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
Positive execution reports also include `profile_gate_count`,
`classified_profile_gate_count`, and `unprofiled_template_count` so an
accepted VM run can be audited for profile coverage without walking every
per-template result. They also include
`profile_consensus_status_counts`, which separates deterministic candidate
profiles from remaining local-only host-FP math.
Passing `--require-profile-roots-bound` turns stale or missing
`numerical_profile_root` bindings into a hard gate without requiring every
profile to be consensus-ready.
Passing `--require-consensus-candidate` is the next stricter gate: every
profile must be `consensus_candidate` or `consensus_ready`, every template must
be profiled, and every numerical profile root must bind. It intentionally
rejects local-only host-math profiles even when VM execution matches.
Passing `--require-consensus-ready` turns that diagnostic boundary into a hard
gate; current non-ready artifacts are expected to reject under that flag with
profile blockers in `consensus_ready_gate.blockers`.
In that strict mode, `execution_status` remains the VM-output result while the
top-level `status` reflects the active consensus-readiness gate.

Failure/atomicity mode is available for definitive rejection cases:

```text
tools/inference_conformance_run.exe \
  --template-index <p0-vm-execution-templates.cjson> \
  --include-failures
```

Qualification runs should add `--require-failure-cases`; otherwise a report can
still be useful local positive execution evidence, but it is not punitive
failure/atomicity evidence. The top-level `failure_case_gate` records whether
failure cases were included, how many counted cases ran, and whether every
counted case accepted.

Runner reports also include a diagnostic `validator_readiness_gate`. That gate
composes VM execution, punitive failure execution, strict effort authority,
consensus-ready profile status, profile-root binding, and cross-platform
evidence into one answer. It is expected to reject the current P0 artifacts
even when positive execution and failure cases pass, because some P0 profiles
are still local-only and the remaining consensus-candidate profiles still carry
blocker codes. It also rejects reports without `--strict-effort` and
single-platform reports with `cross_platform_conformance_missing`; a local
Linux runner result is not a validator portability matrix.

Use `--require-validator-readiness` on `inference_conformance_run.exe` when the
runner itself is acting as a release or admission gate. Without that flag, the
runner can exit successfully for useful local execution evidence while the
nested `validator_readiness_gate.status` remains `rejected`.

Cross-platform evidence is aggregated separately:

```text
tools/inference_conformance_matrix.exe \
  --runner-report <linux-report.cjson> \
  --runner-report <macos-report.cjson>
```

Use `--require-validator-readiness` when the matrix is acting as a release or
admission gate. Without that flag, the command exits successfully when the
cross-platform matrix itself is accepted, even if the lifted
`validator_readiness_status` is still rejected by consensus-profile or
profile-root blockers.

The matrix verifier consumes executed `inference_conformance_run` reports, not
schema-checker reports. It requires accepted local execution, strict effort,
accepted punitive failure cases, distinct platform observations, and identical
per-opcode output/effort signatures. They also require a single shared
`profile_catalog_root`, so matching output bytes cannot hide a profile-contract
drift between reports. A single repeated VPS report is still rejected as
`insufficient_distinct_platforms`. Matrix reports include
`runner_report_sha256`, `result_signature_sha256`, and
`matrix_signature_sha256` as diagnostic evidence roots; those hashes identify
the compared report bytes and normalized result signatures, but they are not
platform attestation. Matrix reports also lift `validator_readiness_status`
and `validator_readiness_blockers` to the top level for compatibility, and
emit the same nested `validator_readiness_gate` shape used by the checker and
runner. New consumers should use the nested gate when distinguishing local
execution evidence from validator-ready inference math. Each matrix
`reports[]` row also preserves the source runner's nested
`validator_readiness_gate`, so aggregate matrix failures and per-platform
runner failures can be inspected without reopening the original report files.

Checker, runner, and matrix reports all expose top-level
`validator_readiness_required` so consumers can tell whether a report was
produced as a diagnostic artifact or as a hard readiness gate.

The saved reports live beside the indexes as
`litenode-failure-atomicity-report.cjson`.
The checker requires each executable failure case to carry `case`, `expected`,
a nonempty `executable_mutations` list, and a nonempty `unchanged_spans` list.
Human-readable mutation prose may exist beside it, but it is not enough for
LiteNode conformance.

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
  "profile": "deterministic-fp64-normalization",
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

Static checker reports include a diagnostic `validator_readiness_gate` too. It
always rejects with `execution_not_run` and
`punitive_failure_cases_not_run`, because the checker never executes VM math.
It also carries `cross_platform_conformance_missing`. That makes schema
acceptance visibly different from validator readiness before any runner report
or multi-platform evidence is attached.
Use `--require-validator-readiness` on the checker only when you intentionally
want that schema-only report to fail unless the nested readiness gate accepts.

Producer-index reports include diagnostic `profile_gates`, using a declared
template profile when present and the opcode's current runtime profile when the
template is profile-less.
If a referenced template also declares `opcode` or `primitive`, those fields
must match the index entry so profile diagnostics and execution cannot be
mislabeled.

After that, LiteNode can execute templates without negotiating schema again.

## Numerical Profile Gate

The conformance template parser now treats the numerical profile as a gate, not
as free-form metadata. P0 templates are no longer forced into one broad host-FP
bucket:

```text
LINEAR_Q1_G128_FP      deterministic-q1-g128-fp64-linear
RMSNORM_FP_EPS         deterministic-fp64-normalization
L2NORM_FP              deterministic-fp64-normalization
SOFTMAX_FP             host-fp-exp-local-candidate
GATED_DELTA_RULE_FP    host-fp-exp-local-candidate
```

`host-fp-local-candidate` is accepted only as local candidate execution and is
reported as `local_only`, with required actions to bind exact arithmetic,
replace or qualify host math, and pass cross-platform conformance before
validator admission. `host-fp-exp-local-candidate` is the narrower local-only
profile for kernels whose remaining native host dependency is `exp`/`log1p`.
`host-fp-trig-local-candidate` is the narrower local-only profile for indexed
rotary math whose remaining native host dependency is exponentiation plus
`cos`/`sin`. `deterministic-q1-g128-fp64-linear` and
`deterministic-fp64-normalization` are reported as `consensus_candidate`; they
still require profile-root binding and independent cross-platform conformance
before any consensus-ready claim.
Conformance JSON includes:

```text
profile_gate.name
profile_gate.consensus_status
profile_gate.local_semantics
profile_gate.consensus_obligations
profile_gate.consensus_blocker_codes
profile_gate.profile_contract
profile_gate.profile_root
```

Checker reports also include `profile_gate_count` and
`classified_profile_gate_count` plus `unprofiled_template_count` so producer
indexes, single templates, and template directories can be audited without
parsing every template body. The conformance-template test also pins every
currently admitted inference runtime opcode to an explicit profile bucket. The
current surface has seventeen profiled opcodes: eleven `consensus_candidate`
gates and six `local_only` gates. No current inference opcode may fall back
silently to the generic `host-fp-local-candidate` bucket. The same test pins
each opcode's current `profile_root`, so profile-contract changes require an
intentional reviewed root update rather than quiet fixture churn. The companion
`profile_consensus_status_counts` object summarizes how many present profile
gates are `local_only`, `consensus_candidate`, `consensus_ready`, or unknown.
Unknown covers malformed gates or unrecognized `consensus_status` values.
Passing `--require-consensus-ready` rejects any report with unprofiled,
unclassified, `local_only`, `consensus_candidate`, or unknown profile gates,
and any `unbound` or `unavailable` profile roots. Rejection reasons are emitted
as stable strings in `consensus_ready_gate.blockers`.
Passing `--require-consensus-candidate` rejects the same unprofiled,
unclassified, unknown, and root-binding blockers, but it allows
`consensus_candidate` gates and rejects only `local_only` math at the profile
status layer. Rejection reasons are emitted in
`consensus_candidate_gate.blockers`.
`schema_status` remains the producer/template shape result in both normal and
strict mode; top-level `status` additionally includes the active readiness gate.
`consensus_blocker_codes` is the stable, machine-readable list of primitive
math/profile blockers behind the prose obligations. `profile_contract` is the
machine-owned numerical descriptor: schema, opcode, arithmetic domain, rounding
mode, operation sequence, edge/overflow/writeback policy, and oracle-vector
root. `profile_root` is the root of that descriptor only. It deliberately
excludes `consensus_status`, summaries, required-action prose, and blocker
wording so documentation churn cannot rebind arithmetic. Current templates
still validate `numerical_profile_root` syntactically; profile-root equality is
diagnostic in default mode, enforced by `--require-profile-roots-bound`, and
also enforced by `--require-consensus-ready`.
Template and P0 execution reports expose that diagnostic as
`profile_root_binding.status`: `matched`, `unbound`, or `unavailable`.
They also expose `profile_root_binding.classification`, where `none` means the
root is bound, `profile_root_mismatch` means the producer supplied a different
numerical root, and `profile_root_unavailable` means the gate root was missing.
If no profile-root observations are present at all, root readiness rejects with
`no_profile_roots`; empty evidence is never treated as bound evidence.
Checker and runner conformance reports summarize those values in
`profile_root_binding_classification_counts` so consumers can detect stale
profile roots without walking every template row. They also expose
`profile_catalog_root`, the root of the current LiteNode profile catalog for
the report's profiled opcodes, and `profile_root_binding_catalog`, which
records each opcode's declared
`numerical_profile_root`, LiteNode `profile_root`, status, and classification
as the exact root-binding worklist. Reports also expose a
deduplicated `profile_root_catalog` with the current LiteNode opcode, profile
name, consensus status, and `profile_root` values. That catalog is a producer
handoff convenience; it does not make an artifact consensus-ready unless its
declared roots also bind and the readiness gate accepts. Reports also include
`consensus_blocker_catalog`, a deduplicated mapping from blocker code to the
opcodes carrying that blocker. This is the machine-readable remaining-math
worklist after local execution succeeds. Each entry also carries an advisory
`blocker_class`, separating software-fp64 qualification, host-native math,
encoding/layout, execution order, safety policy, storage binding, and external
qualification work without changing the stable blocker codes.
The surface test rejects `unknown` blocker classes so newly introduced blocker
codes must be intentionally classified before the report shape is extended.
Reports also expose `consensus_blocker_class_counts`, a class-level summary
derived from the same catalog for planning and review. The detailed
`blocker_code` entries remain the stable automation keys.
LiteNode can also emit the current runtime profile catalog without requiring a
producer fixture:

```text
tools/inference_profile_catalog.exe --p0
tools/inference_profile_catalog.exe --all
```

That standalone catalog is the intended source for producer-side
`numerical_profile_root` remediation. It reports each opcode's current profile
root, consensus status, blocker catalog, and a diagnostic
`profile_catalog_root`. The catalog root identifies the emitted catalog; it is
not a substitute for binding each template's per-opcode numerical profile root.
Each `profile_root_binding_catalog` row also carries
`validator_readiness_status`, `validator_readiness_blockers`, and
`consensus_blocker_codes`. That row is the compact per-primitive answer to:
what ran, which profile it claimed, whether the root bound, and why it is not
validator-ready. A matched profile root can still be validator-rejected when
the profile is only `consensus_candidate` or when blocker codes remain.

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

LiteNode now also has an opcode-policy regression guard for the inference
surface: every admitted inference-specialized opcode must either report a
runtime numerical/ingress profile or be explicitly classified as a non-profile
boundary such as `FLOAD` range binding or the still-experimental Q16 lane.

`LOAD_F32_LE_FP` and `LOAD_F64_LE_FP` are intentionally separated from host-FP
math under the `byte-ingress-exact` profile. They specify little-endian finite
f32/f64 byte materialization from rooted source bytes into VM memory, including
offset/count bounds, non-finite rejection, FLOAD-authenticated range binding,
and decode atomicity. They are transport surfaces for model/session data, not
arithmetic-kernel determinism claims. Opcode policy keeps them under the broad
consensus-unsafe program gate, but `uses_host_float_math` now distinguishes
them from native floating-point arithmetic kernels.

Initial P0 focus:

| Opcode | Current accepted profile | Consensus status | Pinned locally | Why not ready |
| --- | --- | --- | --- | --- |
| `LINEAR_Q1_G128_FP` | `deterministic-q1-g128-fp64-linear` | `consensus_candidate` | Q1 scale/sign edge vectors, exhaustive binary16 scale decode, NaN/infinity scale rejection, finite rejection, overflow rollback, effort floor, output atomicity, and destination/lhs snapshot behavior. | Uses exact integer binary16 scale decode plus LiteNode's software-defined finite binary64 add/mul core; still needs independent cross-platform oracle qualification and profile-root binding. |
| `RMSNORM_FP_EPS` | `deterministic-fp64-normalization` | `consensus_candidate` | Explicit epsilon bits, minimum-subnormal epsilon, deterministic reduction/inverse-root/output multiply, row composition, signed-zero/subnormal acceptance, finite rejection, alias rejection, effort floor, and output atomicity. | Uses LiteNode's software-defined finite binary64 core; still needs independent cross-platform oracle qualification and profile-root binding. |
| `L2NORM_FP` | `deterministic-fp64-normalization` | `consensus_candidate` | Explicit epsilon bits, minimum-subnormal epsilon, deterministic reduction/inverse-root/output multiply, row composition, finite rejection, effort floor, inverse-root overflow rejection, and output atomicity. | Uses LiteNode's software-defined finite binary64 core; still needs independent cross-platform oracle qualification and profile-root binding. |

`LINEAR_Q1_G128_FP` and the normalization opcodes have moved out of the broad
host-FP bucket because their current runtime paths avoid native host
floating-point math in the admitted compute path. They still are not
consensus-ready until profile roots, independent scalar-oracle coverage, and
cross-platform conformance are bound.

The shared finite binary64 helper is covered by a dedicated
`inference_fp64_test` edge-vector test. Primitive tests continue to cover their
own layout, aliasing, effort, and atomicity rules, while common add, multiply,
divide, square-root, and comparison behavior is checked once at the math
substrate boundary.

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

Failure/edge-case status:

| Opcode | Declared cases | Counted cases | Counted cases accepted |
| --- | ---: | ---: | ---: |
| `LINEAR_Q1_G128_FP` | 7 | 7 | 7 |
| `RMSNORM_FP_EPS` | 7 | 7 | 7 |
| `L2NORM_FP` | 7 | 7 | 7 |
| `SOFTMAX_FP` | 7 | 7 | 7 |
| `GATED_DELTA_RULE_FP` | 7 | 7 | 7 |

No current P0 failure/edge cases are left uncounted in the direct runner.

`output_input_aliasing` is counted as a deterministic policy branch: the VM
must either reject before writeback with the declared spans unchanged, or accept
with the active output span changed and finite. Current P0 behavior covers both
branches: Q1, normalization, and softmax use documented safe-copy/in-place
paths, while Gated Delta rejects the alias before writing output/state.

`finite_square_overflow` is now counted for `RMSNORM_FP_EPS` and `L2NORM_FP`
under the current deterministic normalization profile: finite square/reduction
overflow must reject before output writeback.

`overflow_without_max_subtract` is now counted for `SOFTMAX_FP` as a positive
local semantics check: large finite scores must execute via max subtraction,
must update the output span, and must leave finite probabilities. It does not
promote softmax out of `host-fp-exp-local-candidate`; native `exp` remains the
consensus blocker.

LiteNode now also pins `SOFTMAX_FP` locally for extreme finite scores:
equal `max_float` scores produce uniform probabilities after max subtraction,
and a score dominated by `max_float` underflows to a zero probability without
rejecting the finite input. Shifted scores are checked with LiteNode's
deterministic binary64 comparison and must compare `<= +0.0` before native
`exp` is called. This remains exp-local candidate behavior until `exp` and
output encoding are protocol-owned; max selection, score-shift,
exponential-sum, and probability-division steps now use LiteNode's finite
binary64 core.

`fp64_subtract_conformance` is now represented by the named `Inference_fp64.sub`
helper. The helper is deliberately defined through the same deterministic
finite binary64 `add` plus sign-bit negation path used before, so this documents
and tests the subtraction profile without changing runtime arithmetic.

`binary16_scale_decode` is represented by `Inference_fp64.of_binary16`. The
contract VM still reads Q1 scale bytes little-endian, but all binary16
zero/subnormal/normal/non-finite interpretation now lives in the shared
deterministic fp64 substrate and is exhaustively tested over all 65,536
encodings.

`finite_square_overflow` for normalization now maps to the named
`Inference_fp64.square` helper. `RMSNORM_FP_EPS` and `L2NORM_FP` still reduce
squares left-to-right exactly as before, but square overflow/non-finite
rejection is now tested directly in the shared fp64 substrate.

The positive-input gate and reciprocal root are also named in the shared
substrate as `Inference_fp64.positive` and `Inference_fp64.inverse_sqrt`.
RMSNorm, L2Norm, Gated Delta query scaling, and attention score scaling still
perform the same comparison, square-root, and division sequence, but the
deterministic inverse-root obligation now has one tested entry point.

LiteNode reports `GATED_DELTA_RULE_FP` under the same
`host-fp-exp-local-candidate` profile. The recurrence shape, query-scale
sqrt/division, dot products, beta updates, state mutation order, and output
scaling are locally pinned around LiteNode's finite binary64 helper path, but
state decay still calls native `exp` after the deterministic nonpositive
`log_decay` gate. That native exponential is the remaining P0 state-transition
determinism gap.

Host-backed exponential/log helpers are intentionally named `host_fp64_*` in the
contract VM. They are not part of the deterministic `Inference_fp64` substrate
and must remain behind `host-fp-exp-local-candidate` gates until protocol-owned
transcendental math or a qualified replacement profile exists.

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

LiteNode runtime tests now also pin deterministic finite binary64
left-to-right accumulation for `ATTENTION_SCORES_FP` dot products and
`ATTENTION_WEIGHTED_SUM_FP` weighted reductions with cancellation vectors.
LiteNode reports both under the `deterministic-fp64-accumulation`
consensus-candidate profile. They still require profile-root binding and
independent cross-platform conformance before any consensus-ready attention
claim.

LiteNode also reports `ROPE_APPLY_INDEXED_FP` under the current
`host-fp-trig-local-candidate` runtime profile. The local profile covers
finite binary64 input/base reads, exact integer position cells, native
exponentiation, native `cos`/`sin`, finite-buffer writeback, zero-position
behavior, and tail preservation. This remains local-only until rotary math has
a software-defined or otherwise validator-qualified contract.

LiteNode also reports `SIGMOID_FP`, `SOFTPLUS_FP`, and `SILU_FP` under the
`host-fp-exp-local-candidate` runtime profile. `SIGMOID_FP` and `SILU_FP`
now route through a deterministic sign branch: nonnegative inputs use
`exp(-x)`, negative inputs use `exp(x)`, and the runtime rejects any path where
the native `exp` input is not both finite and nonpositive. The surrounding
`1.0 + exp`, ratio, and SiLU multiply steps use the VM's deterministic
binary64 add/divide/multiply helpers. `SOFTPLUS_FP` uses the same deterministic
positive/nonpositive branch policy and nonpositive exp-domain gate; its
positive-branch addition also uses deterministic binary64 addition.
`SOFTPLUS_FP` remains explicitly native-`log1p`-bound. None of these activation
opcodes should be promoted to a consensus-ready profile until the remaining
native transcendental functions are replaced or independently qualified. The
activation effort schedule is unchanged in this local-only profile; it must be
repriced before any broader admission claim because the deterministic helper
path does more host work than the earlier native float mapping.

LiteNode now reports `ELEMWISE_MUL_FP` and `RESIDUAL_ADD_FP` under a
`deterministic-fp64-elementwise` consensus-candidate profile. Their runtime
tests already pin finite reads, exact same-range aliasing, partial-overlap
rejection, overflow rollback, and effort, and the VM path uses LiteNode's
software-defined binary64 add/multiply helpers rather than native host math.
Consensus-ready admission still requires profile-root binding plus independent
cross-platform conformance for signed-zero, subnormal, overflow, aliasing,
effort, and atomic writeback behavior. This also keeps these opcodes from being
mislabeled as fixed-point authority.

LiteNode also reports `CAUSAL_DEPTHWISE_CONV1D_FP` under the same
`deterministic-fp64-accumulation` consensus-candidate profile. The profile
covers finite input/kernel reads, positive shape parameters, causal depthwise
indexing, left-to-right kernel accumulation with deterministic binary64
multiply/add, input/kernel snapshot before writeback, and finite output
atomicity. This is a generic sequence primitive, not a model- or SSM-specific
fused path, and still requires cross-platform conformance plus bound profile
roots before a consensus-ready claim.

The saved report is:

```text
/home/exedev/evidence/octra-inference/determinism-p0-plus-corpus-20260730-022653/litenode-p0-plus-execution-report.cjson
/home/exedev/evidence/octra-inference/determinism-p0-plus-topk-boundary-20260730-025306/litenode-p0-plus-execution-report.cjson
```

The P0-plus pack is still diagnostic and model-neutral. It adds coverage for
attention math, indexed RoPE, argmax tie behavior, and the final logits-tail
composition. `ARGMAX_FP` only returns the selected index, so producer top-k
ordering manifests are preserved as `producer_only` evidence. The explicit
P0-plus runner now reports per-fixture `profile_gates` plus an aggregate
`profile_gate_count`, `classified_profile_gate_count`, and
`profile_root_catalog`; both P0 and P0-plus reports also expose
`consensus_blocker_catalog` and a diagnostic `validator_readiness_gate`.
The P0-plus readiness gate is expected to reject today because P0-plus does
not yet run punitive failure/atomicity cases and still needs bound roots,
consensus-ready profiles, and cross-platform evidence. The composite
logits-tail case reports the component profiles for `RMSNORM_FP_EPS`,
`LINEAR_Q1_G128_FP`, and `ARGMAX_FP`. Its
`profile_consensus_status_counts` now separate deterministic comparison,
normalization, and Q1 candidates from remaining local-only host-FP math.
Like P0 execution reports, `execution_status` is the VM output result; use
`validator_readiness_gate.status` for validator readiness. The explicit top-k
boundary is:

`inference_conformance_matrix.exe` intentionally rejects P0-plus reports as
`p0_plus_matrix_not_supported` today. Cross-platform matrixing is currently
defined for strict P0 runner reports with punitive failure/atomicity coverage
and exact effort authority. P0-plus should gain matrix support only after its
failure cases and effort semantics are promoted with the same discipline.

LiteNode now reports `ARGMAX_FP` under a `deterministic-fp64-comparison`
consensus-candidate profile. Its semantics are finite binary64 input reads,
deterministic binary64 greater-than comparison, lowest-index tie selection,
selected-index writeback after the input span is read, and no native host math
or floating-point arithmetic. Consensus-ready admission still requires profile
root binding and cross-platform comparison conformance. Fixed-point or
ranked-token claims must come from separate ordering-preservation evidence.

```text
/home/exedev/evidence/octra-inference/determinism-p0-plus-topk-boundary-20260730-025306/p0-plus-topk-boundary.cjson
```

It declares `topk_vm_authority = false` and `topk_opcode_proposal =
not_emitted`. A future `TOPK_FP` must be a separate primitive/spec; it must not
be inferred from `ARGMAX_FP` fixtures.

## Q16 Fixed-Point Boundary

LiteNode still exposes several `tensor.fixed` Q16 opcodes for program-mode
experiments, but they are not yet promoted by this audit. The P0 and P0-plus
profile gates cover the VM-proven Bonsai FP path plus exact byte ingress; they
do not prove that Q16 kernels preserve model quality, token ordering, or
cross-platform bit identity for the same inference workload.

The next Q16 step must be narrow and fixture-led: choose the exact Q16 opcode
subset, pin per-opcode scaling/rounding/saturation/aliasing/effort semantics,
and require `q16-exact` only for those opcodes after positive, negative,
atomicity, and token-order fixtures pass. Until then, Q16 is a candidate speed
path, not a replacement claim for the current FP inference proof profiles.
