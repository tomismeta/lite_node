# Inference Determinism Hardening

Status: working hardening plan, 2026-07-29.

This document separates the inference execution surface from the determinism
contract required before plain inference can be treated as validator-grade
execution. The execution surface says what the VM can run. The determinism
contract says what every node must compute bit-for-bit.

## CTO Finding

The current inference branch proves a model-neutral execution surface and local
candidate execution. It does not yet prove consensus-grade deterministic math.

That is a real engineering gap, not only a documentation gap. The branch has
bounded opcodes, capability gates, bytecode support, and many primitive tests.
It still relies on native floating-point operations and host math functions in
the active FP path. Fixed loop order and finite checks are necessary, but they
are not sufficient to make a floating-point inference primitive validator
portable.

## Existing VM Surface

The VM already has three relevant numerical surfaces:

| Surface | Representative opcodes | Current role |
| --- | --- | --- |
| Legacy host-float FP | `MATMUL_FP`, `RMSNORM_FP`, `SILU_FP`, `ARGMAX_FP`, `ATTENTION_KV_FP`, `ROPE_APPLY_FP`, `VECDOT_FP` | Classified as `Consensus_unsafe` by opcode policy. These are useful reference points, but they are not the plain inference consensus path. |
| Existing Q16 fixed point | `SOFTMAX_Q16_INPLACE`, `LAYERNORM_Q16_INPLACE`, `RMSNORM_Q16_INPLACE`, `SILU_Q16_INPLACE`, `ROPE_APPLY_Q16`, `ATTENTION_KV_Q16`, `VECDOT_Q16`, `ARGMAX_Q16` | Classified as `Program_only` and admitted by inference only under `tensor.fixed`. This is the closest existing deterministic math profile. |
| Inference proof surface | `LOAD_F32_LE_FP`, `LOAD_F64_LE_FP`, `LINEAR_Q1_G128_FP`, `SIGMOID_FP`, `SOFTPLUS_FP`, `SILU_FP`, `CAUSAL_DEPTHWISE_CONV1D_FP`, `GATED_DELTA_RULE_FP`, `RMSNORM_FP_EPS`, `L2NORM_FP`, `ELEMWISE_MUL_FP`, `RESIDUAL_ADD_FP`, `ROPE_APPLY_INDEXED_FP`, `ATTENTION_SCORES_FP`, `SOFTMAX_FP`, `ATTENTION_WEIGHTED_SUM_FP`, `ARGMAX_FP` | Selectively admitted by the inference harness under explicit capabilities. Opcode-scoped spine-ready profiles (`byte-ingress-f64-bits` for LOAD_F64, `deterministic-fp64-rmsnorm` for RMSNORM, `deterministic-q1-g128-fp64-linear`, `deterministic-fp64-comparison`, `deterministic-fp64-gated-delta` for GDN) are `consensus_ready` after dual-platform matrix with per-op punitive cases. LOAD_F32 (`byte-ingress-exact`), L2NORM (`deterministic-fp64-normalization`), and Softmax remain `consensus_candidate`. Host-trig/host-exp remain local-only. |

The existing `host_float_hit` policy mechanism is correct and should remain:
plain legacy/program admission still identifies host-floating-point opcodes as
unsafe. Inference admission is narrower, but that narrower gate does not itself
solve numerical determinism. Policy now exposes a separate
`uses_host_float_math` distinction so exact byte ingress can stay under the
broad consensus-unsafe program gate without being mislabeled as arithmetic.

## Numerical Profiles

The hardening path should use explicit numerical profiles. A capability name
does not prove deterministic math; the profile root must bind the math.

| Profile | Admission role | Purpose |
| --- | --- | --- |
| `host-fp-local-candidate` | Local and attested-only | Generic fallback for proof/demo primitives still relying on native host math. This profile is useful for fast engineering but not validator-portable. |
| `host-fp-exp-local-candidate` | Local and attested-only | Current proof/demo path for primitives still relying on native `exp`/`log1p`, including softmax, gated delta, sigmoid, softplus, and SiLU. This profile is useful for fast engineering but not validator-portable. |
| `host-fp-trig-local-candidate` | Local and attested-only | Current proof/demo path for indexed rotary primitives still relying on native exponentiation, `cos`, and `sin`. This profile is useful for fast engineering but not validator-portable. |
| `byte-ingress-exact` | Consensus candidate | Exact little-endian f32/f64 byte loading from authenticated ranges. |
| `deterministic-q1-g128-fp64-linear` | Consensus candidate | Q1-G128 projection with exact binary16 scale decode, pinned sign mapping, and deterministic binary64 accumulation order. |
| `deterministic-fp64-normalization` | Consensus candidate | RMSNorm/L2Norm finite binary64 reductions with explicit epsilon and deterministic sqrt/divide/output multiply policy. |
| `deterministic-fp64-elementwise` | Consensus candidate | Elementwise add/multiply over finite binary64 cells with aliasing and atomicity policy. |
| `deterministic-fp64-accumulation` | Consensus candidate | Finite binary64 multiply/add reductions for attention scores, weighted sums, and causal depthwise convolution. |
| `q16-exact` | First consensus candidate | Integer-defined fixed-point math using exact Q16 semantics where model quality allows it. |
| `soft-fp-exact` | Future consensus candidate | Software-defined floating point or table-driven math if Q16 changes model behavior too much. |

Every optimized kernel must reproduce its profile's scalar oracle bit-for-bit.
Threading, SIMD, caching, predecoding, and prepared layouts may change runtime
cost, but they cannot change reduction order, rounding, or output roots.

## Determinism Bar

Each inference primitive must have a stable semantic root derived from a
complete contract:

- input domain and shape rules;
- byte layout and typed memory interpretation;
- exact non-finite, subnormal, signed-zero, overflow, and underflow policy;
- exact arithmetic order;
- exact rounding behavior;
- exact transcendental function semantics;
- aliasing rules;
- effort formula;
- failure atomicity;
- output encoding; and
- executable scalar oracle plus golden and negative vectors.

For any primitive that uses `sqrt`, `exp`, `log1p`, `cos`, `sin`, exponentiation,
or native binary64 accumulation, the contract must either pin an acceptable
local-runtime profile or replace host math with software-defined deterministic
kernels.

## Priority Queue

| Priority | Primitive family | Why first | Required decision |
| --- | --- | --- | --- |
| P0 | `LINEAR_Q1_G128_FP` | Runtime hotspot and core compressed projection path. | Specify Q1 block format, fp16 scale decode, sign handling, accumulation order, and binary64 or fixed-point profile. |
| P0 | `RMSNORM_FP_EPS`, `L2NORM_FP` | Repeated throughout prompt prefill; reductions and `sqrt` now use LiteNode's deterministic finite binary64 core locally. | Bind exact oracle roots and cross-platform conformance before consensus promotion. |
| P0 | `SOFTMAX_FP` | Attention correctness and token distribution depend on it; max comparison is deterministic locally, but `exp` is still native. | Define exact exponential behavior or use deterministic fixed-point/table implementation. |
| P0 | `GATED_DELTA_RULE_FP` | Stateful recurrence; protocol-owned nonpositive exp and dual output/next-state writeback are matrix-qualified on `deterministic-fp64-gated-delta` (`consensus_ready`). | Keep dual-platform punitive evidence sealed; do not claim S5/devnet model-load or PVAC chat from this promote. |
| P1 | `ATTENTION_SCORES_FP`, `ATTENTION_WEIGHTED_SUM_FP` | Attention composition boundary; finite reductions without native exp/trig. | Keep under `deterministic-fp64-accumulation`; bind profile roots and cross-platform conformance before consensus-ready admission. |
| P1 | `ROPE_APPLY_INDEXED_FP` | Uses trigonometric functions and exponentiation. | Prefer table/indexed deterministic contract over host trig. |
| P1 | `SIGMOID_FP`, `SOFTPLUS_FP`, `SILU_FP` | Nonlinear activations. | Define deterministic exp/log1p behavior or lower to fixed-point profile. |
| P2 | `LOAD_F32_LE_FP`, `LOAD_F64_LE_FP`, `ARGMAX_FP`, `ELEMWISE_MUL_FP`, `RESIDUAL_ADD_FP`, `CAUSAL_DEPTHWISE_CONV1D_FP` | Lower mathematical risk or mostly data movement/vector arithmetic. | Keep the existing narrow candidate profiles; still need edge vectors, aliasing rules, failure atomicity, and cross-platform conformance. |

## First Qualification Corpus

`octra-inference` produced the first independent determinism corpus at source
commit `7ae5fcec302e9d896b65f5e9dee0e4833920588b`, artifact
`determinism-qualification-corpus-20260729-235210`.

That corpus is not normative, but it is the first concrete evidence intake for
this hardening plan:

| Evidence | Result |
| --- | --- |
| Bonsai schedule operations mapped | `18` |
| Independent scalar fixture cases emitted | `19` |
| Malformed/failure/atomicity cases emitted | `11` |
| Bonsai cutpoint comparisons | `32` |
| Q16.16 selected-token changes on the known logits cutpoint | `0` |
| First Q16.16 numeric-root divergence | `fixtures:expected-final-norm.f64le` |

The practical decision from this corpus is conservative:

| Recommendation | Operations |
| --- | --- |
| `q16-exact` viable | immutable range reads; argmax only when ordering preservation is proven |
| wider fixed point needed | residual add, elementwise multiply, attention weighted sum |
| deterministic FP64 candidate, still awaiting profile-root/cross-platform proof | Q1 projection, RMSNorm, L2Norm, residual add, elementwise multiply, causal depthwise convolution, attention scores, attention weighted sum |
| deterministic software transcendental still required | SiLU, sigmoid, softplus, gated delta rule, indexed RoPE, softmax |

This means the existing Q16 surface is useful, but it is not the broad answer
for Bonsai-class inference. The next LiteNode work should prioritize exact
software math or wider fixed-point contracts for the P0 operations rather than
forcing the whole path into Q16.16.

The latest ingestion corpus narrows the P0 intake to five minimized fixtures
and thirty-five failure/atomicity cases. It also shows the important ranking
warning: Q16.16 changes one synthetic near-tie token selection while Q32.32
does not. That does not reject fixed point categorically, but it does reject a
blanket Q16.16 migration for the P0 path.

## Q16 Strategy

Q16 is the most deterministic existing numerical substrate in the VM. It should
be used deliberately, not as an automatic replacement for every FP primitive.

Use Q16 when:

- the model/export path can tolerate the numerical profile;
- the operation already has a Q16 equivalent;
- the output can be compared against a Q16 oracle; and
- fixed-point precision loss is acceptable for the target.

Do not force Q16 when:

- the current model contract requires fp64/fp32-equivalent behavior;
- precision loss changes token selection;
- a Q16 opcode lacks the required shape or parameterization; or
- using Q16 would create a model-specific workaround.

The likely durable shape is therefore:

| Profile | Purpose |
| --- | --- |
| `tensor.fixed` / `q16-exact` | Deterministic fixed-point inference profile built on Q16 semantics. |
| `tensor.strict-fp` / `host-fp` | Local candidate / proof profile until every FP primitive has deterministic software math or a formally accepted host-runtime profile. |
| `soft-fp-exact` | Optional deterministic software-FP profile if fixed point is not numerically viable. |

Q16 itself is not automatically qualified. The current implementation is a good
starting substrate because it uses integer arithmetic, but the profile still
needs punitive vectors. In particular, existing Q16 RMSNorm uses an implicit
epsilon of one Q16 unit, which is about `1.53e-5`; that does not exactly encode
Bonsai's `1e-6` RMSNorm contract without a new explicit-epsilon fixed-point
semantic.

## Minimal Engineering Path

1. Freeze new inference opcodes unless a generated schedule proves there is no
   composable alternative.
2. Root the numerical profiles and require exact root matching at admission.
3. Write P0 primitive determinism contracts before further performance work.
4. Generate scalar oracle fixtures outside the optimized VM implementation.
5. Add a conformance runner that executes the same vectors through VM runtime,
   oracle, and `octra-inference` reference emission.
6. Gate accepted artifacts through `--require-consensus-candidate` before any
   validator-readiness discussion, so local-only host math cannot hide behind
   matching output roots.
7. Run conformance on at least Linux x86_64, macOS arm64, and release/debug
   builds before any validator-readiness claim.
8. Only after P0 conformance passes, decide whether to optimize FP kernels or
   move more of the path onto Q16/fixed-point semantics.

## Immediate Work Split

LiteNode should own:

- semantic contracts under this document;
- VM scalar oracle or independent reference module for admitted primitives;
- conformance runner;
- failure atomicity and edge-case VM tests;
- policy that continues to keep generic host-float opcodes unsafe.

The current P0 runtime audit is tracked in
`docs/architecture/inference-p0-determinism-audit.md`.

`octra-inference` should own:

- model-shaped and synthetic fixture generation;
- independent scalar reference outputs;
- punitive edge-case corpus for each primitive;
- cross-profile comparison between FP candidate and Q16 where applicable; and
- proof bundles that never become the normative source of semantics.

## Claim Boundary

Until the hardening path lands:

```text
Execution surface: implemented locally.
Model-neutral architecture: implemented locally.
Candidate VM inference proof: demonstrated locally.
Consensus-grade deterministic math: open.
Devnet validator readiness: blocked on determinism conformance.
```
