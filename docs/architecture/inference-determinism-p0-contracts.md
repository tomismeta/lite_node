# Inference P0 Determinism Contracts

Status: implementation worklist, 2026-07-30.

This document defines the first LiteNode-side contracts to write after the
initial determinism qualification corpus. It is scoped to P0 math only. It does
not add opcodes and it does not make current inference math validator-ready.

## Contract Shape

Every P0 primitive contract must specify:

- operand registers and memory spans;
- typed input encoding;
- exact shape limits;
- aliasing policy;
- failure checks and failure atomicity;
- arithmetic domain;
- operation order;
- rounding behavior;
- non-finite, signed-zero, subnormal, overflow, and underflow policy;
- output encoding;
- effort formula; and
- conformance vector identity.

The contract must be executable by an independent scalar oracle. Optimized VM
code must be tested against that oracle, not against itself.

## P0.1 Q1-G128 Projection

Current opcode: `LINEAR_Q1_G128_FP`.

Current status: `deterministic-q1-g128-fp64-linear` consensus candidate.
Runtime hotspot.

Required contract decisions:

| Topic | Decision required |
| --- | --- |
| Q1 block bytes | Exact byte layout, row order, group size, and offset rules. |
| Scale decode | Exact fp16/binary16 decode policy, including zeros, subnormals, infinities, and NaNs. |
| Weight decode | Exact sign mapping and zero handling for packed one-bit weights. |
| Accumulation | Fixed inner-loop order and accumulator domain. |
| Output | Exact output cell encoding and finite-result policy. |
| Failure | Reject malformed ranges, bad shape, invalid scale, overflow, and output aliasing before write. |
| Profile decision | Exact integer binary16 scale decode plus deterministic binary64 accumulator. Q16.16 is not sufficient per first corpus. |

Minimum vectors:

- ordinary two-row projection;
- signed-zero/sub-Q16 scale case;
- malformed `k % 128 != 0`;
- invalid scale payload;
- aliasing output/input;
- near-tie logits after LM head.

## P0.2 Explicit-Epsilon Normalization

Current opcodes: `RMSNORM_FP_EPS`, `L2NORM_FP`.

Current status: `deterministic-fp64-normalization` consensus candidate.
Existing Q16 RMSNorm has implicit epsilon of one Q16 unit and does not exactly
represent Bonsai's `1e-6` contract.

Required contract decisions:

| Topic | Decision required |
| --- | --- |
| Epsilon | Exact binary/input representation and positivity check. |
| Reduction | Fixed sum-of-squares order and accumulator domain. |
| Square root | Deterministic software `sqrt` or fixed-point inverse-square-root semantics. |
| Gamma | Exact gamma read, aliasing, and multiplication order for RMSNorm. |
| Output | Signed-zero, subnormal, overflow, and finite-result policy. |
| Failure | Reject invalid spans, invalid epsilon, non-finite inputs, and unsafe aliasing before write. |

Minimum vectors:

- ordinary RMSNorm with explicit epsilon;
- ordinary L2Norm with explicit epsilon;
- near-zero denominator;
- signed-zero/subnormal inputs;
- reduction-order stress;
- negative epsilon rejection;
- gamma alias rejection.

## P0.3 Softmax

Current opcode: `SOFTMAX_FP`.

Current status: `host-fp-exp-local-candidate`. High-risk because token
probabilities and attention weights still depend on native `exp`; max
selection, shifted-score nonpositive gating, summation, and division use
LiteNode's deterministic finite binary64 core locally.

Required contract decisions:

| Topic | Decision required |
| --- | --- |
| Max subtraction | Deterministic finite binary64 max scan order and tie behavior. |
| Exponential | Deterministic `exp` approximation or table semantics; current local runtime first requires every shifted score to compare `<= +0.0`. |
| Sum | Fixed accumulation order and accumulator width/domain. |
| Division | Exact probability rounding and normalization rule. |
| Output | Sum behavior, signed-zero policy, and finite-result policy. |
| Failure | Reject empty spans, non-finite inputs, invalid aliasing, and zero denominator before write. |

Minimum vectors:

- ordinary attention softmax;
- near-tie scores;
- large positive/negative scores;
- all-equal scores;
- all invalid/non-finite rejection;
- output/input same-range in-place case if allowed.

## P0.4 Gated Delta Rule

Current opcode: `GATED_DELTA_RULE_FP`.

Current status: `host-fp-exp-local-candidate`. Stateful recurrence; drift
compounds across layers and tokens. The current local runtime rejects positive
finite `log_decay` before mutation and accepts finite nonpositive values,
including negative zero.

Required contract decisions:

| Topic | Decision required |
| --- | --- |
| State layout | Exact recurrent state shape, grouping, and memory order. |
| Inputs | Exact q/k/v/log-decay/beta/gate interpretation and shape checks. |
| Update order | Fixed loop nesting and state update order. |
| Math | Deterministic nonlinear and normalization semantics used inside the transition; current native decay `exp` is gated to finite nonpositive `log_decay`. |
| Atomicity | Output and next-state writes must be all-or-nothing. |
| Aliasing | Explicit allowed and rejected overlap patterns. |
| Effort | Effort formula must bind state size and transition work. |

Minimum vectors:

- zero-state one-step;
- nonzero-state one-step;
- positive `log_decay` atomic rejection;
- negative-zero `log_decay` acceptance;
- multi-step state carry;
- invalid shape;
- unsafe overlap;
- non-finite input;
- insufficient effort;
- rollback proof for output and state spans.

## P0 Execution Order

1. Land the contract text and independent oracle vectors.
2. Add a LiteNode conformance runner that consumes the minimized P0 fixture
   pack from `octra-inference`.
3. Run current VM inference opcodes against the vectors and record failures as
   local-profile behavior.
4. Implement deterministic software-FP or wider fixed-point kernels one P0
   primitive at a time.
5. Preserve existing opcode policy until every P0 primitive has a semantic root
   and conformance gate.

The current implementation audit and conformance template schema are in
`docs/architecture/inference-p0-determinism-audit.md`.

## Corpus Intake Tool

LiteNode includes a diagnostic corpus consumer:

```text
tools/inference_determinism_check.exe --artifact-dir <determinism-corpus-dir>
```

The tool does not execute math and does not make the corpus normative. It
validates the producer-side corpus shape, checks P0 operation and fixture
coverage, and emits a machine-readable P0 worklist for LiteNode engineering.

The checker accepts both the initial qualification corpus and the newer
ingestion corpus. The latest accepted ingestion readout was:

```text
corpus_type = determinism_ingestion_corpus
fixture_count = 5
failure_case_count = 35
cutpoint_count = 48
logit_cutpoint_count = 2
selected_token_changes_under_q16_16_candidate = 1
P0 worklist = LINEAR_Q1_G128_FP, RMSNORM_FP_EPS, L2NORM_FP, SOFTMAX_FP, GATED_DELTA_RULE_FP
```

## Non-Goals

- No Bonsai-specific opcode.
- No model names in VM semantics.
- No devnet-readiness claim from local or candidate proof mode.
- No performance optimization before semantic conformance.
- No blanket Q16 migration without token-ranking evidence.
