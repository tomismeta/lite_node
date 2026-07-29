# Octra Inference Determinism Agent Brief

Status: handoff ask, 2026-07-29.

This brief is for the `octra-inference` agent. It keeps the repo boundary
clean: `octra-inference` produces independent evidence and fixtures; LiteNode
owns VM semantics, admission, and execution.

## Goal

Produce the independent determinism qualification corpus needed to harden the
LiteNode inference VM from local candidate execution toward validator-grade
math.

Do not add demos in this pass. Do not propose new VM opcodes unless the mapping
work proves an operation cannot be expressed by an existing deterministic
surface.

## Context

The LiteNode fork currently has:

- legacy host-FP opcodes such as `MATMUL_FP`, `RMSNORM_FP`, `SILU_FP`,
  `ARGMAX_FP`, `ATTENTION_KV_FP`, `ROPE_APPLY_FP`, and `VECDOT_FP`;
- existing Q16 fixed-point opcodes such as `SOFTMAX_Q16_INPLACE`,
  `LAYERNORM_Q16_INPLACE`, `RMSNORM_Q16_INPLACE`, `SILU_Q16_INPLACE`,
  `ROPE_APPLY_Q16`, `ATTENTION_KV_Q16`, `VECDOT_Q16`, and `ARGMAX_Q16`;
- inference proof FP opcodes such as `LINEAR_Q1_G128_FP`,
  `RMSNORM_FP_EPS`, `L2NORM_FP`, `GATED_DELTA_RULE_FP`,
  `ROPE_APPLY_INDEXED_FP`, `ATTENTION_SCORES_FP`, `SOFTMAX_FP`,
  `ATTENTION_WEIGHTED_SUM_FP`, and `ARGMAX_FP`.

LiteNode policy still classifies generic host-FP opcodes as unsafe outside the
local inference proof path. Q16 is the closest existing deterministic substrate,
but it is not yet fully qualified for Bonsai.

## Deliverable 1: Operation Mapping

Emit a machine-readable mapping for every Bonsai schedule operation:

```json
{
  "operation": "<schedule operation>",
  "current_host_fp_path": "<opcode or composition>",
  "q16_exact_candidate": "<opcode or composition or null>",
  "requires_new_deterministic_semantic": true,
  "reason": "<short reason>",
  "risk": "p0|p1|p2"
}
```

Classify each operation as one of:

- existing Q16 opcode;
- composable Q16 sequence;
- host-FP/local-only opcode;
- deterministic software-FP required;
- genuinely missing deterministic operation.

## Deliverable 2: Independent Scalar Vectors

Generate independent scalar fixtures for:

- Q1-G128 projection;
- explicit-epsilon RMSNorm;
- explicit-epsilon L2Norm;
- softmax;
- attention scores;
- attention weighted sum;
- indexed RoPE;
- gated delta rule;
- final argmax;
- f32/f64 ingress edge cases.

Each fixture family should include:

- ordinary model-shaped cases;
- tiny cases;
- max-bound cases where feasible;
- near-zero denominator cases;
- near-tie logits;
- signed-zero cases where applicable;
- subnormal, infinity, and NaN rejection cases;
- malformed shape/range cases;
- aliasing cases;
- failure-atomicity cases.

The scalar vectors must be independent from LiteNode's optimized VM
implementation. Rust scalar code in `octra-inference` is acceptable if it is
simple, explicit, and does not call LiteNode implementation code.

## Deliverable 3: FP vs Q16 Differential

For the current Bonsai prompt trace, compare the local host-FP candidate path
against Q16 or fixed-point candidates where possible.

Return:

- per-layer and per-cutpoint roots;
- max absolute error;
- max relative error;
- token-ranking changes;
- selected-token changes;
- first divergent layer/cutpoint;
- recommendation per operation:
  - `q16-exact viable`;
  - `wider fixed point needed`;
  - `deterministic software FP required`;
  - `host-fp local only`.

## Deliverable 4: Rerun Package

Return an artifact directory containing:

- operation mapping JSON;
- scalar vector manifests;
- fixture bytes;
- expected roots;
- negative case manifests;
- exact rerun commands;
- summary report;
- recommended LiteNode P0 worklist.

## First Corpus Status

The first corpus was produced at `octra-inference` commit
`7ae5fcec302e9d896b65f5e9dee0e4833920588b` as
`determinism-qualification-corpus-20260729-235210`.

It established:

- `18` Bonsai schedule operations mapped;
- `19` independent scalar fixture cases;
- `11` malformed/failure/atomicity cases;
- `32` Bonsai cutpoint comparisons;
- Q16.16 preserved the selected token for the known logits cutpoint;
- Q16.16 diverged immediately at numeric roots, first at
  `fixtures:expected-final-norm.f64le`.

The resulting recommendations are:

- `q16-exact` viable: immutable range reads; argmax only if ordering is proven;
- wider fixed point needed: residual add, elementwise multiply, attention
  weighted sum;
- deterministic software FP required: Q1 projection, explicit-epsilon RMSNorm,
  L2Norm, nonlinear activations, gated delta rule, indexed RoPE, attention
  scores, and softmax.

## Follow-Up Ask

The next `octra-inference` pass should strengthen the corpus rather than
broaden demos:

1. Add per-fixture input/output byte manifests suitable for direct LiteNode
   conformance ingestion.
2. Add exact oracle pseudocode or Rust snippets for every P0 primitive case.
3. Split fixed-point differential results by representation:
   - Q16.16;
   - candidate wider fixed point if available;
   - host-FP reference.
4. Add top-k ordering deltas for logits, not only selected-token deltas.
5. Return a P0-only minimized fixture pack for LiteNode CI.

## Constraints

- Do not modify LiteNode.
- Do not put Bonsai, Qwen, GGUF, HuggingFace, tokenizer family, tensor names,
  or layer names into VM target fields.
- Do not create a model-specific VM shortcut.
- Do not treat Q16 as qualified until vector evidence proves it.
- Do not treat host-FP as validator-ready.

## Copy/Paste Ask

```text
Pause new demos and opcode proposals. Produce the independent determinism
qualification corpus for LiteNode.

1. Map every Bonsai schedule operation to:
   - existing Q16 opcode,
   - composable Q16 sequence,
   - existing host-FP/local-only opcode,
   - deterministic software-FP required, or
   - genuinely missing deterministic operation.

2. Generate independent scalar vectors for:
   - Q1-G128 projection
   - explicit-epsilon RMSNorm
   - explicit-epsilon L2Norm
   - softmax
   - attention scores
   - attention weighted sum
   - indexed RoPE
   - gated delta rule
   - final argmax
   - f32/f64 ingress edge cases

3. Include ordinary, boundary, overflow, underflow, malformed, aliasing,
failure-atomicity, signed-zero, non-finite rejection, and near-tie-logit cases.

4. Compare host-FP against Q16/fixed-point candidates for Bonsai cutpoints and
report exact roots, max absolute error, max relative error, token-ranking
changes, selected-token changes, and first divergent layer/cutpoint.

5. Do not modify LiteNode. Do not add model-specific VM concepts. Return a
machine-readable operation mapping, fixture corpus, rerun commands, and a
recommendation for each operation: q16-exact viable, wider fixed point needed,
deterministic software FP required, or host-fp local only.
```
