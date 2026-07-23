# Inference Capability Requirements

Status: working design input, 2026-07-21.

This document records the compute requirements discovered while qualifying
Bonsai 27B. It is not a promise to add one opcode per row, and it is not a
permanent model checklist.

Git records changes to this inventory. Capability names do not carry manual
version suffixes. Once a capability becomes protocol-visible, its exact
semantics are identified by a content root.

## Purpose

Bonsai is the first design partner because it exercises compressed linear
algebra, attention, recurrent state, and mixed sequence operations. The model
is a demanding acceptance case, not the owner of LiteNode's instruction set.

Every model requirement receives one of four dispositions:

- **existing**: an existing operation already has the required semantics;
- **primitive**: a new model-neutral VM operation is justified;
- **composition**: generated target code composes existing primitives; or
- **implementation**: the concern belongs below an existing semantic boundary.

No proposed operation is accepted until its scalar semantics, bounds, aliasing,
effort, rollback, and conformance are complete.

## Guiding Principles

1. Express mathematical algorithms, not model layers.
2. Prefer composition until atomicity or measured transition cost justifies a
   fused semantic operation.
3. Keep layouts and kernel selection below the opcode boundary.
4. Keep model-family positions, grouping, routing, and tensor names in the
   generated target.
5. A full layer should use a small number of batched operations, not scalar VM
   microcode and not one opaque layer opcode.
6. Stabilize a capability only after a non-model-specific synthetic fixture
   exercises it.
7. Treat the second structurally different model as a release gate, not an
   optional demonstration.

## Tracking Summary

This table is planning metadata, not a protocol definition. The phase is the
earliest roadmap phase in which the semantic disposition can be accepted.

| Need | Disposition | Capability family | Phase |
| --- | --- | --- | --- |
| Q1-G128 linear | primitive | `tensor.q1-g128` | 4 |
| Q1-G128 gather | conditional primitive | `tensor.q1-g128` | 4 |
| little-endian F32 load | primitive | `storage.authenticated-range` | 3 |
| RMS normalization with epsilon | primitive | `tensor.strict-fp` | 3 |
| L2 normalization | conditional primitive | `tensor.strict-fp` | 4 |
| sigmoid | primitive | `tensor.strict-fp` | 3 |
| softplus | primitive | `tensor.strict-fp` | 3 |
| SiLU | primitive | `tensor.strict-fp` | 3 |
| causal depthwise convolution | primitive | `sequence.causal-convolution` | 3 |
| causal convolution with retained state | conditional primitive | `sequence.causal-convolution` | 4 |
| gated delta-rule update | candidate primitive | `sequence.delta-rule` | 4 |
| interleaved multi-axis RoPE | composition | `tensor.attention` | 3 |

`tensor.strict-fp` remains a proof-harness capability. Plain inference admission
only accepts explicitly enumerated proof opcodes under that capability; it does
not open the existing host-floating-point family. Default node support should
not advertise `tensor.strict-fp` until the numerical root and replay gate are
ready for production use.

`tensor.q1-g128` now has a proof-branch scalar fixture and VM opcode for the
linear case. That is sufficient for the next direct Bonsai primitive canary.
It is not yet a release gate: binary64 accumulation still needs
cross-platform conformance, or replacement with software-defined fixed
arithmetic, before being treated as consensus-ready.

The inventory closes at the Phase 0 design gate. New rows require evidence from
a new model or a previously unrepresented generated schedule; they are not
added as speculative VM features.

## Compressed Linear Algebra

### Q1-G128 linear projection

Legacy label: `linear_q1_0_g128_fp`.

Disposition: **primitive**.

The VM needs a generic matrix operation over a precisely specified Q1-G128
encoding. The contract defines block bytes, scale decoding, row order,
accumulation order, shape rules, offsets, output ownership, and effort.

Proof-branch status: `LINEAR_Q1_G128_FP` implements the
`linear_q1_0_g128_fp` scalar fixture contract under capability
`tensor.q1-g128`. This is the narrow demo primitive for the next canary, not a
blanket admission of `tensor.strict-fp`, and not an ordinary Program opcode
outside inference admission.

Physical block-major layouts, row bundling, worker pools, SIMD, and lookup
tables are implementations of the same operation. They do not create new
capabilities or model identities.

### Q1-G128 gather

Legacy label: `q1_gather_g128_fp`.

Disposition: **primitive**, if the generated embedding/output schedule requires
compressed rows without materializing a dense matrix.

The operation is generic indexed row selection over the same rooted Q1
encoding. Index bounds, duplicate indices, output order, effort, and no-partial
write behavior require independent vectors.

The Q1 capability may contain both linear and gather operations. A capability
is a coherent semantic family, not necessarily one opcode.

## Typed Data Ingress

### Little-endian F32 load

Legacy label: `load_f32_le_fp`.

Disposition: **primitive**.

This is storage and memory ingress rather than mathematics. It decodes bounded
little-endian IEEE-754 bit patterns from an authenticated byte range into typed
tensor memory. It rejects malformed spans and disallowed non-finite values
before writing.

Filesystem paths, model tensor names, and store transport never cross this
boundary.

Proof-branch status: `LOAD_F32_LE_FP` decodes bounded little-endian f32 owner
bytes into VM f64 cells, rejects malformed spans and non-finite values before
writing, and remains gated by `storage.authenticated-range` in plain inference.
Generic Program admission still rejects it as inference-only.

## Normalization And Activation

### RMS normalization with explicit epsilon

Legacy label: `rmsnorm_fp_eps`.

Disposition: **primitive**.

The existing fixed epsilon is insufficient for generated targets that declare
a different numerical contract. Epsilon is therefore explicit and governed by
the active numerical profile. The existing `RMSNORM_FP` is not a substitute:
it bakes in a different epsilon and a different multiplication order, so it
remains forbidden in plain inference even under `tensor.strict-fp`.

Proof-branch status: `RMSNORM_FP_EPS` is a four-register in-place vector
operation: address, count, gamma address, and an integer register carrying the
binary64 epsilon bits. Row batching stays in generated target code by invoking
the same vector primitive per row.

### L2 normalization

Legacy label: `l2norm_fp`.

Disposition: **primitive** when required by the generated Q/K or state schedule.

L2 normalization is not substituted with RMS normalization. Its axis, epsilon
placement, zero-vector behavior, operation order, and row-batched form are
explicit.

### Sigmoid

Legacy label: `sigmoid_fp`.

Disposition: **primitive** when the generated schedule consumes the standalone
activation result.

The fact that SiLU computes a sigmoid internally does not expose the sigmoid
result. A standalone operation is justified only when generated code consumes
that result independently. Algebraic recovery from SiLU is rejected because it
changes zero handling and numerical semantics.

Proof-branch status: `SIGMOID_FP` implements the schedule frontier scalar
contract as an in-place bounded f64 vector operation. It is admitted only by
the inference policy when `tensor.strict-fp` is present; generic Program
admission still rejects it as consensus unsafe.

### Softplus

Legacy label: `softplus_fp`.

Disposition: **primitive** when the generated schedule consumes the standalone
activation result.

Softplus becomes a standalone operation only when target composition needs its
output outside a larger accepted transition. Its stable piecewise evaluation
and overflow behavior belong to the numerical profile.

Proof-branch status: `SOFTPLUS_FP` implements the schedule frontier scalar
contract using the stable branch `x + ln1p(exp(-x))` for positive inputs and
`ln1p(exp(x))` otherwise. It is admitted only by the inference policy when
`tensor.strict-fp` is present; generic Program admission still rejects it as
consensus unsafe.

### SiLU

Legacy label: `silu_fp`.

Disposition: **primitive** when generated code composes it with another accepted
operation.

Proof-branch status: `SILU_FP` now uses the same bounded, candidate-write f64
activation path as sigmoid and softplus. It is admitted only by the inference
policy when `tensor.strict-fp` is present; generic Program admission still
rejects it as consensus unsafe.

If sigmoid, softplus, or SiLU is used only inside an accepted delta-rule
transition, its semantics may remain part of that transition instead of adding
another opcode.

## Sequence Computation

### Causal depthwise convolution

Legacy labels: `ssm_conv_silu_fp`, `causal_conv_with_state_fp`.

Disposition: **primitive**, with activation composed separately.

The operation performs a bounded stateless depthwise causal convolution. Input
values are row-major by timestep and channel; kernel values are channel-major by
channel and kernel offset. The source spans are snapshotted before the
destination is written, so destination/input and destination/kernel aliases are
well-defined. Kernel width, channel count, bounds, non-finite handling, overlap,
and effort are explicit.

Proof-branch status: `CAUSAL_DEPTHWISE_CONV1D_FP` implements the scalar fixture
for the order-3 Bonsai frontier. `SILU_FP` composes after it under
`tensor.strict-fp`, and the composed canary admits, runs, and matches roots. The
VM does not add `SSM_CONV_SILU_FP`; a fused operation or retained-state variant
requires future evidence that it is a reusable semantic boundary rather than a
generated schedule convenience.

### Gated delta-rule state transition

Legacy label: `gated_delta_net_step_fp`.

Disposition: **profiled primitive**.

A named published algorithm is not automatically model-specific. The accepted
boundary must nevertheless be a parameterized mathematical state transition,
not a Qwen or Bonsai layer. Its grouping, gating, normalization, update order,
state ownership, in-place behavior, and rollback are completely specified.

The opcode name describes the reusable delta-rule transition rather than a
network product: `GATED_DELTA_RULE_FP`. It consumes prepared q/k/v,
log-decay, beta, and recurrent-state tensors, writes recurrent output and next
state, and requires `sequence.delta-rule`.

Order-4 frontier status: `octra-inference` produced a scalar contract, five
tiny golden fixtures, negative cases, and an actual Bonsai/Qwen35 layer-0
binding. LiteNode implements the primitive behind inference admission only.
The local OCaml VM matches all five golden fixtures and, when run against the
VPS evidence bundle, matches the Bonsai recurrent output and next-state
SHA-256 values. Q/K normalization, head preparation, projections, and model
gate preparation stay outside the primitive.

## Position And Attention

### Interleaved multi-axis rotary application

Legacy label: `imrope_apply_fp`.

Disposition: **composition**.

Interleaving and multi-axis position construction are model-schedule rules.
The generated target builds the per-pair position vector and invokes a generic
indexed rotary operation. Gating and attention remain separate generic
operations.

The temporary fused IMRoPE/gated-attention operation is not part of the target
architecture. Prior evidence showed the generic composition could preserve the
accepted output and internal roots.

### Existing attention operations

Disposition: **audit before reuse**.

`ATTENTION_KV_FP` and `ATTENTION_KV_Q16` already exist, but existing status does
not exempt them from the primitive admission rule. The audit must make scaling,
masking, GQA/MQA grouping, causal/window bounds, empty context, accumulation
order, and non-finite behavior explicit.

If those semantics cannot remain broadly parameterized and testable, attention
should be decomposed into a small number of batched score, mask, softmax, and
weighted-sum operations. It must not become a model-specific mega-operation.

The Phase 4 attention audit has a binary outcome: fully specify the existing
generic operation or select the decomposition before Bonsai qualification.

## Supporting Tensor Operations

Generated schedules also require ordinary model-neutral support:

- bounded two-dimensional copy;
- elementwise multiply and residual add;
- matrix/vector products;
- indexed and contiguous gathers;
- KV or recurrent-state append;
- argmax or explicitly declared token selection; and
- row-batched normalization where it reduces caller-visible VM transitions.

Token selection remains target-owned. The request binds generation policy and
any deterministic seed; the target consumes them through generic selection
operations. The first Bonsai target may use argmax. LiteNode does not apply a
separate scheduler or RPC sampling policy to unrooted logits.

Existing operations are reused only after bounds, memory authority, effort,
non-finite, aliasing, and rollback audits. Being present in the current VM is
not itself conformance.

Proof-branch status: `ELEMWISE_MUL_FP` and `RESIDUAL_ADD_FP` now have strict
finite reads, checked large spans, exact in-place alias support,
partial-overlap rejection, and candidate writes. The dynamic effort tariffs are
`3 * count` for multiply and `2 * count` for residual add. They are admitted
only by the inference policy when `tensor.strict-fp` is present.

Primitive specifications record analytic shape scaling before effort
coefficients are chosen. Compressed linear work scales from declared output
rows and encoded inner blocks; normalization and activation scale from declared
elements; attention and state transitions use their explicit head, position,
and state dimensions. The effort schedule owns exact coefficients and checked
formulas.

## Capability Families

The likely requirement surface is intentionally coarser than the legacy list:

- `tensor.fixed`;
- `tensor.strict-fp`;
- `tensor.q1-g128`;
- `tensor.attention`;
- `sequence.causal-convolution`;
- `sequence.delta-rule`; and
- `storage.authenticated-range`.

`tensor.strict-fp` remains a roadmap family and proof-harness capability, not
a default plain-runtime capability.

These names are descriptive handles. The semantic root, not a number appended
to the name, identifies the exact accepted definition.

A target declares only the families and roots it actually needs. Admission
also verifies the concrete opcode inventory, so a broad family name cannot
silently authorize unrelated behavior.

## Stabilization Gates

A capability remains provisional until:

1. its scalar contract and effort formula are written;
2. malformed, bounds, overlap, non-finite, and rollback vectors pass;
3. bytecode, assembler, compiler, verifier, policy, and type flow agree;
4. scalar and optimized implementations match under the declared numerical
   profile;
5. a synthetic fixture exercises it without model-family names or data;
6. Bonsai qualification proves the real target need; and
7. a structurally different model or schedule proves the boundary is reusable
   before network activation.

Semantic qualification and performance qualification are separate. A slow
reference operation may be semantically complete while its optimized kernel is
still under development.

## Current Scope

The old ten-item inventory successfully identified real Bonsai compute gaps.
It does not define the implementation count:

- Q1 linear and gather form one compressed semantic family;
- sigmoid, softplus, and SiLU may remain internal to another accepted transition;
- causal convolution composes with SiLU;
- IMRoPE remains generated composition; and
- prepared layouts and native kernels stay below semantic operations.

The first implementation should therefore be driven by the rooted generated
target and these dispositions, not by a requirement to reproduce ten old names.
After Phase 0, this inventory changes only when new model evidence demonstrates
a missing semantic family.
