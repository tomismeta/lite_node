# Inference Devnet Readiness

Status: readiness checkpoint, 2026-07-30.

This document is the short operational view of where the inference VM stands.
It does not define new semantics. Runtime authority remains in
`inference-runtime.md`, session/root authority remains in
`inference-runtime-contracts.md`, performance authority remains in
`inference-fast-path-runtime.md`, and deterministic-math authority remains in
`inference-determinism-hardening.md`.

## Current Position

LiteNode has a model-neutral local inference execution surface. Bonsai is the
qualification model, not a VM special case.

The current branch has demonstrated:

- rooted model deployment and model-neutral target/session inputs;
- authenticated range reads through `FLOAD`;
- local VM execution for prompt-to-token proof stages;
- `ARGMAX_FP` selected-index token authority;
- batch execution for repeated proof stages;
- opcode timing sufficient to identify `LINEAR_Q1_G128_FP` as the current
  recurrent hotspot;
- P0 and P0-plus conformance ingestion from independent `octra-inference`
  fixtures; and
- explicit top-k boundary: top-k evidence is reference-only unless a future
  `TOPK_FP` primitive is proposed and accepted.

This is not yet devnet validator readiness. The open blocker is validator-grade
deterministic math for the admitted host-FP inference profile, plus product
session/runtime packaging.

## Evidence Of Record

| Gate | Artifact | LiteNode status |
| --- | --- | --- |
| P0 schema, positive execution, exact effort, failure/atomicity | `/home/exedev/evidence/octra-inference/determinism-ingestion-corpus-effort-authority-20260730-014647` | accepted |
| P0-plus execution | `/home/exedev/evidence/octra-inference/determinism-p0-plus-corpus-20260730-022653` | accepted `9/9` |
| P0-plus top-k boundary | `/home/exedev/evidence/octra-inference/determinism-p0-plus-topk-boundary-20260730-025306` | accepted `9/9` |
| Recurrent-heavy performance fixture on rebased LiteNode | `/home/exedev/evidence/octra-inference/litenode-recurrent-heavy-reemitted-2ca5dfd-754b0a5-20260729` | accepted |

Latest producer checkpoints consumed by LiteNode:

```text
808d60370898ef71800ea6cea960b0f30a9592b4 Add P0 effort and observational authority
426d86e8c84664f4b8b6b3ba76d6f517960ef4b9 Add P0-plus determinism corpus
cffcf90eb741d7c56bb9c984711a1232107e6da1 Classify P0-plus top-k evidence
ee6180db7df5585e4e6558ef8505f9d5b0c80073 Record accepted P0-plus top-k boundary
```

## Readiness Gates

| Readiness level | Status | Required evidence |
| --- | --- | --- |
| Local correctness demo | Met | Prompt-to-token candidate proof with LiteNode-selected token and rooted outputs. |
| Local performance demo | Partially met | Batch mode and opcode timing exist; full prompt runtime remains too slow for product use. |
| Share-with-Octra-devs architecture review | Met with caveats | One-VM architecture, model-neutral primitive surface, P0/P0-plus conformance gates, and explicit claim boundaries are documented. |
| Mergeable runtime branch | Not met | Needs footprint reduction, rebased patch review, CI gates, and removal or quarantine of evidence-only tooling as appropriate. |
| Devnet candidate | Not met | Requires deterministic math profile, target-owned prefill/decode session, resource scheduling, replay/restart evidence, and multi-node conformance. |
| Validator-ready inference | Not met | Requires protocol-owned arithmetic, multi-platform conformance, deterministic receipts, and agreed resource policy. |

## Devnet Blockers

1. Deterministic math profile.
   Current inference FP opcodes still use host `float`, `sqrt`, `exp`, `cos`,
   `sin`, or binary64 accumulation. P0/P0-plus fixtures prove current LiteNode
   agreement with producer fixtures; they do not make host math
   validator-portable.

2. Target-owned session program.
   The proof path can chain admitted stages. Product runtime needs one
   target-owned prefill/decode lifecycle:

   ```text
   open_session -> prefill -> decode -> ARGMAX_FP -> receipt -> finalize
   ```

3. Runtime performance.
   The current local proof mode is correctness-first. Batch execution improved
   harness overhead, but full prompt runtime still needs resident execution,
   cached authenticated ranges, and measured kernel optimization.

4. Multi-platform conformance.
   P0 and P0-plus must run across the intended validator platforms and build
   modes before a consensus claim.

5. Resource isolation.
   Inference must remain outside consensus proposal, epoch apply, finality, and
   private-execution critical paths, with explicit memory/effort/concurrency
   ceilings.

## Next Engineering Sequence

1. Keep P0 and P0-plus fixtures immutable.
2. Add a deterministic arithmetic profile decision for P0:
   - exact software FP, or
   - wider fixed point where quality evidence supports it, or
   - explicitly local-only host-FP profile.
3. Wire conformance into CI for schema, positive execution, strict effort, and
   counted failure/atomicity.
4. Build the target-owned prefill/decode session path; do not continue relying
   on caller-visible layer orchestration as product runtime.
5. Re-run one Bonsai prompt-to-token proof under the target-owned session shape.
6. Re-run recurrent-heavy and logits-tail performance gates against that shape.
7. Only then prepare the mergeable branch by reducing evidence-only scaffolding
   and isolating conformance tooling.

## What Not To Claim

- Do not claim devnet readiness from local host-FP roots.
- Do not claim LiteNode owns top-k ranking; current authority is selected
  index through `ARGMAX_FP`.
- Do not claim the VM is Bonsai-specific. The VM surface is generic; Bonsai is
  the first qualification workload.
- Do not treat Q16 as qualified for the full path without preserving token
  selection and numerical bounds under independent vectors.
- Do not add fused layer/model opcodes before composed primitives and measured
  performance prove they are necessary.

## Current Summary

The inference VM is structurally on the right path and has meaningful local
execution evidence. The next decisive step is not more demo breadth. It is
turning the accepted execution surface into a deterministic arithmetic profile
and a target-owned session lifecycle. Without those two, the branch remains a
strong local candidate runtime rather than a devnet-ready inference VM.
