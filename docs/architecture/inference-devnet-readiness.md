# Inference Devnet Readiness

Status: readiness checkpoint, 2026-08-01.

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
- P0 conformance ingestion from independent `octra-inference` fixtures;
- P0-plus diagnostic ingestion with the current `SOFTMAX_FP`
  wide-tail portability gap isolated; and
- explicit top-k boundary: top-k evidence is reference-only unless a future
  `TOPK_FP` primitive is proposed and accepted.
- a product-facing prefill session bundle shape with rooted open, prefill,
  decode, finalize, and receipt-chain identities.

This is not yet devnet validator readiness. The open blocker is validator-grade
deterministic math for the admitted host-FP inference profile, plus product
session/runtime packaging.

## Evidence Of Record

| Gate | Artifact | LiteNode status |
| --- | --- | --- |
| P0 positive execution and effort authority | `/home/exedev/evidence/octra-inference/determinism-ingestion-corpus-effort-authority-20260730-014647` plus local rerun `/private/tmp/octra-p0-positive-report-envelope.cjson` | positive execution accepted `5/5`; validator readiness still blocked by failure-case/root/matrix gates |
| P0 punitive failure/profile gate | local rerun `/private/tmp/octra-p0-full-failure-profile-report-envelope.cjson` | positive execution accepted `5/5`, but failure gate rejected: `30/35` counted/accepted; Q1 failure-contract rows need producer repair |
| P0 producer repair hints | local rerun `/private/tmp/octra-p0-repair-hints-current.cjson` | emits diagnostic producer-side root, ABI, and case-specific Q1 failure-case repair hints; positive execution still accepted `5/5` |
| P0-plus execution | `/home/exedev/evidence/octra-inference/determinism-p0-plus-corpus-20260730-022653` plus local rerun `/private/tmp/octra-p0-plus-softmax-diagnostic-report-envelope.cjson` | diagnostic execution rejected `8/9`; `SOFTMAX_FP wide-1024-stable-tail` is classified as `host_transcendental_portability_gap` with a one-bit f64 mismatch |
| P0-plus top-k boundary | `/home/exedev/evidence/octra-inference/determinism-p0-plus-topk-boundary-20260730-025306` | boundary accepted as product authority classification; top-k remains reference-only and does not qualify P0-plus validator readiness |
| Recurrent-heavy performance fixture on rebased LiteNode | `/home/exedev/evidence/octra-inference/litenode-recurrent-heavy-reemitted-2ca5dfd-754b0a5-20260729` | accepted |
| Prefill session bundle shape | `/home/exedev/evidence/octra-inference/prefill-session-bundle-55cd597-20260730-121116` | producer-shaped; not continuous LiteNode execution |
| Batch runtime readiness | local `--run-batch` report `runtime_semantics` | diagnostic-only; current mode is `independent_session_per_stage`, with `session_continuation_state_carry_not_supported` as the next runtime blocker |
| Session bundle harness | local `--run-inference-session` report | accepts one target-owned stage labeled `prefill` or `decode` and rejects multi-transition bundles until canonical state carry is implemented |

The `/private/tmp` reports above are local rerun snapshots, not durable
artifacts. The durable identity of the current local rerun is:

| Field | Value |
| --- | --- |
| LiteNode source state | local report-envelope working tree; identify by runner SHA below |
| Runner | `_build/default/tools/inference_conformance_run.exe` |
| Runner SHA-256 | `3891bb360a0468073f374796014c79003ba1bc68e6d13fe906d381e066ab428d` |
| Platform | `macosx`, `arm64`, OCaml switch `octra-lite-4.14.2`, compiler `4.14.2` |
| P0 template index SHA-256 | `4e3de9d7ed36b33bf33b54160b431318f474bcdbadf610940039e28f102a5db1` |
| P0 positive report SHA-256 | `69034cb334be2e5fb49343e17f16a412ac693c2a02fe164484e18f15d5ab2e98` |
| P0 full failure/profile report SHA-256 | `bd8c0187c6c209f7b7f57b282b3d042cdf28ca53d96c6440c2d2f78c4329bf14` |
| P0 repair-hint runner SHA-256 | `3891bb360a0468073f374796014c79003ba1bc68e6d13fe906d381e066ab428d` |
| P0 repair-hint report SHA-256 | `a1dee8cb4f627aee7bccc3f2528dd8417c6440b09fcea543e119c06012ecbfe9` |
| P0-plus fixture pack SHA-256 | `ccf0a834e55a8c0a25e75f4e3c1fab8392c2a683e11021c69a7dad33bafb203d` |
| P0-plus diagnostic report SHA-256 | `c1e043f160a17be3390afd6590c8e72d9cb4571e6b4eb4252dce1c9efa9c6408` |

Rerun shape:

```text
opam exec --switch=octra-lite-4.14.2 -- \
  dune exec tools/inference_conformance_run.exe -- \
  --template-index /private/tmp/octra-conformance-3feb1677-20260801-024010/source/p0/p0-vm-execution-templates.cjson \
  --strict-effort

opam exec --switch=octra-lite-4.14.2 -- \
  dune exec tools/inference_conformance_run.exe -- \
  --template-index /private/tmp/octra-conformance-3feb1677-20260801-024010/source/p0/p0-vm-execution-templates.cjson \
  --strict-effort \
  --include-failures \
  --require-failure-cases \
  --require-profile-roots-bound

opam exec --switch=octra-lite-4.14.2 -- \
  dune exec tools/inference_conformance_run.exe -- \
  --template-index /private/tmp/octra-conformance-3feb1677-20260801-024010/source/p0/p0-vm-execution-templates.cjson \
  --strict-effort \
  --include-failures \
  --require-failure-cases \
  --require-profile-roots-bound \
  --require-validator-readiness

opam exec --switch=octra-lite-4.14.2 -- \
  dune exec tools/inference_conformance_run.exe -- \
  --p0-plus-pack /private/tmp/octra-conformance-3feb1677-20260801-024010/source/p0-plus/p0-plus-fixture-pack.cjson
```

Latest producer checkpoints consumed by LiteNode:

```text
808d60370898ef71800ea6cea960b0f30a9592b4 Add P0 effort and observational authority
426d86e8c84664f4b8b6b3ba76d6f517960ef4b9 Add P0-plus determinism corpus
cffcf90eb741d7c56bb9c984711a1232107e6da1 Classify P0-plus top-k evidence
ee6180db7df5585e4e6558ef8505f9d5b0c80073 Record accepted P0-plus top-k boundary
d69fc419908fa95936a8357aa5f01ca5bcb7bf20 Emit prefill session bundle shape
```

## Readiness Gates

| Readiness level | Status | Required evidence |
| --- | --- | --- |
| Local correctness demo | Met | Prompt-to-token candidate proof with LiteNode-selected token and rooted outputs. |
| Local performance demo | Partially met | Batch mode and opcode timing exist; full prompt runtime remains too slow for product use. |
| Product session shape | Partially met | Producer emits open/prefill/decode/finalize roots and receipt chain; execution still depends on producer-visible layer packets. |
| Share-with-Octra-devs architecture review | Met with caveats | One-VM architecture, model-neutral primitive surface, P0/P0-plus conformance gates, and explicit claim boundaries are documented. |
| Mergeable runtime branch | Not met | Needs footprint reduction, rebased patch review, CI gates, and removal or quarantine of evidence-only tooling as appropriate. |
| Devnet candidate | Not met | Requires deterministic math profile, target-owned prefill/decode session, resource scheduling, replay/restart evidence, and multi-node conformance. |
| Validator-ready inference | Not met | Requires protocol-owned arithmetic, multi-platform conformance, deterministic receipts, and agreed resource policy. |

## Devnet Blockers

1. Deterministic math profile.
   Current inference FP opcodes still use host `float`, `sqrt`, `exp`, `cos`,
   `sin`, or binary64 accumulation. P0 positive execution now proves the five
   local P0 primitives can match their producer fixtures, but validator
   readiness is still blocked by punitive failure coverage, root binding, and
   cross-platform matrix evidence. P0-plus now gives an explicit portability
   warning: `SOFTMAX_FP wide-1024-stable-tail` executes locally but differs by
   one f64 bit pattern, so host-native `exp` cannot be counted as
   consensus-safe.

2. Target-owned session program.
   The proof path can chain admitted stages. Product runtime needs one
   target-owned prefill/decode lifecycle:

   ```text
   open_session -> prefill -> decode -> ARGMAX_FP -> receipt -> finalize
   ```

3. Runtime performance.
   The current local proof mode is correctness-first. Batch execution improved
   harness overhead, but full prompt runtime still needs resident execution,
   cached authenticated ranges, and measured kernel optimization. Batch reports
   now make this explicit in diagnostic `runtime_semantics`: owner-byte and
   model-range pin caches are supported, while resident session state carry,
   prefill/decode phase ownership, and decode-loop ARGMAX outputs remain the
   runtime blocker.

4. Multi-platform conformance.
   Strict P0 reports must run across the intended validator platforms and
   build modes before a consensus claim. When a strict P0 runner report is
   missing the cross-platform matrix, its `cross_platform_evidence` now emits
   an actionable `matrix_request` with the required opcode scope, catalog
   roots, local result signature, and structured runner/matrix/rerun argv
   templates. P0-plus matrixing is intentionally not supported yet; its
   host-transcendental surfaces need deterministic replacement or narrower
   qualification first.

5. Resource isolation.
   Inference must remain outside consensus proposal, epoch apply, finality, and
   private-execution critical paths, with explicit memory/effort/concurrency
   ceilings.

## Next Engineering Sequence

1. Keep P0 and P0-plus fixtures immutable.
2. Add a deterministic arithmetic profile decision for P0 and P0-plus:
   - exact software FP, or
   - wider fixed point where quality evidence supports it, or
   - explicitly local-only host-FP profile.
3. Keep `SOFTMAX_FP` behind `host-fp-exp-local-candidate` until its
   max-subtract, exp, summation, division, rounding, output encoding, and
   punitive vectors are protocol-owned.
4. Wire conformance into CI for schema, positive execution, strict effort, and
   counted failure/atomicity.
5. Build the target-owned prefill/decode session path; do not continue relying
   on caller-visible layer orchestration as product runtime. The first
   LiteNode harness step exists as `--run-inference-session`: it accepts a
   single target-owned stage labeled `prefill` or `decode` and reports the
   exact state-carry blocker for multi-transition sessions. The label is not
   yet a VM-proven prefill/decode phase contract.
6. Re-run one Bonsai prompt-to-token proof under the target-owned session shape.
7. Re-run recurrent-heavy and logits-tail performance gates against that shape.
8. Only then prepare the mergeable branch by reducing evidence-only scaffolding
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
