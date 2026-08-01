# Inference Fast Path Runtime

Status: candidate LiteNode-side design, 2026-07-27.

This document defines the smallest LiteNode runtime surface needed to turn the
current Bonsai correctness proof into a practical local runtime path. It is a
companion to [`inference-runtime.md`](inference-runtime.md) and
[`inference-runtime-contracts.md`](inference-runtime-contracts.md).

The current proof branch established that model-neutral inference programs can
be admitted and executed against rooted model deployments. The measured fast
path work now needs to remove proof-harness repetition without changing the
VM's authority boundary.

## Current Finding

The benchmark from `octra-inference` shows three different bottleneck classes:

| Path | Dominant cost | LiteNode implication |
| --- | --- | --- |
| Attention prefix and frontier | Producer materialization | Keep this outside LiteNode; consume rooted packets and cached ranges. |
| Recurrent layers | LiteNode `run-session` | Add timing and optimize from measured opcode data. |
| Attention tail assembly | Under-instrumented | Require phase timing before optimizing or fusing. |

This means the old chained proof is not product runtime. It also means the VM
is not uniformly the bottleneck. LiteNode should first make repeated execution
cheap and measurable, then optimize the recurrent path with data.

## Decision

Add one local harness/runtime surface first:

```text
inference_admit.exe --run-batch <bundle.json>
```

`--run-batch` is a local candidate-evidence tool, not a new VM, model registry,
or devnet RPC contract. It should reuse the existing admission, plan, session,
receipt, and model-deployment modules. Its job is to remove harness repetition:
parse once, cache shared inputs, run many existing one-shot stages in one
process, and return compact receipt and timing evidence.

The first batch runner must not claim one canonical session across unrelated
stage targets. Current session identity is intentionally strict: target root,
request root, model-ranges root, and model-deployment root are checked for the
session. Loosening that check would make caller-selected layer orchestration
look like target-owned inference. Batch mode is therefore a transitional
harness optimization until `octra-inference` emits one target-owned prefill or
decode program.

The product protocol direction remains explicit lifecycle operations around one
target-owned program:

```text
open_session
advance_session
finalize_session
cancel_session
status
```

The batch harness should be shaped so the file parsing, admission, pinning,
timing, and reporting code can later be reused by those operations without
changing roots or receipt semantics.

## Non-Goals

Do not add any of the following to LiteNode:

- Bonsai, Qwen, HuggingFace, GGUF, tokenizer, tensor-name, or layer-number
  dispatch.
- A durable LiteNode model registry.
- A second VM, inference-only bytecode, or special model execution engine.
- Sampling outside rooted target execution.
- Prepared-view or cache roots in canonical session identity.
- Fused model-layer opcodes without measured data and a model-neutral scalar
  contract.
- PVAC dependencies in the plain inference fast path.

## Batch Request Shape

The first batch bundle should be deliberately small:

```json
{
  "schema": "octra.inference.run_batch",
  "model_deployment": "model-deployment.json",
  "support": "support.json",
  "receipt_mode": "compact",
  "stages": [
    {
      "stage_id": "layer16.recurrent",
      "program": "program.ocpg",
      "requirement": "requirement.json",
      "target": "target.json",
      "request": "request.json",
      "input": "request-input.bin",
      "model_ranges": "model-ranges.json",
      "range_sources": [
        {"owner_root": "<root>", "path": "owners/<root>.bin"}
      ],
      "expected_output_root": null
    }
  ]
}
```

Paths are local harness inputs. Roots remain the authority. Unknown fields fail
closed until a format explicitly declares them diagnostic.

`receipt_mode` values:

- `compact`: roots, effort, status, and minimal timing.
- `debug`: compact output plus the same diagnostics currently emitted by
  one-shot runs.

Timing is selected by the harness CLI rather than the batch JSON, so fixtures
remain stable while instrumentation changes:

```text
--timing-mode none|stage|opcode
```

Values:

- `none`: no local timing evidence.
- `stage`: admission, pinning, plan creation, execution, finalize.
- `opcode`: stage timing plus diagnostic execution-phase and opcode-level
  counters.

## Batch Response Shape

The response should mirror existing one-shot reports while making stage
ordering explicit:

```json
{
  "status": "accepted",
  "batch_report_sha256": "<diagnostic-sha256>",
  "runtime_semantics": {
    "session_mode": "independent_session_per_stage",
    "stage_lifecycle": ["open_session", "advance_session", "finalize_session"],
    "batch_cache_scope": ["owner_bytes", "model_range_pins"],
    "continuation_supported": false
  },
  "last_stage_output_root": "<root>",
  "unsupported_opcodes": [],
  "missing_capabilities": [],
  "policy_violations": [],
  "stages": [
    {
      "stage_id": "layer16.recurrent",
      "status": "accepted",
      "session_status": "accepted",
      "reference_status": "matched",
      "target_root": "<root>",
      "request_root": "<root>",
      "output_root": "<root>",
      "candidate_root": "<root>",
      "advance_receipt_root": "<root>",
      "opened_session_root": "<root>",
      "advanced_session_root": "<root>",
      "final_session_root": "<root>",
      "effort_delta": 0,
      "timing": {},
      "execution_timing": [
        {"phase": "vm_run", "microseconds": 0}
      ],
      "opcode_timing": [
        {"opcode": "LINEAR_Q1_G128_FP", "count": 0, "effort_delta": 0}
      ]
    }
  ]
}
```

The batch response does not prove model correctness. It proves that LiteNode
admitted and executed the supplied rooted stages under the existing VM and
session contracts. `batch_report_sha256` is diagnostic evidence over the
reported deterministic fields, not a session root and not a receipt root.
`runtime_semantics` is additive diagnostic metadata and is excluded from
`batch_report_sha256`.
`runtime_semantics` is an explicit claim boundary: current batch mode reuses
local owner bytes and model-range pins, but it still opens, advances, and
finalizes an independent session per stage. It does not carry target state
across stages.

## Session Semantics

The current local runner opens, advances, and finalizes one packet. The first
batch mode should keep that canonical shape per stage while sharing local
process work around it.

The initial implementation can be conservative:

1. Parse the batch descriptor once.
2. Cache support, decoded programs, owner verification, pinned ranges, fixed
   bytecode, and entrypoint lookups by roots where safe.
3. For each stage, validate the stage exactly as one-shot `--run-session` does.
4. Open, advance, and finalize an independent session for that stage.
5. Return the same roots and effort as the one-shot command for each stage.

This keeps canonical roots in `Inference_session`. It does not require target
state or checkpoints to become session identity and it does not invent an
aggregate session root.

The harness reports this directly as:

```text
session_mode = independent_session_per_stage
continuation_supported = false
batch_cache_scope = owner_bytes, model_range_pins
```

The next runtime blocker has narrowed. The core session runtime now has an
ABI-v2 continuation context for repeated advances, and
`--run-inference-session` can route uniform ABI-v2 multi-transition bundles
through one opened session, repeated advance, and one finalization. The batch
harness still opens an independent session for each stage. The product runtime
must now bind committed state payload transport while avoiding caller-selected
layer orchestration. The generic phase sequence is bound as `prefill*` followed
by `decode*`, with `decode_steps` matching the declared decode transition count.
Session reports include each canonical output payload and payload digest next to
`output_root`, and a target-owned decode transition can declare an
`selected_index` `output_contract` to expose a root-bound one-cell token output.
When the decode program uses `ARGMAX_FP`, opcode evidence supplies the ARGMAX
provenance.

Compatibility checks:

- stages may keep their own target and request roots;
- stages should bind the same `model_deployment_root` for a fast-path demo
  bundle unless the batch descriptor explicitly declares otherwise;
- stages should use the same `session_abi_root`;
- stages should use the same support object;
- stage order is the input list order; and
- a failed stage stops the batch with the failing stage report and no aggregate
  success claim.

If the current `Inference_session.check_identity` is too strict for multi-stage
targets because it requires one target and request root for the whole session,
do not loosen it. The near-term harness uses one session per stage. Product
runtime requires `octra-inference` to emit one target-owned program whose
internal bytecode owns layer selection, tensor selection, token selection, and
continuation.

## Timing

Add timing without changing roots. Timing is local evidence only.

Stage timing should measure:

- parse/read files;
- admission;
- policy scan when requested;
- model range pinning;
- plan creation;
- open session;
- VM execution;
- finalize;
- JSON/report emission.

The one-shot runner should get this timing first. Batch timing is easier to
trust after standalone stage timing is complete.

Opcode timing should be opt-in and diagnostic-only:

```text
opcode
invocation_count
effort_delta
microseconds
```

The first implementation exposes opcode timing through
`--timing-mode opcode` and keeps it out of canonical receipts. The report also
includes `execution_timing` so non-opcode costs such as scratch/candidate
canonicalization are visible before optimizing kernels.

## Recurrent Hotspot

The measured recurrent path is LiteNode `run-session` bound. The likely
hotspots are:

1. `GATED_DELTA_RULE_FP`
2. `LINEAR_Q1_G128_FP`
3. `LOAD_F64_LE_FP`

That order was a hypothesis. On the locked recurrent-heavy fixture, diagnostic
timing showed `LINEAR_Q1_G128_FP` dominates opcode wall time, followed by
`GATED_DELTA_RULE_FP` and `LOAD_F64_LE_FP`. It also showed candidate/scratch
canonicalization is large enough to track separately from opcode execution.
A mergeable first optimization slice should therefore target measured Q1
linear execution or canonicalization overhead, not fused model-layer opcodes.

## Measured Optimization Slices

The locked recurrent-heavy fixture is the benchmark input of record:

```text
/home/exedev/evidence/octra-inference/litenode-recurrent-heavy-batch-fixture-55cd597-082a638-20260728-212517/recurrent-heavy-batch-fixture.cjson
```

The local runtime slices preserved the deterministic
`batch_report_sha256`:

```text
8d6b4abdb640cfc74ce03a9d60f572732959a0d4b8533ab5cfddb128f08e3f3c
```

| Harness | Change | Avg advance | Result |
| --- | --- | ---: | --- |
| `e69be83` | diagnostic opcode/phase timing | `43.033s` | accepted baseline |
| `3a0f6d9` | compute candidate payload once; reuse for scratch check | `38.718s` | accepted, `scratch_check` collapsed to `0.0s` |
| `d00f0d3` | plus memory-safe Q1 block scale predecode | `34.841s` | accepted, `LINEAR_Q1_G128_FP` improved by `21.447s` |

An earlier Q1 scale/sign predecode also accepted, but copied sign bytes into an
unbounded `int array`. That was rejected during review because maximum admitted
dimensions could allocate too much host memory outside VM limits:

```text
63b744a: avg advance 36.602s, LINEAR_Q1_G128_FP 170.374s
```

The signed-weight predecode experiment preserved roots but regressed wall time
and should not be kept:

```text
0785c9b: avg advance 51.932s, LINEAR_Q1_G128_FP 260.624s
```

Two merge-readiness cleanups were validated on the same fixture:

| Harness | Change | Validation |
| --- | --- | --- |
| `61e0dee` | `run` and `run_profiled` share one VM loop | accepted, roots/hash unchanged |
| `fd0c6f0` | normal `Inference_execution.run` result no longer carries diagnostic timing fields | accepted, roots/hash unchanged |

These are not new speed baselines. They make the patch cleaner to review while
preserving the memory-safe `d00f0d3` performance keeper as the current
comparison point.

The next measured target remains `LINEAR_Q1_G128_FP`. The clean direction is a
smaller inner-loop/memory-representation improvement, followed by an explicit
cache contract only if it remains local, keyed by rooted immutable ranges, and
kept out of canonical receipts.

Current execution also pays for fresh VM state, jump fixing, boxed sparse
memory, scalar OCaml loops, output extraction, and candidate memory
canonicalization. `candidate_root` is included in receipts today, so compact
reporting does not automatically remove that cost. Changing receipt semantics
is a separate pre-devnet decision and must not happen casually on this proof
branch.

## Separation From Other Compute

LiteNode remains one VM with domain-specific admission:

| Domain | Runtime boundary |
| --- | --- |
| General compute | Existing program admission and VM semantics stay unchanged. |
| Encrypted compute | PVAC/FHE capabilities remain explicit and disabled in the plain inference context. |
| Inference compute | Requirement-bound programs use admitted model-neutral tensor and sequence opcodes. |

Batch execution is an inference harness/scheduler concern. It must not change
ordinary program admission, encrypted execution policy, or consensus behavior.

## Implementation Order

1. Add stage timing around the existing one-shot `--run-session` path.
2. Add opcode timing behind an explicit flag and verify it accounts for VM
   execution time.
3. Add a read-only batch JSON parser in `tools/inference_admit.ml`.
4. Add batch execution that reuses the current one-shot validation per stage
   and runs each stage in one process.
5. Return compact per-stage receipts and timings, with no aggregate session
   root.
6. Use recurrent timing to choose the first optimization target.
7. Add the product-facing session bundle harness. The first accepted shapes are
   a single target-owned stage labeled `prefill` or `decode`, plus uniform
   ABI-v2 multi-transition bundles that reuse one opened session. ABI-v1 or
   mismatched multi-transition bundles must still fail closed.
8. Only then consider resident prepared views or native strict kernels.
9. Require one target-owned prefill/decode program before claiming product
   session execution.

## Acceptance Criteria

The first batch slice is acceptable when:

- existing tests pass unchanged;
- one-shot `--scan-policy` and `--run-session` output remains compatible;
- batch mode runs at least two accepted stages in one process;
- batch and standalone stages produce identical roots and effort;
- no model-family strings appear in LiteNode runtime code;
- compact batch reports include per-stage output roots, receipt roots, effort,
  and timing;
- stage failures do not produce a misleading final success root;
- timing is explicitly diagnostic, not canonical;
- opcode timing accounts for at least 95% of measured VM execution time when
  enabled; and
- product prefill uses one target-owned transition, not caller-selected layer
  stages.
