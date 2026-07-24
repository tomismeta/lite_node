# Octra Inference Agent Brief

Status: working interop brief, 2026-07-21.

This brief is for the agent working in `octra-inference`. Its purpose is to
make the next Bonsai demo step concrete without moving model-specific logic
into LiteNode.

## Environment

Use the existing `octra-inference.exe.xyz` VPS for integration because it
already contains the model artifacts, evidence directories, Rust toolchain, and
LiteNode OCaml build environment.

Do not depend on dirty working-tree state as demo history. If current
`octra-inference` changes are needed, checkpoint them in Git first or create a
clean worktree from the needed commit. Keep local generated evidence under the
VPS evidence/artifact directories, but keep source, schema, and command changes
in Git.

Recommended layout:

```text
/home/exedev/codex-edit/octra-lite-node-inference
/home/exedev/codex-edit/octra-inference-admit-packet
/home/exedev/models/...
/home/exedev/evidence/...
```

## LiteNode Boundary

LiteNode now exposes a local admission harness:

```sh
cd /home/exedev/codex-edit/octra-lite-node-inference
PATH=/home/exedev/.cargo/bin:$PATH opam exec -- \
  dune exec tools/inference_admit.exe -- \
    --program program.ocpg \
    --requirement requirement.json \
    --target target.json \
    --support node-support.json \
    --model-ranges model-ranges.json \
    --range-source <owner-root>=owner.bin \
    --request request.json \
    --run-session
```

`tools/inference_admit.exe` is a Dune build target, not a checked-in file. A
built checkout may also invoke
`_build/default/tools/inference_admit.exe` directly after `dune build`, but the
portable command is `dune exec tools/inference_admit.exe -- ...`.

`--program` must be a program envelope, not raw bytecode, so the existing
bytecode certificate path is exercised before target admission.

`--support` is required for every admission and represents declared node
capability advertisement. The harness does not derive support from the supplied
requirement and reports `runtime_support_verified=false`.
`--model-ranges` is optional for smoke packets and should be present for the
Bonsai demo packet.
`--range-source` and `--run-session` are local proof options. They are required
only when the agent wants LiteNode to authenticate owner bytes and emit local
session and receipt roots.
Use `--scan-policy` when the goal is frontier discovery rather than execution.
It returns every visible inference opcode-policy violation in one JSON report:

```sh
PATH=/home/exedev/.cargo/bin:$PATH opam exec -- \
  dune exec tools/inference_admit.exe -- \
    --program program.ocpg \
    --requirement requirement.json \
    --target target.json \
    --support support.json \
    --scan-policy
```

This scan is advisory only. It does not admit new opcodes and it does not prove
runtime correctness. It cannot be combined with `--run-session`.

## Required Output Packet

`octra-inference` should emit five files for the Bonsai demo:

- `program.ocpg`, a LiteNode program envelope;
- `requirement.json`;
- `target.json`;
- `model-ranges.json`; and
- `request.json`.

Smoke tests may omit `model-ranges.json`, but target-bearing admission still
requires an OCPG program envelope. The Bonsai demo packet must include all five
files so LiteNode checks the existing program certificate and type-flow path.
The OCPG certificate must be a LiteNode-compatible
`aml_bytecode_certificate_v2`; an envelope with placeholder certificate bytes
will be rejected.

The requirement object:

```json
{
  "vm_semantics_root": "<64 lowercase hex>",
  "numerical_root": "<64 lowercase hex>",
  "effort_root": "<64 lowercase hex>",
  "capabilities": [
    { "name": "tensor.fixed", "root": "<64 lowercase hex>" }
  ],
  "limits": {
    "max_model_bytes": 0,
    "max_view_bytes": 0,
    "max_session_bytes": 0,
    "max_scratch_bytes": 0,
    "max_output_bytes": 0,
    "max_advance_effort": 0
  },
  "requirement_root": "<optional expected root>"
}
```

The target object:

```json
{
  "program_root": "<sha256 over domain-tagged admitted bytecode encoding>",
  "requirement_root": "<computed requirement root>",
  "model_root": "<64 lowercase hex>",
  "execution_descriptor_root": "<64 lowercase hex>",
  "store_root": "<64 lowercase hex>",
  "session_abi_root": "<64 lowercase hex>",
  "entrypoints": [
    { "name": "advance", "label": 100 }
  ],
  "target_root": "<optional expected root>"
}
```

The request object remains `octra-inference` owned, but the LiteNode harness
now admits the following boundary fields:

```json
{
  "schema": 1,
  "target_root": "<computed target root>",
  "entrypoint": "advance",
  "input_root": "<64 lowercase hex>",
  "request_nonce": "<64 lowercase hex>",
  "max_output_bytes": 0,
  "max_advance_effort": 0,
  "request_root": "<optional expected root>"
}
```

LiteNode checks that the request targets the admitted target, the entrypoint is
exactly `advance`, request limits fit inside the execution requirement, and an
optional `request_root` matches the computed root. The admitted target must use
the LiteNode session ABI root and exactly one `advance` entrypoint at label
`100`.

The model ranges object:

```json
{
  "model_root": "<target model_root>",
  "store_root": "<target store_root>",
  "ranges": [
    {
      "owner_root": "<64 lowercase hex>",
      "offset": 0,
      "length": 4096,
      "encoding": "octets",
      "shape_root": null
    }
  ],
  "model_ranges_root": "<optional expected root>"
}
```

LiteNode checks only descriptor identity and byte bounds here. It does not
trust a filesystem path, load the model, prepare native views, or prove model
correctness from this descriptor.
For local `--run-session` proof, provide each owner byte source with
`--range-source <owner-root>=<path>`. LiteNode hashes the file contents and
rejects the run if the digest does not equal `owner_root`.

## Acceptance

The harness must return:

```json
{
  "status": "accepted",
  "program_root": "...",
  "requirement_root": "...",
  "target_root": "...",
  "model_ranges_root": "...",
  "request_root": "...",
  "final_session_root": "...",
  "final_receipt_root": "...",
  "consensus_accepted": false
}
```

This proves packet compatibility only. It does not prove model correctness,
consensus execution, private execution, or encrypted inference.
When `--run-session` is used, it additionally proves the local lifecycle
boundary for the supplied target-owned program and authenticated range bytes.
It still does not prove Bonsai numerical correctness unless the supplied
program implements that path with accepted generic VM primitives.

## Guardrails

- Keep Qwen/Bonsai parsing, tokenizer behavior, family-specific execution
  semantics, and model qualification evidence in `octra-inference`.
- Keep LiteNode-facing packet fields model-neutral: roots, capabilities,
  limits, entrypoints, and request identity.
- Do not add a model-family name to LiteNode runtime code or tooling.
- Do not invent non-Git version labels. Use content roots for deployed object
  identity and Git commits for source history.
- Keep `consensus_accepted=false` on the local runtime proof path until a real
  network receipt exists.
- Expect the LiteNode report to include `program_provenance` and
  `program_attested`; the current local checked-envelope path should report
  `program_attested=false`.

## Frontier Ask

Produce commands such as:

```sh
octra-inference emit-vm-packet <packed-model> \
  --out /home/exedev/evidence/<run>/vm-packet

octra-inference emit-vm-frontier <packed-model> \
  --out /home/exedev/evidence/<run>/vm-frontier
```

The frontier command should derive the next Bonsai/Qwen operation frontier from
the generated schedule and emit independent tiny canaries for each generic
operation family in that frontier. Do not emit one large packet when separate
packets would reveal clearer failures. Each canary should invoke LiteNode with
`--scan-policy` first, then `--run-session` only if admission passes.

Every frontier canary should preserve the five demo files above, keep
`target.json` model-neutral, put model-family details in sidecars, and classify
failures as packet, admission-policy, missing-primitive, data-binding,
effort/limit, determinism, or harness-only.

Current LiteNode proof branch includes `LOAD_F32_LE_FP`, `LOAD_F64_LE_FP`,
`SIGMOID_FP`, `SOFTPLUS_FP`, `SILU_FP`, `CAUSAL_DEPTHWISE_CONV1D_FP`,
`GATED_DELTA_RULE_FP`, `RMSNORM_FP_EPS`, `L2NORM_FP`, and
`ELEMWISE_MUL_FP`.

The next `octra-inference` artifact should refresh the order-5 frontier against
this LiteNode branch. Emit real canaries for explicit-epsilon RMSNorm and
elementwise multiply, request `tensor.strict-fp`, run `--scan-policy`, run
`--run-session` when admitted, and compare roots against the existing order-5
evidence package. Compose row-batched RMSNorm by calling
`rmsnorm_fp_eps(addr, n, gamma, epsilon_bits)` once per row. Keep Q/K
normalization, projection outputs, tokenizer, and Bonsai/Qwen metadata in
sidecars, not `target.json`.

## Current Interop Check

The latest local interop smoke produced a five-file packet with a
LiteNode-compatible OCPG envelope, authenticated owner bytes, local
open/advance/finalize session roots, receipt roots, and
`consensus_accepted=false`. It used a tiny effect-free target program. The next
packet must replace that tiny program with the generic numerical target path
before it can serve as Bonsai demo evidence.
