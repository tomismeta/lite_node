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
    --request request.json
```

`tools/inference_admit.exe` is a Dune build target, not a checked-in file. A
built checkout may also invoke
`_build/default/tools/inference_admit.exe` directly after `dune build`, but the
portable command is `dune exec tools/inference_admit.exe -- ...`.

For the demo, `--program` should be a program envelope, not raw bytecode, so
the existing bytecode certificate path is exercised.

`--support` is optional for local bring-up. If omitted, the harness derives
exact support from the supplied requirement. That is acceptable for packet
alignment, but it is not node capability advertisement.

## Required Output Packet

`octra-inference` should emit four files:

- `program.ocpg`, a LiteNode program envelope;
- `requirement.json`;
- `target.json`; and
- `request.json`.

Smoke tests may use raw bytecode to validate packet shape. The Bonsai demo
packet must use an OCPG program envelope so LiteNode checks the existing
program certificate and type-flow path.

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
  "program_root": "<sha256 over admitted bytecode encoding>",
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
declared by the target, request limits fit inside the execution requirement,
and an optional `request_root` matches the computed root.

## Acceptance

The harness must return:

```json
{
  "status": "accepted",
  "program_root": "...",
  "requirement_root": "...",
  "target_root": "...",
  "request_root": "..."
}
```

This proves packet compatibility only. It does not prove model correctness,
consensus execution, private execution, or encrypted inference.

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

## Next Ask

Produce a command such as:

```sh
octra-inference emit-vm-packet <packed-model> \
  --out /home/exedev/evidence/<run>/vm-packet
```

The command should produce the four files above and then invoke the LiteNode
admission harness as a readiness gate. Once that passes, the Bonsai runtime
proof flow can consume the same packet before local execution.

## Current Interop Check

The `octra-inference` branch `codex/emit-vm-packet` at commit `6a06128`
produced a smoke packet accepted by the LiteNode harness on the VPS. The smoke
packet proves the roots and JSON boundary line up. It used a tiny raw bytecode
program, so the next packet must switch to a real program envelope before it is
used as Bonsai demo evidence.
