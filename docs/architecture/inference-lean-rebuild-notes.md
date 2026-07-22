# Inference Lean Rebuild Notes

Status: proof-branch notes, 2026-07-22.

This branch is now treated as a product-learning branch. It should not be the
shape merged back to LiteNode. Its job is to prove which VM capabilities are
actually needed for Bonsai-shaped inference, then inform a smaller rebuild from
fresh `upstream/main`.

## Current Evidence

The strongest completed VM-side proof is the Bonsai slim range/session run on
the inference VPS:

```text
/home/exedev/evidence/octra-inference/bonsai-slim-range-session-20260722-031944
```

The packet was emitted by `octra-inference` and admitted by the LiteNode
inference harness:

```text
/home/exedev/codex-edit/octra-lite-node-inference/_build/default/tools/inference_admit.exe
```

The direct rerun on 2026-07-22 returned:

| Field | Value |
| --- | --- |
| Status | `accepted` |
| Program root | `b745dca633e034f702f98c1e44c2acb55b656d1edd853dd6e4f1d4a864c69509` |
| Requirement root | `71c0e07327526c04cd9cae8a3b730b0c8014f8aee8b0de9a37fd33e4d2fb12a6` |
| Target root | `5a76493a52e4705b417051c841a8d6e6294e41e901f7a546cbfafbbc7a1de36c` |
| Request root | `5448511b2a531609fc7ca7c9002d45c51da3df1118e6f777432ddacc9168a521` |
| Model ranges root | `c9f94eb82cfdfedd14b0b1c2ea038be7ea56dfe5e8ceea8dc6206538c0712b71` |
| Program instructions | `8` |
| Program effects | `memory_write` |
| Model range count | `1` |
| Entrypoint | `advance` at label `100` |
| Output root | `c4b6e2bd7fc99932610633bdcfe9a0083b1658d5eb345ec652a5584a3174f238` |
| Candidate root | `e1e4660ac0eb18d914f8bf654f9b2585cbfba339536f595abe7438d75b2efe46` |
| Effort delta | `111` |
| Consensus accepted | `false` |

This proves:

- OCPG program-envelope admission works through the LiteNode harness.
- Requirement, target, request, and model-range roots bind coherently.
- The current `advance` ABI works: label `100`, request input cell `1000`,
  output base/count in `r0`/`r1`.
- Authenticated immutable owner bytes can be pinned and addressed by a rooted
  model range.
- The local session path can open, advance, finalize, and emit output,
  candidate, session, and receipt roots.

This does not prove:

- LiteNode executes the full Bonsai/Qwen graph.
- LiteNode has accepted implementations for the full required math primitive
  set.
- The current session and receipt protocol should survive into the lean branch.
- Consensus or encrypted inference is ready.

## Bonsai Canary Status

A stronger one-token reference-output canary was attempted on the VPS:

```text
/home/exedev/evidence/octra-inference/bonsai-canary-packet-20260722-114633
```

It uses:

```text
/home/exedev/models/bonsai/Bonsai-27B-Q1_0.gguf
/home/exedev/models/bonsai-27b-octra-q1-bundled
/home/exedev/codex-edit/octra-inference-lite-node-admission-adapter/target/release/octra-inference
```

The reference side completed and wrote:

| Artifact | SHA-256 |
| --- | --- |
| `packet/reference-output.cjson` | `67df6c6a9c9ab07acb592bc1d85e2ba9687ea73d46e1676692a8fe4dc2511c3d` |
| `packet/opcode-manifest.json` | `d8ec170017b4f7334d4884c9f8a9292d0dc70ec09b8cea1b390077293464282b` |
| `packet/unsupported-capability-assumptions.json` | `71a5b819037cc86d25505838531799f47caa1a97ba834b045ac8774562d9d24e` |

The expected reference output is:

| Field | Value |
| --- | --- |
| Generated text | `One` |
| Generated token id | `3833` |
| Expected output root | `4374f65fc875ba67eb98770874794a1d75cf71537db3439b63ee235a25191cc9` |
| Expected output SHA-256 | `5d3c7b1f6cedbfade29a6c9e5b2a01df796207e3c748619d6bc1db3a56f9d4ea` |
| Final hidden SHA-256 | `666ba09199554f24ea04e17b24999391f6fd22d3ead502d3429ac62d30384308` |
| Final norm SHA-256 | `49fe2f2a3d5aad5f959927f8201ff46a097f51180830a5005827a52e309366dd` |
| Logits SHA-256 | `4fc46f7f8728b92d13c7d2fe74a70ba854a13de63a9e0cf411ecd25dcf6e541b` |

Packet completion failed before `program.ocpg`, `requirement.json`,
`target.json`, `request.json`, `model-ranges.json`, and the LiteNode admission
report were written:

```text
error: OCTB bytecode is not in canonical LiteNode encoding
```

This is classified as a packet/encoding producer gap on the `octra-inference`
side. It does not establish a new LiteNode primitive requirement. The next
`octra-inference` action is to make the reference-output canary emit canonical
OCPG/OCTB bytes accepted by the existing LiteNode decoder.

Future failures should be classified into one of these buckets before changing
LiteNode:

| Bucket | Meaning |
| --- | --- |
| Packet/ABI mismatch | Root, entrypoint, request, or ABI fields disagree |
| Admission policy gap | Program uses an opcode forbidden by plain inference |
| Missing generic primitive | Bonsai needs a model-neutral operation not present in LiteNode |
| Data binding gap | Model range, owner bytes, input root, or store root cannot be proven |
| Effort or memory limit gap | The request cannot fit declared limits |
| Determinism gap | Output depends on host float, thread count, kernel choice, or ambient state |
| Harness-only gap | The local proof harness lacks a product runtime feature |

## Observed VM Requirements

The lean branch should preserve only the requirements that the completed proof
actually exercised:

1. Verify program envelopes with the existing admission machinery.
2. Apply an inference-specific opcode policy after generic program admission.
3. Bind a requirement root to VM semantics, numerical semantics, effort
   semantics, capabilities, and limits.
4. Bind a target root to program, requirement, model, store, ABI, and
   entrypoint roots.
5. Bind a request root to target, input root, entrypoint, output limit, effort
   limit, and nonce.
6. Authenticate immutable owner bytes and pin bounded model ranges before
   execution.
7. Execute one deterministic `advance` against `Contract_vm`.
8. Return rooted output, rooted candidate state, and effort used.

Everything else in this branch is candidate evidence, not proven minimum
product surface.

## Lean Rebuild Shape

Start fresh from `upstream/main` and reintroduce the VM work with fewer public
nouns:

```text
lib/vm/policy/inference_opcode_policy.ml
lib/vm/runtime/inference_admission.ml
lib/vm/runtime/inference_runtime.ml
```

Preferred public ontology:

```text
Inference_admission.t
Inference_runtime.requirement
Inference_runtime.range
Inference_runtime.request
Inference_runtime.prepared
Inference_runtime.result
```

`Admission` should remain generic. The inference wrapper should delegate to
`Admission`, then expose an opaque type whose invariants are:

- program envelope verified;
- program provenance acceptable for inference;
- requirement checked against support;
- inference opcode policy passed; and
- admitted code is available for execution.

`Inference_runtime.prepare` should bind the admitted program, requirement,
model ranges, request, input bytes, and local range sources once. It should
return an opaque prepared value. `Inference_runtime.execute` should run one
plain `advance` and return a small result record.

## Do Not Port By Default

The lean branch should not automatically port these proof-branch surfaces:

- `tools/inference_admit.ml` as product code;
- public `Inference_target`, `Inference_request`, `Inference_model`,
  `Inference_store`, `Inference_plan`, `Inference_session`,
  `Inference_receipt`, and `Inference_session_abi` modules;
- session lifecycle states unless product scheduling/persistence requires
  them immediately;
- receipt roots unless they are part of the next demonstrable contract;
- broad `Opcode_policy` rewrites;
- six architecture documents;
- the full old math-capability matrix; or
- any encrypted inference stub.

## Next Ask For `octra-inference`

The next useful packet from the `octra-inference` side is not another range
smoke proof. It is the smallest completed reference-output canary:

```text
octra-inference demo bonsai-canary-packet \
  --gguf /home/exedev/models/bonsai/Bonsai-27B-Q1_0.gguf \
  --release /home/exedev/models/bonsai-27b-octra-q1-bundled \
  --rehearsal \
  --out <evidence-dir>/packet \
  --prompt <short prompt> \
  --max-new-tokens 1 \
  --qwen35-chat \
  --disable-thinking \
  --session-abi-root 5f4adf0f9083297e5d89e82b504ce113e532264ec9ebc5403a8c7cdba19de26e \
  --entrypoint advance=100 \
  --request-entrypoint advance \
  --admission-harness /home/exedev/codex-edit/octra-lite-node-inference/_build/default/tools/inference_admit.exe \
  --admission-report <evidence-dir>/admission-report.json \
  --run-session
```

That packet must include:

- `program.ocpg`;
- `requirement.json`;
- `target.json`;
- `request.json`;
- `model-ranges.json`;
- `request-input.json`;
- `reference-output.cjson`;
- `opcode-manifest.json`;
- `unsupported-capability-assumptions.json`; and
- the LiteNode admission/session report.

Only after that packet is available should the VM add or revise generic math
capabilities.

## Rebuild Acceptance Bar

The fresh branch is ready to replace this proof branch when:

- the changed-file count is materially smaller than this branch;
- generic admission and normal VM behavior remain unchanged by default;
- inference code is isolated behind the inference wrapper/runtime modules;
- the slim range/session packet still admits and executes its eight-instruction
  smoke program;
- the reference-output canary either admits or fails with a classified generic
  primitive gap; and
- all design notes can be reduced to one short architecture note plus tests.
