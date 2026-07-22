# Inference Lean Rebuild Notes

Status: proof-branch notes, 2026-07-22.

This branch is now treated as a product-learning branch. It should not be the
shape merged back to LiteNode. Its job is to prove which VM capabilities are
actually needed for Bonsai-shaped inference, then inform a smaller rebuild from
fresh `upstream/main`.

## Current Evidence

There are three completed VM-side proofs on the inference VPS.

The first is the Bonsai slim range/session run:

```text
/home/exedev/evidence/octra-inference/bonsai-slim-range-session-20260722-031944
```

It admits and executes an eight-instruction range-read smoke program with one
authenticated model range. The direct rerun on 2026-07-22 returned
`status=accepted`, output root
`c4b6e2bd7fc99932610633bdcfe9a0083b1658d5eb345ec652a5584a3174f238`,
candidate root `e1e4660ac0eb18d914f8bf654f9b2585cbfba339536f595abe7438d75b2efe46`,
and `consensus_accepted=false`.

The second, stronger proof is the Bonsai/Qwen reference-output canary:

```text
/home/exedev/evidence/octra-inference/bonsai-qwen-canary-20260722-115933
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
| Program root | `a8974fd88f84f7c7b1d60018524c563a8b65633e5e81a7ed371b841761d9cca1` |
| Requirement root | `4addd0966ae40f9987da47619a9a133a84935a23d8d1b3a6936c0a457b183051` |
| Target root | `ac5b0da3ed1b2ad2bbe89020016af8f41e563df39d5f0936b33783e6df32fbd1` |
| Request root | `0c0a5bda0f4e4733d630bb3687e8277f3b600b38f9db61576c8036659902945f` |
| Model ranges root | `7c62e4d070b9275920adf543d8d0b663715bb92b4d7a6a1c5db3162292f4b117` |
| Program bytes | `920` |
| Program instructions | `9` |
| Program effects | `memory_write`, `memory_read` |
| Model range count | `1` |
| Entrypoint | `advance` at label `100` |
| Output root | `17f407f6b6575913f1e9614d16569be4c7646d22fb5f505289af3f35496c2966` |
| Candidate root | `de9b5af76925c08216f7cbb45deb4c8bea5cd092fad6fe0641842ce2f98831fc` |
| Effort delta | `15` |
| Consensus accepted | `false` |

The accepted canary includes:

| Artifact | SHA-256 |
| --- | --- |
| `program.ocpg` | `b0d615a47f65456d783fbc8de5d3f8ce74fe977c2f7e96edf2b78fdb63f9f2e5` |
| `requirement.json` | `056eb61e7179b59a0cadcd4ccc2bd2264211565ed8a48e8cdfb538cb8f3c3996` |
| `target.json` | `b5b58b097b9dd6c0bf9d691154e88df1aef60450057fc8136f5f2b8b368c0701` |
| `request.json` | `b854e6b1627fe2ae2b31b36da882f1f07740e8e1f47af51e004485baf1799364` |
| `model-ranges.json` | `17af531afbb08f23acc507723606d8079a07bca09d81f1d817568140626d2d51` |
| `request-input.json` | `1735d767ed73ef9078c082bdef8edf17ff293f14b5bde56f72562b423a221fd4` |
| `reference-output.cjson` | `b552ac18f2cf4d69c0bf6bfeee7c9607b6dd36dc460a15d3e362e455a3c7bcef` |
| `opcode-manifest.json` | `d8ec170017b4f7334d4884c9f8a9292d0dc70ec09b8cea1b390077293464282b` |
| `unsupported-capability-assumptions.json` | `71a5b819037cc86d25505838531799f47caa1a97ba834b045ac8774562d9d24e` |
| `admission-report.json` | `95c063e200ba8760e5aa50998b7c5a06e2a8983c893288f3a74cdc24dbd852db` |

The canary reference execution used Bonsai 27B GGUF source hash
`17ef842e47450caeb8eaa3ebfbbab5d2f2278b62b79be107985fb69a2f819aa0`,
prompt `In a hidden network, the model whispered`, generated token id `310`,
generated text ` to`, expected output SHA-256
`6ebe62fa9087afa9a69b0b0baf0533ac07b8711db7ff11771e953d9540bf4a1f`,
and expected output root
`f3f1c38b855446011b4706015bee9a16406b3d0af4484172e76e586bba13367c`.

The third proof is the first VM-native Bonsai primitive canary:

```text
/home/exedev/evidence/octra-inference/bonsai-primitive-canary-20260722-131306
```

It binds immutable Q1 owner bytes through `model-ranges.json` and `FLOAD`,
then attempts a projection using the existing generic `MATMUL_FP` opcode. The
packet shape is clean: `target.json` remains model-neutral, Bonsai/Qwen/Q1
details stay in sidecars, and admission reaches the real VM opcode policy
decision.

The LiteNode harness rejects the packet:

```text
consensus unsafe opcode MATMUL_FP at pc 2569
```

The direct rerun on 2026-07-22 returned:

| Field | Value |
| --- | --- |
| Status | `rejected` |
| Failure classification | `admission-policy` |
| Attempted primitive | `linear_q1_0_g128_fp` |
| Executed VM primitive | `matmul_fp` |
| Program root | `1cd4cbf8b91569c58515922f98fdf396c2da7c98b58455324dac67897e4e41b0` |
| Requirement root | `bc25891494f8ecc4c39bf3b7bde374d094b92280c9185407dd501937ba6711ac` |
| Target root | `2630810c31ec05ac1d26d29f63a557a6ec38af73d19e684c952622d949bf27ac` |
| Request root | `aca610446a5b1fbff216f3b0d136b356fd3a734cf076f0defb82b678804b0e25` |
| Model ranges root | `2e75eba8c4c8503bc35cd92b45bd1d286d98a0d02ecc305d050ba2a64b25d148` |
| Q1 owner root | `57f9dadd168580d1b39660f54e5a21b480a70bc3017a90e3892f7c74f2895785` |
| Program bytes | `12013` |
| Opcode counts | `JDEST=1`, `LDI=1290`, `FLOAD=1`, `MSTORE=1280`, `MATMUL_FP=1`, `STOP=1` |

The primitive canary proves that range binding and packet neutrality are good
enough to hit a real primitive admission decision. It does not justify
admitting `MATMUL_FP`. In this branch, `tensor.strict-fp` remains a roadmap
capability family, not an accepted consensus profile. Broad host floating-point
execution should stay rejected until LiteNode has deterministic numerical
semantics, fixtures, and cross-platform conformance tests.

The lean path should instead add the smallest deterministic, model-neutral
primitive needed by Bonsai/Qwen projection, most likely a Q1-G128 projection
primitive under a capability such as `tensor.q1-g128`. Treat
`linear_q1_0_g128_fp` as a descriptive legacy label until the final semantics
are fixed. That keeps the VM generalized for inference without turning plain
inference admission into a blanket host-float permission.

That primitive must specify byte layout, group size, scale decoding,
accumulation order, rounding, bounds, output representation, failure
atomicity, and effort accounting. If it returns floating-point bytes, those
bytes still need software-defined numerical semantics and cross-platform
conformance vectors.

Caveat: this evidence used the existing built VPS harness at LiteNode source
commit `2a5803b`, with executable hash
`98ac8bed595e180adb252f9e6247b4db6087db1b3d35b03e6544ab963d4cefcc`.
The VPS source was not rebuilt for this rerun because `dune exec` was blocked
by a missing `digestif.c`. Treat the result as valid gate evidence from that
built harness, not as a fresh source-build reproducibility proof.

These proofs show:

- OCPG program-envelope admission works through the LiteNode harness.
- Requirement, target, request, and model-range roots bind coherently.
- The current `advance` ABI works: label `100`, request input cell `1000`,
  output base/count in `r0`/`r1`.
- Authenticated immutable owner bytes can be pinned and addressed by a rooted
  model range.
- The local session path can open, advance, finalize, and emit output,
  candidate, session, and receipt roots.
- A Bonsai/Qwen reference-output sidecar can stay outside the VM while the VM
  admits and executes the small model-neutral output ABI program.
- A VM-native primitive canary can bind immutable Q1 bytes and reach the
  inference opcode policy boundary.

This does not prove:

- LiteNode executes the full Bonsai/Qwen graph.
- LiteNode has accepted implementations for the full required math primitive
  set.
- LiteNode decodes Q1 bytes or projects directly from authenticated Q1 ranges.
- `MATMUL_FP` is safe to admit for consensus inference.
- The current session and receipt protocol should survive into the lean branch.
- Consensus or encrypted inference is ready.

## Gap Classification

The accepted reference-output canary is intentionally an ABI and boundary proof.
Its unsupported-assumptions sidecar states that it does not claim LiteNode
executes the full Qwen/Bonsai graph. Full Bonsai execution still requires
VM-native support for the Qwen35/Bonsai kernel set, including Q1 grouped binary
projections, deterministic RMSNorm with model epsilon, RoPE, attention,
SSM/recurrent state, softmax/argmax, tokenizer-compatible session state, and
bounded output receipts.

This accepted canary therefore does not establish a new primitive requirement
by itself. It establishes that the packet boundary, target neutrality,
reference-output sidecars, immutable range pinning, and output ABI are coherent.
The rejected primitive canary establishes the next admission boundary, but not
Q1 execution inside LiteNode, because `octra-inference` decoded Q1 and
materialized operands before the VM reached `MATMUL_FP`.

Future failures should be classified into one of these buckets before changing
LiteNode. The primitive canary above is the first classified
`admission-policy` failure:

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
smoke proof, reference-output canary, or generic-`MATMUL_FP` primitive canary.
Those now exist.

Until LiteNode exposes a deterministic projection primitive, the useful
`octra-inference` work is to supply the exact reference contract for that
primitive:

1. Provide scalar reference semantics for Q1-G128 grouped linear projection,
   including byte layout, scale decode, accumulation, rounding, and output
   bytes.
2. Provide tiny golden fixtures with input bytes, Q1 owner bytes, dimensions,
   expected output bytes, output SHA-256, and output root.
3. Preserve the same model-neutral packet boundary: roots and capabilities in
   LiteNode-facing files; Bonsai/Qwen/tokenizer details in sidecars.
4. Once LiteNode has the primitive, re-emit the primitive canary using that
   opcode rather than `MATMUL_FP`.
5. Run the LiteNode admission/session harness and classify any failure as a
   packet, admission-policy, missing-primitive, data-binding, effort/limit,
   determinism, or harness-only gap.

Only after that deterministic primitive canary fails for a classified LiteNode
reason should the VM add or revise additional math capabilities.

## Rebuild Acceptance Bar

The fresh branch is ready to replace this proof branch when:

- the changed-file count is materially smaller than this branch;
- generic admission and normal VM behavior remain unchanged by default;
- inference code is isolated behind the inference wrapper/runtime modules;
- the slim range/session packet still admits and executes its eight-instruction
  smoke program;
- the Bonsai/Qwen reference-output canary still admits and executes its
  nine-instruction output ABI program; and
- all design notes can be reduced to one short architecture note plus tests.
