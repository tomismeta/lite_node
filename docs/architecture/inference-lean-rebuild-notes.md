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
capability family and proof-harness gate for enumerated activation opcodes, not
an accepted consensus profile. Broad host floating-point execution should stay
rejected until LiteNode has deterministic numerical semantics, fixtures, and
cross-platform conformance tests.

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

The `octra-inference` side has now supplied the scalar Q1-G128 contract and
tiny golden fixtures:

```text
/home/exedev/evidence/octra-inference/q1-g128-golden-fixtures-20260722-134349
```

Key fixture values:

| Field | Value |
| --- | --- |
| Scalar contract | `contracts/q1-g128-scalar-reference.md` |
| Rust oracle | `contracts/q1-g128-scalar-reference.rs` |
| Primitive label | `linear_q1_0_g128_fp` |
| Dimensions | `m=2`, `k=256`, `n=3`, `group_size=128`, `block_bytes=18` |
| Numeric profile | `scalar_binary64_accumulation` |
| Input bytes | `4096`, SHA-256 `7c0a594504cdff24a4869c837082443200a552263c209821610033b1c88d3bec` |
| Q1 owner bytes | `108`, SHA-256 `57f9dadd168580d1b39660f54e5a21b480a70bc3017a90e3892f7c74f2895785` |
| Expected output bytes | `48`, SHA-256 `43411283d083bd6e959bca6aa7edbebc55d8ad251510d992bd52046ac71d9d22` |
| Output root | `ff5f8319d1e207f368c50ed98b8a361639db527f486c8acdbb62a2c56037cea1` |

This proof branch exposes that contract as `LINEAR_Q1_G128_FP`, gated in plain
inference by `tensor.q1-g128`. Generic Program admission still rejects it as
consensus-unsafe, and `MATMUL_FP` remains rejected. The implementation is still
evidence for the lean rebuild, not the final merge shape: the fresh branch
should either prove the binary64 profile with cross-machine conformance or move
this primitive to software-defined fixed arithmetic before treating it as
consensus-ready.

The existing LiteNode session ABI roots VM values, not raw tensor bytes. A
direct canary should therefore compare the output cells decoded as little-endian
binary64 bit patterns against `expected-output.f64le.bin`, while reporting the
LiteNode session output root separately.

The direct Q1-G128 canary now exists:

```text
/home/exedev/evidence/octra-inference/q1-g128-direct-canary-20260722-154147
```

It uses `LINEAR_Q1_G128_FP`, requests `tensor.q1-g128`, does not request
`tensor.strict-fp`, binds the Q1 owner bytes through `model-ranges.json` and
`FLOAD`, and keeps `target.json` model-neutral.
The accepted LiteNode rerun is recorded beside the original artifact as
`lite-node-admission-session-report.litenode-rerun.json`.

The same proof branch now exposes the standalone activation frontier as
`SIGMOID_FP`, `SOFTPLUS_FP`, and `SILU_FP`, gated in plain inference by explicit
`tensor.strict-fp`. The gate is enumerated opcode-by-opcode; it does not
authorize `RMSNORM_FP` or the rest of the older host-floating-point family.

## Schedule Frontier Bundle

`octra-inference` emitted the first schedule-derived frontier bundle:

```text
/home/exedev/evidence/octra-inference/schedule-frontier-bundle-20260722-172350
```

The next Bonsai frontier is order `3`:
`load_f32_le_fp + ssm_conv_silu_fp + softplus_fp + sigmoid_fp`. This branch adds
the generic `LOAD_F32_LE_FP` typed-ingress opcode under
`storage.authenticated-range`, the standalone activation opcodes `SIGMOID_FP`,
`SOFTPLUS_FP`, and `SILU_FP` under explicit `tensor.strict-fp` proof admission,
and resolves the `ssm_conv_silu_fp` boundary as `CAUSAL_DEPTHWISE_CONV1D_FP`
followed by `SILU_FP`. LiteNode still does not add a fused SSM opcode or admit
the older host-floating-point instruction family.

`octra-inference` re-emitted that order-3 frontier against LiteNode `d9aa419`:

```text
/home/exedev/evidence/octra-inference/schedule-frontier-bundle-d9aa419-clean-20260722-203424
```

`load_f32_le_fp`, `sigmoid_fp`, `softplus_fp`, and composed
`ssm_conv_silu_fp` all passed `--scan-policy`, `--run-session`, and root
comparison. The SSM scalar fixture root matched
`f1b483b26afa993db5622ad0010c3e56b3e4a7cefaf687529dad1c3c66767d56`; the
session output root was
`b0a0dab87ae96b29d34b36ee9a07732bb272fddb5b78c311f4c19b77519beabd`.

The next derived frontier after order `3` is order `4`:
`gated_delta_net_fp`.

```text
/home/exedev/evidence/octra-inference/schedule-frontier-after-order3-d9aa419-20260722-205053
```

That bundle is a range-bound sentinel only. It proves the packet boundary and
target neutrality, but it does not define or execute the delta-rule recurrence.
The sidecar names `q`, `k`, `v`, `prepared_gate`, `prepared_beta`, and
`recurrent_state` inputs, and `recurrent_output` plus `next_recurrent_state`
outputs. LiteNode should not add a delta-rule opcode until `octra-inference`
emits a deterministic scalar contract and golden fixture for that transition.

The source-built LiteNode harness was run from this branch with the explicit
inference harness profile:

```sh
PATH="$HOME/.cargo/bin:$PATH" dune build --profile inference-harness tools/inference_admit.exe
```

The inference path remains independent of PVAC operations. On the latest
upstream-based branch, the harness builds against the accepted PVAC backend
rather than an unavailable replacement. The direct canary returned:

| Field | Value |
| --- | --- |
| Status | `accepted` |
| Program root | `56e62d99541f63047bf764284bd08eacf66adf193934e63e2f6e49aaf5f5d2cf` |
| Requirement root | `86dd8f5c256316a284a25fdc8a8283276b1ad63b00e1a916e426dba8b526d2fd` |
| Target root | `12f39161a78f6059002d6cd4478ff94baa30289a282cdcbba74165393db88cbd` |
| Model ranges root | `6e424f390369268cfd1b04474d98811f1fa58741b2218c6b6ed5db9086e73bb5` |
| Request root | `c71daa34cb9d5fa86d8d614d02d63feb8aa61e8a1e38a64e3d24438914e32f48` |
| Program instructions | `1037` |
| Program effects | `memory_read`, `memory_write` |
| Output root | `c247a85d33a9f54701d786a08515ca57cce6da2ed18b49547720bfd6d816f215` |
| Candidate root | `d963aa41b0250732baa50fb1c9089519a2eb129cef8a9214afc982174370ffd9` |
| Effort delta | `2364` |
| Consensus accepted | `false` |

The raw tensor fixture remains separately rooted by `octra-inference`: output
SHA-256 `43411283d083bd6e959bca6aa7edbebc55d8ad251510d992bd52046ac71d9d22`
and raw tensor fixture root
`ff5f8319d1e207f368c50ed98b8a361639db527f486c8acdbb62a2c56037cea1`.

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
smoke proof, reference-output canary, generic-`MATMUL_FP` primitive canary,
standalone Q1 fixture package, direct Q1 canary, order-3 frontier canary, or
order-4 scalar evidence package. Those now exist.

The next useful artifact is an executable order-4 canary that emits
`GATED_DELTA_RULE_FP` directly, requests `sequence.delta-rule`, runs
`--scan-policy`, then runs `--run-session` when admitted. It should compare the
VM recurrent output and next-state bytes to the existing order-4 evidence roots
and keep all Bonsai/Qwen/tokenizer details in sidecars. It should also clean up
the stale `model_binding_pending` frontier metadata noted in the first evidence
package.

Preserve the same model-neutral packet boundary, keep Bonsai/Qwen/tokenizer
details in sidecars, and classify every failure as a packet, admission-policy,
missing-primitive, data-binding, effort/limit, determinism, or harness-only gap.
Only after the executable order-4 canary fails for a classified LiteNode reason
should the VM add or revise the primitive.

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
