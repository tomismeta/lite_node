# Inference Runtime Capability Ladder

Status: active, 2026-08-06.

This document is LiteNode-owned. It states **model-neutral runtime capabilities**
the inference harness and VM expose. LiteNode does **not parse or enforce**
producer stage ids, model-family names, GGUF paths, tensor names, or layer
indices. A non-normative human mapping may mention producer stages; that mapping
is not protocol surface.

Producer-side stage claims live only in `octra-inference`
(`docs/specs/spine-stages.md`). Producers map their stages onto these
capabilities through the existing packet and session-bundle surface.

## Authority

- Roots, sessions, effort, and receipts remain owned by
  [`inference-runtime-contracts.md`](inference-runtime-contracts.md).
- One-VM ownership and scope remain owned by
  [`inference-runtime.md`](inference-runtime.md).
- Deterministic math profiles remain owned by
  [`inference-determinism-hardening.md`](inference-determinism-hardening.md).
- This ladder cannot introduce model-family APIs, new report ontology for
  convenience, or packet fields outside the contracts document.

On conflict, contracts win for roots and session semantics; the numerical
profile catalog wins for math status; this ladder wins only for **ordered
runtime capability readiness**.

## Boundary

LiteNode accepts neutral packets:

```text
program.ocpg
requirement.json
target.json
request.json
model-ranges.json          # optional but required for authenticated loads
model-deployment.json      # optional when harness binds deployment roots
session bundle / transitions
```

LiteNode never interprets:

- producer stage names (`s1` … `s5`);
- model family names, GGUF paths, tensor names, or layer indices;
- host-local operator evidence paths as protocol identity.

Coupling to `octra-inference` is **content roots + harness report JSON only**.

## Capability Ladder

Capabilities are cumulative. Higher rungs assume lower rungs.

| Rung | Capability id | Meaning | Math constraint |
| --- | --- | --- | --- |
| C0 | `admission.neutral_packet` | Exact requirement/target/request admission; fail-closed policy | Profile roots exact-match |
| C1 | `math.p0_candidate` | P0 ops under profile-catalog `consensus_candidate` (local matrix tooling may gate); multi-platform static promotion is C11 | No host exp/trig on these ops |
| C2 | `session.resident_lifecycle` | `open → advance* → finalize`; ABI-v2 continuation; identity CAS | Atomic failed advances |
| C3 | `session.committed_state` | Rooted payload transport under `session.committed-state` | Root-only session identity |
| C4 | `graph.authenticated_ranges` | `FLOAD` of admitted model ranges; session-stable pins preferred | Owner root = content hash |
| C5 | `graph.real_compute` | `execution_contract=graph_real` with executed inference compute evidence | Only admitted opcodes |
| C6 | `graph.composed_slice` | Multiple compute ops in one transition program (e.g. norm + linear) | All ops on admitted profiles |
| C7 | `session.continuous_prefill` | Multi-advance or one fat prefill under one resident session | Same deployment/ranges binding |
| C8 | `decode.selected_index` | Decode transition + VM-emitted selected_index contract | Token authority is the cell only |
| C9 | `decode.prior_state` | Optional prior selected-index inclusion in committed state | Byte/root inclusion only |
| C10 | `runtime.resource_isolation` | Inference off consensus critical path; explicit ceilings | Non-semantic for roots |
| C11 | `math.consensus_ready` | Static `consensus_ready` promotion for ops used on path | Multi-platform ceremony |

### Current rung status (LiteNode view)

| Rung | Status | Notes |
| --- | --- | --- |
| C0 | Met | Neutral admission harness |
| C1 | Met | P0 spine ops statically `consensus_ready` via C11 promotion |
| C2 | Met (local) | Resident lifecycle accepted for candidate sessions |
| C3 | Met (local) | Committed-state ABI present; not durable network store |
| C4 | Partial | FLOAD works; pin reuse / residency still product work |
| C5 | Met (local) | `graph_real` / `lifecycle_only` evidence rules |
| C6 | Met (local) | Composed spine programs admitted and executed: S4 FLOAD+RMSNORM+LINEAR+ARGMAX ran end-to-end on the external harness (`46d63d1`) |
| C7 | Met (local, spine path) | Continuous prefill+decode under one resident session accepted by the harness (spine S4); full-model continuous graph still not claimed |
| C8 | Met (local) | Decode transition bound `decode_token_contract_status` with VM-emitted `selected_index` matching the pinned reference (`310`) |
| C9 | Partial | Prior-state selected-index contract present; S5 multi-decode remains blocked |
| C10 | Not met | Inference off consensus critical path; explicit ceilings |
| C11 | Near-complete | 16 ops statically `consensus_ready`: spine ops (sealed at `46d63d1`), activations on protocol exp/log1p (sealed at `f259b45`), and attention/conv/elementwise on deterministic accumulation/elementwise profiles (sealed at `ccf93cc`, `remaining-path-matrix-20260807-180137`). Only `ROPE_APPLY_INDEXED_FP` host trig remains local_only. |

## Packet contracts LiteNode enforces (no stage names)

### Lifecycle modes

| `lifecycle_mode` | Token contracts | Intent |
| --- | --- | --- |
| `token_generation` | selected_index required for decode product claim | Product decode path |
| `graph_slice` | selected_index optional unless mode is token generation | Compute composition path |

### Execution contracts

| `execution_contract.kind` | Requirement |
| --- | --- |
| `lifecycle_only` | Session binding only; no graph compute claim |
| `graph_real` | At least one executed inference compute opcode under timing; optional `min_program_instructions` |

Transport opcodes (`FLOAD`) alone do **not** satisfy `graph_real`.

### Claim freeze (Rail Lock)

While open focus rungs are **C4–C6**:

1. Do not add harness report fields unless an existing field cannot name a
   fail-closed rejection.
2. Do not add model-named CLI flags or opcodes.
3. Prefer fixing residency, FLOAD pinning, and compute under existing schema.
4. Profile root changes require an explicit catalog cut and dual-repo pin
   update (producer checkpoint first or runtime first by ownership of the
   change).

## Math policy on the ladder

Ops used under C5–C8 must be drawn from matrix-admitted candidate profiles
(or later `consensus_ready`). Host-math profiles (`host-fp-exp-local-candidate`,
`host-fp-trig-local-candidate`) remain local-only and must not be required by
a product continuous path until replaced.

P0 ops currently treated as candidate for ladder use:

- `LINEAR_Q1_G128_FP`
- `RMSNORM_FP_EPS`
- `L2NORM_FP`
- `SOFTMAX_FP` (protocol-owned nonpositive exp)
- `GATED_DELTA_RULE_FP` (protocol-owned nonpositive exp)

P0-plus candidate ops may appear on later composed slices when their profiles
are bound; they do not expand this document’s authority.

## Mapping hint (non-normative)

Producers may map stages to rungs without LiteNode learning stage ids:

| Producer stage (octra-inference) | Minimum LiteNode rungs |
| --- | --- |
| S1 | C0–C6 with FLOAD + norm + linear |
| S2 | C0–C6 larger composed program |
| S3 | C0–C7 continuous prefill |
| S4 | C0–C8 token selected_index |
| S5 | C0–C9 multi-decode prior-state |

This table is documentation for humans. It is not parsed by LiteNode.

## What LiteNode will not do

- Own producer stage progression or Bonsai layer schedules.
- Co-maintain a shared canary repository as protocol authority.
- Treat operator evidence paths as roots.
- Grow report JSON as the default response to composition friction.
- Claim `consensus_accepted=true` from local matrix admission alone.

## Next LiteNode work (ordered)

1. Make C4 solid: session-stable authenticated range pins for repeated advances.
2. Support C6 reliably: multi-op `graph_real` programs with real ranges.
3. Close C7 for multi-advance continuous prefill under one deployment binding.
4. Keep C8/C9 contracts stable for decode without tokenizer semantics.
5. Only then C10 isolation and C11 static math promotion ceremonies.
