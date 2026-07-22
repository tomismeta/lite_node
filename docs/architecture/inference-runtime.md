# LiteNode Inference Runtime

Status: proposed, 2026-07-21. Phase 0 design-lock pass.

Baseline: `octra-labs/lite_node` `main` at
`2a5803bf62b33d542a5d7adab64a1133e9c0340c`.

## Document Authority

- This document owns the one-VM decision, ownership, roadmap, code shape, and
  scope.
- [`inference-runtime-contracts.md`](inference-runtime-contracts.md) owns roots,
  identity, effort, sessions, cancellation, atomic execution, receipts, and
  scheduler isolation.
- [`inference-capability-requirements.md`](inference-capability-requirements.md)
  is the non-normative working inventory of model-driven compute needs.
- [`vm-semantic-inventory.md`](vm-semantic-inventory.md) is the observational
  audit of the current VM surface and has no authority over runtime contracts.
- [`bonsai-demo-readiness.md`](bonsai-demo-readiness.md) records the current
  local demo path and is not a protocol authority.

On conflict, contracts override this document for roots, identity, effort,
sessions, cancellation, and receipts. The capability inventory cannot override
either document. After Phase 0, roots, canonical advances, effort, rollback,
and the one-VM boundary are closed unless implementation reveals a factual
contradiction.

Bonsai 27B is the first qualification model. It is not part of the VM
contract.

## Decision

LiteNode will have one VM, one bytecode, one verifier, and one effort model.
Inference will be a first-class node execution domain built from:

- an admitted program;
- explicit execution requirements;
- authenticated immutable data;
- bounded, sequenced sessions;
- resident resource management; and
- rooted execution receipts.

The VM supplies small, model-neutral operations. The node runtime owns
residency, scheduling, and session lifecycle. `octra-inference` owns model
interpretation and generates the program that composes the operations.

Inference is therefore first class without becoming a third VM or a
model-specific VM mode.

Git is the history of record. Protocol-visible semantics use content roots for
identity rather than release names such as `v1` or manually maintained revision
numbers. A serialized object has a format discriminator only when incompatible
encodings must coexist safely. Hashing reuses LiteNode's existing hash and
canonical encoding infrastructure with inference-specific domain tags. There is
no second hashing stack.

## Invariants

1. Existing legacy, program, consensus, and HFHE behavior stays unchanged by
   default.
2. A model that fits existing capabilities requires no LiteNode source change.
3. LiteNode source must not contain model names, tensor names, layer numbers,
   tokenizer rules, or model-family dispatch.
4. Every accepted operation has bounded inputs, explicit aliasing rules,
   analytic effort, atomic failure, and a scalar reference definition.
5. Protocol semantics never depend on ambient environment variables, cache
   warmth, thread count, CPU dispatch, or a selected native kernel.
6. Public advance boundaries are target-declared and deterministic. Local
   scheduler slices never create session identity or receipts.
7. Model and session identities exclude KV caches, prepared layouts, and other
   recomputable execution state.
8. A failed or canceled advance changes no visible session state.
9. Inference never executes on the consensus proposal, epoch apply, finality,
   or private-execution critical path.
10. Normal, inference, and private work have protected resource ceilings. A
    resident model cannot starve consensus or private execution.
11. Plain inference semantics and conformance do not depend on PVAC. The full
    node may compose plain and encrypted capability families.
12. Unknown profiles, capabilities, encodings, opcodes, and format
    discriminators fail closed.
13. LiteNode admission proves bounded, permitted execution. It does not prove
    that a target faithfully implements a claimed model.

## Ownership

The fork has two authors of truth. `octra-inference` produces a rooted target.
The Octra VM admits and executes that target without learning source-model
rules.

| Surface | `octra-inference` responsibility | Octra VM responsibility |
| --- | --- | --- |
| Source model | Parse and convert releases | Treat model identity as rooted data |
| Target program | Generate bytecode and ABI | Verify code, flow, effects, and policy |
| Execution requirement | Declare roots, capabilities, and limits | Match roots and limits exactly |
| Tensor layout | Choose packing and bundle layout | Execute model-neutral memory ops |
| Numerical behavior | Produce qualification evidence | Implement semantics named by the root |
| Scheduling shape | Declare per-advance expectations | Reserve, dispatch, cancel, and meter |
| Sessions | Define advance ABI and canonical payloads | Enforce sequence CAS and atomic mutation |
| Model residency | Build bundles and content roots | Authenticate ranges and prepare views |
| Sampling | Generate rooted sampling policy | Execute target-owned selection |
| Encrypted work | Declare private values and capabilities | Fail closed without crypto support |
| Receipts | Provide target and diagnostic evidence | Commit execution and output roots |

The public boundary is a rooted target. LiteNode does not inspect a model
format or reconstruct a model graph.

### Controversial boundaries

| Boundary | Current decision |
| --- | --- |
| Target correctness | VM proves safety; `octra-inference` owns qualification evidence. |
| Numerical roots | Roots bind semantics; performance evidence is separate. |
| Tensor packing | Packing belongs to target data; VM operations stay model neutral. |
| Prepared views | Views are acceleration only, never canonical state. |
| Sampling | Sampling policy is target-owned and rooted with the request. |
| Effort schedules | Capability roots and effort roots are matched together. |
| Encrypted inference | A separate PVAC-backed capability family; not part of the plain inference path. |
| Model names | Runtime code admits capabilities and roots, never model names. |

## Runtime Shape

```text
octra-inference
  model release
  execution descriptor
  generated program
  execution requirement
  rooted request
          |
          v
LiteNode
  target + request admission
          |
          v
  inference scheduler ---- normal/private protected reservations
          |
          v
  resident immutable model ---- prepared execution views
          |
          +---- optional checkpoint (acceleration only)
          |
          v
  sequenced session ---- target-owned advance entrypoint
          |
          v
  execution receipt ---- separate diagnostics
```

Authoritative field lists, error conditions, and transition rules live in the
contracts document. The summaries below are architectural orientation only.

### Execution requirement

A target declares its requirements. LiteNode must never infer a profile from
the first tensor opcode it encounters.

The canonical requirement binds:

- `vm_semantics_root` (instruction encoding, opcode semantics, admissible
  policy surface);
- numerical profile root;
- effort schedule root;
- a sorted set of model-neutral capability names and semantic roots;
- memory and per-advance limits; and
- a format discriminator only when required for safe decoding.

Capabilities describe semantics, not implementations. Representative
capability families are:

- `tensor.fixed`;
- `tensor.q1-g128`;
- `tensor.attention`;
- `sequence.causal-convolution`;
- `sequence.delta-rule`; and
- `storage.authenticated-range`.

The exact names are protocol decisions made with the first implementation.
They must remain algorithmic and model neutral. Host-floating-point capability
families are future-only until a numerical profile and admission path make them
deterministic.

The node advertises supported requirements. Admission performs exact matching
of declared capability roots, numerical profile roots, and effort schedule
roots together. It does not negotiate a fallback or silently downgrade a
target. Kernel names and CPU features belong in runtime evidence, not in the
semantic requirement root. Runtime-derived support roots require the VM
semantic inventory to be executable and are not claimed by the local harness.

### Target

An inference target binds:

- `program_root` (the concrete admitted program);
- execution requirement root;
- model release and execution descriptor roots;
- authenticated store root;
- `session_abi_root` (input/output shape and canonical transition rules); and
- the single target-owned `advance` entrypoint for the current phase.

The runtime ABI is small:

- `open` creates a session against immutable roots and reserves the session
  slot and canonical-state quota until finalize or cancel;
- `advance` runs the single target-owned advance entrypoint to the next
  canonical transition;
- `status` returns a pure coherent snapshot of sequence and progress;
- `finalize` seals the output and receipt; and
- `cancel` is a canonical terminal transition that ends the session and
  releases retained resources.

Only `advance` enters generated model execution. Successful completion at
`STOP` produces exactly one canonical transition. Prefill chunking and
one-token generation rules are bound by the target's `session_abi_root`. The
scheduler may yield internally, but those local slices produce no session
sequence, root, or receipt. Layer selection, tensor selection, and token
selection stay inside the generated program.

### Immutable model data

The runtime exposes authenticated, immutable byte ranges to admitted programs.
A range is identified by an owner root, offset, length, encoding, and shape.
Aliases are zero-allocation subranges of one authenticated owner.

A prepared view may transform an authenticated range into a runtime-efficient
layout. It is keyed by source identity, encoding, shape, numerical profile
root, and implementation root. It is bounded, read-only, evictable, and
reconstructable.

Prepared views never become model identity, contract state, or session state.
Logical effort remains identical on a cold miss and a warm hit.

### Session

A session owns only canonical request progress. Immutable model data is shared.
Each mutation carries an expected sequence and commits a new sequence
atomically. Authoritative session fields are defined in the contracts document.

KV caches, recurrent buffers, and native workspaces are derived execution
state. Canonical continuation never requires a checkpoint. Missing or invalid
derived state is rebuilt by replaying prior canonical advances. Optional
checkpoints are acceleration only. Advances write isolated candidate pages and
commit with a constant-size handle/root swap.

### Memory

Inference uses one unified semantic address space. `MLOAD`, `MSTORE`, and
tensor operations observe the same authoritative value per address. Dense
pages, prepared layouts, and caches are physical implementations underneath
that space. A separate inference arena is chosen only if compatibility testing
proves the unified implementation untenable.

### Scheduler

Inference is first class at the scheduler, not privileged over the rest of the
node. Consensus and private work retain hard protected reservations. Inference
may borrow only unreserved capacity, and borrowed resources are revocable.

`open` reserves the session slot and canonical-state quota until finalize or
cancel. Model residency and advance workspace are acquired per advance.
Insufficient transient capacity returns `Resource_unavailable` without changing
sequence or session identity.

Admission ceilings cover:

- immutable model bytes;
- prepared-view bytes;
- session bytes;
- concurrent sessions;
- effort per advance;
- queued work; and
- execution time used for local cancellation policy.

The node may advertise an inference capability without keeping every model
resident. Residency is a cache governed by roots and resource limits, not a
global model registry. An active advance pins its immutable ranges, prepared
views, and candidate pages until commit or discard.

### Receipt

A stable receipt binds semantic facts defined in the contracts document,
including prior and next session roots and sequences. Receipt sequences must
equal the sequences encoded by those session roots. Timing, cache hits,
preparation time, selected kernel, thread count, and CPU features are
diagnostics. They may be attested in evidence, but they do not alter execution
identity.

## VM Composition

### Primitive families

The VM should grow by coherent primitive families:

1. Tensor memory and movement: typed loads, bounded copies, views, and gathers.
2. Dense arithmetic: matrix products, elementwise operations, reductions,
   normalization, and activations.
3. Position and attention: indexed rotary position application, KV append,
   attention, and selection.
4. Compressed arithmetic: fully specified encodings such as Q1-G128 behind exact
   logical semantics.
5. Stateful sequence operations: causal convolution and algorithmic state
   transitions such as gated delta-rule updates.

An algorithmic delta-rule state transition is defensible if its grouping,
gating, aliasing, and rollback semantics are complete and model neutral. Its
name must describe the mathematical transition rather than a model or network.
`SSM_CONV_SILU_FP` should first be expressed as causal convolution followed by
the existing SiLU operation. A fused operation is acceptable only when a
measured boundary cost or atomicity requirement justifies a separately
specified semantic primitive.

### Primitive admission rule

A new primitive is admitted only when all of the following are true:

1. Its name and behavior are independent of a model family.
2. Shapes, ranges, encodings, and output ownership are explicit.
3. Bounds, overflow, overlap, non-finite values, and partial writes are
   specified.
4. Effort is derived from logical work and is independent of optimization.
5. A simple scalar implementation defines the result.
6. Bytecode, assembler, compiler, verifier, policy, and type-flow surfaces are
   updated together.
7. Malformed, bounds, rollback, round-trip, and differential vectors pass.
8. Each optimized kernel reproduces the reference result across seeded,
   irregular shapes.

Large model-layer opcodes and opaque host callbacks are not accepted.

### Numerical profiles

Q16 remains a deterministic reference and conformance profile. Advanced Bonsai
execution requires a content-rooted floating-point profile that defines
operation order, rounding, FMA and subnormal policy, transcendental
approximations, finite-value rules, and allowed implementation transformations.

An exact profile produces bit-identical committed roots. A bounded profile may
permit stated tolerances, but its receipt binds the implementation root and it
cannot claim cross-implementation output-root replay. Bonsai first qualifies
against an exact profile; semantic and performance qualification remain
separate.

The exact floating-point oracle, compiler and toolchain lock, golden vectors,
and cross-build matrix live in Git and are identified by content roots. They
do not require a manually named profile generation.

Host floating-point tensor operations remain admission-disabled until that
profile passes cross-build and cross-machine replay. A capability label alone
does not authorize host floating point. First-class local or attested inference
does not require premature consensus activation.

### Sampling and selection

Token selection belongs to the generated target, not the LiteNode scheduler or
RPC layer. The request roots generation policy and any deterministic seed. The
target invokes generic selection operations. The first Bonsai path may use
in-program argmax. LiteNode never informally receives logits and samples
outside rooted execution.

### Encrypted inference

Encrypted inference reuses the target, session, scheduler, storage, and receipt
lifecycle. It adds explicit encrypted value and operation capabilities.

There is no implicit conversion between plain and encrypted tensors. A target
declares which values are public, encrypted, or committed. Public immutable
weights may share prepared views while prompts, activations, state, or outputs
use private capabilities. This keeps privacy composable without placing PVAC
inside every plain inference path.

## Code Shape

Implementation should follow the existing LiteNode character:

- small records and variants instead of classes or service containers;
- abstract types with narrow `.mli` files;
- pure planning and validation functions in `lib/vm`;
- effectful residency, scheduling, RPC, and wiring in `node_runtime`;
- `*_shell` modules at I/O and lifecycle boundaries;
- explicit function arguments or small dependency records;
- no dynamic plugin system, model registry, generated framework, or ambient
  global policy; and
- one focused commit per protocol surface.

Expected modules are deliberately few:

| Module | Responsibility |
| --- | --- |
| `Execution_requirement` | Canonical requirements and capability matching |
| `Admission` | Existing verification plus optional profiled-program admission |
| `Opcode_policy` | One authoritative opcode classification |
| `Inference_target` | Root binding and target admission |
| `Inference_request` | Request root binding and target compatibility checks |
| `Inference_model` | Authenticated immutable range descriptors and later views |
| `Inference_plan` | One validated binding of program, target, request, model, pins, and input |
| `Inference_session` | Sequenced state and lifecycle transitions |
| `Inference_scheduler` | Resource reservation and bounded dispatch |
| `Inference_receipt` | Stable semantic receipt construction |

Supporting pure modules such as `Numerical_profile`, `Effort_schedule`, and
`Inference_checkpoint` may keep contracts and golden fixtures focused. They are
not additional runtime services, registries, or extension frameworks.

The existing `Admission.of_code`, `Admission.of_program`, and deployment paths
retain their behavior. Profiled admission is additive and generic. It should
reuse the existing program certificate and verifier rather than introduce an
`Admission.Inference` bytecode category or a parallel decoder.

## Workable Fork Done Line

The fork is workable for the first plain inference demo when all of these are
true:

1. `octra-inference` emits a Git-reproducible packet with an OCPG program
   envelope, execution requirement, target, request, and immutable range
   descriptor.
2. LiteNode admits that packet from a clean branch and rejects mismatched roots,
   unsupported requirements, over-limit requests, and invalid ranges.
3. A local session runner can `open`, `advance`, `status`, `finalize`, and
   `cancel` without mutating committed state on failure.
4. The Bonsai canary produces the expected token sequence and internal roots
   through target-owned execution, not caller-owned layer orchestration.
5. The result includes a semantic receipt, separate diagnostics, exact source
   and binary roots, and `consensus_accepted=false`.
6. No model-family identifier appears in LiteNode runtime code or tooling.
7. PVAC remains an external encrypted-workload boundary. Plain inference does
   not invoke it, and no substitute crypto implementation is shipped here.

That is the demo-done line. It is not the devnet-done or encrypted-done line.
Remote capability advertisement, cross-node receipts, consensus activation,
and encrypted inference remain later phases.

Current implementation status: the local fork can admit a five-file packet,
authenticate and pin immutable owner bytes, bind them with a request and
authenticated input into one execution plan, execute one target-owned
`advance` through the fixed session ABI, finalize, and emit output and
diagnostic candidate roots. The
plan-bound runner is still local candidate evidence: it does not provide
runtime-derived support roots, resident model state,
node scheduling, or qualified Bonsai numerical semantics. The Bonsai canary is
not complete until accepted tensor primitives execute the target-owned
numerical path inside LiteNode.

## Roadmap

The order below is a dependency order, not a calendar estimate.

### Phase 0: design lock

1. Accept this architecture, the companion runtime contracts, and the document
   authority rules above.
2. Lock checkpoint recovery, canonical advance mechanism, sequence conflicts,
   reservation lifetime, sampling ownership, unified memory stance, identity
   terminology, and hashing reuse as recorded in the contracts document.
3. Resolve every old Bonsai compute item through the capability requirements
   process without freezing the legacy list as an API. Close the inventory to
   expansion except from new model evidence.
4. Define golden root, effort, session-transition, requirement-mismatch, and
   numerical fixtures.
5. Record deferred policy, including attestation and network activation, as
   explicitly outside the initial runtime.

Exit gate: no unresolved decision can change roots, effort, admission types, or
canonical session transitions. After this gate, those surfaces are closed
unless implementation reveals a factual contradiction.

### Phase 1: clean lineage and build boundary

1. Preserve prototype branches and remote evidence as reference.
2. Create the implementation branch from the then-current `upstream/main` and
   record the exact Git commit.
3. Carry over only the accepted design packet and test fixtures.
4. Establish a clean full-node build against the accepted PVAC backend. The
   inference conformance path must remain independent of PVAC operations.
5. Establish PVAC-independent requirement, policy, effort, and tensor
   conformance.
6. Add a source scan rejecting Qwen and Bonsai identifiers from LiteNode runtime
   code.

Exit gate: a clean upstream branch with reproducible full-node and pure
conformance boundaries.

### Phase 2: execution profile foundation

1. Add content-rooted execution requirements and exact capability matching.
2. Refactor opcode policy into one authoritative classification without
   changing current decisions.
3. Add profiled program admission by composing the existing verifier,
   certificate, effect scan, and type-flow checks.
4. Add target, request, and immutable range-descriptor root binding against an
   admitted program and execution requirement.
5. Add local authenticated owner-byte pinning and a minimal session/receipt
   proof harness.
6. Add the unsigned checked effort representation and schedule roots.
7. Keep legacy and ordinary program admission behaviorally compatible.

No tensor opcode is added in this phase.

Exit gate: a program can declare model-neutral requirements, bind target,
request, and immutable data roots, and fail closed on an unsupported node while
existing admission remains unchanged.

### Phase 3: tensor substrate

1. Establish one authoritative memory value per address in the unified
   semantic address space while proving existing memory behavior unchanged. A
   separate inference arena is a fallback only if that prove-out fails.
2. Add bounded typed range access and isolated candidate writes.
3. Harden active Q16 and floating-point operations against invalid ranges,
   overlap, non-finite values, effort exhaustion, and partial writes.
4. Add only missing generic shape operations, beginning with bounded copy,
   explicit-epsilon normalization, F32 loading, and indexed RoPE.
5. Land every operation with wire, verifier, compiler, policy, effort, type
   flow, and scalar conformance.

Exit gate: synthetic programs safely compose the substrate without model data.

### Phase 4: compressed and stateful semantics

1. Specify Q1-G128 encoding, linear, and gather semantics.
2. Specify required standalone activation and normalization operations.
3. Add generic causal convolution with candidate state.
4. Specify the parameterized delta-rule state transition.
5. Attention binary outcome: either existing attention becomes fully generic
   and specified under the primitive admission rule, or it is decomposed into
   batched score, mask, softmax, and weighted-sum operations before Bonsai
   qualification.
6. Exercise a synthetic structurally different schedule before stabilizing
   capability roots.

Exit gate: scalar semantics pass malformed, boundary, rollback, effort, and
exact numerical fixtures. Attention has a recorded binary disposition.

### Phase 5: optimized execution views

1. Add native kernels behind accepted scalar semantics.
2. Add bounded authenticated prepared views and per-advance pinning.
3. Prove scalar/optimized equality across irregular shapes, offsets, thread
   counts, cancellation points, and cold/warm preparation.
4. Keep logical effort and semantic roots independent of implementation.

Exit gate: optimized execution changes latency and physical resources only.

### Phase 6: first-class node runtime

1. Add target and request admission with authenticated range access.
2. Add resident model ownership and bounded eviction.
3. Add canonical sessions, optional checkpoints, and single-writer sequencing.
4. Add protected scheduling and opportunistic inference borrowing outside all
   consensus/private critical paths.
5. Implement `open`, `advance`, `status`, `finalize`, and `cancel` internally.
6. Emit semantic receipts, golden fixtures, and separate diagnostics.

Exit gate: many sessions share one model safely; failures and cancellation
discard candidate state; normal and private limits remain intact.

### Phase 7: Bonsai 27B qualification

1. Generate a rooted Bonsai target using only accepted generic capabilities.
2. Move layer and tensor dispatch into target-owned canonical advances.
3. Eliminate caller-visible microprogram orchestration and dynamic bundles.
4. Pass exact semantic qualification independently of performance.
5. Exercise cold load, warm residency, concurrency, cancellation, checkpoint
   restoration, eviction, and process restart.
6. Publish repeatable performance and physical-resource evidence against a
   separately agreed gate.

Exit gate: Bonsai runs end to end with zero model-specific LiteNode code and no
host-owned layer schedule.

### Phase 8: generality and devnet

1. Qualify a second model whose schedule exercises a materially different
   primitive family.
2. Add capability advertisement and remote target admission.
3. Exercise mixed normal, private, and inference load.
4. Verify receipts, replay, restart recovery, Git-pinned upgrades, and exact
   requirement rejection across a devnet.
5. Activate only profiles whose deterministic and resource gates pass.

Exit gate: a new compatible model requires target generation and data only,
not a LiteNode patch.

### Phase 9: encrypted inference

1. Define encrypted tensor and value requirements without changing plain
   profiles.
2. Bind private value ownership into requests and optional checkpoints without
   changing canonical plain-session semantics.
3. Compose existing private capabilities with public immutable weights.
4. Add encrypted rollback, effort, leakage, and receipt conformance.
5. Qualify mixed plain/encrypted scheduling under the same resource model.

Exit gate: encrypted inference is an explicit capability composition, not a
fork of the runtime.

## Prototype Disposition

The current experiments are evidence, not the implementation base.

Carry forward:

- generic primitive semantics and conformance vectors;
- exact Bonsai token and internal-root evidence;
- authenticated physical bundle rules;
- prepared-view performance evidence;
- atomic state and rollback behavior; and
- current-base opcode allocation work.

Rebuild from upstream:

- `Admission.Inference` and `decode_inference`;
- `Inference_profile.required` and `first_q16` profile inference;
- the experimental `Inference_policy` admission path;
- duplicated opcode-name inventories;
- model-specific fused operations;
- ambient kernel selection as semantic policy; and
- the monolithic inference extension commit.

## Completion Criteria

The first stable plain inference runtime is complete when:

1. Existing workloads pass unchanged.
2. Bonsai 27B runs through a resident, target-owned session lifecycle.
3. A second model schedule runs without LiteNode source changes.
4. Every inference-activated VM operation is model neutral, bounded, metered,
   and atomic.
5. Numerical profiles and implementation evidence are separately rooted.
6. Normal, inference, and private workloads remain independently bounded.
7. A prepared-view miss changes latency only, never result or effort.
8. No caller selects model layers, tensors, or checkpoints.
9. The effort schedule is content-rooted and covered by golden vectors.
10. Source scanning rejects model-family identifiers from LiteNode runtime code.
11. Cross-build replay passes for every activated exact profile.

Encrypted composition readiness is complete when the plain target, session,
scheduler, storage, and receipt contracts require no incompatible change to
support explicit encrypted capabilities. Encrypted execution is qualified
separately in Phase 9.
