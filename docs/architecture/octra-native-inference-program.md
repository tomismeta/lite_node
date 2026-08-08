# Octra-Native Inference Program

Status: active coordination charter, 2026-08-08.

## Authority

This document owns the integration direction, workstream boundaries, and
delivery gates for converging the inference runtime with Octra-native Circles,
programs, node scheduling, and receipts.

It does not redefine protocol semantics:

- [`inference-runtime.md`](inference-runtime.md) owns the one-VM decision and
  the producer/runtime ownership boundary.
- [`inference-runtime-contracts.md`](inference-runtime-contracts.md) owns
  roots, targets, requests, sessions, effort, atomicity, and receipts.
- [`inference-determinism-hardening.md`](inference-determinism-hardening.md)
  owns the validator-grade numerical qualification bar.
- [`inference-fast-path-runtime.md`](inference-fast-path-runtime.md) owns
  non-semantic performance work.
- [`inference-runtime-capability-ladder.md`](inference-runtime-capability-ladder.md)
  owns ordered runtime capability readiness.

On conflict, those documents win in that order of subject-matter ownership.
This charter cannot broaden the opcode surface, weaken deterministic math,
introduce model-family behavior into LiteNode, or create a second VM, storage
system, scheduler, or receipt ontology.

Git is the history of record. Protocol objects use content roots for identity.
This program does not add release numbers to capability names, modules, or
documents. A serialized format gets a discriminator only when incompatible
encodings must coexist safely.

## Decision

Inference will be delivered as a composition of existing Octra architecture:

```text
Circle resources             immutable model and tokenizer bytes
Circle-backed store root     authenticated resource graph
AML/OCTB program             authority, policy, and forkable product behavior
LiteNode VM                  deterministic model-neutral execution
Node runtime                 residency, scheduling, cancellation, and recovery
Program/session receipts     canonical execution commitments
PVAC                         later private transport and state composition
```

There is no inference-specific chain, storage network, public packet protocol,
VM, finality path, or privacy protocol.

The current `ModelDeployment` ontology remains the VM-facing identity. It does
not become Circle-specific. Its `store_root` binds a Circle resource graph that
the node runtime resolves before execution. The VM continues to receive
authenticated immutable ranges and never performs network or Circle reads from
inside an opcode.

The physical model bytes live in Circle assets. Program state holds deployment,
program, request, session, and receipt roots, not gigabytes of weights or KV
state.

## Product Boundary

The normal product path is:

```text
user
  -> Circle-hosted application
  -> normal Octra program request
  -> AML/OCTB authority and policy binding
  -> node inference service reservation
  -> resident deployment and session
  -> admitted OCPG execution in the existing VM
  -> standard program/session receipt
  -> rooted program state transition
```

Local packet files, conformance corpora, cutpoint reports, and proof-lab
commands remain developer evidence. They are not the public product API.

## Execution Authority Modes

The product may expose more than one execution implementation, but every result
must name its authority honestly.

| Mode | Purpose | Authority |
| --- | --- | --- |
| Signed worker candidate | Low-latency preview and performance development | Worker identity and signed request/output binding only |
| Deterministic validator | Consensus-admissible execution and replay | Bound numerical profile, VM semantics, and canonical receipt roots |
| Private PVAC execution | Future private request, state, or result transport | Separately specified PVAC-backed capability plus the same plain numerical semantics |

A worker signature is not a deterministic proof. A signed worker may prepare
views, retain resident state, and produce a candidate result, but consensus
authority requires the network's explicitly selected validation policy over the
deterministic execution contract.

The exact network settlement policy is a deliberate integration decision. It
must reuse Octra consensus and resource-attestation machinery and must keep
large inference work outside proposal, epoch-apply, finality, and private
transaction critical paths.

## Stable Logical Contracts

### Model deployment

The existing deployment identity remains:

```text
ModelDeployment
  model_root
  store_root
  tensor_index_root
  tokenizer_root option
  numerical_profile_root
  capability_set_root
  default_program_root option
```

`default_program_root` is a convenience pointer. A program or contract may bind
another admitted inference program to the same deployment.

### Circle resource graph

`store_root` binds a canonical resource graph. A resource entry contains the
minimum information required to resolve and verify bytes:

```text
CircleResource
  circle_id
  circle_assets_root
  asset_path
  blob_hash
  asset_offset
  length
```

The graph also commits its deterministic ordering, child graph roots when
present, and the Circle snapshot used for each asset root. The graph must be
content-addressed and immutable for the lifetime of an admitted deployment.

The root derivation must define the binding between Octra's Circle asset roots
and the inference `store_root`. Neither a gateway response nor a worker-signed
manifest is independent storage authority.

### Authenticated range resolution

The node runtime resolves ranges before VM execution:

```text
resolve_range(model_deployment_root, model_range)
  -> verify deployment and store roots
  -> locate Circle resource and snapshot
  -> verify Circle assets root and blob hash
  -> return immutable bytes from cache or resource storage
  -> bind bytes to the admitted FLOAD owner root
```

The resolver may cache and prefetch. Cache paths, warmth, eviction, and prepared
layouts never enter model, session, or receipt identity.

### Program invocation

The invocation binds:

```text
InferenceInvocation
  model_deployment_root
  inference_program_root
  request_root
  input_root
  entrypoint
  execution limits
```

The public request enters through normal Octra program or Circle behavior. The
node may schedule an asynchronous inference advance, but it must preserve the
program's authority, request identity, cancellation rules, and eventual receipt
binding. The integration must not recursively create a second VM execution
model inside an opcode.

### Session lifecycle

The canonical lifecycle remains:

```text
open -> prefill -> decode* -> finalize
```

The session identity binds deployment, program, request, sequence, committed
state root, generated-token root, output-prefix root, and receipt-chain root as
defined by the contracts document.

Large KV or recurrent-state bytes may remain in authenticated resident storage.
Canonical state roots and transition payloads must support restart recovery and
fail-closed continuation. Process-local cache contents are never authority.

## Numerical Contract

Storage determinism and arithmetic determinism are separate requirements.
Authenticated int8, Q1, or floating-point bytes do not define the arithmetic
used to consume them.

Every consensus-admissible operation binds:

- input and output encodings;
- tensor layout and traversal order;
- quantization scale and zero-point interpretation;
- accumulator domain and operation order;
- rounding, overflow, saturation, and non-finite behavior;
- aliasing and mutation rules;
- exact effort;
- atomic failure behavior; and
- scalar oracle and punitive fixture roots.

The scalar profile is permanent semantic authority. Native kernels, SIMD,
threading, prepared weights, and hardware dispatch are implementations only.
They are eligible for validator use after bit-identical differential coverage
across supported architectures, CPU paths, floating-point environments,
failure cases, effort, mutations, and receipt roots.

An int8 deployment does not justify an int8 VM operation by itself. The
authoring side must first supply the complete numerical contract, including
scale granularity, accumulator width, normalization, softmax, activation, and
KV-cache semantics. New operations remain model neutral.

## Repository Ownership

### `octra-inference`

The producer owns:

- source-model parsing and conversion;
- content-addressed chunking and Circle upload plans;
- Circle resource graph and tensor-index generation;
- quantization and layout selection;
- OCPG program generation;
- AML-facing deployment and invocation artifacts;
- reference execution and qualification evidence; and
- developer CLI and proof-lab workflows.

It does not define LiteNode opcode semantics, admission policy, scheduler
behavior, or consensus readiness.

### LiteNode VM

The VM owns:

- bytecode and type-flow verification;
- model-neutral operation semantics;
- numerical and effort profile matching;
- atomic memory mutation and failure;
- authenticated-range instruction behavior; and
- canonical execution output used by receipts.

It does not parse GGUF, model families, tokenizer rules, tensor names, layer
indices, Circle application UX, or producer stage names.

### LiteNode runtime

The node runtime owns:

- Circle deployment lookup and root verification;
- immutable range resolution and cache pinning;
- resident deployment lifecycle;
- inference session lifecycle and restart recovery;
- resource reservations, fairness, cancellation, and timeouts;
- candidate-worker dispatch when policy permits it; and
- standard program/session receipt integration.

It does not alter VM semantics based on cache state, CPU type, worker choice, or
runtime configuration.

## Parallel Workstreams

Each workstream has one write owner. Agents may read other workstreams but may
not change their files or schemas without handing the change back to the owner.

### A. Foundation and deterministic math

Repository: LiteNode.

Write scope:

- `lib/vm/` inference semantics and admission;
- `native_math/`;
- focused VM and native differential tests; and
- deterministic-math architecture documents.

Deliverables:

1. Green build and strict test suite from a clean committed tree.
2. One safe streaming root implementation using existing hash infrastructure.
3. No unsafe polymorphic native transport or private runtime-layout parsing.
4. Scalar/native equality across supported implementation paths.
5. Cross-platform and hostile-environment conformance for the current binary.

This workstream adds no inference operations while stabilization is open.

### B. Circle deployment authoring

Repository: `octra-inference`.

Write scope:

- model conversion and authoring modules;
- Circle deployment/resource graph schemas owned by the producer;
- upload planning and verification commands; and
- producer-side fixtures and tests.

Deliverables:

1. Content-addressed chunking with deterministic packing.
2. Resumable Circle upload plan with no local path authority.
3. Canonical Circle resource graph and `store_root` derivation.
4. Canonical tensor index over authenticated ranges.
5. Small fixture, int8 qualification model, and Bonsai-scale import plans.

This workstream does not modify LiteNode or introduce model-family fields into
VM-facing objects.

### C. Circle range resolver

Repository: LiteNode, separate worktree from A.

Write scope:

- new node-runtime Circle deployment and range-resolution modules;
- immutable cache and pinning modules;
- focused node-runtime tests; and
- no VM opcode implementation files.

Deliverables:

1. Resolve `store_root` to a verified Circle resource graph.
2. Resolve an admitted model range to verified immutable bytes.
3. Supply the existing FLOAD execution boundary without semantic changes.
4. Cache by content identity with deterministic invalidation on root mismatch.
5. Prove missing, stale, moved, or mutated resources fail closed.

### D. Resident service and program boundary

Repository: LiteNode, separate worktree from A and C.

Write scope:

- new node-runtime inference service modules;
- scheduler/resource-lane integration;
- normal program/AML invocation adapter;
- standard receipt integration; and
- no numerical opcode implementation files.

Deliverables:

1. Deployment lookup and admission by root.
2. Resident `open`, `advance`, and `finalize` lifecycle.
3. Explicit CPU, memory, concurrency, and cancellation limits.
4. Restart-safe committed-state continuation.
5. Standard program/session receipts with no parallel product report format.
6. Inference excluded from consensus and private critical paths.

### E. Conformance and integration

Repository: test/evidence scope in the owning repository; no production source
changes without reassignment.

Deliverables:

1. Shared canonical root fixtures consumed unchanged by both repositories.
2. Circle asset to FLOAD to VM output integration test.
3. AML/program request to resident session to receipt integration test.
4. Multi-platform deterministic root matrix.
5. Mixed normal, private, and inference load-isolation tests.
6. Performance reports that keep diagnostics outside canonical roots.

### F. Merge curation

Repository: clean worktrees from current upstream heads.

This workstream remains read-only until earlier gates pass. It prepares a small
review series rather than merging the research history as one change:

1. model-neutral VM semantics and scalar authority;
2. requirements, admission, profiles, and effort;
3. authenticated ranges and Circle resolver;
4. sessions, residency, scheduling, and receipts;
5. conformance and consensus promotion; and
6. independently qualified native acceleration.

Model-specific harnesses, generated evidence, local paths, and proof-lab
utilities do not enter the LiteNode production series.

## Dependency Graph

```text
                 contract and root freeze
                           |
          +----------------+----------------+
          |                |                |
          v                v                v
 A. VM and math    B. Circle authoring   C. range resolver
          |                |                |
          +----------------+----------------+
                           |
                           v
               D. resident service + AML
                           |
                           v
                 E. integration matrix
                           |
                           v
                    F. merge curation
```

A, B, and C may proceed in parallel after the shared root fixture is frozen. D
may build against test doubles, but it does not claim integration until B and C
meet through the same fixture. E consumes accepted interfaces and never repairs
producer or runtime mismatches by changing evidence values.

## Delivery Gates

### Gate 0: clean foundation

- LiteNode and `octra-inference` build from clean committed trees.
- Formatting, strict lint, tests, and diff checks pass.
- Unsafe candidate-root and duplicate hash paths are removed.
- Model-specific LiteNode harnesses are excluded from the production surface.

### Gate 1: shared root identity

- Both repositories consume one unchanged Circle deployment fixture.
- Model deployment, store, resource, tensor-index, and range roots match.
- Unknown fields, stale snapshots, and altered bytes fail closed.

### Gate 2: storage vertical slice

- A public Circle asset is resolved through the node runtime.
- FLOAD receives the authenticated bytes without filesystem authority.
- VM output and receipt roots match the scalar reference.

### Gate 3: Octra program vertical slice

- A normal program request binds deployment, program, request, and limits.
- The node opens a resident inference session and executes one advance.
- The resulting standard receipt commits the expected roots.
- Restart and cancellation preserve atomicity.

### Gate 4: deterministic qualification model

- A small model performs prompt-token input through VM-owned token selection.
- At least three validators on at least two supported architectures produce the
  same output, state, effort, and receipt roots.
- Signed-worker and validator modes are labeled and cannot be confused.

### Gate 5: first devnet model

- A Circle-deployed int8 model uses a fully specified numerical profile.
- Resident prefill and multi-token decode run through the Octra program path.
- Resource ceilings protect normal and private work.
- Product latency and validator replay latency are reported separately.

### Gate 6: Bonsai scale target

- The full Bonsai deployment is content-addressed across Circle resources.
- Range hydration is resumable and root-verified.
- Resident execution uses no per-layer process or packet reconstruction.
- Deterministic and accelerated paths preserve identical canonical roots.

### Gate 7: private composition

- Public deterministic inference remains unchanged.
- PVAC protects the explicitly selected request, state, or output surfaces.
- Privacy failures cannot downgrade into public execution.

## Agent Coordination Protocol

Every work item starts with:

- repository and base commit;
- exclusive write scope;
- input contract roots or fixture hashes;
- required tests;
- forbidden changes; and
- the delivery gate it advances.

Every handoff returns:

```text
commit SHA
files changed
contracts and fixture roots consumed
contracts and fixture roots produced
tests run and exact results
performance results when relevant
known limitations and first blocker
git status
```

An agent must not:

- modify another repository without explicit ownership;
- modify files owned by another active workstream;
- regenerate an accepted fixture to make a mismatch pass;
- add fields to a shared schema without owner review;
- turn host paths, environment variables, worker identities, or cache state into
  protocol authority;
- claim consensus readiness from a single machine or native path;
- put model-family terms into LiteNode production interfaces; or
- launch or clean remote workloads owned by another active agent.

## Qualification Sequence

The sequence separates platform proof from scale proof:

1. A tiny model-neutral fixture proves Circle storage, FLOAD, VM execution,
   sessions, and receipts.
2. A small int8 model proves the first useful devnet product path and its exact
   numerical profile.
3. Bonsai remains the demanding compatibility, import-scale, and performance
   target.
4. Private inference follows the public deterministic path rather than
   redefining it.

Model choice does not change the VM architecture. It changes only producer
artifacts, required generic capabilities, resource demand, and qualification
evidence.

## Definition Of The Desired Result

The program is complete when a user can invoke a forkable Octra program bound
to a Circle-hosted model deployment, LiteNode can resolve and retain the model
without trusting host paths, a resident session can produce tokens under a
rooted deterministic numerical profile, and Octra can commit and replay the
result through its standard state and receipt architecture without starving
normal or private workloads.

