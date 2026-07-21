# Inference Runtime Contracts

Status: proposed, 2026-07-21.

This document defines the contracts that must be stable before implementation.
The north-star design is in
[`inference-runtime.md`](inference-runtime.md). Model-driven compute
requirements are tracked in
[`inference-capability-requirements.md`](inference-capability-requirements.md).

## Change And Identity

Git is the history of record for source, specifications, and design changes.
Names do not acquire `v1`, `v2`, or date suffixes merely because their
definition changes during development.

Runtime objects still need unambiguous identities when independently deployed
nodes exchange them. Those identities are content roots, not release numbers:

- a numerical profile is identified by its profile root;
- an effort schedule is identified by its schedule root;
- a capability is identified by its stable name and semantic root;
- a target is identified by its target root; and
- an implementation used for attested evidence is identified by its binary or
  source root.

A serialized format gets a discriminator only when a decoder must safely
distinguish coexisting incompatible encodings. The discriminator belongs in
the encoded object, not in capability names or module names.

## Root Domains

Every root uses canonical encoding and a distinct domain separator. The exact
canonical encoder will be selected with the first implementation and covered
by golden fixtures.

`H` reuses LiteNode's existing hash implementation and canonical encoding
infrastructure. Inference adds domain tags, not a second hashing stack.

```text
requirement_root = H("octra:inference:requirement\0" || requirement)
target_root      = H("octra:inference:target\0"      || target)
request_root     = H("octra:inference:request\0"     || request)
session_root     = H("octra:inference:session\0"     || session)
checkpoint_root  = H("octra:inference:checkpoint\0"  || checkpoint)
receipt_root     = H("octra:inference:receipt\0"     || receipt)
```

Canonical maps sort keys. Canonical sets sort by encoded value. Integer and
floating-point encodings are explicit. Unknown fields fail closed unless a
format explicitly declares them non-semantic.

## Execution Requirement

An execution requirement declares the semantics and limits a target needs. It
contains:

- `vm_semantics_root`, identifying instruction encoding, opcode semantics, and
  the admissible policy surface;
- numerical profile root;
- effort schedule root;
- a sorted set of capability names and semantic roots;
- maximum model, prepared-view, session, scratch, and output bytes;
- maximum effort for one canonical advance; and
- any format discriminator required to decode the object.

The requirement does not contain kernel names, CPU features, thread counts,
cache policy, or model-family names.

Nodes advertise the exact capability roots, numerical profile roots, and
effort schedule roots they support. Admission performs exact matching. It does
not negotiate a fallback, select a nearby profile, or silently downgrade a
target.

The requirement root binds capability and effort schedule roots together. A
node cannot advertise an accepted capability with a different schedule that
undercharges it. Golden fixtures cover accepted and rejected pairings.

Multiple definitions may coexist during a rollout because their roots differ.
Removing support is an operator and network activation decision recorded in
Git and deployment policy, not an implicit decoder preference.

## Target

The target binds:

- admitted program root;
- execution requirement root;
- model release root;
- execution descriptor root;
- authenticated store root;
- session ABI root, identifying target entrypoints, canonical input/output
  shapes, and transition boundaries; and
- target-owned entry points and canonical transition rules.

`octra-inference` produces and qualifies the target. LiteNode verifies its
roots, admitted program, declared requirements, effects, and limits. LiteNode
does not parse the source model or prove that the program faithfully implements
the claimed model.

Admission is therefore a safety and compatibility decision, not a model
correctness proof. Correctness is established by release qualification,
publisher policy, and reference execution evidence.

## Request

A request binds:

- target root;
- input or prompt-token root;
- generation and sampling policy;
- deterministic seed or randomness root where required;
- generation limits;
- stop policy;
- caller authority where required; and
- a caller nonce.

Raw prompt bytes, tokenization rules, and chat templates remain outside
LiteNode unless a separately admitted target explicitly implements them. The
first runtime accepts canonical token input generated and rooted by
`octra-inference`.

Token selection belongs to the target program. The request roots its generation
policy and deterministic seed; the target consumes those values through generic
selection operations. LiteNode does not receive logits and apply an informal
runtime or RPC sampling policy.

## Canonical Session

A canonical session contains only logical progress:

- target root;
- request root;
- sequence counter;
- phase and logical position;
- append-only output-prefix root;
- cumulative committed effort;
- optional committed target-state root;
- terminal status; and
- optional final output root.

The sequence counter is concurrency state, not a software version. A caller
supplies the expected sequence for every mutating operation.

KV caches, recurrent buffers, prepared weights, native workspaces, and other
recomputable execution state are not session identity. They may be held in
memory or restored from a checkpoint, but their presence, layout, and eviction
cannot change the session root.

The optional committed target-state root is reserved for bounded state that
cannot be reconstructed from the target, request, and canonical progress. The
initial deterministic generation target is expected not to need it. A target
must explicitly declare and bound such state; it cannot use this field to root
ordinary KV or recurrent caches.

When committed target state exists, its payload is durable canonical session
data. It is not stored only in an optional checkpoint.

The output prefix is append-only and hash-chained. A successful advance may
append output, replace logical phase or position, increase cumulative effort,
and increment the sequence exactly once.

## Checkpoints

A checkpoint is an optional acceleration artifact for one canonical session
state. It may contain KV pages, recurrent state, or other derived execution
data.

A checkpoint binds:

- canonical session root;
- target and numerical profile roots;
- chunk or page roots;
- logical position; and
- implementation root when the state is not portable across implementations.

Checkpoint roots are not session roots. Nodes may choose different checkpoint
frequencies, layouts, and eviction policies without changing logical identity.
Canonical continuation never requires a checkpoint. Restoring a checkpoint is
an optimization equivalent to replaying prior canonical advances under the
same target, request, numerical profile, and committed canonical state. A
missing or mismatched checkpoint is discarded and derived state is rebuilt by
full canonical replay.

An active advance pins every checkpoint, immutable range, and prepared view it
uses. Pinned objects cannot be evicted until the advance completes or its
candidate state is discarded.

## Session Operations

Every mutating operation supplies an expected sequence. A stale sequence
returns `Sequence_mismatch` and leaves the session unchanged. Concurrent
mutations against the same sequence are rejected rather than queued. `status`
is a pure read and may race with a mutation; it returns one coherent snapshot.

### Open

`open` verifies the target and request, reserves a session slot and canonical
state quota, and creates sequence zero. Those reservations remain until
`finalize` or `cancel`.

Model residency, prepared views, and candidate workspace are acquired per
advance. Insufficient transient capacity returns `Resource_unavailable` or
leaves the advance queued under explicit scheduler policy. It never changes
the session sequence. A queued request holds no sequence lock and revalidates
its expected sequence before execution. `open` does not promise uninterrupted
physical residency.

### Advance

`advance` accepts a session root and expected sequence. It executes exactly one
target-declared canonical transition.

The initial ABI uses one admitted target-owned advance entrypoint. A successful
run to `STOP` produces one canonical transition. The target's session ABI root
binds prefill chunking, generation-step rules, inputs, and outputs. LiteNode
does not infer a boundary from elapsed time or effort, and the initial ABI does
not add a VM yield instruction.

Canonical transition boundaries are deterministic. For example, a target may
declare fixed prompt-prefill chunks followed by one generated token per
transition. Local queue quantum, wall-clock budget, thread count, or kernel
choice cannot change those boundaries.

The scheduler may yield and resume an advance internally. These scheduler
slices are invisible: they create no session sequence, session root, or
receipt. If the target cannot reach its next canonical transition boundary within the
admitted effort limit, the advance fails with no session mutation.

### Status

`status` reads the canonical session and local residency information. Local
diagnostics are returned separately and are not part of the session root.

### Finalize

`finalize` requires a terminal target state, seals the final output and receipt,
and releases resources not needed for receipt retention.

### Cancel

`cancel` is a canonical terminal transition. If an advance is running, a cancel
request sets a cooperative signal. A cancel and advance racing against the same
expected sequence are winner-takes-sequence. If the advance commits first,
cancel returns `Sequence_mismatch`. If cancel wins, the advance discards its
candidate at a bounded safe point and cancel commits the terminal sequence.

There is never a partially committed advance.

## Atomic Execution

A mutating advance never writes committed session or derived state in place.
It operates on an isolated candidate made from copy-on-write pages,
append-only chunks, or an equivalent journal.

Execution follows this order:

1. validate registers, shapes, ranges, encodings, and expected sequence;
2. compute and reserve logical effort with checked arithmetic;
3. pin immutable and prepared inputs;
4. execute into candidate state;
5. validate the candidate output and canonical transition;
6. atomically swap the canonical session and derived-state handles; and
7. release old unpinned resources.

Failure, effort exhaustion, cancellation, or a kernel error discards the
candidate. Commit is a constant-size handle/root swap. Work and memory are
proportional to changed pages, not total session state.

Native kernels may poll cancellation at bounded row, block, or tile boundaries.
Their partial output remains candidate-local and cannot be observed by another
session or receipt.

## Effort

Logical effort measures declared VM work. It is not elapsed time, CPU cycles,
energy, memory pressure, preparation time, or operator cost.

The canonical representation is an unsigned 64-bit integer. Addition,
multiplication, and shape products use checked arithmetic. Overflow rejects the
operation before mutation.

An effort schedule maps an opcode and validated shape metadata to:

```text
base cost + analytic shape cost
```

The calculation is constant time in the number of tensor elements. Metering
never increments inside a kernel element loop. Optimized and scalar
implementations of the same primitive charge identical effort.

Dynamic effort is calculated and reserved before tensor mutation. An advance
is rejected before execution when its maximum remaining effort cannot be
satisfied.

The canonical session accumulates effort from committed advances only. Failed
attempts, scheduler slices, checkpoint reconstruction, model loading, and
prepared-view construction are local resource diagnostics. Existing
transaction or operator charging may account for failed attempts separately,
but it cannot alter the session's logical effort.

Every effort formula and boundary value has golden vectors. Changing a formula
changes the effort schedule root.

## Floating-Point Semantics

The numerical profile is a machine-checkable contract, not a label. It defines
for each active operation:

- input and output bit representation;
- operation and accumulation order;
- rounding behavior;
- FMA policy;
- subnormal behavior;
- transcendental approximation;
- non-finite input and output policy;
- allowed aliasing and in-place behavior; and
- exact or bounded comparison rules.

An exact profile produces bit-identical committed outputs and roots. Scalar and
optimized kernels must match its golden vectors exactly.

A bounded profile may permit a stated numerical tolerance. Its receipts bind
the implementation root and cannot claim cross-implementation output-root
replay. A bounded profile is suitable only for explicitly attested execution.

The first Bonsai semantic qualification uses an exact profile. Performance is
a separate gate. Consensus activation is considered only for an exact profile
after cross-build and cross-machine replay.

## Authenticated Data

The model release contains a content-rooted manifest. An immutable range read
binds owner root, offset, length, encoding, and shape to the authenticated store
root. Publisher signatures or release policy may establish provenance, but
range authenticity does not depend on a trusted filesystem path.

Programs may read authenticated immutable ranges and allocate bounded
session-local scratch. Scratch is not authenticated model state. It becomes
externally meaningful only when committed through a canonical output, session,
or optional checkpoint root.

Prepared views are deterministic derivatives of authenticated ranges. They are
read-only, bounded, pinned while in use, and evictable afterward. Preparation
changes physical resource use and latency only.

## VM Memory

The VM exposes one semantic address space and one authoritative value per
address. Ordinary `MLOAD` and `MSTORE` operations and tensor operations observe
the same value through shared accessors.

Dense tensor pages, prepared layouts, and caches may use specialized physical
storage, but they are not a second inference-visible memory domain. Writes
invalidate or update every physical representation before another operation
can observe the address. A separate inference arena is considered only if the
unified implementation cannot preserve existing VM behavior under compatibility
conformance.

## Scheduler Isolation

Inference never runs on the consensus proposal, epoch apply, finality, or
private-execution critical path.

The node maintains hard protected reservations for consensus and private work.
Inference receives its configured reservation and may borrow only currently
unreserved capacity. Borrowed capacity is revocable and cannot reduce the
protected floors.

Under pressure the runtime, in order:

1. refuses new inference admission;
2. stops starting queued borrowed work;
3. evicts unpinned prepared views;
4. unloads zero-reference resident models; and
5. cooperatively cancels borrowed active work only when required to restore a
   protected floor.

Active advances hold explicit pins and reservations. Scheduler policy may alter
latency, queuing, cancellation, and residency, but never numerical results,
logical effort, or canonical transition boundaries.

## Receipts

A semantic receipt binds:

- target, program, model, requirement, and numerical profile roots;
- request root;
- prior and next session roots and sequences;
- output-prefix or final-output root;
- committed logical effort;
- completion status; and
- node attestation only when required by a separately defined trust policy.

Sequence is encoded inside each canonical session and repeated in the receipt
for auditability. Receipt verification requires the repeated values to match
the prior and next session objects; they are not a second source of truth.

Diagnostics are a separate object. They may include timing, queue delay,
prepared bytes, cache hits, selected kernel, implementation root, CPU features,
thread count, and physical memory use.

Attestation policy is outside the initial runtime contract. Until such a policy
is specified, an implementation root in diagnostics is evidence, not a trust
claim.

## Conformance

Before a capability is activated, conformance covers:

- canonical encoding and root fixtures;
- exact requirement matching and downgrade rejection;
- capability and effort-schedule pairing, including undercharge rejection;
- expected-sequence conflicts and single-writer behavior;
- deterministic canonical advance boundaries;
- cancellation at every supported safe point;
- candidate-state discard after all failure classes;
- effort formula boundaries and overflow;
- checkpoint absence, restoration, pinning, and eviction;
- scalar and optimized numerical vectors; and
- isolation under mixed consensus, private, and inference load.

Pure requirement, policy, effort, and tensor conformance must run without PVAC.
Full-node integration may link an accepted PVAC backend or an explicit
unavailable backend that fails closed for every PVAC entrypoint. No fake
cryptographic behavior is accepted as a production or integration-test boundary.
