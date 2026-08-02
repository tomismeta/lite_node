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

Every root uses a schema-specific encoding and a distinct domain separator. The
local implementation uses fixed field order for records, explicit sorting for
semantic sets, and the existing bytecode encoder for programs; golden fixtures
preserve those decisions.

`H` reuses LiteNode's existing hash implementation plus schema-owned
deterministic encoders. Inference adds domain tags, not a second hashing stack.

```text
requirement_root = H("octra:inference:requirement\0" || requirement)
program_root     = H("octra:inference:program\0"     || admitted_program)
target_root      = H("octra:inference:target\0"      || target)
capability_set_root = H("octra:inference:capability-set\0" || capabilities)
model_deployment_root = H("octra:inference:model-deployment\0" || model_deployment)
range_root       = H("octra:inference:model-range\0" || range)
model_ranges_root = H("octra:inference:model-ranges\0" || model_ranges)
request_root     = H("octra:inference:request\0"     || request)
session_abi_root = H("octra:inference:session-abi\0" || session_abi)
session_root     = H("octra:inference:session\0"     || session)
output_prefix_root = H("octra:inference:output-prefix\0" || output_prefix)
checkpoint_root  = H("octra:inference:checkpoint\0"  || checkpoint)
receipt_root     = H("octra:inference:receipt\0"     || receipt)
```

Incoming JSON object order is not semantic: packets are parsed into typed
records and re-encoded before hashing. Unknown fields fail closed unless a
format explicitly declares them non-semantic. A future binary descriptor
encoding may replace JSON, but it must preserve the same field semantics and
golden roots or land as an explicit incompatible encoding.

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

Inference admission also checks the instruction stream against the declared
capability names. Authenticated range loads require
`storage.authenticated-range`; fixed-point tensor instructions require
`tensor.fixed`. Host-floating-point instructions remain rejected by the
consensus-safe admission path until a numerical profile defines and enforces
their semantics. A capability declaration never turns an otherwise unsafe
instruction into a consensus-safe one.

The local inference harness may admit selected proof opcodes under explicit
capabilities while their deterministic semantics are being qualified. That is
candidate execution, not validator readiness. A proof opcode becomes
validator-grade only when its semantic root is backed by the determinism
contract, scalar oracle, edge-case vectors, cross-platform conformance, and
failure atomicity tests described in
[`inference-determinism-hardening.md`](inference-determinism-hardening.md).

Plain inference forbids `FHE_*` opcodes, storage/object-state reads and writes,
blob writes, external calls, deploys, transfers, events, and journal operations.
It also does not advertise `MATMUL_Q16` until Program type-flow supports that
opcode. Encrypted inference must enter through a separately defined encrypted
profile; the plain runner also disables FHE in its VM execution context.

Multiple definitions may coexist during a rollout because their roots differ.
Removing support is an operator and network activation decision recorded in
Git and deployment policy, not an implicit decoder preference.

## Model Deployment

A model deployment is rooted model identity. It is not a LiteNode-owned model
registry and it does not contain raw tensor bytes. Octra-native storage roots
carry durable model state; LiteNode admits, executes, and proves against those
roots.

The first deployment object contains:

- model root;
- store root;
- tensor index root;
- optional tokenizer root;
- numerical profile root;
- capability set root; and
- optional default program root.

The default program root is a convenience pointer, not authority. A session may
bind another admitted program to the same deployment if the target,
requirement, and policy roots check out.

When supplied to the local harness, the deployment descriptor must match the
admitted target's model and store roots, the requirement's numerical profile
root, and the canonical capability set root derived from the requirement
capabilities. During a session run, the deployment root is included in session
identity, so receipts commit to the descriptor. LiteNode still does not parse
source model metadata, model-family names, tokenizer files, or publisher
evidence.

## Target

The target binds:

- admitted program root;
- execution requirement root;
- model release root;
- execution descriptor root;
- authenticated store root;
- session ABI root, identifying the canonical input/output shape and transition
  boundary; and
- the single target-owned `advance` entrypoint for this phase.

The first accepted target ABI is exact: request schema `1`, entrypoint
`advance`, label `100`, request input root at memory cell `1000`, output base in
`r0`, output cell count in `r1`, and retained candidate state equal to canonical
VM memory.

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

The first local admission packet uses the narrower boundary fields `schema`,
`target_root`, `entrypoint`, `input_root`, `request_nonce`,
`max_output_bytes`, and `max_advance_effort`. Admission checks target-root
equality, requires `entrypoint = "advance"`, and checks request limits against
the execution requirement. Generation policy, stop policy, caller authority,
and sampling roots remain target/request-owned extensions before network
activation.

Raw prompt bytes, tokenization rules, and chat templates remain outside
LiteNode unless a separately admitted target explicitly implements them. The
first runtime accepts canonical token input generated and rooted by
`octra-inference`.

Token selection belongs to the target program. The request roots its generation
policy and deterministic seed; the target consumes those values through generic
selection operations. LiteNode does not receive logits and apply an informal
runtime or RPC sampling policy.

## Session ABI

The session ABI is a small descriptor, not a registry. ABI v1 seeds
`memory[1000]` with `VString request.input_root` before entering the target
program and exposes the exact request input bytes as `blobs[input_root]`.
Bytecode can therefore use `MLOAD` followed by authenticated `FLOAD` without
baking request-specific roots into the admitted program.

ABI v2 preserves the v1 input/output convention and additionally seeds
canonical pre-advance progress:

```text
memory[1001] = VInt sequence
memory[1002] = VInt logical_position
memory[1003] = VString output_root
memory[1004] = VString output_prefix_root
memory[1005] = VString committed_target_state_root, or "" when absent
```

The v2 ABI root binds these cell numbers and value encodings. It does not
promote diagnostic `candidate_root` into committed target state.

Only `advance` at label `100` enters model execution. Additional target
entrypoint aliases are rejected until the session protocol explicitly grows.

## Output ABI

The first local execution path uses one fixed, model-neutral output convention:

- `r0` is the non-negative base memory address;
- `r1` is the non-negative number of memory cells; and
- the canonical output is the ordered value encoding of that span.

The span must be initialized, its encoded size must fit `max_output_bytes`, and
opaque values fail closed. Registers, storage, and blobs are not retained
candidate state in the plain runner. The convention is bound into the output
root with the target's `session_abi_root`; a future ABI descriptor may replace
the fixed convention without changing the surrounding session protocol.

Local session reports may include `output_payload` and `output_payload_sha256`
next to `output_root`. The payload is a client witness, not a second authority:
`output_root` commits to the target, session ABI, and exact payload bytes. A
decode target that writes a single `ARGMAX_FP` selected index through this ABI
can therefore return a root-bound token id without adding tokenizer or
model-family knowledge to LiteNode.

Session bundles may declare a transition-level `output_contract`:

```json
{"kind":"selected_index","output_base":200,"output_count":1}
```

For a `decode` transition this binds the generic token-output contract. The
runner checks that the canonical output payload uses the declared base, contains
one initialized integer cell, and reports the selected index. The contract does
not by itself prove which opcode produced the cell; ARGMAX provenance comes from
the admitted program and opcode evidence. A mismatch is an execution-contract
failure; an absent decode contract leaves the session accepted as a resident
candidate but keeps `decode_loop_token_contract` in
`missing_runtime_capabilities`.

This is an execution boundary, not a model-format contract. Token sequences,
logits, tensor layouts, and sampling behavior remain target-owned.

Session bundles may also declare a transition-level `execution_contract`:

```json
{"kind":"graph_real","min_program_instructions":4}
```

`lifecycle_only` is the explicit shell contract: the transition proves resident
session binding but does not claim graph execution. `graph_real` requires the
admitted program to contain generic inference compute opcode evidence, run with
opcode timing, show at least one generic inference compute opcode in the
executed opcode profile, and meet any declared minimum instruction count. Static
opcode presence is preflight evidence only. A preflight mismatch rejects before
session advance; a runtime evidence mismatch is reported after the transition
runs. An inference opcode in an untaken branch remains an overclaim and reports
`no_inference_opcode_executed`. This keeps lifecycle-only prefill from being
overclaimed as Bonsai graph work. The contract is intentionally generic. It does
not prove model-family semantics, tensor-layout correctness, full-model
completeness, or tokenization. `runtime_semantics` also reports a diagnostic
`graph_execution_contract_summary` with required, bound, and mismatched counts
so callers can audit session-level graph-real claims without re-walking every
transition. Decode token and prior-state contracts use the same summary shape
under `decode_token_contract_summary` and `decode_prior_state_contract_summary`;
`graph_executed_opcodes` lists matched graph-real transition ids with the
runtime-observed generic inference compute opcodes; transport opcodes such as
range loading are not graph compute evidence.
`decode_selected_indices` is a diagnostic list derived only from matched decode
selected-index output contracts; the per-transition contract objects remain the
detailed authority. A
`transition_root_chain_summary` reports how many transitions produced advanced
session roots, advance receipt roots, and output-prefix roots, plus any
incomplete transition ids. `first_transition_issue` points to the first
transition explaining the session-level blocker when one exists; callers still
use the transition object for exact contract and root evidence. Preflight
graph-real rejections expose the same pointer under `continuation_preflight`.
For multi-decode sessions, an omitted prior-token contract is reported as
`decode_loop_prior_state_contract_not_bound`; a declared-but-invalid contract is
reported as `decode_loop_prior_state_contract_mismatch` and fails closed.
Likewise, an omitted decode selected-index contract is reported as
`decode_loop_token_contract_not_bound`; a declared-but-invalid selected-index
contract is reported as `decode_loop_token_contract_mismatch`.
If any transition program is inadmissible before resident execution, the
session report stays structured with `program_admission_rejected`,
`execution_attempted = false`, an empty transition execution list, and a
preflight transition plan carrying unsupported opcode, missing capability, and
policy details.

## Canonical Session

A canonical session contains only logical progress:

- target root;
- request root;
- model-ranges root;
- optional model-deployment root;
- sequence counter;
- phase and logical position;
- append-only output-prefix root;
- cumulative committed effort;
- optional committed target-state root;
- candidate-state root for diagnostics and receipts, excluded from session identity;
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

Committed target-state payload transport uses a separate session ABI root from
the progress-only v2 ABI. The existing v2 ABI root is unchanged. A target that
needs this transport must use the committed-state session ABI root and request
the generic `session.committed-state` capability. Under that ABI:

- `memory[1005]` remains the committed target-state root cell;
- the root is the raw SHA-256 of the opaque payload bytes;
- the payload is bound through the VM blob table by that root;
- `FSTORE` is admitted only with `session.committed-state` and carries the
  `storage_write` program effect;
- prior payloads are rebound before execution only when
  `sha256(payload) == committed_target_state_root`;
- after execution, the runner inspects `memory[1005]`, requires the referenced
  blob to exist, verifies the raw SHA-256 again, and retains only that payload;
  and
- retained payload bytes count against `max_session_bytes` but are not copied
  into the session identity JSON.

This is resident committed-state transport. It is sufficient for a local
`open_session -> prefill -> decode -> finalize` harness, but it is not a
durable devnet state store until the payload is written to a content-addressed
session store outside the in-process runner.

ABI v1 accepts at most one successful advance. ABI v2 permits repeated
advances when the target consumes prior canonical progress or explicitly
committed target state through the ABI cells above. In both cases, logical
position starts at zero and increments after a committed transition. The
candidate root is the canonical VM memory payload retained at the transition
boundary for diagnostics. It is not committed target state and not part of the
session root.

The output prefix is append-only and hash-chained from target-owned output
roots. A successful advance may append output, replace logical phase or
position, increase cumulative effort, and increment the sequence exactly once.
Generated tokens are target outputs under this prefix, not a separate
model-specific session field. Receipts chain through their prior and next
session roots; there is no separate receipt-chain root in session identity.
The initial prefix is:

```text
H("octra:inference:output-prefix\0" || request_root || "\0open")
```

Each committed output appends:

```json
{"prior_root":"<previous output prefix root>","output_root":"<transition output root>"}
```

under the same `octra:inference:output-prefix` domain. The transition
`output_root` itself remains the fixed output ABI root and is reported
separately from `output_prefix_root`.

Receipts expose `effort_delta`, the effort consumed by that transition. The
session retains cumulative committed effort for admission and accounting; it
is not repeated as the per-receipt value.

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

The first local runner enforces `max_session_bytes` against the canonical
session identity encoding at transition boundaries.

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

The first local range descriptor binds `model_root`, `store_root`, and a sorted
set of immutable ranges. Each range contains `owner_root`, `offset`, `length`,
`encoding`, and optional `shape_root`. Admission checks roots, names, positive
lengths, checked byte bounds, duplicate ranges, and equality with the admitted
target's model and store roots. It does not perform storage I/O, prepare native
views, or assert publisher provenance.

Plain inference programs may read authenticated immutable ranges and request
input bytes through `FLOAD`, then allocate bounded session-local memory scratch.
They may not read or mutate storage/object state, store blobs,
call/deploy/transfer, emit events, or use journal operations. Scratch is not
authenticated model state. It becomes externally meaningful only when committed
through a canonical output, session, candidate, or optional checkpoint root.

The first local runner enforces `max_scratch_bytes` against the canonical
mutable-memory payload at the transition boundary. Peak host allocation
reservation is deferred until VM memory writes share one bounded allocator.

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
Full-node integration links the accepted PVAC backend. No fake cryptographic
behavior or unavailable replacement is part of the production boundary.
