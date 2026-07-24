# Recordless: reducing type-feedback recording overhead in the Ř interpreter

**Status:** working design snapshot as of **2026-07-24**. The author is actively
iterating; see *Current implementation status* for what is stable vs. in flux.

**Scope / provenance note.** This document was reconstructed from a single long
working session. A few pieces of framing (notably the "observation vs.
persistence" distinction and the "is this general or Ř-specific?" concern raised
at an ECOOP defense) were referenced as already-settled but their originating
discussion was **not fully available** when this was written — those spots are
flagged inline with **[RECONSTRUCTED]** or **[GAP]**. Treat flagged passages as
prompts to cross-check against the author's own notes, not as authoritative.

---

## 1. Problem & motivation

Ř (a.k.a. RIR/PIR, the R JIT built on the RIR bytecode interpreter) gathers
*type feedback* to drive speculative optimization. In the baseline scheme, the
compiler emits a `record_type_` bytecode after essentially **every value load
and every interesting intermediate result**. When the interpreter executes such
an opcode it *observes* the runtime value (its SEXPTYPE, scalar-ness, presence
of attributes, object-ness, etc.) and folds that observation into a per-slot
feedback record (`ObservedValues`). PIR later reads these records to speculate.

This is pure interpreter overhead: on a hot loop the interpreter re-observes the
same variable's type on every iteration, even though the feedback stabilizes
almost immediately. The recording work — a `record_type_` dispatch plus the
`doRecord` flag updates — is paid per execution, indefinitely.

**The key conceptual separation (observation vs. persistence).**
**[RECONSTRUCTED — verify wording against the author's notes.]** Type-feedback
recording conflates two things that can be pulled apart:

- **Observation** — the runtime act of looking at a concrete value and deriving
  type facts from it (the `doRecord` on an `ObservedValues`). This is the thing
  that costs interpreter cycles.
- **Persistence** — the state that ends up in the feedback slot that the JIT
  actually reads at compile time.

The JIT only cares about *persistence*: it needs each slot it consults to hold a
sound, sufficiently-precise summary by the time it compiles. It does **not** care
how many times, or at which bytecode site, the underlying observation happened.
"Recordless" is the family of transformations that lower observation cost while
keeping the persisted feedback the JIT consumes unchanged (or a sound
over-approximation):

1. **Record-once** — for a value whose observed type is invariant across a
   function activation, observe on the first execution of the site per
   invocation and skip the rest.
2. **No-record (def-site subsumption)** — for a use whose value is provably the
   same as one already observed at another instrumented site, emit *no* opcode
   at all and, at JIT time, copy the source slot's persisted feedback into the
   dependent slot.
3. **Expression-tree inner-node elision** — for an interior node of an
   expression whose result type is inferable from its operands' (leaves')
   recorded types, suppress recording unless something makes it non-inferable
   (an object appears, triggering S3/S4 dispatch that can return anything).

There is a second, **orthogonal feedback dimension** that rides on the same
machinery: **force-behavior (FB) recording** (§4). It records whether a loaded
binding was a value, an already-forced promise, or an unforced promise — the
`stateBeforeLastForce` lattice. It is a separate axis from the value-type record
and has its own set of interpreter entry points and its own compile-time
strategy selection, but shares the once-gating infrastructure.

Why it matters: the interpreter dispatch loop (`evalRirCode`) is the hot path
for interpreter-only execution and for warmup before PIR kicks in. Shaving
per-execution recording work directly reduces interpreter time and reduces the
volume of redundant feedback churn.

---

## 1.1 The baseline, precisely (what recordless is measured against)

The comparison baseline is the git branch **`recordLess-baseline-outline`**,
whose tip (`7580761b`) is **the merge-base ancestor of the current branch**. In
other words the baseline is the pre-recordless state, and the *entire* diff
`recordLess-baseline-outline → HEAD` (≈ **3,100 insertions across 28 files**) is
the recordless contribution. (Verified 2026-07-24 against
`recordLess-baseline-outline` and current `HEAD` `38d498c6`.)

**Baseline recording model — "record after every load."** Baseline
`compileGetvar` emits a *single, unconditional* record after each non-dd,
non-missing value load, when profiling is on:

```cpp
// baseline recordLess-baseline-outline, Compiler.cpp::compileGetvar
if (ctx.code.top()->isCached(name))  cs << BC::ldvarCached(name, cache_slot);
else                                 cs << BC::ldvar(name);
if (Compiler::profile)               cs << ctx.recordType();     // one opcode, every load
```

- Baseline has exactly **three** record opcodes: `record_call_`, `record_type_`,
  `record_test_`. There is **no** `record_type_once_`, no leaf/inner/notify
  variants, and no No-Record (every load that is profiled gets a `record_type_`).
- Baseline `ObservedValues` is **4 bytes**:
  ```cpp
  uint8_t numTypes:2, stateBeforeLastForce:2, notScalar:1, attribs:1, object:1, notFastVecelt:1;
  std::array<uint8_t,3> seen;              // → static_assert(sizeof==4)
  ```
  It has **none** of the expression-tree fields. The current 16-byte layout adds
  `isLeaf`, `shouldNotRecord`, `hasPropagatedNotification`, and the 8-byte
  `parent` pointer — **+12 bytes/slot**. This is the "node-size confound"
  (§7): any cross-branch timing partly reflects the larger feedback node, not the
  algorithm.

**Force-behavior is PRE-EXISTING baseline machinery — not a recordless
invention.** The `stateBeforeLastForce` lattice and the `recordForceBehavior`
interpreter helper are both present in the baseline (8 references in baseline
`interp.cpp`). Baseline has a single `ldvar_cached_` with **no** FB-strategy
variants. Recordless's contribution to the FB axis is therefore narrow and
should be described as such in any writeup:

1. compile-time **per-slot strategy selection** (`ForceBehaviorKind`) surfaced as
   the `ldvar_cached_{,noRecordFB_,fbRecordOnce_}` opcode split;
2. **once-gating** of FB via the shared `fired` bitmap;
3. (this session) the **monotonic early-out** in `recordFbAtSlot` (§4.1) and the
   generic dispatcher recognizing the `leaf_notify_` opcodes.

The value-type recording scheme (record-once / no-record / inner-node elision)
is the genuinely new part.

**What recordless adds (file inventory).** New files: `bc/DefUseAnalysis.h`
(≈803 LOC — the def-use + structural dominance analysis, §3), `bc/CompilerCFG.{h,cpp}`
(control-flow/scope construction feeding dominance), `bc/CodeContext.h`,
`bc/LoopScopeGuards.h` (loop-scope tracking for once-bit clearing, §2A.2.2),
`bc/recordless.h` (config macros), `interpreter/record_stats.{h,cpp}`
(instrumentation, §7-8). Largest edits: `bc/Compiler.cpp` (+871:
`classifyUse` consumption, `emitRecordTypeForVar`, the `setTypeFeedbackParents`
post-pass), `runtime/TypeFeedback.h` (+248: exptree fields, notify, `typeDeps_`,
`ForceBehaviorKind`), `interpreter/interp.cpp` (+314: once-gating, the new record
handlers, FB variants), plus `bc/BC*`/`insns.h` for the new opcodes.

---

## 2. Core mechanism: two cooperating strategies

Recordless is best understood as **two distinct optimizations over the
expression tree that RIR builds for each recorded expression**, plus the
machinery that lets them share one runtime representation. For an expression
like `a <- f(x) + 1`, RIR conceptually builds a tree: **leaves** at the bottom
(the variable loads `x`, `1` and value-producing results), **inner nodes** above
them (the `f(x)` call result, the `+` result), and a **root** at the top (the
value assigned to `a`).

- **§2A — Optimizing the leaves (variables / value loads).** Cut *how often* a
  leaf is observed, or elide it entirely when its type is recoverable from
  another instrumented site.
- **§2B — Optimizing the inner nodes (sub-expression results).** Don't observe a
  node whose result type is *inferable from its operands* — unless something
  breaks that inference at runtime.
- **§2C — Bringing the two together.** Both are realized on one `ObservedValues`
  node type, one notification function, and one opcode family; and they interact
  (a leaf's "source" may be an inner node; an inner node leans on its leaves).

Each strategy has its **own optimizations and its own failure modes**; they are
presented separately, then unified.

---

## 2A. Optimizing the leaves (variables / value loads)

A *leaf* is a value-producing load: an `ldvar`(`_cached_`), or an opaque value
result (a call return, `[[` extraction, `for` element, replacement-function
result). The compiler classifies each leaf via `DefUseAnalysis::classifyUse`
(consumed in `Compiler.cpp::emitRecordTypeForVar`).

### 2A.1 The three recording classes

- **RecordAlways** → emit `record_type_` (or the notifying variant
  `record_type_leaf_notify_`, §2C). Records on every execution.
- **RecordOnce** → emit `record_type_once_` (or `record_type_leaf_notify_once_`).
  Records on the first execution *per function invocation*, gated by a
  per-invocation bitmap (§2A.2). Later executions are skipped.
- **NoRecord** → emit **no opcode at all**. The type is recovered at JIT time
  from a *source* slot via a compile-time dependency (`typeDeps_`; the soundness
  argument is §3).

### 2A.2 Record-once: the per-invocation `fired` bitmap

`record_type_once_` carries a packed 32-bit immediate:

```
low 16 bits  = slotIdx  (index into the function's TypeFeedback slots)
high 16 bits = iidx     (index into the per-invocation "fired" flag array)
```

Packing/unpacking macros live in `bc/BC_inc.h`:

```c
#define RECORD_TYPE_ONCE_MAX_IIDX 512              // compile-time cap on once-slots/function
#define RECORD_TYPE_ONCE_SLOT_IDX(imm) ((imm)&0xFFFF)
#define RECORD_TYPE_ONCE_IIDX(imm)     ((imm) >> 16)
#define RECORD_TYPE_ONCE_PACK(slotIdx, iidx) (((iidx) << 16) | (slotIdx))
```

**The `fired` flags are a frame-local `bool` array, one entry per once-slot,
zeroed with `memset`, living for exactly one `evalRirCode` activation.** This is
the crux of both correctness (reentrancy, §2A.2.1) and cheapness.

Two storage forms have been used (see §5 for the trade-off):

- **`alloca` form (original / committed):**
  ```cpp
  bool* fired = nullptr;
  if (c->recordTypeOnceCount > 0) {
      fired = (bool*)alloca(c->recordTypeOnceCount * sizeof(bool));
      memset(fired, 0, c->recordTypeOnceCount * sizeof(bool));
  }
  ```
  Sized to exactly what this Code object uses (`recordTypeOnceCount`), no fixed
  cap, no bit-packing.

- **Fixed-size form (working-tree, this session):**
  ```cpp
  bool fired[RECORD_TYPE_ONCE_MAX_IIDX];   // 512 bytes on the stack frame
  if (c->recordTypeOnceCount > 0)
      memset(fired, 0, c->recordTypeOnceCount * sizeof(bool));
  ```
  Only the used prefix is zeroed. The 512 cap is *already* enforced at compile
  time (the compiler never assigns an `iidx >= RECORD_TYPE_ONCE_MAX_IIDX`; see
  the `bitmapSize < RECORD_TYPE_ONCE_MAX_IIDX` guards in `emitRecordTypeForVar`),
  so the fixed array can never be over-run — it is a representation change, not a
  new limit.

**`bool` per flag, not bit-packing.** Direct byte indexing avoids shift/mask on
the hot test. The gate is branch-hinted toward "already fired" (the common
steady-state outcome in a loop):

```c
#define RECORD_TYPE_ONCE_BITMAP_TEST(bitmap, iidx)  (__builtin_expect((bitmap)[iidx], 1))
#define RECORD_TYPE_ONCE_GATE(bitmap, raw, onFired)                 \
    do { if (RECORD_TYPE_ONCE_BITMAP_TEST((bitmap),                 \
                 RECORD_TYPE_ONCE_IIDX(raw))) onFired; } while (0)
#define RECORD_TYPE_ONCE_SET(bitmap, raw)  ((bitmap)[RECORD_TYPE_ONCE_IIDX(raw)] = true)
```

Note this is `__builtin_expect(..., 1)`, i.e. a GCC/Clang likely-hint — the
codebase is C++14, so the C++20 `[[likely]]` attribute is **not** used even
though the intent is the same ("already fired" is the hot case). **[The design
was described elsewhere as `[[likely]]`-hinted; the actual mechanism is
`__builtin_expect`.]**

The `record_type_once_` interpreter handler is, in effect:

```cpp
uint32_t raw = readImmediate(); advanceImmediate();
RECORD_TYPE_ONCE_GATE(fired, raw, { /* stat: skip */ NEXT(); });   // already fired → skip
typeFeedback->record_type(RECORD_TYPE_ONCE_SLOT_IDX(raw), ostack_top());
RECORD_TYPE_ONCE_SET(fired, raw);                                   // mark fired
NEXT();
```

#### 2A.2.1 Reentrancy / recursion safety

Because `fired` is **allocated fresh per `evalRirCode` activation** (on the C
stack), record-once is "once *per invocation*," not "once globally." A recursive
or otherwise reentrant call gets its own `fired` array, so each activation
records its once-slots independently. This is both a correctness property (a
recursive callee still observes its own values) and the reason "once" is cheap
(a loop body's RecordOnce leaf observes only on iteration 1 of *that* call).

#### 2A.2.2 Clearing once-bits for nested loops

A RecordOnce variable that is *re-assigned in an enclosing loop* may take a
different type on each outer iteration, so its "once" must be reset per outer
iteration. Two opcodes clear bits at loop boundaries:

- `clear_record_type_once_bit_` — clear a single bit.
- `clear_record_type_once_bits_range_` — clear a contiguous `[start, start+count)`
  range (packed immediate, same low16/high16 split as the once immediate but
  meaning start/count). Implemented as `memset(fired + start, 0, count)`.

The compiler defers bit assignment for such "dynamic" variables to the innermost
*clearable scope* so that stable once-bits never interleave with a cleared
range (see `emitRecordTypeForVar`: the `isRangeBasedForLoopVar` and
`assignedInEnclosingLoop && hasClearableScope` branches, which call
`registerRangeVarUse` / `registerClearableUse`; supported by `LoopScopeGuards.h`).
Range-based for-loop iteration variables (`:`, `seq_len`, `seq_along`) are
treated as stable-within-this-loop but cleared on entry so a different outer
iteration can re-record.

### 2A.3 No-record: def-site subsumption

The strongest leaf optimization: emit **nothing**. When a use's value is
provably the same as a value already observed at another instrumented site (a
def, or an earlier use), the use's slot is fed at JIT time by *copying* the
source slot's feedback (`typeDeps_[use] = source`). The static condition that
makes this sound is the subject of §3.

### 2A.4 Force-behavior rides on the leaf load

The orthogonal force-behavior axis (§4) is recorded at the *load* — so its
strategy selection (`ldvar_cached_` / `ldvar_cached_noRecordFB_` /
`ldvar_cached_fbRecordOnce_`) is a leaf-level concern, chosen by the same
`classifyUse`/`emitRecordTypeForVar` pass.

### 2A.5 Problems specific to leaves

- **RecordOnce staleness across enclosing-loop reassignment.** A once-recorded
  type is only invariant *within* one activation; a var reassigned in an outer
  loop can change type across outer iterations. Solved by the clear-bit
  machinery (§2A.2.2) — but it makes once-bit *assignment* order-sensitive
  (stable bits must not interleave with a cleared range), which is why bit
  assignment is deferred to clearable scopes.
- **No-record soundness depends on an approximate analysis.** Marking a use
  NoRecord is a *correctness* commitment: if the source's type ever differs from
  what the use would see, the JIT speculates on wrong feedback. The dominance
  test is a conservative structural approximation (§3), and it deliberately bails
  (unseen loop defs, closed scopes).
- **Over-eager source marking (a fixed bug).** A def was at one point marked a
  NoRecord *source* even when its NoRecord dependent had no parent inner node to
  un-suppress — pointless work and a classification muddle. Fixed by requiring
  the dependent to be a registered child (`childSlots.find(d) != end`) before
  treating `typeDep(d)`'s target as a notifying source (§2C.5).
- **Reentrancy** — handled by the per-invocation `fired` array (§2A.2.1); would
  be a correctness bug if the bitmap were global.
- **Stats ambiguity** — a genuinely untracked leaf and a RecordAlways leaf both
  emit plain `record_type_`; telling them apart for measurement needs a runtime
  side table (`isStatsUntracked`, §2C.3).

---

## 2B. Optimizing the inner nodes (sub-expression results)

An *inner node* is the result of a sub-expression: the `f(x)` call result, the
`x + 1` arithmetic result, the `x[i]` extraction. **Insight:** for many
operators the result type is *inferable from the operand (leaf/child) types*, so
the inner node need not be observed at all — its feedback can be reconstructed
from its children's.

### 2B.1 Suppress-by-default (the inferability insight)

The compiler's post-pass marks every non-leaf slot suppressed:
`shouldNotRecord = !isLeaf`. A suppressed inner node's handler
(`recordInner`) does nothing:

```cpp
void recordInner(SEXP e) { if (shouldNotRecord) return; doRecord(e); }
```

So in the common case an inner node costs only a suppressed check, never a full
observation.

### 2B.2 Reversal: un-suppress when inference breaks

Inference from operand types is unsound in exactly one situation: an **object**
flows through, because S3/S4 dispatch on `[`, `+`, etc. can return *anything*,
untethered from operand types. So suppression must be **reversible**: when a
child leaf observes an object, it un-suppresses its parent inner node, which then
starts recording. This is the `notifyRelatedNodes` mechanism (code + design in
§2C.2). Key properties: it is **object-gated** (only an object triggers it),
**monotonic** (`shouldNotRecord` only goes true→false), and **latched** (done
once per activation).

The reversal is the subtle part of inner-node optimization: getting it cheap
(don't pay on every non-object load) *and* sound (never miss an object that
would invalidate the inference) is what the object-gate + one-shot latch buy.

### 2B.3 Which inner nodes are inferable (`[` vs `[[`, colon)

Inferability is decided per primitive at compile time:

- `[` (`Bracket`) is **type-preserving**: `x[...]` has the same SEXPTYPE as `x`
  for non-object `x`, so its result is inferable from the lhs leaf → recorded as
  an **elidable inner node** (`recordTypeTracked(true)`). If `x` is ever an
  object, the lhs leaf's `notifyRelatedNodes` un-suppresses this record.
- `[[` (`DoubleBracket`) extracts an *element* whose type varies (e.g.
  `list(3, "hello")[[i]]`) — **not** inferable, so it stays a tracked
  always-record leaf (`recordTypeTracked(false)`), *not* an inner node.
- Colon `m:n` operand casts are internal and untracked.

### 2B.4 Problems specific to inner nodes

- **Determining inferability** is per-operator and conservative. `[[`, general
  calls, and anything whose result type isn't a function of operand types must
  *not* be made an elidable inner node, or the JIT gets wrong feedback.
- **Sound + cheap reversal** (§2B.2). Cheapness relies on the object-gate/latch;
  soundness relies on *some* child always observing the object before the inner
  node's (elided) feedback is consumed.
- **Graceful degradation / leaning on leaves.** An inner node can only be elided
  if it has registered tracked children to lean on. An inner node with no tracked
  children cannot infer anything and must fall back to always-record. This is
  precisely why opaque value results were reclassified from untracked to
  *tracked* leaves (§2C.4) — to give enclosing inner nodes something to lean on.
- **Root inner nodes that are also sources.** The top of an assigned expression
  (`a <- f(x)+1`) is an inner node that may *also* be a NoRecord source for other
  reads of `a`; when it observes an object it must notify its *dependents'*
  parents, not a parent of its own. This is the reason the inner opcodes split on
  the "notifies-or-not" axis (§2C.3), not the root-vs-non-root axis.

---

## 2C. Bringing the two strategies together

### 2C.1 One node type: `ObservedValues`

Both strategies are carried by extra fields on the single feedback node
(`runtime/TypeFeedback.h`):

```cpp
uint8_t isLeaf : 1;                    // set by the compiler post-pass
uint8_t shouldNotRecord : 1;           // inner nodes start suppressed (= !isLeaf)
uint8_t hasPropagatedNotification : 1; // notify latch
ObservedValues* parent;                // 8-byte, reconstructed at compile time
```

Total size with these fields: **16 bytes** (1+1+3 `seen`+3 pad+8 pointer) vs. the
baseline **4 bytes** — the +12-byte "node-size confound" (§1.1, §7). `parent`
links a node to its enclosing inner node; it is reconstructed at compile time,
not serialized.

### 2C.2 One notification function for both edges

The single mechanism that serves *both* "leaf/inner notifies its parent" and
"source notifies its dependents' parents":

```cpp
// TypeFeedback::notifyRelatedNodes — un-suppress this slot's own parent (if any)
// AND the parents of all its NoRecord dependents, together, once. idx is passed
// in directly (previously derived via pointer arithmetic on the slot).
void notifyRelatedNodes(ObservedValues& slot, bool isObject, uint32_t idx) {
    if (!isObject || slot.hasPropagatedNotification) return; // object-gate + one-shot latch
    slot.hasPropagatedNotification = true;
    if (slot.parent)                              // leaf-with-parent / non-root inner
        slot.parent->shouldNotRecord = false;
    for (uint32_t d : noRecordSourceToDeps_[idx]) // source → dependents' parents
        if (ObservedValues* p = types_[d].parent)
            p->shouldNotRecord = false;
}
```

Design points:

- **Optimistic bail.** The common case is a non-object value: `!isObject` returns
  immediately, touching nothing.
- **One-shot latch (`hasPropagatedNotification`).** Un-suppression is monotonic,
  so the latch makes the cross-slot writes happen once per activation rather than
  on every object observation — the real win, since the parent lives on a
  *different* cache line. Cleared by `ObservedValues::reset()` (`*this = {}`), so
  re-profiling after deopt notifies correctly.
- **Generic over both edges.** One function handles the parent edge (leaf 2B
  reversal / non-root inner) and the dependents edge (leaf 2A NoRecord source),
  with the inapplicable branch a cheap no-op. This is *the* place the two
  strategies literally share code, and it is why the opcode taxonomy collapses to
  "notifies or not" rather than a 2×2 of parent×source.

### 2C.3 One unified opcode family

After the post-pass (`setTypeFeedbackParents`), the value-type record opcodes are
(contiguous in the `Opcode` enum — the stats range-check relies on it):

| opcode | strategy | meaning |
|---|---|---|
| `record_type_` | leaf | plain leaf, RecordAlways; also every *untracked* record and the RecordOnce fallback |
| `record_type_once_` | leaf | plain leaf, RecordOnce (fired-gated) |
| `record_type_leaf_notify_` | leaf | leaf that **notifies**: a NoRecord *source*, and/or a leaf with a parent inner node |
| `record_type_leaf_notify_once_` | leaf | RecordOnce variant of the above |
| `record_type_inner_` | inner | **standalone** inner node: root, no dependents — nothing to notify |
| `record_type_inner_notify_` | inner | inner node that **notifies**: non-root (parent) **or** root source (dependents) |

Note the two families are parallel: each has a plain form and a `*_notify_` form,
and the "notify" form of both routes through the same `notifyRelatedNodes`.
`record_test_`/`record_call_` (branch/callee feedback) sit outside this range.

The `ObservedValues` recording primitives:

- `doRecord(e)` — shared core: updates `numTypes/seen[]`, `notScalar`, `object`,
  `attribs`, `notFastVecelt`.
- `record(e)` → just `doRecord(e)`. Used by plain `record_type_`/`_once_`.
  (Leaves are never suppressed, so `record()` has **no** `shouldNotRecord` check;
  it was dead for its callers and was removed this session.)
- `recordSimple(e)` — historically a thin alias for `doRecord`; **inlined away**
  this session (`record_type_leaf_notify` calls `doRecord` directly, being a
  friend of `ObservedValues`).
- `recordInner(e)` — `if (shouldNotRecord) return; doRecord(e);`. Used by *both*
  inner-node opcodes.

**Untracked records** (a leaf concern that touches the shared opcode). Not every
`record_type_` participates in the analysis. `recordTypeUntracked()` emits a
plain `record_type_` and registers nowhere, so the post-pass leaves it untouched
(`isLeaf && isRoot && !isSource`): always records, never notifies, never
suppressed, never NoRecord/RecordOnce. The truly-untracked sites: the colon
`m:n` operand casts (×2), the super-assign target read-for-update, and the
`ldvar` fallback while compiling default formal args (no main-body context).
Because a genuine untracked record and a RecordAlways leaf share the *same*
opcode, the stats build distinguishes them at runtime via
`setStatsUntrackedSlots` / `isStatsUntracked(idx)` (a compile-time-populated slot
set, consulted only under `RIR_RECORD_STATS`).

### 2C.4 Where the two strategies interact

- **A leaf's source can be an inner node.** The `a <- f(x)+1; …; a` case: the
  root inner node (`f(x)+1`) is the source, later reads of `a` are NoRecord
  leaves depending on it. When the source (an inner node) records an object it
  must un-suppress the *dependent leaves' parents* — a "two-step" propagation
  that `notifyRelatedNodes`'s dependents loop performs.
- **Inner nodes lean on leaves for graceful degradation.** An inner node is only
  elidable if it has tracked children; the reclassification of opaque value
  results (call return, `[[`, `for` element, replacement-fn) from *untracked* to
  `recordTypeTracked(isParent=false)` was done specifically so they act as
  defs/sources and as children an enclosing inner node can lean on, letting the
  inner node degrade gracefully instead of being pinned to always-record.
- **The `tracked` vs `untracked` distinction is the bridge.**
  `recordTypeTracked(isParent)` registers a slot in the expression-tree structure
  (parent/leaf role) so the post-pass can specialize it and inner nodes can lean
  on it; `recordTypeUntracked()` opts a slot out entirely.

### 2C.5 Compile-time construction and the specialization post-pass

- During compilation, a `slotsStack` and a `parents` map record the tree shape as
  expressions are compiled (`registerSlot`, `registerLeafSlot`;
  `bc/CodeContext.h`, `bc/CompilerCFG.{h,cpp}` support the CFG/scope side).
- A post-pass, `TypeFeedback::setTypeFeedbackParents` (called from
  `Compiler::finalize`), then: sets each slot's `parent`, sets `isLeaf`
  (a slot with no children) and `shouldNotRecord = !isLeaf`, computes the
  `sourceSlots` set (a def is a notifying source only if its NoRecord dependent
  is a registered child — the over-marking fix of §2A.5), and **specializes each
  placeholder `record_type_`/`record_type_once_` opcode** into the right family
  member (`leaf_notify_[once_]` / `inner_` / `inner_notify_`) by `(isLeaf, isRoot,
  isSource)`.
- `buildNoRecordReverseMap` builds `noRecordSourceToDeps_` (source slot → its
  NoRecord dependent slots) from `typeDeps_`, so `notifyRelatedNodes` can walk a
  source's dependents at runtime.

---

## 3. Formal justification (No-Record / def-site subsumption)

The soundness argument for eliding a use-site record entirely and recovering its
feedback from another slot.

**Claim.** A use `U` of variable `v` need not be instrumented if there is an
already-instrumented site `S` (a definition of `v`, or an earlier recorded use)
such that the value observed at `U` is *necessarily* the value observed at `S`.
Then at JIT time the compiler copies `S`'s persisted feedback into `U`'s slot
(the `typeDeps_` map: `typeDeps_[U] = S`), and `U` emits no opcode.

**The control-flow condition.** "Necessarily the same value" is established from
def-use analysis with dominance/postdominance. In `DefUseAnalysis::classifyUse`
the two No-Record returns are:

The two predicates, as actually defined in `DefUseAnalysis.h` (verified
2026-07-24):

- `dominates(D)` → **D dominates the current use U** (D is on every path from
  entry to U). Approximated structurally: true iff D's scope is still open on the
  scope stack (scopeId 0 = function top level always dominates).
- `postDominates(D)` → **U post-dominates D** (every path from D to function exit
  passes through U). Approximated structurally: D and U in the same innermost
  scope, and no `return`/`break`/`next` fired in a closed scope between them.

The two NoRecord returns in `classifyUse`:

1. **Use-to-use dedup** (`if (dominates(ud) && postDominates(ud))`, for an
   *optimizable* variable — a formal, outer-controlled capture, or local/param
   with a reaching def): a previously recorded use `ud` such that **`ud`
   dominates `U` and `U` post-dominates `ud`**. Mutual dominance/post-dominance
   with no intervening redefinition (guaranteed by the reaching-def precondition)
   means `ud` and `U` are in the same single-entry/single-exit region on the same
   binding → identical value. Return `{NoRecord, ud.feedbackSlot, …}`.

2. **Def-to-use** (`if (d && isLocalOrParam(name) && postDominates(*d) && d.feedbackSlot != kNoSlot)`):
   a unique reaching def `d` — `findReachingDef` only returns a def that already
   **dominates** `U` — with the added condition that **`U` post-dominates `d`**.
   The `a <- expr; …; a` pattern: `a <- expr` records the type of `expr`; a later
   read of `a` that `d` reaches, and which `d` cannot avoid flowing to, sees the
   same value. Return `{NoRecord, d.feedbackSlot, FBValue}`.

Both cases reduce to the same condition: **D dominates U ∧ U post-dominates D**
(mutual dominance) with the binding unchanged between them.

**Reasoning, spelled out.** Recording at a site *observes* the type; what the JIT
consumes is the *persisted* slot (observation vs. persistence again). If `S`
uniquely reaches `U` and the dominance relation guarantees `S` runs on every path
that reaches `U` with the binding unchanged, then `S`'s persisted feedback is a
sound and equally-precise summary of what `U` would have observed. Instrumenting
`S` therefore *subsumes* instrumenting `U`: we get the same persisted state for
`U`'s slot without paying any runtime observation at `U` — the compile-time copy
does it for free.

Postdominance additionally ensures the source's observation is actually
"consumed" / that the def reliably leads to the use, so the copy is never reading
a slot that the relevant path failed to populate.

**Structural approximation (important for the generality question, §6).** Ř does
**not** compute a general CFG dominator tree here. `dominates`/`postDominates`
are cheap **structural** approximations that exploit the tree/scope nesting of
the source being compiled: dominance = "the def's scope is still open," and
post-dominance = "same innermost scope + no early exit (`return`/`break`/`next`)
fired in a closed scope in between." `findReachingDef` keeps only the most-recent
def per name and bails (returns `nullptr`) if that def sits in an already-closed
scope (ambiguous on some paths) or if a loop body has an unseen later def
(`hasUnseenLoopDef`). This single-pass, scope-based approximation is sound
(conservative — it only says "same value" when the nesting guarantees it) and
cheap, but it is one of the places the design leans on Ř's compilation structure
rather than a language-agnostic dataflow pass.

**Force-behavior interaction.** No-Record uses are, by construction, dominated /
post-dominated re-reads of a materialized local (or a local-stvar reach). Such a
binding already holds a concrete value — never an unforced promise — so its
force-behavior is trivially `value`. That is why No-Record uses are assigned
`ForceBehaviorKind::FBValue`/`Infer` and their FB recording is also elided: there
is genuinely nothing to observe.

---

## 4. The force-behavior (FB) dimension

A second feedback axis: `ObservedValues::stateBeforeLastForce`, a **monotonic
lattice**

```
unknown < value < evaluatedPromise < promise
```

recording, for a loaded binding, whether it was a plain value, an already-forced
(evaluated) promise, or a still-unforced promise. PIR uses this to decide whether
forcing is likely cheap.

### 4.1 Recording core (`recordFbAtSlot`)

Classifies the loaded SEXP `s` and moves the slot up the lattice. This session it
was rewritten to **switch on the current state and inspect `s` only as far as the
current state still leaves room to rise** (monotonicity → skip checks that cannot
raise the state):

```cpp
using SBLF = ObservedValues::StateBeforeLastForce;
ObservedValues& fb__ = typeFeedback->types(slotIdx);
switch (fb__.stateBeforeLastForce) {
case SBLF::promise:           break;                    // top — never touch `s`
case SBLF::evaluatedPromise:                            // only `promise` is higher
    if (TYPEOF(s) == PROMSXP && PRVALUE(s) == R_UnboundValue &&
        CAR(PREXPR(s)) != symbol::lazyLoadDBfetch)
        fb__.stateBeforeLastForce = SBLF::promise;
    break;
case SBLF::value:                                       // rises only via a promise
    if (TYPEOF(s) == PROMSXP) {
        if (PRVALUE(s) != R_UnboundValue) fb__.stateBeforeLastForce = SBLF::evaluatedPromise;
        else if (CAR(PREXPR(s)) != symbol::lazyLoadDBfetch) fb__.stateBeforeLastForce = SBLF::promise;
    }
    break;
case SBLF::unknown:                                     // full classification
    if (TYPEOF(s) != PROMSXP) fb__.stateBeforeLastForce = SBLF::value;
    else if (PRVALUE(s) != R_UnboundValue) fb__.stateBeforeLastForce = SBLF::evaluatedPromise;
    else if (CAR(PREXPR(s)) == symbol::lazyLoadDBfetch) fb__.stateBeforeLastForce = SBLF::value;
    else fb__.stateBeforeLastForce = SBLF::promise;
    break;
}
```

The classification of `s`: not a `PROMSXP` → `value`; a promise with `PRVALUE`
set → `evaluatedPromise`; a `lazyLoadDBfetch` stub → `value`; otherwise (unforced
promise) → `promise`. The previous straight-line implementation is kept
commented out directly above the switch. `lazyLoadDBfetch` is treated as `value`
because it is a lazy-load stub that materializes to a value.

### 4.2 Compile-time FB strategy (`ForceBehaviorKind`)

`runtime/TypeFeedback.h` defines the per-slot compile-time decision:

- `FBValue` — statically known to be a value (post-stvar reach, RecordOnce local,
  for-loop iter var). Runtime FB recording skipped; JIT treats it as `value`.
- `Infer` — runtime FB recording also skipped, but the JIT infers it from the
  def/useDef chain. Kept distinct from `FBValue` so the JIT can tell them apart.
- `Always` — record FB unconditionally.
- `EnvBit` — **disabled** (was: gate on a per-environment bitmap, paired with the
  also-disabled `record_type_once_promise_`).
- `RecordOnce` — record FB once per invocation, gated by the same per-code
  `fired` bitmap.

### 4.3 How FB gets recorded: cached vs. non-cached loads

**Cached loads bake the strategy into the opcode** (chosen by the compiler,
patched in `emitRecordTypeForVar`):

- `ldvar_cached_` → `recordForceBehaviorAlways` (unconditional).
- `ldvar_cached_noRecordFB_` → skips FB entirely (`FBValue`/`Infer`).
- `ldvar_cached_fbRecordOnce_` → `recordForceBehaviorRecordOnce` (fired-gated).

`recordForceBehaviorAlways` reads the slot index directly from `pc+1` with **no
opcode peek and no unpacking**. Its soundness rests on an invariant proven this
session: base `ldvar_cached_` is emitted *only* for `fbKind == Always`, which
`classifyUse` returns *only* for `RecordAlways` uses, which always emit a plain
(non-once) leaf record right after the load. The two cases that would break a
blind `pc+1` read — a No-Record elision (no opcode follows) and a packed `_once_`
immediate — are exactly the ones the compiler patches to `noRecordFB_` /
`fbRecordOnce_`. A `SLOWASSERT(*pc == record_type_ || *pc == record_type_leaf_notify_)`
pins this in debug builds.

**Non-cached loads use a generic runtime dispatcher.** `ldvar_`,
`ldvar_for_update_*`, `ldvar_super_`, `ldddvar_` call the generic
`recordForceBehavior`, which peeks the following opcode and dispatches:

```cpp
switch (*pc) {
case record_type_:  case record_type_leaf_notify_:            idx = raw;          break; // always
case record_type_once_: case record_type_leaf_notify_once_:                            // once
    RECORD_TYPE_ONCE_GATE(fired, raw, { /*skip*/ return; });  idx = RECORD_TYPE_ONCE_SLOT_IDX(raw); break;
default: /* no record follows this load — bail */             return;
}
recordFbAtSlot(idx, s);
```

Note the dispatcher recognizes **all four** value-type leaf opcodes, including
the `leaf_notify_` variants. Recognizing only the plain `record_type_`/`_once_`
(the older behavior) silently dropped FB for any non-cached, tree-participating
leaf — a real bug fixed this session.

The `default` bail ("`fbgeneric_bail`") fires when no value-type record follows
the load. Empirically the callers that bail are: the discarded subassign
protective read (`ldvarForUpdate…; setShared; pop` — value thrown away) and
`ldddvar_` (dd-vars `..1`/`...` are never type-profiled, so no record is ever
emitted after them). Both are correct-to-skip; the bail is not a lost recording.

### 4.4 `ldvar_for_update_cache_noRecordFB_` (this session)

A dedicated cached for-update opcode for the *discarded protective read* of a
subassign target. That read exists only for its `setShared`/NAMED side effect and
its value is immediately popped, so it is never followed by a record and its FB
dispatch always bailed. The new opcode's handler is identical to
`ldvar_for_update_cache_` minus the FB call, removing the wasted (bailing)
dispatch on subassign-heavy loops (e.g. nbody's inner loop: ~15k wasted
dispatches per run eliminated). Wired through all opcode sites
(`insns.h`, `BC.h`/`BC_inc.h` factory, `BC.cpp` ×4 switches, `CodeVerifier.cpp`,
`rir2pir.cpp` — translated identically to a for-update `LdVar`, `interp.cpp`).
Non-cached protective reads still use the plain opcode and bail harmlessly (no
dedicated variant added).

---

## 5. Alternatives considered and rejected

- **`std::bitset` as the `fired` storage.** Rejected: cannot zero only a partial
  (used) prefix cheaply, and the bounds-checked `.test()`/`.set()` add overhead.
  A `reinterpret_cast<std::bitset<512>&>` *view* over the `bool[512]` was briefly
  used for documentation clarity on TEST/SET/CLEAR, but with unchecked
  `operator[]` (not `.test()`/`.set()`), and ultimately the plain array + macros
  were kept.
- **Bit-packed `fired` (1 bit/flag).** Rejected in favor of `bool` per flag:
  direct byte indexing avoids shift/mask on the hot gate; 512 bytes of frame is
  cheap.
- **`alloca` vs. fixed-size `bool[512]`.** Both implemented. `alloca` sizes to
  exactly `recordTypeOnceCount` (no wasted frame, no fixed cap) but is a dynamic
  stack allocation. Fixed-size is a static frame slot (predictable, no `alloca`
  call) at the cost of up to 512 bytes of frame even when fewer are used.
  Measurements showed the fixed-size form *modestly* faster on spectralnorm /
  mandelbrot — **but within codegen-artifact noise** (§7), so this is not a
  confident result. Currently the working tree uses fixed-size; committed history
  uses `alloca`. **Unresolved.**
- **`record_type_dep_` / `record_type_once_dep_` as separate opcodes.** Merged
  this session into `record_type_leaf_notify_[once_]`. They had become
  byte-identical handlers once `notifyRelatedNodes` was made generic over
  parent-and-dependents; the split was residual. The former name `leafWithParent`
  was renamed to `leaf_notify` because the merged opcode also covers a parentless
  source.
- **Splitting inner nodes on the root-vs-non-root axis** (`root_inner_` /
  `inner_node_`). Replaced this session by the **notifies-vs-standalone** axis
  (`record_type_inner_` / `record_type_inner_notify_`). The old axis lumped a
  root that *is* a source (must notify its dependents) together with a root that
  is not (nothing to notify), forcing the common no-notify case to still run the
  notify gate every execution. The new axis moves that cost off the common path;
  a root+source now correctly lands in the notifying bucket. This also let
  `notifyParent()` and the separate `recordInnerNode()` primitive be deleted
  (both inner opcodes route through `recordInner` + `notifyRelatedNodes`).
- **`shouldNotRecord` check inside `record()`** (the plain-leaf recorder).
  Removed: leaves are never suppressed (`shouldNotRecord = !isLeaf`), so the check
  was provably dead for its callers and cost a load+branch on the hottest record
  opcode.
- **Env-bitmap force-behavior (`ForceBehaviorKind::EnvBit` +
  `record_type_once_promise_` + `ldvar_cached_envRecordFB_`).** Disabled/removed
  from the emitted set. Promise-context free variables record FB via the normal
  path instead.
- **`RECORDLESS_EXPTREE_ENABLED` compile-time guard.** Removed this session; the
  expression-tree optimization is now unconditionally compiled in. (A separate
  `RECORDLESS_EXPTREE_DEBUG` print toggle remains.) The dead non-recordless
  code paths and the `sizeof(ObservedValues)==4` static_assert were deleted.

---

## 6. Open questions / unresolved issues

- **Generality vs. Ř-specificity (the ECOOP-defense concern).** **[GAP — the
  originating discussion was not available when writing this; the framing below
  is my reconstruction of the axis, to be sharpened against the author's notes.]**
  The open question is whether def-site subsumption + record-once + inner-node
  elision is a *general* technique for feedback-directed JITs, or leans on Ř/RIR
  specifics. Now that §3 is grounded, a concrete split:
  - **Portable / classical:** the observation-vs-persistence separation; the core
    subsumption result (a use whose value is provably identical to an
    already-instrumented site needs no runtime observation — the feedback can be
    copied at compile time). These are standard dataflow ideas.
  - **Ř-flavored (implementation, possibly not fundamental):** the dominance /
    post-dominance test is **not** a general CFG dominator computation but a cheap
    *scope-structural approximation* that exploits the single-pass compile over
    tree-nested bytecode (§3, "Structural approximation"). A CFG-based JIT would
    need a real dominator/post-dominator analysis to get the same subsumptions;
    the *result* ports, the *cheap way of computing it here* may not.
  - **Ř-specific in detail:** the exact opcode taxonomy, the suppress/notify
    inner-node scheme (tied to RIR's expression-tree bytecode), and the
    force-behavior axis (R promise/force semantics). The inner-node elision in
    particular assumes result types are inferable from operand types, which is an
    expression-tree property; a stack/CFG bytecode without that structure would
    need a different formulation.
  A reviewer will push here; the honest answer is "the principle is general, this
  instantiation exploits Ř's tree-structured single-pass compiler."
- **`alloca` vs. fixed-size `fired`** — no confident perf verdict (see §5, §7).
- **`pc`-by-reference capture in the FB lambdas.** The FB helper lambdas capture
  the interpreter frame by reference (`[&]`), including `pc`. Whether taking
  `&pc` forces the hottest interpreter variable out of a register (a classic
  threaded-interpreter footgun) was raised but not measured. If it does, passing
  `pc` by value to the FB helpers could help; this is the one identified lever
  that could actually move steady-state speed. Pre-existing (not introduced by
  recordless), unverified.
- **Uniform FB-helper ABI.** The FB helpers have *different* ABIs — GCC
  `.isra`-specializes the simpler ones (scalar args) vs. the general one (closure
  pointer). Consequently, changing which helper an opcode handler calls perturbs
  register allocation across the whole `evalRirCode` (see §7). Giving the family
  one uniform explicit signature would remove that sensitivity (a *stability*,
  not *speed*, improvement). Not done.
- **Exact dom/postdom predicate directions** in `classifyUse` (see §3 gap).
- **Whether the generic `recordForceBehavior` should honor `fbKind`.** Non-cached
  loads currently ignore the compile-time FB strategy (they always attempt FB via
  the peek). This is inherited from the pre-recordless single-dispatcher design;
  harmless but asymmetric with the cached path.
- **`recordForceBehaviorAlways` assumes profiling is on.** It blind-reads `pc+1`
  as a raw slot index with no opcode peek; that is sound only because base
  `ldvar_cached_` is always followed by a record instruction — which holds only
  when `Compiler::profile` is true (default; disabled by `RIR_PROFILING=off`).
  With profiling off, no record follows and the blind read would consume a
  non-record opcode's bytes as a slot index → out-of-bounds `types()`. A
  `SLOWASSERT` catches it in debug builds, but recordless effectively presumes
  "profiling on." Whether `RIR_PROFILING=off` is a reachable/supported config on
  this branch is unresolved. (Pre-existing assumption of the cached fast path,
  not introduced by recordless, but now also relied on by `ldvar_super_`.)

---

## 7. Current implementation status (snapshot, 2026-07-24)

Working branch: `recordLessNew2-alloca` (the canonical/"official" branch).
A sibling clone at `~/rsh-recordLess-baseline` (branch
`recordLess-baseline-outline`) is used for A/B binary builds.

**Stable / committed:**
- The three recording classes (RecordAlways/RecordOnce/NoRecord) and the
  `typeDeps_` copy-at-JIT mechanism.
- Record-once with the per-invocation `fired` array (committed form: `alloca`).
- The expression-tree suppress/notify scheme; `notifyRelatedNodes`.
- The FB dimension and the `ldvar_cached_` FB opcode variants.
- `RIR_RECORD_STATS` instrumentation (compile-time toggle, **off** for perf/prod;
  `RecordSkipStats` prints leaf/inner/untracked/force-behavior tables at exit).

**Working tree, this session, likely to change / not all committed:**
- Opcode merge/rename: `record_type_leaf_notify_[once_]` (from dep+leafWithParent)
  and the inner re-split `record_type_inner_` / `record_type_inner_notify_`.
- `RECORDLESS_EXPTREE_ENABLED` removed (always-on).
- `record()` dead `shouldNotRecord` check removed; `recordSimple` inlined;
  `notifyRelatedNodes` takes `idx` directly.
- Fixed-size `bool[512]` `fired` replacing `alloca` (**candidate, unverified**).
- `recordFbAtSlot` monotonic switch-on-current-state (§4.1).
- Generic `recordForceBehavior` recognizing the `leaf_notify_` family (bug fix).
- `ldvar_super_` → `recordForceBehaviorAlways`.
- New opcode `ldvar_for_update_cache_noRecordFB_` for the discarded protective
  read.
- Force-behavior stats table (`fbgeneric` / `always` / `record_once` /
  `no_record` rows + `fbgeneric_bail` reported separately).

To collect stats: flip `//#define RIR_RECORD_STATS` on in
`rir/src/interpreter/record_stats.h`, rebuild, run; **flip it back off for any
timing.**

---

## 8. Benchmark / evaluation notes

**The single most important evaluation finding: performance comparisons here are
dominated by codegen artifacts, not by the algorithm.** This was established
rigorously this session and must be front-and-center in any writeup.

- `evalRirCode` is one ~50 KB function (a computed-goto/threaded dispatch loop).
  A *one-line* source change (e.g. switching `ldvar_super_` from the generic FB
  dispatcher to `recordForceBehaviorAlways` — literally one `callq` target)
  caused GCC to re-run whole-function register allocation, reshuffling registers
  (`%rsi`↔`%rcx`↔`%rdi`) throughout the dispatch loop and shifting every
  downstream instruction by 16 bytes. Root cause traced concretely: the two FB
  callees have *different ABIs* (one is a GCC `.isra` clone taking scalar args,
  the other takes a closure pointer), so the call-site marshalling changed,
  which cascaded through the global register allocator.
- Consequence: comparing two functionally-equivalent commits (CI job
  `9ed44d9c` vs `73730ba9`, interpreter-only, warmup 10) produced **±10%
  bidirectional per-benchmark scatter with a 1.00 summary mean** — the signature
  of layout/alignment noise, not a real delta. Bidirectional "big-diffs"
  (cholesky 0.90, em 0.91 slower; text_look 1.12, fasta_naive 1.10 faster) around
  a 1.0 mean confirm this.
- Recommended methodology (not yet fully adopted): compare **retired instruction
  counts** (`perf stat -e instructions`) — layout/frequency-invariant, so
  ~identical counts across functionally-equal builds prove wall-clock scatter is
  artifact; and establish the **noise floor by benchmarking a commit against
  itself**.
- Prior confounds recorded in the author's memory notes: a "node-size confound"
  (the +12-byte `ObservedValues` growth for the expression-tree fields inflates
  per-node cost vs. a clean master, muddying speedup claims); several observed
  "speedups"/"regressions" (a "specialized nodes" 1.38× on spectralnorm; a ~0.7%
  mandelbrot regression) were each traced to GCC codegen shifts, **not** to the
  algorithm — record counts were byte-identical across those commits.

**The stats output (`RecordSkipStats::~RecordSkipStats`, printed to stderr at
process exit).** Four tables, all with `should | recorded | skipped | skip%`
columns (skip% = skipped/should):

1. **Overall (vs. record-everything baseline):** rows `leaves`, `inner nodes`,
   `untracked`, `TOTAL`. "should" = how many records the baseline would have
   done; "skipped" = what recordless avoided.
2. **leaves by class:** `RecordAlways`, `RecordOnce`, `NoRecord (elided)` — each
   row's "should" carries its share of all leaves.
3. **inner nodes by opcode:** `inner (standalone)`, `inner_notify` — here
   "skipped" means *suppressed* via `shouldNotRecord` (the elision), not gated.
4. **force behavior (recordForceBehavior variants):** rows `fbgeneric`,
   `always`, `record_once`, `no_record`, `TOTAL`; plus `fbgeneric_bail`
   reported *separately* below the table (loads where the generic dispatcher
   found no following record — a genuine "nothing to attribute FB to," not a
   skip, hence not a table row).

**Recording-elision rates (what the stats *do* reliably show).** With
`RIR_RECORD_STATS` on, the fraction of baseline "record-everything" sites the
recordless build skips is substantial and stable across runs (these are counts,
not timings, so they are trustworthy). Representative figures:

- **mandelbrot (areWeFast):** overall ~76% of baseline type-record sites skipped
  (leaves ~58% skipped; inner nodes ~100% skipped in that run; ~45% of leaves are
  NoRecord-elided, ~13% RecordOnce with ~97% of those gated away). Value-type
  record counts were **byte-identical** between two commits that differed only in
  the inner-node opcode split — confirming the refactor preserved recording
  behavior.
- **nbody_naive_inner:** overall ~52% skipped; force-behavior table showed
  `no_record` as the dominant FB class (compiler proved most cached loads
  statically value/inferable), `record_once` ~76% gated.
- **storage / binarytrees_naive:** much lower leaf skip% (~0–4%) — object- and
  allocation-heavy code where most leaves are genuine RecordAlways.

**Bottom line for the paper:** lead with the *recording-elision counts* (sound,
reproducible) and the *observation-cost* argument; treat wall-clock deltas on
this codebase with extreme caution and back any speed claim with instruction
counts + a self-vs-self noise floor, or run on a codegen-artifact-controlled
setup.

**Methodology notes (how runs were done this session).**
- Local interpreter-only runs: `PIR_ENABLE=off PIR_OSR=0 <build>/bin/R -f
  harness.r --args <benchmark> <OUTERITER> <INNERITER>` against the
  RBenchmarking harness (`~/benchmarks/RBenchmarking/Benchmarks/{areWeFast,
  shootout,RealThing,...}`). Interpreter-only is the relevant regime because the
  optimization targets interpreter recording cost.
- The harness calls `gc()` before each iteration (added after finding that
  deterministic GC/deopt spikes at identical iteration indices, seen in two CI
  runs four days apart, were a large noise source — those were GC, not the
  algorithm).
- Additional (secondary) noise sources identified: ASLR on the local machine
  (library load addresses shift set-index/alignment) — pin with `setarch -R` for
  controlled runs; scaling governor / turbo / thermal for wall-clock. The CI
  machine uses rebench with a `denoise` config (governor, no_turbo, shielding,
  nice). A duplicate-uncommented-benchmark bug in `runBenchmark.sh` was also found
  and fixed (one benchmark silently overriding another).
- CI comparison surface: the GitLab `benchmark_llvm` job (`e:PIR-LLVM`,
  `GIT_STRATEGY: none`, dedicated `benchmarks` runner), results viewed via the
  MeasuR diff UI. Remember MeasuR's `warmup` setting and that its ratio column is
  direction-labeled; a 1.00 summary with bidirectional per-benchmark scatter is
  the noise signature.

---

## Appendix: key files

- `rir/src/interpreter/interp.cpp` — `evalRirCode`; the `fired` array; the
  `recordFbAtSlot` / `recordForceBehavior*` lambdas; all `record_type_*` and
  `ldvar*` handlers.
- `rir/src/runtime/TypeFeedback.h` — `ObservedValues` (flags, `doRecord`,
  `record`/`recordInner`, notify), `TypeFeedback` (`record_type_*` methods,
  `notifyRelatedNodes`, `typeDeps_`, `noRecordSourceToDeps_`,
  `buildNoRecordReverseMap`, `ForceBehaviorKind`).
- `rir/src/bc/Compiler.cpp` — `compileGetvar`, `emitRecordTypeForVar`,
  `setTypeFeedbackParents` (the post-pass that specializes opcodes and sets
  `isLeaf`/`shouldNotRecord`/parents), once-bit assignment & clearing.
- `rir/src/bc/DefUseAnalysis.h` — `classifyUse` (RecordAlways/RecordOnce/NoRecord
  + `ForceBehaviorKind`), dominance/postdominance conditions.
- `rir/src/bc/insns.h` — opcode definitions (the value-type record family must
  stay contiguous for the stats range-check).
- `rir/src/bc/BC_inc.h` — once-immediate pack/unpack + gate macros.
- `rir/src/interpreter/record_stats.{h,cpp}` — `RIR_RECORD_STATS` instrumentation.
