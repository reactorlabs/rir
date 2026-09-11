# Recordless: reducing type-feedback recording overhead in the Ř interpreter

**Status:** working design snapshot, last revised **2026-09-02**. The author is
actively iterating; see *Current implementation status* (§7) for what is stable
vs. in flux.

**Scope.** This document covers the **producer side only**: what the compiler
emits and what the interpreter does at runtime. PIR/JIT-side consumption of the
new feedback (using `typeDeps_`, `ForceBehaviorKind`, etc.) has deliberately
**not been started** and is out of scope here — where a mechanism ends at the
boundary, the doc says so rather than describing intended consumer behaviour.

**Provenance.** Reconstructed from a long working session, then **re-verified
directly against the source tree on 2026-07-27** (`classifyUse`,
`setTypeFeedbackParents`, `LoopScopeGuards.h`, `markRelatedDirty`, the opcode
tables, and the baseline diff were all read rather than recalled; statements
sourced that way are marked "verified"). Two pieces of framing — the
"observation vs. persistence" distinction and the "is this general or
Ř-specific?" concern raised at an ECOOP defense — were referred to as
already-settled but their originating discussion was **not available**; they are
flagged **[RECONSTRUCTED]** / **[GAP]** inline and should be cross-checked
against the author's own notes rather than treated as authoritative.

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

The JIT only cares about *persistence*: it needs each slot it consults to hold
the right summary by the time it compiles. It does **not** care how many times,
or at which bytecode site, the underlying observation happened.

"Recordless" is the family of transformations that lower observation cost while
keeping the persisted feedback the JIT consumes **exactly unchanged**. This is a
*losslessness* claim, not an approximation: the goal is bit-identical feedback at
lower cost, and each mechanism is only applied where a static condition
guarantees that. Concretely:

1. **Record-once** — for a value whose observed type is invariant across a
   function activation, observe on the first execution of the site per
   invocation and skip the rest.
   *Why lossless:* `doRecord` is idempotent for a repeated value-type (it folds
   into a set of `seen[]` types and monotone flags), so observing an invariant
   value once produces **bit-identical** slot state to observing it N times. The
   classification only assigns RecordOnce where the analysis establishes that
   invariance (§2A.1.1).
2. **No-record (def-site subsumption)** — for a use whose value is provably the
   same as one already observed at another instrumented site, emit *no* opcode
   at all, annotating the dependent slot with a compile-time dependency so the
   source slot's persisted feedback can be copied into it.
   *Why lossless:* the enabling condition is mutual dominance (source dominates
   the use, use post-dominates the source, binding unchanged in between, §3), so
   the two sites execute the same number of times on the same value. The source
   slot therefore already holds exactly what the use would have accumulated; the
   copy reproduces it. This additionally requires the variable to be **in our
   control** — created by the function itself (or a stable capture) — so that no
   invisible mutation (reflection, or `<<-` from a nested closure) can invalidate
   the reasoning; see §2A.1.2, which is a soundness precondition, not a heuristic.
3. **Expression-tree inner-node elision** — for an interior node of an
   expression whose result type is inferable from its operands' (leaves')
   recorded types, suppress recording unless something makes it non-inferable
   (an object appears, triggering S3/S4 dispatch that can return anything).
   *Why lossless:* "inferable" means exactly reconstructible from the operands,
   and the moment inferability could break — an object is observed — suppression
   is reverted and the node records normally (§2B.2). Losslessness here is
   therefore a *proof obligation on the inference rule chosen per operator*
   rather than a property of the runtime mechanism; see the open question in §6
   about reconstructing the auxiliary flags (`notScalar`, `attribs`,
   `notFastVecelt`), not just the SEXPTYPE.

**Failure mode, for the record.** If one of these static conditions were wrong,
the result would *not* be a safe over-approximation — it would be feedback that
is too **narrow** (a type the site really does see never gets recorded). In a
guarded speculative JIT that is not a memory-safety problem (the guard catches
it), but it is a correctness problem for the feedback and shows up as extra
deoptimization. So the conditions carry real weight; none of them is a
"conservative default."

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
the recordless contribution. (Verified 2026-07-27 against
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
  It has **none** of the expression-tree fields. The current **8-byte** layout
  adds `dirty`, `lastSig`, and the 2-byte biased parent index —
  **+4 bytes/slot**. This is the "node-size confound" (§7): any cross-branch
  timing partly reflects the larger feedback node, not the algorithm. It used to
  be +12 (a 16-byte slot with an 8-byte parent *pointer*); §2C.1 records how it
  came down.

**Force-behavior is PRE-EXISTING baseline machinery — not a recordless
invention.** The `stateBeforeLastForce` lattice and the `recordForceBehavior`
interpreter helper are both present in the baseline (8 references in baseline
`interp.cpp`). Baseline has a single `ldvar_cached_` with **no** FB-strategy
variants. Recordless's contribution to the FB axis is therefore narrow and
should be described as such in any writeup:

1. compile-time **per-slot strategy selection** (`ForceBehaviorKind`) surfaced as
   the `ldvar_cached_{,noRecordFB_,fbRecordOnce_}` opcode split, with the
   peek-free helpers `recordForceBehaviorNoCheck` /
   `recordForceBehaviorRecordOnceNoCheck`;
2. **once-gating** of FB via the shared `fired` bitmap;
3. teaching the generic dispatcher to recognize the `leaf_notify_` opcodes
   (without which FB was silently dropped for tree-participating non-cached
   leaves).

`recordFbAtSlot` itself is unchanged from the baseline formulation (§4.1).

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
- **NoRecord** → emit **no opcode at all**. A slot is still allocated and
  annotated with a compile-time dependency on a *source* slot (`typeDeps_`); the
  intended JIT-side recovery is §2A.3 and the soundness argument is §3.

### 2A.1.1 The full classification, in decision order

`classifyUse` (verified against `DefUseAnalysis.h`, 2026-07-27) returns
`{UseKind, sourceSlot, ForceBehaviorKind}`. The order matters — the first
matching rule wins:

| # | condition | result | FB kind |
|---|---|---|---|
| 0 | `isAssignedInPromise(name)` — assigned in a **promise-argument position** | `RecordAlways` | `Always` |
| 1 | *(eligible)* ∧ ∃ recorded use `ud` with `dominates(ud) ∧ postDominates(ud)` | `NoRecord` → `ud.feedbackSlot` | `FBValue` if local-stvar-reach or for-loop var, else `Infer` |
| 2 | reaching def `d` ∧ `isLocalOrParam` ∧ `postDominates(*d)` ∧ `d` has a slot | `NoRecord` → `d.feedbackSlot` | `FBValue` |
| 3 | `d` ∧ local/param ∧ `loopDepth_>0` ∧ `!assignedInInnermostLoop` ∧ `assignedInEnclosingLoop` | `RecordOnce` *(dynamic — bit gets cleared)* | `FBValue` |
| 4 | *optimizable* ∧ `loopDepth_>0` ∧ `!assignedInEnclosingLoop` | `RecordOnce` *(stable — bit excluded from clear range)* | `FBValue` if local-stvar-reach, else `RecordOnce` |
| 5 | `isRangeBasedForLoopVar` ∧ `loopDepth_>0` | `RecordOnce` | `FBValue` |
| 6 | *(fallthrough)* | `RecordAlways` | `Always` |

where **eligible** (rule 1) = *optimizable* ∨ `isForLoopVar` ∨ (local/param with
a dominating def), and ***optimizable*** = `isOuterControlled(name) ∨
(isLocalOrParam(name) ∧ (isFormal(name) ∨ reaching-def exists))`.

Note `isLocalOrParam` is a **conjunct**, not an alternative beside `isFormal` —
§2A.1.2 for why that is a soundness requirement and §2A.1.3 for what broke when
it was not. **eligible** simplifies; see §2A.4.1, which also explains why rule 1
takes `findDominatingDef` while rule 2 takes the back-edge-vetoed
`findReachingDef`.

Three consequences worth stating explicitly, none of which were obvious from the
prose description:

- **Record-once is a *loop* optimization.** Every RecordOnce rule (3, 4, 5) is
  guarded by `loopDepth_ > 0`. Outside any loop a use is either NoRecord (if
  subsumed) or RecordAlways — never once-gated. This makes sense: outside a loop
  a site executes once per activation anyway, so gating would add a check and
  save nothing.
- **There is a soundness carve-out for promise-argument assignment** (rule 0).
  Variables assigned in promise-argument position are excluded from *all*
  optimizations, because the assignment runs when the promise is forced, which is
  invisible to the main code's `stvar` sequence — so the def-use analysis cannot
  see it and its dominance conclusions would be wrong. This is the one place the
  analysis explicitly bails for R's lazy-evaluation semantics.
- **Free variables that are not stable captures are always recorded.** Only
  formals, outer-*controlled* captures, and locals/params with a reaching def are
  even eligible; an arbitrary free variable from an enclosing scope falls through
  to RecordAlways.

### 2A.1.2 Eligibility: the "in our control" requirement

The `optimizable` predicate above is not a heuristic — it is a **soundness
requirement**, and it is the piece that makes the whole scheme defensible.

**We may only optimize a variable whose every mutation we can see.** The
def/use analysis reasons from the `stvar` sequence it compiles. If a binding can
change by any route the compiler cannot observe, its dominance conclusions are
invalid, and a NoRecord use would inherit a type the variable no longer has.
Two such routes exist in R:

- **Reflection.** For a variable the function did not create, arbitrary code
  (`assign`, `eval`, `<<-` from elsewhere, a modified environment) may have
  changed it between two uses. The compiler cannot know whether reflection
  occurred, so it must assume it did.
- **Super-assignment from a nested function.** A `<<-` inside an inner closure
  mutates an outer binding without any `stvar` appearing in the outer body's
  instruction stream.

Hence the rule, as implemented:

> **We optimize parameters and locals — variables the function itself creates —
> except those that are super-assigned in a nested function. A nested function may
> additionally optimize variables from its enclosing scope, so long as they are
> not mutated after the inner function was created.**

Mechanically (`Compiler::finalize` and `computeCapturesForInner`, verified
2026-07-27, re-verified 2026-09-02):

Write $F_f$ for `f`'s formals, $L_f$ for its body-assigned names, and $S_f$ for
`innerSuperAssigned_` — the names `<<-`-assigned anywhere in `f`'s nested
closures. Both of the sets below are built from **$(F_f \cup L_f) \setminus S_f$**;
that subtraction is the whole soundness requirement, and it applies to formals
and locals alike (see §2A.1.3 for what happened, twice, when it did not).

The predicate that consumes them is the `optimizable` expression at the top of
`classifyUse`, and the shape that matters is that `isLocalOrParam` — i.e.
membership in $(F_f \cup L_f) \setminus S_f$ — is a **conjunct on every local
route**, never a disjunct beside them:

```cpp
const bool optimizable =
    isOuterControlled(name) ||
    (isLocalOrParam(name) && (isFormal(name) || d != nullptr));
```

`isFormal` and a dominating def `d` are two ways to establish that the read is
not a fallthrough to an uncontrolled scope; neither is a licence to skip the
`\ S_f` filter. `isOuterControlled` sits outside the conjunction because
`computeCapturesForInner` has already applied that same subtraction on the way
down.

- `functionLocalOrParam_` = $(F_f \cup L_f) \setminus S_f$. `S_f` is collected by
  a pre-scan, `collectInnerSuperAssigned`, which walks nested `function` bodies
  at any depth looking for `<<-`.
- For a nested compilation, the enclosing function hands down two capture sets:
  - **`controlled`** — "in our realm": carried-through outer captures, plus
    $(F_f \cup L_f) \setminus S_f$. The inner function may rely on name lookup
    falling through to a controlled environment rather than to the global env.
    This is what enables **RecordOnce** on such captures, and — via
    `isOuterControlled` in `classifyUse` — **NoRecord** subsumption too.
  - **`immutable`** — a strict subset whose *value* cannot change during the
    inner function's lifetime: formals never body-assigned, and body-locals
    assigned exactly once, not for-loop variables, with a dominating def at the
    point the closure is created. (Both start from the $\setminus S_f$ set, so
    `<<-`-escaped names are already gone.) Reserved for future cross-invocation
    optimizations; not yet exploited.
- The carry-through of an *outer* function's `controlled`/`immutable` needs no
  `S_f` filter of its own. `collectInnerSuperAssigned` → `scanForSuperAssigns`
  recurses through **all** nested functions at any depth, so a `<<-` anywhere in
  a subtree is already in the `innerSuperAssigned_` of *every* ancestor whose
  subtree contains it. Filtering where a name **enters** the capture sets is
  therefore complete; carry-through only ever sees already-filtered names.
- Both sets are shadowed correctly on the way down: a name that the inner
  function re-declares as its own formal (or, for `immutable`, body-assigns)
  is dropped from the inherited set.

**This is a deliberate tightening over an earlier, unsound version.** Previously
*any* variable could be optimized. The canonical example:

```r
function() { i; f(); i }      # `i` is a global — not created by this function
```

The earlier analysis made the second `i` a **NoRecord** use subsumed by the
first. That is wrong: `f()` may reflect on the environment and rebind `i`, so the
second use can see a different type. Under the current rules `i` is neither a
formal, nor an outer-*controlled* capture, nor `isLocalOrParam`, so `optimizable`
is false and both uses record. Note the cost of the fix is real — see the
nbody_naive figures in §8, where it is measurable.

The `<<-`-from-a-nested-function half of the rule has its own regression test
shape. This is the case that broke when `isLocalOrParam()` was once dropped from
`classifyUse`:

```r
f <- function() {
    x <- g()
    h <- function() { x <<- a() }   # mutates x invisibly to f's stvar sequence
    h()
    x                               # must NOT be no-record
}
```

`x` is a local of `f`, and `f`'s instruction stream contains exactly one `stvar`
for it — so a naive reaching-def analysis concludes the final `x` is subsumed by
`x <- g()`. But `h()` rebinds it with no `stvar` visible in `f`. The
`collectInnerSuperAssigned` pre-scan is what excludes `x`: it walks nested
`function` bodies at any depth for `<<-` targets and removes them from
`functionLocalOrParam_`, so `optimizable` is false and the read records.

### 2A.1.3 Two `<<-`-escape holes (found and fixed 2026-09-02)

The regression shape just above covers the case where the `<<-`-escaped variable
is used **in `f` itself** *and reaches the optimization through*
`functionLocalOrParam_` — that filter was correct. But `functionLocalOrParam_`
is only one of **three** disjuncts in the eligibility predicate, and the other
two each bypassed it. Both are fixed; they are described in the order they were
found, because the second subsumes the first.

#### The capture route (`isOuterControlled`)

`computeCapturesForInner` → `controlled` → `isOuterControlled` applied the
`\ S_f` subtraction to body-locals but **not to formals**:

```cpp
for (SEXP f : formalNames_) {
    result.controlled.insert(f);                 // unconditional — the bug
    if (!bodyAssignedCount_.count(f) && !innerSuperAssigned_.count(f))
        result.immutable.insert(f);              // guard only on immutable
}
```

That let a `<<-`-escaped **formal** reach an inner closure as an outer-controlled
capture, where `optimizable` is true and rule 1 can subsume. The witness needs a
`<<-` in one nested closure, a use of that name in a **sibling** closure, and the
first closure called between two uses:

```r
f <- function(x) {                    # x is a FORMAL of f
  h <- function() { x <<- "str" }     # rebinds f's x, no stvar in f
  g <- function() {
    x                                 # use 1 — records, sees double
    h()                               # x becomes character
    x                                 # use 2 — subsumed against use 1
  }
  g()
}
f(1)
```

Confirmed by disassembly, not by reading alone. Before the fix, `g` compiled to:

```
  0   ldvar_cached_  x{1}
  9   [ double (s) | evaluatedPromise ] Type#0 (record_type_)   <- use 1
 51   ldvar_cached_noRecordFB_  x{1}                            <- use 2, elided
...
NoRecord Type#1 (dep: #0)   /   FB Type#1 (Infer)
```

Slot Type#1 claims `double` for a binding that observes `"str"` — a **narrowing**,
the unsound direction. Changing `x` to a body-local of `f` made the identical
program safe, because that branch was guarded; nothing about the hazard differs
between the two, since `<<-` skips `h`'s own frame and lands in `f`'s frame either
way.

The fix is the missing guard, making the formals loop structurally identical to
the body-locals loop and the two together yield $(F_f \cup L_f) \setminus S_f$:

```cpp
for (SEXP f : formalNames_) {
    if (innerSuperAssigned_.count(f))
        continue;
    result.controlled.insert(f);
    if (!bodyAssignedCount_.count(f))
        result.immutable.insert(f);
}
```

After the fix `g` records use 2 as `character (s)`, matching the local variant.

#### The `isFormal` route — the same hole, one closure shorter

Fixing the capture route was **not sufficient**, because `classifyUse`'s
eligibility predicate had `isFormal` as a *bare* disjunct:

```cpp
const bool optimizable = isFormal(name) || isOuterControlled(name) ||
                         (isLocalOrParam(name) && d != nullptr);
```

`formalNames_` is populated unconditionally and is **not** filtered by
`innerSuperAssigned_` — and it cannot be, because the same set does shadowing on
the way down (§2A.1.2), where a `<<-`-escaped formal must still shadow an outer
name. So `isFormal` admitted exactly the names `isLocalOrParam` was excluding,
and the sibling closure turned out to be unnecessary — both uses can sit in `f`'s
own body:

```r
f <- function(x) {
  h <- function() { x <<- "str" }
  x        # use 1 — recorded, becomes a useDef via trackUseDef
  h()      # retypes x
  x        # use 2 — optimizable via isFormal, dominated and post-dominated
}          #          by use 1 -> NoRecord
f(1)
```

`isLocalOrParam(x)` is false here (the pre-scan filter works), but `optimizable`
was true anyway through `isFormal`, so rule 1's `useDefs` dedup fired.
Disassembly before the fix, identical in kind to the sibling case:

```
 25   ldvar_cached_  x{1}
 34   [ double (s) | evaluatedPromise ] Type#0 (record_type_)   <- use 1
 76   ldvar_cached_noRecordFB_  x{1}                            <- use 2, elided
...
NoRecord Type#1 (dep: #0)
```

The fix makes `isLocalOrParam` a **mandatory conjunct on every route**. Since
`functionLocalOrParam_` is $(F_f \cup L_f) \setminus S_f$, for a formal
`isLocalOrParam` is true precisely when it is not `<<-`-escaped, so the
conjunction supplies the missing filter without touching `formalNames_` —
`isFormal` keeps its real job, which is letting a formal be optimizable with **no
dominating store** (it is bound at call time):

```cpp
const bool optimizable =
    isOuterControlled(name) ||
    (isLocalOrParam(name) && (isFormal(name) || d != nullptr));
```

This is strictly a tightening: the new predicate's truth set is a subset of the
old one.

**A dead duplicate is what hid this.** A helper `isOptimizable` also existed,
with a third, *differently worded* disjunct (`hasDominatingDef` with no
`isLocalOrParam` conjunct) — but it had **no callers**; `classifyUse` carried its
own copy of the predicate, and the two had silently diverged unnoticed. Patching
the helper alone would have compiled cleanly and changed nothing. It has been
**deleted**, leaving the `classifyUse` expression above as the only definition —
the divergence is impossible because there is now just one copy, and the comment
on it says explicitly not to lift either sub-case out beside `isLocalOrParam`.

#### Cost of both fixes: none measured

Each guard was instrumented in turn and the benchmark suite run (fasta,
fastaredux, nbody, binarytrees, storage, bounce, mandelbrot, random — all chosen
because they contain `<<-`): **0 firings** for both, against 2 apiece on the
witnesses. The window is genuinely narrow, which is likely why they survived this
long. No timing comparison was attempted — a change this small can shift
`evalRirCode`'s codegen and any delta would be unattributable (§8).

**Note on `recordLess_Leaf_Enabled`.** Making `isLocalOrParam` mandatory looks
like it could change behaviour when the flag is off, since `localOrParam_` would
then be empty. It cannot: `Compiler::finalize` populates `formalNames_`,
`outerControlled_` **and** `functionLocalOrParam_` only under that flag, so with
it off all three are empty and the old predicate was already false everywhere.

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

**Current form on this branch — `alloca`** (verified 2026-07-27,
`interp.cpp` in `evalRirCode`):

```cpp
bool* fired = nullptr;
if (c->recordTypeOnceCount > 0) {
    fired = (bool*)alloca(c->recordTypeOnceCount * sizeof(bool));
    memset(fired, 0, c->recordTypeOnceCount * sizeof(bool));
}
```

Sized to exactly what this Code object uses (`Code::recordTypeOnceCount`, set in
`CompilerContext::pop()` from the context's `recordTypeOnceBitmapSize`), no fixed
cap in the frame, no bit-packing. (A fixed-size alternative was evaluated and not
adopted — see §5.)

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

"Once per invocation" is too coarse for a variable that is **re-assigned by an
enclosing loop**: its type may differ on each outer iteration, so its once-bit
must be reset before each run of the inner loop. Two opcodes do this:

- `clear_record_type_once_bit_` — clear a single bit.
- `clear_record_type_once_bits_range_` — clear a contiguous `[start, start+count)`
  range. Its immediate uses the same low16/high16 packing, but meaning
  `start`/`count` (`RECORD_TYPE_ONCE_RANGE_PACK/_START/_COUNT`). The handler is
  simply `memset(fired + start, 0, count * sizeof(bool))`.

The range form is why once-bits must be **contiguous per clearable scope**, and
that in turn is why bit assignment is *deferred*: when the compiler enters a
loop it does not yet know how many dynamic once-uses the body will contain.

**Two-phase placeholder-and-patch (`bc/LoopScopeGuards.h`).** The mechanism is a
pair of RAII guards that emit placeholders on scope entry and patch them on
scope exit:

1. **On entering a nested loop** (`loopDepth() > 0`), emit a *placeholder*
   `clearRecordTypeOnceBitsRange(0, 0)` **in the enclosing scope** (before the
   loop) and remember its bytecode position.
2. **While compiling the body**, each dynamic RecordOnce use emits
   `recordTypeOnce(slot, 0)` with a *placeholder* `iidx` of 0, registering its
   bytecode position and slot (`registerClearableUse` / `registerRangeVarUse`).
3. **On leaving the scope** (`finish()`), allocate a contiguous range
   `[base, base+count)` out of `CodeContext::recordTypeOnceBitmapSize`, then
   back-patch:
   - each use site's immediate → `RECORD_TYPE_ONCE_PACK(slot, base + i)`;
   - the clear placeholder → `RECORD_TYPE_ONCE_RANGE_PACK(base, count)`;
   - and bump `bitmapSize += count`.
4. **If `count == 0`** (no dynamic use materialized), the placeholder is
   **deleted** from the bytecode (`cs.remove(clearTemplatePos)`) rather than left
   as a no-op clear.

This required two new `CodeStream` primitives added by recordless:
`patchImmediate(bcPos, val)` and `patchOpcode(bcPos, op)` (the latter is also
what the specialization post-pass and the FB-variant patching use, §2C.5, §4.3).

**The two guards.**

- **`ClearableScopeGuard`** — the general case: user variables assigned in an
  outer loop and used inside this loop. Active for *all* loop kinds when nested.
- **`RangeBasedIterVarScope`** — for range-based for-loop iteration variables
  (`for (i in 1:n)`, `seq_len`, `seq_along`). The iteration variable's type is
  stable across iterations of *this* loop (so RecordOnce is sound despite the
  implicit per-iteration reassignment), but a different *outer* iteration could
  see a different element type, so a nested range loop still gets a clear
  placeholder. Its pending entries are assigned in **reverse order**
  (`pending.rbegin()`) once the outermost range scope completes
  (`rangeVarAssignmentReady`), so nested range loops get properly nested ranges.

**Why stable and dynamic bits must not interleave.** A *stable* RecordOnce use
(rule 4 in §2A.1.1 — not reassigned in any enclosing loop) must **not** be
cleared, or it would re-record every outer iteration and lose the optimization.
A *dynamic* use (rule 3) must be cleared. Since clearing is done by contiguous
range, the dynamic bits of a scope must form an uninterrupted block with no
stable bit inside it — which the deferred, patch-on-scope-exit assignment
guarantees by allocating each scope's dynamic bits together at the moment the
scope closes.

**Interaction with the `RECORD_TYPE_ONCE_MAX_IIDX` cap.** Bit assignment checks
the running total (`bitmapSize` plus any pending range-var bits) against the 512
cap; if a use cannot get a bit, the compiler falls back to emitting a plain
`record_type_` (always-record) for it rather than a once variant. This fallback
is also what makes the FB `RecordOnce` → `ldvar_cached_fbRecordOnce_` patch
conditional on `emittedRecordTypeOnce` (§4.3).

#### 2A.2.3 Promises: where their once-flags would live, and why the scheme is off

Record-once inside a **promise** cannot reuse the machinery above, and the reason
is a lifetime argument that follows directly from §2A.2.1.

The frame-local `fired` array is cheap and reentrancy-safe precisely *because* it
lives exactly as long as one `evalRirCode` activation. But **a promise may outlive
the frame that created it**: it is built in one activation and forced later,
possibly after its creator has returned. So a promise's once-flags cannot live in
the creating frame — by the time the promise runs, that storage is gone.

**The only place with the right lifetime is the environment in which the promise
was created.** That environment is kept alive by the promise itself, so flags
stored there survive exactly as long as they may be needed. The designed scheme
was therefore:

- extend `ENVSXP` with a `uint64_t recordTypeOnceBitmap` (hence
  `RECORD_TYPE_ONCE_PROMISE_MAX_IIDX = 64` — one word, unlike the 512-entry frame
  array), zeroed on function entry;
- gate promise-context once-records on it via
  `RECORD_TYPE_ONCE_PROMISE_GATE(env->u.envsxp.recordTypeOnceBitmap, raw)` /
  `RECORD_TYPE_ONCE_PROMISE_BITMAP_TEST`;
- with a dedicated opcode `record_type_once_promise_`, a load variant
  `ldvar_cached_envRecordFB_`, a per-function counter
  `Function::recordTypeOncePromiseCount`, and the FB strategy
  `ForceBehaviorKind::EnvBit`.

**Current status: disabled, on both counts.**

1. **Deliberate scoping.** Promises are left unoptimized for now, to avoid
   juggling too many interacting mechanisms at once. Reads in a promise context
   fall through to the normal always-record path (the promise-specific branch in
   `emitRecordTypeForVar` is commented out, and `ldvar_cached_envRecordFB_` /
   `record_type_once_promise_` are not emitted).
2. **The `ENVSXP` field is not present in this configuration.** Verified
   2026-07-27: `struct envsxp_struct` in the `custom-r` submodule (clean at
   `6483fffd7e`) still has only `frame`, `enclos`, `hashtab`. Every reference to
   `envsxp_struct::recordTypeOnceBitmap` in this repo is a **comment**, so the
   scheme could not run as configured even if re-enabled in the compiler.

What survives in the tree are the vestiges — the `RECORD_TYPE_ONCE_PROMISE_*`
macros in `bc/BC_inc.h`, `Function::recordTypeOncePromiseCount`,
`ForceBehaviorKind::EnvBit` (which currently falls through to `Always`), and
commented-out `DEF_INSTR`/handler/emission sites. They document the intended
design; none of them is live. Note also that re-enabling it means re-landing an R
submodule change, which is a heavier lift than a compiler-only change and carries
its own measurement confound (a wider `SEXPREC` for *every* environment).

### 2A.3 No-record: def-site subsumption, and its data path

The strongest leaf optimization: emit **nothing** at the use. When a use's value
is provably the same as a value already observed at another instrumented site (a
def, or an earlier use), the use's feedback is intended to be recovered from that
source rather than observed. The static condition that makes this sound is §3.

The data path, end to end (verified 2026-07-27):

1. **Compile time.** `emitRecordTypeForVar` sees `UseKind::NoRecord` and calls
   `CompilerContext::registerNoRecordDep(uc.defSlot)`, which:
   - allocates a *real* feedback slot for the elided use
     (`typeFeedbackBuilder.addType()`),
   - records the dependency `typeDeps_[slot] = sourceSlot`
     (`Builder::setTypeDep`), and
   - calls `registerLeafSlot(slot)` so the elided use still gets a **parent
     pointer** in the expression tree — this is what lets the source's
     notification reach the elided use's enclosing inner node (§2C.4).
   No opcode is emitted.
2. **`Compiler::finalize`.** `buildNoRecordReverseMap()` inverts `typeDeps_` into
   `noRecordSourceToDeps_` (source → list of dependent slots), so the runtime can
   walk a source's dependents.
3. **Runtime.** The dependent slot is never written (no opcode). The *notify*
   half is live: when the source's signature changes, `markRelatedDirty` walks
   `noRecordSourceToDeps_[source]` and marks each dependent's parent dirty.
4. **Consumer side (out of scope here).** `TypeFeedback::reconstructFeedback()` is the
   recovery step: it fills every dependent slot from its source
   (`copyTypeObservationsFrom`, the type fields only) and derives the
   force-behavior dimension from `forceBehaviorKinds_` in the same pass. It has
   **no callers** — wiring it into the JIT is future work and is not covered by
   this document (see the scope boundary in §7), so dependent slots are all-zero
   at runtime today. **§4.6** gives the full rule set, why the type copy must not
   be a whole-struct assignment, and the verification.

### 2A.4 Def/use tracking and invalidation (where source slots come from)

The classification in §2A.1.1 consumes two pieces of analysis state that the
compiler maintains as it walks the body. Both live in `DefUseAnalysis`:

- **`defs_[name]`** — the *most recent definition* of `name`: its scope id, the
  closed-return / loop-exit counters used by the dominance tests, and a
  `feedbackSlot`.
- **`useDefs_[name]`** — a list of *previously recorded uses* of `name`, each with
  the same scope metadata plus the slot that recorded it.

```cpp
void trackUseDef(SEXP name, int slot) {              // a use recorded itself
    useDefs_[name].push_back({currentScopeId(), closedReturnCount_,
                              currentLoopExitCount(), slot});
}

void trackDef(SEXP name, int feedbackSlot = kNoSlot) {   // name was (re)defined
    useDefs_.erase(name);                                //  (1) invalidate uses
    defs_[name] = {currentScopeId(), closedReturnCount_,
                   currentLoopExitCount(), feedbackSlot}; //  (2) reseat the def
    bumpSeen(name);                                       //  (3) loop bookkeeping
}
```

There are exactly four call sites (line numbers re-verified 2026-09-02):

| site | construct | what it records |
|---|---|---|
| `Compiler.cpp:612` | `recordTypeTracked(name)` — a **RecordAlways** leaf | `trackUseDef(name, slot)` — this use is now a subsumption candidate |
| `Compiler.cpp:2638` | a **RecordOnce** leaf | `trackUseDef(name, slot)` — same |
| `Compiler.cpp:1219` | **plain assignment** `x <- expr` | `trackDef(lhs, defSlot)` **with a real slot** |
| `Compiler.cpp:1388` | **subassignment** `x[i] <- v` | `trackDef(target, kNoSlot)` — **slot-less** |

Note what is *absent* from that table: a **NoRecord** read does not call
`trackUseDef`. It emits no record, so it has no slot of its own to offer; it
stamps `uc.defSlot` instead (see "NoRecord reads stamp the source" below). So
`useDefs_[name]` holds only uses that genuinely recorded, and every subsumed use
points at the original recording site rather than chaining through its
predecessor. Nothing is lost — a subsumed use holds the same value by
construction.

**Plain assignment donates its rhs's slot — but only when it can prove which
slot that is.** The requirement on the donated slot is stronger than "it
describes the value": it must observe the stored value on **every execution of
the assignment**, because later NoRecord reads of `x` will claim "my type is
whatever that slot accumulated".

This used to be inferred positionally — *the last type slot allocated while
compiling the rhs* — which is wrong whenever the rhs's outermost operation emits
no record of its own. Measured failures: `x <- -a` and `x <- !f()` and
`x <- is.null(f())` all donated the *operand's* slot (so `x <- -a` with a logical
`a` claimed `logical` where the truth is `integer`), and `x <- if (c) f() else g()`
donated **one branch's** slot, losing the other's types. All under-approximations,
the unsound direction.

It is now established from what was actually emitted (`valueRecordSlotHere`).
The record helpers stamp `(slot, instruction position, scope id)`; the assignment
queries the stamp immediately after `compileExpr(rhs)`, before anything else is
emitted, and accepts it only if:

1. the stamped instruction is still the last **value-changing** one — nothing
   replaced the value since. (Value-*neutral* instructions such as `visible_`
   and `ensure_named_` are excluded, or `x <- v[i]` and `x <- (f())` would be
   rejected for the trailing `visible_`.)
2. the stamped scope is still open — the record is on every path here, not on
   one branch.

Neither condition subsumes the other: the position test catches `-a` but accepts
the `if`, whose last emitted instruction genuinely *is* a record; the scope test
catches the `if` but accepts `-a`. Anything unproven yields `kNoSlot`, so later
reads record for real — an elision lost, never a wrong type.

A stamp that is merely *inaccurate* is safe by construction: C++ leaves argument
evaluation unsequenced in `cs << a << b << c`, so a helper buried mid-chain may
capture a stale position — which makes condition 1 fail. It can cost an elision,
never grant a wrong slot.

**NoRecord reads stamp the source, keeping the dep graph flat.** A NoRecord read
emits no record at all, so it stamps `uc.defSlot` — the *ultimate source* — with
the `ldvar`'s position. That is what makes `a <- f(); b <- a; c <- b; d <- c`
resolve as `#1→#0, #2→#0, #3→#0, #4→#0` instead of a chain, which
`markRelatedDirty`'s one-level walk requires (§2C.2).

**Subassignment is a type barrier.** At site 1199 (`x[i] <- v`, `x[[i]] <- v`,
and the multi-dim forms), the emitted sequence is
`subassign1_1/…; stvar[Cached]` — **no `record_type_` follows**, so nothing
observed the new value of `x`. Hence `kNoSlot`. This matters because a
subassignment can *change the type*:

```r
x <- c(1, 2, 3)   # REALSXP
x[1] <- "a"       # x is now STRSXP
```

Without invalidation, a later read of `x` could be classified NoRecord against a
def or use from *before* the subassign and inherit a stale type — and since
NoRecord is a correctness commitment (§3), that would simply be wrong feedback.

**Why `defs_` is reseated rather than erased.** This is the subtle part. The three
operations in `trackDef` have distinct effects, and the split is deliberate:

- `useDefs_.erase(name)` kills **use-to-use** subsumption (rule 1).
- `feedbackSlot = kNoSlot` kills **def-to-use** subsumption (rule 2 requires
  `d->feedbackSlot != kNoSlot`).
- Keeping an entry in `defs_` preserves `d != nullptr`, which still gates
  `optimizable`, `hasLocalStvarReach`, and the **RecordOnce** rules 3 and 4.

So a slot-less def says precisely: *"`x` was redefined here; I have no type for
the new value, but it is still a tracked local with a known def site."* Erasing
`defs_` instead would make `d == nullptr`, and for a plain local (not a formal,
not an outer-controlled capture) `optimizable` would go false — so rules 3 and 4
could not fire and the next read would degrade all the way to **RecordAlways**.
That is strictly more conservative than necessary: the type is unknown, but the
variable is still locally controlled, so "observe once per activation" remains
sound (rules 3/4 carry their own `!assignedInInnermostLoop` /
`!assignedInEnclosingLoop` guards for the cases a loop could change it between
iterations).

Erasing would also lose **force-behavior precision**: `hasLocalStvarReach =
(d != nullptr && isLocalOrParam(name))`, and rule 4 returns
`hasLocalStvarReach ? FBValue : RecordOnce`. After a subassign `x` definitely
holds a materialised value — never an unforced promise — so `FBValue` is correct
and patches the load to `ldvar_cached_noRecordFB_`, skipping FB recording
entirely (§4.2–4.3). With the def erased this would fall back to a runtime FB
gate for no reason.

Note that locality is *not* what `defs_` signals: `isLocalOrParam`, `isFormal`
and `isOuterControlled` all read pre-scan sets (`localOrParam_`, `formalNames_`,
`outerControlled_`) and never consult `defs_`.

**Two related details.**

- **`bumpSeen(name)`** feeds `hasUnseenLoopDef`, which makes `findReachingDef`
  return `nullptr` when a loop body still contains a def it has not compiled yet
  — preventing a def from being used as a source when a later iteration will
  overwrite it.
- **Super-assignment deliberately does not track.** The `superAssign` branch
  emits `stvarSuper` and skips `trackDef`: the write targets an *enclosing*
  environment, not the local binding, so local def/use reasoning does not apply
  (and such names are not `isLocalOrParam` anyway).

This is the same family of guard as the `isAssignedInPromise` carve-out (rule 0,
§2A.1.1): both are points where a write happens that the naive `stvar` sequence
would otherwise misrepresent.

### 2A.4.1 Why the two subsumption rules disagree about loops

Rules 1 and 2 both ask "is this read's value already described by an existing
slot?", and both are gated on dominance — but they take **different** queries for
the dominating def, and the difference is deliberate. The two differ by exactly
one line:

```cpp
const Def* findReachingDef(SEXP name) const {
    auto it = defs_.find(name);
    if (it == defs_.end()) return nullptr;
    if (hasUnseenLoopDef(name)) return nullptr;      // <-- the only difference
    return dominates(it->second) ? &it->second : nullptr;
}
```

`hasUnseenLoopDef` is the single-pass correction described above: while some
currently-open loop body still has `seen < expected` assignments to `name`, a
store *later in the body* has not been compiled yet, but from iteration 2 onward
it has already *executed* via the back edge. So "most recent def in source order"
is not the def that reaches here.

**Rule 1 (use-to-use) does not care.** Its gate is:

```cpp
if (optimizable || isForLoopVar(name) ||
    (isLocalOrParam(name) && findDominatingDef(name)))
```

Since `findReachingDef != nullptr` implies `findDominatingDef != nullptr`, the
third disjunct absorbs the `d` term inside `optimizable`, and the whole gate
reduces to:

> `isOuterControlled || isForLoopVar || (isLocalOrParam && (isFormal || findDominatingDef))`

— *formal, or outer-controlled, or a local with **some** dominating def*, with the
back-edge veto playing no part. What that dominating def establishes is **not**
locality (`isLocalOrParam` already says that, statically, from a pre-scan set). It
establishes that **the binding already exists in our frame at this point**: before
a body-local's first store, an `ldvar` for it falls through to the enclosing env,
which may be uncontrolled. That is also exactly why `isFormal` is an *alternative*
to it rather than an addition — a formal is bound at call time, so no store is
needed to rule out the fallthrough.

Value identity is then established independently, by
`dominates(ud) && postDominates(ud)` on the use-record — and *that* test is
immune to the back edge:

- a def **between** `ud` and here would have erased `useDefs_[name]` entirely;
- a def **after** here in the body executes before `ud` on the next iteration,
  never in the gap between them.

**Rule 2 (def-to-use) very much cares**, and uses the vetoed `d`:

```cpp
if (d && isLocalOrParam(name) && postDominates(*d) && d->feedbackSlot != kNoSlot)
```

The asymmetry is not an inconsistency — the two rules make different claims:

| | claim | effect of a later in-loop def |
|---|---|---|
| rule 1, use-to-use | *these two **reads** see the same value as each other* | none — both re-execute each iteration and see whatever is live |
| rule 2, def-to-use | *this read's value is the one **that store** wrote* | fatal — from iteration 2 the live value comes from the back edge |

**What rule 1's third disjunct buys, measured.** Removing
`(isLocalOrParam(name) && findDominatingDef(name))` and recompiling turns three
recording sites into one, in the ordinary accumulator loop:

```r
f <- function(n) {
  x <- 1
  i <- 0
  while (i < n) {
    x                 # records
    x                 # subsumed only because of the third disjunct
    x <- x + 1        # the x operand here: subsumed too; then the "unseen" def
    i <- i + 1
  }
  x
}
```

```
WITHOUT the disjunct              WITH it
 75  ldvar_cached_  x{0}           75  ldvar_cached_  x{0}
 90  ldvar_cached_  x{0}           90  ldvar_cached_noRecordFB_  x{0}
105  ldvar_cached_  x{0}          100  ldvar_cached_noRecordFB_  x{0}
131  stvar_cached_  x{0}          121  stvar_cached_  x{0}
```

Without it the gate never opens, so the `useDefs_` loop is not entered at all,
`d` is null so rules 2/3/4 cannot fire either, and every read degrades to
RecordAlways. Note the third read is the `x` *inside* `x <- x + 1` — the operand
of the very store whose existence caused `findReachingDef` to abstain. Since this
is the shape of any loop that carries state, the disjunct is load-bearing rather
than an edge case.

The clause is *only* what separates that function from this one, which is
identical but for the missing pre-loop store, and where `findDominatingDef` fails
too so both reads record:

```r
g <- function(n) {
  i <- 0
  while (i < n) {
    y                 # records
    y                 # records again — no dominating def of y exists yet
    y <- i
    i <- i + 1
  }
}
```

### 2A.5 Force-behavior rides on the leaf load

The orthogonal force-behavior axis (§4) is recorded at the *load* — so its
strategy selection (`ldvar_cached_` / `ldvar_cached_noRecordFB_` /
`ldvar_cached_fbRecordOnce_`) is a leaf-level concern, chosen by the same
`classifyUse`/`emitRecordTypeForVar` pass.

### 2A.6 Problems specific to leaves

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

An inner node starts clean and records only when a child tells it to. Its
handler does nothing otherwise:

```cpp
void recordInner(SEXP e) { if (!dirty) return; dirty = false; doRecord(e); }
```

So in the common case an inner node costs only a bit test, never a full
observation. Note there is no separate `shouldNotRecord` flag: "is this an
elidable inner node" is carried by the opcode, and `dirty` is the whole runtime
state.

### 2B.2 Reversal: record when the operands change

**This section describes the mechanism as of 2026-08-11; it replaced an
object-gated design, and §2B.2.1 explains why.**

An inner node must re-record whenever its result could differ from the one it
already absorbed. Each child therefore computes a **per-execution signature** of
the value it just observed, compares it against the previous execution's, and on
a change marks its parent `dirty`. The parent records once and re-arms.

```
sig(v) = 1 + TYPEOF*4 + isScalar*2 + hasDim      (see signatureOf)
sig(v) = 0  ("always dirty") for objects, attributes beyond a lone `dim`,
            S4, and length 0 — values that cannot be summarised
```

Soundness is one induction: *not dirty* means every operand had the same
signature as last execution, so — given the operator's result is a function of
its operands' signatures — the result is the one already recorded, and since the
accumulated state only grows, having absorbed it once is permanent. The base
case holds because a slot's initial signature is 0, which never compares equal,
so the first execution always records.

The critical property is that the signature describes **this execution**, not
the accumulated state. The accumulated state is monotone and stops changing
while the live value keeps varying underneath it: after operands have separately
been seen as `int` and `double`, an accumulated-state watcher sees nothing when
the first `(int, int)` pair arrives, and misses the `integer` result.

### 2B.2.1 What this replaced, and why

The previous design un-suppressed a parent when a child observed an **object**,
via `notifyRelatedNodes` — object-gated, monotonic, and latched once per
activation. It was cheaper (one test on a value `doRecord` had already computed)
but not lossless, for two independent reasons:

1. **The type set is unreconstructible when both operands are polymorphic.**
   `h(1L,1.5); h(1.5,1L)` and the same plus `h(1L,1L)` leave *byte-identical*
   operand feedback — `[integer,double]`, `[double,integer]`, both scalar — but
   ground truths of `{double}` and `{double,integer}`. No function of the
   operand feedback can be right for both. This is an impossibility, not a
   missing rule.
2. **`attribs` is unreconstructible**, because `copyMostAttrib` gates on the
   operand's actual length while the feedback carries only `notScalar`
   (§2B.3).

Worse, inner-node reconstruction was never implemented — `reconstructFeedback` only
does the leaf copy, and is itself never called (§2A.3, §4.6) — so suppressed
inner slots simply stayed empty
(`numTypes == 0`, which `PirType::merge` asserts against). And when the old gate
*did* fire it recorded a biased sample: only post-object executions, missing
every one before the latch.

The signature mechanism needs no reconstruction at all: the inner node holds its
own recorded observations. Two further consequences of dropping the latch:

- it is **re-armable**. The old latch was monotone in the wrong direction — one
  object anywhere meant the parent recorded on *every* subsequent execution for
  the life of the function. The signature version re-suppresses as soon as the
  operands settle, so a loop that sees an object early and plain doubles for a
  million iterations is far cheaper.
- an object operand keeps signature 0, which never compares equal even to
  itself, so it re-records for as long as objects keep arriving — the old
  behaviour, without the permanence.

### 2B.3 What actually becomes an inner node

There is exactly **one** `recordTypeTracked(true)` call site in the compiler
(verified 2026-08-07, `Compiler.cpp:952`) — that call is the *only* way a slot
becomes an inner node, and it covers a fixed list of **13 binary operators**:

> `Add Sub Mul Div Idiv Mod Pow` (arithmetic), `Eq Ne Lt Le Gt Ge` (comparison).

Everything else — general calls, `[`, `[[`, `:`, replacement functions, `for`
elements — is a **leaf**, not an inner node. `:` is the one case that looks like
it belongs: it shares the binary-operator bytecode shape (two operands then a
dedicated opcode) and lives in the same `compileSpecialCall` block, but is
explicitly routed to `recordTypeOpaqueResult()` instead — see below.

**The inference rule is per-operator, and not "result type = operand type."**
Two distinct shapes among the 13:

- **Arithmetic** (`+ - * / %/% %% ^`): result SEXPTYPE follows the usual
  promotion rules over the operands, and result length is
  `max(operand lengths)` — so *scalar-ness is also derivable* (result is scalar
  iff both operands are). Both the type and the `notScalar` flag are inferable.
- **Comparison** (`== != < <= > >=`): result is **always `LGLSXP`**, independent
  of operand types; length again `max(operand lengths)`. Inferable, but by a
  constant rule, not by propagation.

**Why `[` is *not* an inner node (it was, until it was demoted).** From
2026-06-03 (`b659827e`, "optimnize for extract1") until recently, `[` (`Bracket`)
*was* emitted as an elidable inner node, on the justification: "`x[...]` has the
same SEXPTYPE as `x` for non-object `x`, so its result type is inferable from the
lhs leaf." The SEXPTYPE half is true; the conclusion does not follow, because
eliding the node discards the **entire** `ObservedValues`, and `notScalar` is not
inherited. Measured on the baseline (record-everything) build with
`f <- function(x,i) x[i]`, `x = c(1,2,3)`, `i = 1L`:

```
ldvar_cached_ x   → [ double () | promise ]   Type#0    ← lhs: notScalar
ldvar_cached_ i   → [ integer (s) | value ]   Type#2
extract1_1_       → [ double (s) ]            Type#3    ← result: scalar
```

(flags are `(o)`bject `(a)`ttribs `(v)`notFastVecelt `(s)`calar.)

Worse, `notScalar` for `x[i]` is **not inferable from the operands' feedback at
all** — it depends on the *length of the index value*, which type feedback never
records (`x[1]` is scalar, `x[1:2]` is not; `i` is `INTSXP` in both). The best a
consumer could do is conservatively assume `notScalar`, which is a precision loss
— i.e. exactly the over-approximation the design is meant to avoid (§1). Since
scalar-ness drives PIR's unboxing and fast-path selection, this is a materially
useful bit to lose.

`[` has therefore been **demoted to `recordTypeTracked(false)`** — a tracked
always-record leaf, the same treatment as `[[`. Confirmed in the emitted
bytecode: `x[i]`'s result slot is now a plain always-recording leaf and its
operands carry no parent pointer, while `x + y` still yields a suppressed inner
node.

`:` is excluded for the same reason: its result type and length come from the
operand *values*, not their types, so the feedback cannot be reconstructed. It
is emitted as `recordTypeOpaqueResult()`.

The general lesson, worth carrying into any future inner-node candidate: **the
inference obligation is over the whole `ObservedValues`, not just the SEXPTYPE.**
An operator qualifies only if *every* recorded field — type set, `notScalar`,
`attribs`, `notFastVecelt` — is derivable from the operands' recorded feedback.
`[` fails on `notScalar`; the arithmetic ops pass because result length is
`max(operand lengths)`.

`[[` (`DoubleBracket`) extracts an *element* whose type genuinely varies (e.g.
`list(3, "hello")[[i]]`), so it was never a candidate. The colon *operand casts*
(`colonCastLhs/Rhs`) emitted by `compileSimpleFor` are internal and untracked —
distinct from the `Colon` result node.

### 2B.4 Problems specific to inner nodes

- **Determining inferability** is per-operator, conservative, and easy to get
  wrong. `[[`, general calls, and anything whose recorded feedback isn't a
  function of the operands' recorded feedback must *not* be made an elidable
  inner node. Note the obligation covers the **whole `ObservedValues`**, not just
  the SEXPTYPE — `[` was demoted for exactly this reason (§2B.3), having been an
  inner node for ~7 weeks on a SEXPTYPE-only argument.
- **Sound + cheap reversal** (§2B.2). Cheapness relies on the signature compare;
  soundness relies on *some* child always observing the object before the inner
  node's (elided) feedback is consumed.
- **Graceful degradation / leaning on leaves.** An inner node can only be elided
  if at least one child registered under it: the post-pass sets
  leaf-vs-inner (`slot ∉ parentSlots`) into the opcode, and
  `registerSlot(parent, /*isParent=*/true)` only enters `parents` for the slots
  pending at that level. So an expression whose operands registered nothing stays
  leaf status and falls back to always-record — it degrades rather than eliding
  unsoundly.

  Keep this distinct from the *soundness* requirement, which is the stronger one:
  it is not enough for an inner node to have **some** child — the child that
  actually **produces its operand** must be tracked, or an object appearing only
  in that operand will never un-suppress the node. That is the real reason opaque
  value results are `recordTypeTracked(false)` rather than untracked; see the
  worked `x[[i]] + 1` counterexample in §2C.4. (Note the two can come apart:
  because `popNodeForSlots` propagates unadopted slots upward, an enclosing node
  often has the *inner* expression's operands as children even when the inner
  result itself is untracked — enough to make it an inner node, not enough to make
  the elision sound.)
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
uint8_t  dirty : 1;    // inner node: a child's signature changed, re-record
uint8_t  lastSig;      // signature of the value seen on the PREVIOUS execution
uint16_t parentPlus1;  // parent slot index, biased by one (0 = no parent)
```

Total size: **8 bytes** (1 flags + 1 `dirty` + 3 `seen` + 1 `lastSig` + 2
parent) vs. the baseline **4 bytes**. `parentPlus1` links a node to its
enclosing inner node; it is reconstructed at compile time, not serialized.

Three fields that earlier versions carried are **gone**:

- `isLeaf` / `shouldNotRecord` — nothing read them at runtime (`PirType::merge`,
  the JIT-side consumer, never looks at them) and the opcode already says
  whether a slot is a leaf or an elidable inner node. `shouldNotRecord` was in
  any case only ever set as `!isLeaf`.
- `hasPropagatedNotification` — the notify latch, meaningless once notification
  became per-execution rather than one-shot (§2B.2.1).

Two deliberate layout choices:

- **`parent` is an index, not a pointer.** Every edge is within one `types_`
  array, so 2 bytes replace 8 — most of the way from 13 bytes to 8. It is
  **biased by one** so that an all-zero slot means "no parent": the array is
  `memcpy`'d from a vector and the constructor `memset`s, so a `0xFFFF` sentinel
  would be one forgotten initializer away from silently making slot 0 everyone's
  parent. An edge whose parent index will not fit is *dropped* at compile time,
  not truncated — a truncated edge would leave an inner node that no child can
  reach, so it would never record at all.
- **`lastSig` gets a whole byte** rather than sharing one with `dirty`. That
  costs a byte and buys three things: no read-modify-write to preserve a
  neighbouring bit; no range guard on `TYPEOF` (a 5-bit field, so `≤ 31`, and
  the widest encoding fits a byte — at 7 bits the encoding saturated and needed
  a `type <= 30` check to stop a wrapped signature aliasing another type's); and
  `sizeof` becomes a power of two.

### 2C.2 One notification function for both edges

The single mechanism that serves *both* "leaf/inner notifies its parent" and
"source notifies its dependents' parents":

```cpp
// TypeFeedback::markRelatedDirty — mark this slot's own parent (if any) AND the
// parents of all its NoRecord dependents. Called only when the signature
// actually changed.
void markRelatedDirty(ObservedValues& slot, uint32_t idx) {
    if (slot.hasParent())                          // leaf-with-parent / non-root inner
        types_[slot.parentSlot()].dirty = true;
    for (uint32_t d : noRecordSourceToDeps_[idx])  // source → dependents' parents
        if (types_[d].hasParent())
            types_[types_[d].parentSlot()].dirty = true;
}
```

Design points:

- **Fires on change, not on objects, and is not latched.** The predicate is "the
  per-execution signature differed", and the consumer clears `dirty` after
  recording. See §2B.2.1 for why the object gate and the one-shot latch went
  away.
- **Generic over both edges.** One function handles the parent edge (leaf 2B
  reversal / non-root inner) and the dependents edge (leaf 2A NoRecord source),
  with the inapplicable branch a cheap no-op. This is *the* place the two
  strategies literally share code, and it is why the opcode taxonomy collapses to
  "notifies or not" rather than a 2×2 of parent×source.
- **The walk is one level deep, and that is load-bearing.**
  `noRecordSourceToDeps_` holds only *direct* edges, so a source reaches its
  dependents' parents but not a dependent's dependents'. This is only correct
  because the dep graph is kept **flat**: a NoRecord read stamps the *ultimate
  source* slot rather than the dep slot just allocated, so `b <- a; c <- b`
  yields `#2→#0, #3→#0` rather than a `#3→#2→#0` chain (§2A.4). With chains, a
  copy sitting under a suppressed inner node was two hops from the source and
  never reached — the inner node stayed suppressed permanently and recorded
  nothing at all, a real divergence from baseline. Flattening removes the need
  for a transitive walk rather than adding one.
- **Only leaves that need it pay.** A parentless non-source leaf keeps the plain
  `record_type_` opcode and never calls this, nor maintains a signature.

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
and the "notify" form of both routes through the same `markRelatedDirty`.
`record_test_`/`record_call_` (branch/callee feedback) sit outside this range.

The `ObservedValues` recording primitives:

- `doRecord(e)` — shared core: updates `numTypes/seen[]`, `notScalar`, `object`,
  `attribs`, `notFastVecelt`.
- `record(e)` → just `doRecord(e)`. Used by plain `record_type_`/`_once_`.
  (Leaves are never suppressed, so `record()` has **no** `dirty` check;
  it was dead for its callers and was removed this session.)
- `recordSimple(e)` — historically a thin alias for `doRecord`; **inlined away**
  this session (`record_type_leaf_notify` calls `doRecord` directly, being a
  friend of `ObservedValues`).
- `recordInner(e)` — `if (!dirty) return; dirty = false; doRecord(e);`. Used by *both*
  inner-node opcodes.

**Untracked records** (a leaf concern that touches the shared opcode). Not every
`record_type_` participates in the analysis. `recordTypeUntracked()` emits a
plain `record_type_` and registers nowhere, so the post-pass leaves it untouched:
always records, never notifies, never suppressed, never NoRecord/RecordOnce. The
truly-untracked sites: the colon `m:n` operand casts (×2), the super-assign
target read-for-update, and the `ldvar` fallback while compiling default formal
args (no main-body context).

`recordTypeTracked` also **degrades to untracked while compiling default formal
arguments** (`!mainBodyCtx_`), so every record inside a default arg records always
and is attributed to the untracked row — matching the baseline, which has no
suppression anywhere. Without this, an expression like `a + f(1)` in a default arg
would suppress the `+` on the strength of the tracked call result while `a`, being
untracked, held no parent pointer to revoke it.

Not registering guarantees leaf && root — nothing maps to or from the slot
in `parents`. It does **not** structurally guarantee `!isSource`: the post-pass
derives `sourceSlots` from `typeDeps_` targets, and `trackDef` records whatever
slot the value-record stamp names, without regard to tracking (§2A.4), so an
untracked slot could in principle be named as a def's feedback slot and then be specialized
to `record_type_leaf_notify_`. In practice it never is, but *contingently*, for a
different reason at each site: the colon casts and the default-arg fallback are
never reached by `trackDef` at all (the latter returns before `classifyUse`), and
the super-assign branch deliberately skips `trackDef` (§2A.4). Worth re-checking
if a new untracked site is ever added.
Because a genuine untracked record and a RecordAlways leaf share the *same*
opcode, the stats build distinguishes them at runtime via
`setStatsUntrackedSlots` / `isStatsUntracked(idx)` (a compile-time-populated slot
set, consulted only under `RIR_RECORD_STATS`).

### 2C.4 Where the two strategies interact

- **A leaf's source can be an inner node.** The `a <- f(x)+1; …; a` case: the
  root inner node (`f(x)+1`) is the source, later reads of `a` are NoRecord
  leaves depending on it. When the source (an inner node) records an object it
  must un-suppress the *dependent leaves' parents* — a "two-step" propagation
  that `markRelatedDirty`'s dependents loop performs.
- **An inner node's elision is sound only if the node that *produces* its operand
  can un-suppress it.** This is why opaque value results (call return, `[`, `[[`,
  `for` element, replacement-fn) use `recordTypeTracked(isParent=false)` rather
  than `recordTypeUntracked()`. The two helpers differ in exactly one line —
  `recordTypeTracked` calls `registerSlot(slotIdx, isParent)`, so the slot joins
  `slotsStack.top()` as a child candidate and the enclosing
  `recordTypeTracked(true)` adopts it, giving it a **`parent` pointer**. An
  untracked slot never gets one, so it can never un-suppress anything.

  Why that is not academic — `g <- function(x, i) x[[i]] + 1` run with
  `x = list(structure(1, class = "myclass"))` and a `+.myclass` method
  (measured 2026-07-27):

  ```
  x       → [ list    (s)    … ] Type#0   ← NOT an object
  i       → [ integer (s)    … ] Type#2   ← NOT an object
  x[[i]]  → [ double  (oavs) … ] Type#3   ← IS an object;  parent -> Type#4
  add_    → [ double  (oavs) … should not record: 0 ] Type#4  ← un-suppressed
  ```

  `x[[i]]` is an object while `x` and `i` are not, so the `[[` result is the
  *only* node that can tell `+` its inference has broken. Because it is tracked it
  holds `parent -> Type#4`, so its `markRelatedDirty` set `+`'s
  `dirty` and `+` correctly recorded the S3-dispatched result. Had the
  `[[` result been untracked, `+`'s children would be only `x` and `i` — both
  non-objects — so **nothing would ever have un-suppressed `+`**: it would have
  kept empty feedback while dispatching to `+.myclass` and returning an object.

  **Runtime cost of tracking: none.** The same `record_type_` opcode is emitted,
  and a tracked slot that ends up with no parent and no dependents is left plain
  by the post-pass. It is compile-time bookkeeping that *enables* the enclosing
  elision. (Only other effect: stats attribution — tracked leaves are counted in
  the "leaves" row, untracked ones in the "untracked" row.)

  Note this benefit is specifically about the **parent edge**. Acting as a *def's
  feedback slot* does **not** require tracking: `trackDef` records whatever slot
  the value-record stamp names, regardless of whether that slot was ever
  registered in the tree (§2A.4).
- **The `tracked` vs `untracked` distinction is the bridge.**
  `recordTypeTracked(isParent)` registers a slot in the expression-tree structure
  (parent/leaf role) so the post-pass can specialize it and an enclosing inner
  node can lean on it; `recordTypeUntracked()` opts a slot out entirely. The
  genuinely untracked sites (§2C.3) are the ones that can never be the operand of
  a tracked expression, so they have no parent edge to lose.

### 2C.5 Compile-time construction and the specialization post-pass

**Building the tree during a single compile pass.** The expression tree is never
materialized as a data structure; it is discovered with a **stack of pending
child-slot lists** (`slotsStack`) plus a flat `parents` map (child slot → parent
slot):

- `slotsStack` lives on the **`CodeContext`**, i.e. one per Code object, not per
  function. An expression tree never spans a Code boundary: a promise body is a
  separate Code object whose evaluation is decoupled in time from the expression
  that created it, so its operands are not operands of that expression. When
  this lived on the function-wide `CompilerContext`, a promise's leaves landed
  in whatever level the *enclosing* function had open — in `f(x) + g(x)` the
  promise loads of `x` became children of the `+`.

  **`valueRecord` (§2A.4's def-slot stamp) lives there for the same reason**, and
  the reason is worth stating because it is not the obvious one. Both quantities
  the stamp compares are *per Code object*: `insnPos` is an offset into that
  `CodeContext`'s `CodeStream`, and `scopeId` comes from that `CodeContext`'s
  `DefUseAnalysis`. Both counters restart — `pos` at 0, `nextScopeId_` at 1 — so
  a stamp carried across a Code boundary would be compared against a different
  counter space and could match by *coincidence* rather than by meaning. A
  top-level record stamps `scopeId == 0`, which `scopeStillOpen` accepts
  unconditionally, so the scope half would offer no protection at all there.

  Note this is **not** covered by promises being excluded from the optimization.
  That exclusion stops a stamp made in a promise from being *consumed* in that
  promise (`emitRecordTypeForVar` returns early, so `classifyUse` never runs),
  but the degradation path is `recordTypeUntracked()`, which still stamps — and
  the consumer at the assignment is guarded only by the global
  `isRecordlessLeafEnabled()`. What made the leak unreachable in practice was
  unrelated and contingent: every `mk_promise_` happens to be followed by a
  `call_` whose result re-stamps. Putting the state on the `CodeContext` makes
  it unreachable by construction instead, which is why there is deliberately no
  "same Code object?" check in `valueRecordSlotHere`.
- `compileExpr`'s `LANGSXP` case brackets every call/expression with
  `pushNewNodeForSlots()` … `compileCall(...)` … `popNodeForSlots()`. One stack
  level per nested expression; `compileExpr` is the *single owner* of the
  push/pop balance.
- `registerSlot(slotIdx, isParent)`:
  ```cpp
  if (!isParent) {
      currentSlots.push_back(slotIdx);        // a leaf/operand at this level
  } else {
      for (auto child : currentSlots)         // this slot is the result of the
          parents[child] = slotIdx;           //   expression → adopt the pending
      currentSlots.clear();                   //   operands as children,
      currentSlots.push_back(slotIdx);        //   then stand in for them
  }
  ```
  So `recordTypeTracked(isParent=true)` (emitted for a type-preserving
  sub-expression result) **collapses** the operands registered at the current
  level into children of the new node, and the new node becomes the single
  pending slot at that level — which the *enclosing* expression will in turn
  adopt. That is how `a <- f(x) + 1` links `x`→`+` etc. without an explicit tree.
- `popNodeForSlots()` moves any still-unadopted slots **up to the enclosing
  level** rather than dropping them. This matters for non-profiled or
  non-type-preserving calls, where no `recordTypeTracked(true)` was emitted for
  the result: the operands would otherwise be orphaned (and the stack would go
  out of balance).
- **`recordTypeOpaqueResult()` — the third role.** An opaque value result (call
  return, `[`, `[[`, `for`, replacement-fn) is a recording *leaf*, so it must not
  adopt its operands as children (that would make it an inner node and hence
  suppressible). But it must still **consume** them: it clears the pending list
  and pushes only itself.

  Without that, the operands stay pending, `popNodeForSlots` lifts them to the
  enclosing level, and the *enclosing* inner node adopts them. In `v[i] + 1`,
  `v` and `i` would become children of the `+` even though its only operand is
  `v[i]` — so they would be specialized to `record_type_leaf_notify_` and pay the
  notification check on every execution, and an object-valued `v` would
  **un-suppress the `+` needlessly** (the `[` result observes the object and
  notifies anyway). Consuming leaves them parent-less, hence plain
  `record_type_`:

  ```
  v      → Type#0 (record_type_)                        ← no parent
  i      → Type#2 (record_type_)                        ← no parent
  v[i]   → Type#3 (record_type_leaf_notify_)  -> Type#4 ← the real operand
  add_   → Type#4 (record_type_inner_)  suppressed
  ```

  The second effect is the more valuable one. Measured 2026-07-27, the stray
  inner-node *recordings* that spurious notifications were causing disappeared
  entirely: `nbody_naive_inner`, `binarytrees_naive` and `spectralnorm` each went
  from exactly 1 recorded inner node to **0**, i.e. 100% inner-node elision.
  Recording counts otherwise barely move, since this changes *which* opcode a
  leaf uses, not *whether* it records.
- `registerLeafSlot(slot)` is the `isParent=false` shorthand, used for
  RecordAlways/RecordOnce leaves *and* for NoRecord elided uses (so the elided
  use still gets a parent, §2A.3).
- `bc/CodeContext.h` holds the per-Code compile state (including
  `recordTypeOnceBitmapSize`); `bc/CompilerCFG.{h,cpp}` supplies the CFG/scope
  structure the dominance approximation reads (§3).
- A post-pass, `TypeFeedback::setTypeFeedbackParents` (called from
  `Compiler::finalize`), then: sets each slot's `parentPlus1`, computes the
  `sourceSlots` set (a def is a notifying source only if its NoRecord dependent
  is a registered child — the over-marking fix of §2A.6), and **specializes each
  placeholder `record_type_`/`record_type_once_` opcode** into the right family
  member (`leaf_notify_[once_]` / `inner_` / `inner_notify_`) by `(isLeaf, isRoot,
  isSource)`.
- `buildNoRecordReverseMap` builds `noRecordSourceToDeps_` (source slot → its
  NoRecord dependent slots) from `typeDeps_`, so `markRelatedDirty` can walk a
  source's dependents at runtime.

---

## 3. Formal justification (No-Record / def-site subsumption)

The soundness argument for eliding a use-site record entirely and recovering its
feedback from another slot.

**Claim.** A use `U` of variable `v` need not be instrumented if there is an
already-instrumented site `S` (a definition of `v`, or an earlier recorded use)
such that the value observed at `U` is *necessarily* the value observed at `S`.
Then `U` emits no opcode, and `U`'s slot is annotated `typeDeps_[U] = S` so the
JIT can recover `U`'s feedback by copying `S`'s persisted state
(`reconstructFeedback`, §2A.3 — defined but not yet wired up, as PIR-side adaptation
has not started).

**The control-flow condition.** "Necessarily the same value" is established from
def-use analysis with dominance/postdominance. In `DefUseAnalysis::classifyUse`
the two No-Record returns are:

The two predicates, as actually defined in `DefUseAnalysis.h` (verified
2026-07-27):

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

### 3.0 Why post-dominance is required for *precision*, not only soundness

It is tempting to read the post-dominance condition as merely guarding against
"the source slot might not have been written." It does more than that: **without
it, copying the source's feedback actively makes the feedback worse.** This is the
argument that separates the two mechanisms, and it is worth stating in full
because it is what makes "record-once" necessary rather than a mere optimization.

Take a use that is *dominated* by its def but does **not** post-dominate it — the
def is unconditional, the use sits inside a branch:

```r
f <- function(s) {
    x <- getValueOfType(s)        # [S1]  the def — always executes
    if (s == "int") {
        x            + 1L         # [S2]  the use — executes only sometimes
    }
}
```

Compare recording at both sites against copying `S1`'s feedback into `S2`, over
four call histories:

| calls | record at both (`S1`, `S2` separately) | copy `S1` → `S2` |
|---|---|---|
| `f("string")` | `S1={string}`; `S2` never ran → no speculation | `S2={string}` → no speculation |
| `f("string"); f("int")` | `S1={string,int}`, **`S2={int}`** → specialise on `int` ✅ | `S2={string,int}` → **no speculation** ❌ |
| `f("int")` | `S1={int}`, `S2={int}` → specialise ✅ | `S2={int}` → specialise ✅ |
| `f("float")` | `S1={float}`; `S2` never ran → no speculation | `S2={float}` → **speculates on a type `S2` never sees** ❌ |

The middle row is the crux. `S2` only ever observes `int`, because it only runs
when `s == "int"`. Its own slot is therefore *more precise* than the def's, which
accumulates every type `x` ever held. Copying the def's slot **over-approximates**
— it hands the JIT a two-type set where a one-type set was available, losing the
specialisation. The last row is worse still: the copy invents feedback for a site
that never executed, inviting speculation that can only deoptimise.

So the general picture:

- **Post-dominance holds** ⇒ def and use execute together on the same value ⇒
  their slots are *identical* ⇒ eliding the use and copying is **lossless**. This
  is the No-Record case (§2A.3).
- **Post-dominance fails** ⇒ the use's observations are a **subset** of the def's
  ⇒ copying is a precision loss, and the use must observe for itself. This is
  exactly why such sites fall through to RecordAlways or RecordOnce (§2A.1.1
  rules 3–5) instead of being elided.

**And why not copy at run time instead?** Because the def's slot is *already
merged*: each execution's observation is folded into the same `ObservedValues` the
moment it happens, so by the time the use runs there is no per-execution value
left to copy — only the accumulated union. A runtime copy would therefore
reproduce the same over-approximation as the compile-time one. Making it work
would require keeping the def's per-execution information *separate* from its
accumulated slot, which is strictly more machinery than simply re-observing at the
use. Hence: straight-line non-post-dominating uses record unconditionally, and
uses inside loops use the per-site once-flag (§2A.2) — one observation per
activation, of the use's *own* value.

**Caveat — the approximation ignores errors.** `postDominates` accounts for
`return`/`break`/`next` (via the closed-return and loop-exit counters) but **not**
for R conditions: any call between def and use may `stop()` and unwind, so a path
exists that reaches the def and never reaches the use. Strictly, that path breaks
post-dominance. The effect is confined to precision rather than soundness — an
erroring run contributes an observation to the def that the use never saw, i.e.
the same over-approximation as above — but it means the post-dominance test is
"quasi post-dominance", and worth remembering when reasoning about the guarantee.

### 3.1 The guarded-fast-path problem (`v[[i]]`): a real dominance failure

The dominance condition is broken by a construct that looks innocuous at source
level. **`v[[i]]` (and `v[i]`) does not compile to straight-line code** — it
compiles to a *guarded two-branch* form: a fast path taken when `v` is not an
object, in which the index is evaluated **eagerly**, and a slow path in which the
index is wrapped in a **promise** for the generic dispatch. Disassembly of
`function(x, i) x[i]` (verified 2026-07-27):

```
 0  ldvar_cached_ x{0}      ; the target
 9  [ … ] Type#0
14  dup_
15  is_ NonObject           ; the guard
20  [ _ ] Test#0
25  brfalse_ 1
30  br_ 2
1:  35  mk_promise_ 0       ; SLOW path — index becomes a promise
    40  br_ 3
2:  45  ldvar_cached_ i{1}  ; FAST path — index loaded eagerly …
    54  [ … ] Type#2        ; … and recorded HERE only
3:  59  extract1_1_
```

The consequence for subsumption: the record of `i` at offset 54 executes **only
on the fast path**. So in

```r
v[[i]]; i          # can the second `i` copy from the first?
```

the answer is **no** — the first `i`'s slot is not guaranteed to have been
populated when the second use runs, because control may have taken the promise
branch. It is **dominance** that fails, not post-dominance: the slow path reaches
the second use without ever executing the first use's record, so the first use
does not lie on every path to the second. (Post-dominance does hold here — every
path from the fast-path record continues through `extract1_1_` to the second
use.) Treating the first use as a source would mean copying from a slot that was
never written.

**This was overlooked initially and the correction is expensive.** Because
guarded fast paths appear throughout real bytecode, tightening this rule removes
the second-use optimization at a great many sites — the author reports the
regression "shows up almost everywhere." It is the single largest known cost of
making the analysis sound.

**Open direction:** a **run-time patch** for this case, to recover the lost
subsumptions without giving up soundness (rather than a purely static fix). In
progress; see §6.

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

Classifies the loaded SEXP `s` and raises the slot's state if the new
classification is higher on the lattice (verified 2026-07-27):

```cpp
auto recordFbAtSlot = [&](uint32_t slotIdx, SEXP s) __attribute__((always_inline)) {
    ObservedValues::StateBeforeLastForce state = /* unknown */;
    if      (TYPEOF(s) != PROMSXP)                      state = value;
    else if (PRVALUE(s) != R_UnboundValue)              state = evaluatedPromise;
    else if (CAR(PREXPR(s)) == symbol::lazyLoadDBfetch) state = value;
    else                                                state = promise;

    ObservedValues& fb__ = typeFeedback->types(slotIdx);
    if (fb__.stateBeforeLastForce < state)
        fb__.stateBeforeLastForce = state;
};
```

Classification of `s`: not a `PROMSXP` → `value`; a promise with `PRVALUE` set →
`evaluatedPromise`; a `lazyLoadDBfetch` stub → `value` (a lazy-load stub that
materialises to a value); otherwise (unforced promise) → `promise`.
`always_inline`: this is the shared core, inlined into the `noinline` wrappers
below, which are the outlined call targets in the dispatch loop.

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

`FBValue` and `Infer` emit the *same* opcode (`ldvar_cached_noRecordFB_`); the
split exists purely so a consumer can tell "statically a value" from "derivable
from the source slot". **§4.5** proves each strategy sound and **§4.6** gives the
reconstruction rule each one implies.

### 4.3 How FB gets recorded: cached vs. non-cached loads

**Cached loads bake the strategy into the opcode** (chosen by the compiler,
patched in `emitRecordTypeForVar`):

- `ldvar_cached_` → `recordForceBehaviorNoCheck` (unconditional).
- `ldvar_cached_noRecordFB_` → skips FB entirely (`FBValue`/`Infer`).
- `ldvar_cached_fbRecordOnce_` → `recordForceBehaviorRecordOnceNoCheck` (fired-gated).

`recordForceBehaviorNoCheck` reads the slot index directly from `pc+1` with **no
opcode peek and no unpacking**. Its soundness rests on an invariant proven this
session: base `ldvar_cached_` is emitted *only* for `fbKind == Always`, which
`classifyUse` returns *only* for `RecordAlways` uses, which always emit a plain
(non-once) leaf record right after the load. The two cases that would break a
blind `pc+1` read — a No-Record elision (no opcode follows) and a packed `_once_`
immediate — are exactly the ones the compiler patches to `noRecordFB_` /
`fbRecordOnce_`. A `SLOWASSERT(*pc == record_type_ || *pc == record_type_leaf_notify_)`
pins this in debug builds.

**Non-cached loads use a generic runtime dispatcher.** `ldvar_`,
`ldvar_for_update_`, `ldvar_for_update_cache_`, `ldddvar_` call the generic
`recordForceBehavior`, which peeks the following opcode and dispatches.
(`ldvar_super_` does **not**: it calls `recordForceBehaviorNoCheck`, which is
safe because a super-assign target is always followed by
`recordTypeUntracked()` — a plain record with a raw immediate.)

```cpp
switch (*pc) {
case record_type_:  case record_type_leaf_notify_:            idx = raw;          // always
    REC_STAT(fbGenericRec++);                                                     break;
case record_type_once_: case record_type_leaf_notify_once_:                        // once
    RECORD_TYPE_ONCE_GATE(fired, raw, { REC_STAT(fbGenericSkip++); return; });
    idx = RECORD_TYPE_ONCE_SLOT_IDX(raw);
    REC_STAT(fbGenericOnceRec++);                                                 break;
default: REC_STAT(fbGenericBail++); /* no record follows this load */  return;
}
recordFbAtSlot(idx, s);
```

Note the dispatcher recognizes **all four** value-type leaf opcodes, including
the `leaf_notify_` variants. Recognizing only the plain `record_type_`/`_once_`
(the older behavior) silently dropped FB for any non-cached, tree-participating
leaf — a real bug fixed this session.

**The two arms are counted apart** (`fbGenericRec` vs `fbGenericOnceRec`). They
are different concepts and pair with different cached rows: the plain arm has no
gate and records every time, making it the generic counterpart of `fbAlwaysRec`,
while the once arm reaches the *same* `RECORD_TYPE_ONCE_GATE` as
`recordForceBehaviorRecordOnceNoCheck` and so pairs with
`fbRecordOnceRec`/`fbRecordOnceSkip`. A single counter across both arms cannot
tell a first firing from an unconditional record, which is what the composition
figure in the paper needs in order to group by gate outcome rather than by
dispatch path.

Expect `fbGenericOnceRec` and `fbGenericSkip` to be **small**. A once-record
after a non-patched load needs `UseKind::RecordOnce`, and `ldvar_for_update_*`
can never get it: the complex assignment *is* the assignment to its own target,
so the target is always in the containing loop's `expected` map (note
`collectAssignedVars` walks `` `<-`(`[`(x,i), v) `` down to `x`), which fails
`!assignedInInnermostLoop` on the enclosing-loop path and `!assignedInEnclosingLoop`
on the stable path; and outside a loop both paths fail `loopDepth_ > 0`. That
leaves only non-cached `ldvar_` — names passed to `rm()` (explicitly
`BindingCacheDisabled` by `scanNames`) and functions exceeding
`MAX_CACHE_SIZE` = 255 distinct names.

The `default` bail ("`fbgeneric_bail`") fires when no value-type record follows
the load. Empirically the callers that bail are: the discarded subassign
protective read (`ldvarForUpdate…; setShared; pop` — value thrown away) and
`ldddvar_` (dd-vars `..1`/`...` are never type-profiled, so no record is ever
emitted after them). Both are correct-to-skip; the bail is not a lost recording.

### 4.4 The `fbgeneric_bail` cases (known, not optimized away on this branch)

The generic dispatcher's `default:` arm ("no value-type record follows this
load") is reached by two real patterns, both correct-to-skip:

- **The discarded subassign protective read.** `x[i] <- v` (when
  `maybeChanges(target, rhs)`) compiles the target's *first* read as
  `ldvarForUpdate[Cached]; setShared; pop` — a read that exists only for its
  NAMED/shared side effect and whose value is immediately popped. No record
  follows, so the FB dispatcher peeks, sees `set_shared_`, and bails. This is the
  dominant source: measured **15,003 bails** in `nbody_naive_inner` at 200 inner
  iterations (one per subassign execution).
- **`ldddvar_`** (`..1`, `...`): dd-vars are never type-profiled — `compileGetvar`
  takes the `DDVAL` branch and emits no record at all — so this load always bails.

Both are wasted dispatches rather than lost feedback: in each case there is
genuinely no slot to attribute force-behavior to, so bailing is correct. They are
a (small) missed opportunity to avoid the call entirely, not a soundness or
precision problem.

### 4.5 Soundness of the four live strategies

**What "sound" means on this axis.** The lattice has `promise` at the *top*, and
the baseline accumulates a **max** over every execution of a site. A consumer
reads the slot to decide how aggressively to speculate that forcing is cheap; the
dangerous direction is therefore **under**-approximation (reporting a value lower
than the truth — claiming "never saw an unforced promise" when one occurs).
Over-approximation is merely pessimistic. So the obligation per strategy is
`reconstructed ≥ truth`, and *losslessness* is `reconstructed == truth`.

Write `c(e)` for the per-execution classification of §4.1 and
`FB_base(s) = max_e c(e)` over all executions at site `s`.

**`Always` — exact.** Identical code path to the baseline, on every execution.

**`FBValue` — exact, statically.** All three admission routes in `classifyUse`
(post-`stvar` reach via `hasLocalStvarReach`, RecordOnce on a local, for-loop
iter var) mean the binding was last written by `stvar_`, which stores an
*evaluated* value off the stack. So `c(e) == value` for every execution and the
reconstruction `value` is exact, not conservative.

`ldvar_noforce_` cannot undermine this. It is the one load that does not force,
but it is emitted only at `Compiler.cpp` in the `identicalNoforce` call-target
guard sequence — never through `compileGetvar` — so it never reaches
`emitRecordTypeForVar`, never calls `trackUseDef`, and therefore can never be the
source of a useDef subsumption.

**`RecordOnce` — exact, given a monotonicity lemma.** The strategy records only
the first execution of the site per invocation (`fired` is `alloca`'d and
zeroed per `evalRirCode` entry, §2A.2.1). This equals the baseline max iff, at a
fixed site within one invocation, the sequence `c(e)` is **non-increasing**.

That holds because forcing moves *down* the lattice, never up. The only
transitions available without rebinding are `promise(3) → evaluatedPromise(2)`
(the binding still holds the `PROMSXP`; R sets `PRVALUE` rather than replacing
it), and a local `stvar_` rebind to `value(1)`. So the first observation
dominates the invocation, and

```
max over all executions  ==  max over first-of-each-invocation
```

because the per-invocation maxima are exactly the first observations. Note the
gate only *tests* the bit — `RECORD_TYPE_ONCE_SET` is issued by the following
`record_type_once_`, so the FB record and the type record fire on the same
first execution, and FB is taken *before* the force (`record_fb_action` precedes
`evaluatePromise` in `LDVAR_CACHED_BODY`), which is what makes it a
"state *before* last force".

**`Infer` — exact, via a clamp (not a copy).** This is the case whose
reconstruction rule is least obvious, and the natural guess (copy the source
slot) is wrong.

No-Record admission requires a prior use that **dominates and post-dominates**
this one (§3), and every load that can be a useDef source forces
(`if (TYPEOF(res) == PROMSXP) res = evaluatePromise(res)`). Since R does not
replace the binding when a promise is forced, the dependent read observes,
per execution:

| `c(source_e)` | ⇒ `c(dep_e)` |
|---|---|
| `promise` | `evaluatedPromise` |
| `evaluatedPromise` | `evaluatedPromise` |
| `value` | `value` |

i.e. exactly `c(dep_e) = min(c(source_e), evaluatedPromise)`. Because that clamp
is **monotone**, it commutes with the max the lattice accumulates:

```
min(max_e c(source_e), evaluatedPromise) == max_e min(c(source_e), evaluatedPromise)
```

so the clamp can be applied once to the *stored* source value rather than
per execution. `Infer` is therefore losslessly reconstructible as
`min(FB(source), evaluatedPromise)` — and this stays exact even when the source's
recorded value mixes invocations (eager argument in one call, promise in
another).

**Two identified holes.**

- **The `lazyLoadDBfetch` inversion (theoretical).** The classifier deliberately
  reports `value(1)` for an *unforced* lazy-load stub, but once forced the next
  read takes the `PRVALUE != R_UnboundValue` arm and reports
  `evaluatedPromise(2)`. That is an *increase*, which breaks the
  `RecordOnce` monotonicity lemma above and would under-approximate. No reachable
  instance was found: lazy-load stubs live in package environments, and
  `outerControlled_` is populated only from an enclosing *compiled closure's*
  captures (`Compiler.cpp`, the closure-compile prologue), so such a name is
  never `isFormal` nor `isOuterControlled` and never reaches a `RecordOnce`
  classification. Recorded as theoretical, not live.
- **Non-`stvar` rebinding (reachable, but not FB-specific).** `delayedAssign`,
  `assign(..., envir=)` and `makeActiveBinding` can turn a binding into a promise
  without any `stvar_` the DFA can see. In a loop over an eagerly-passed formal
  this produces the forbidden increase (`value` on iteration 1, `promise`
  afterwards), so `RecordOnce` would capture the lower value. This is the same
  "no local `stvar` ⇒ binding stable" premise the **type** dimension already
  relies on, and the type recording has identical exposure — it is a pre-existing,
  dimension-independent hole rather than something the FB strategies introduce.

**Measured distribution** (mandelbrot 500, areWeFast — see §8) confirms which
paths carry weight. The FB table decomposes exactly against the leaf table:

```
fbNoRecordSkip 148,512,896 − noRecordSkip 115,732,690 = 32,780,206  RecordOnce uses given FBValue
                                       + fb record_once     940,038
                                       = 33,720,244  == RecordOnce leaf total ✓
```

So all No-Record loads and ~97% of RecordOnce loads pay **zero** FB cost, the
gated `fbRecordOnce_` path is a rounding error, and essentially the whole residue
is `Always`. 59.0% of baseline FB work eliminated.

### 4.6 Reconstruction: what a consumer must compute

`TypeFeedback::reconstructFeedback()` implements the reconstruction for **both**
dimensions. It still has **no callers** — wiring it into the JIT is the consumer
side and remains out of scope — but it is now a complete, tested recovery step
rather than a placeholder.

Three things are persisted per slot, and all three are needed:
`stateBeforeLastForce` (the recorded observation), `forceBehaviorKinds_[i]` (the
compile-time decision, so "nothing recorded because statically a value" is
distinguishable from "nothing recorded yet"), and `typeDeps_[i]` (the No-Record
source, shared with the type dimension).

```
Always     → the recorded value
RecordOnce → the recorded value
FBValue    → value
Infer      → min(FB(typeDeps_[i]), evaluatedPromise)     // resolve source first
```

`Infer` needs its source resolved before itself. The forward iteration order
already guarantees this — a dep always references a lower-numbered slot — and
since the No-Record graph is kept **flat** (§2A.4, sources rather than chains) the
resolution is a single hop, never a walk.

Slots that are not loads (inner nodes, opaque call/`[`/`:` results) have no FB in
*either* mode: FB is recorded in the load handler and keyed to the following
record's slot, so a slot with no load in front of it is untouched. They keep
`stateBeforeLastForce == unknown` and the default kind `Always`, which is
consistent with the baseline and needs no special case.

**Why the type copy is field-by-field.** `reconstructFeedback` used to be a
whole-struct assignment:

```cpp
types_[i] = types_[typeDeps_[i]];   // copies ALL of ObservedValues
```

That is wrong in three separate ways under the current 8-byte layout (§2C.1),
which is why the type half now goes through
`ObservedValues::copyTypeObservationsFrom` — copying `numTypes`, `seen[3]`,
`notScalar`, `attribs`, `object` and `notFastVecelt`, and nothing else:

- **It copied `stateBeforeLastForce`.** For `FBValue` that is strictly worse than
  doing nothing: in `b <- a; use(b)` the dependent inherits `a`'s `promise` when
  the truth is statically `value`. For `Infer` it yields `promise` where §4.5
  proves `evaluatedPromise`. Both are pessimistic rather than unsound, but the
  correct answers are cheaply derivable and the copy discarded them.
- **It clobbered `parentPlus1`.** That field did not exist when `reconstructFeedback`
  was written. Overwriting a dependent's parent index with the *source's* parent
  corrupts the expression-tree notification graph (§2C.2) — harmless if
  propagation runs strictly once and recording never resumes, but RIR re-enters
  the interpreter after deopt and keeps recording.
- **Same for `lastSig` and `dirty`**, which imported the source's per-execution
  signature state into a slot that never records for itself.

**Structure of the pass.** One forward loop over all slots, doing the type copy
and then the FB rule for each. Both rules read only lower-numbered slots — a
dependency is registered immediately after its source was allocated
(`registerNoRecordDep` is the sole `setTypeDep` caller), so `src < i` always, and
an `assert` pins it. A source is therefore fully resolved before any dependent
reads it, which is what makes the `Infer` clamp read a *final* source value and
makes a chain of copies resolve in this single pass even though the graph is kept
flat (§2A.4) and chains should not arise. Every rule is idempotent, so calling
the pass more than once — e.g. re-optimizing after a deopt has let the
interpreter record more — is safe and simply refreshes the derived slots.

**Verified behaviour** (via a temporary env-gated call from `Function::disassemble`,
removed again). For

```r
f <- function(a) { x <- a + 1; y <- a + 2; z <- x + 1; y + z }
```

slot 0 (the `a` read, `Always`) records `double (s) | promise`; before the pass
the four dependents print `<?>`, and after it:

```
SLOT#0 double (s) | promise           -> Type#1     (recorded, untouched)
SLOT#2 double (s) | evaluatedPromise  -> Type#3     (Infer:   clamped from #0)
SLOT#4 double (s) | value             -> Type#5     (FBValue)
SLOT#6 double (s) | value             -> Type#8     (FBValue)
SLOT#7 double (s) | value             -> Type#8     (FBValue)
```

Slot 2 is the case that matters: a whole-struct copy would have given it
`promise` **and** re-pointed its parent from `Type#3` to `Type#1` (slot 0's
parent). It shows the clamped `evaluatedPromise` and keeps its own parent. A
loop-and-formal case (`for (i in 1:n) acc <- acc + v + v`) reproduces the same
pattern from the other direction: `v`'s `RecordOnce` slot records `promise`, and
the second `v` — an `Infer` dependent — reconstructs `evaluatedPromise`, which is
the truth, because the first read forced the promise on its way through. Running
the pass twice produces byte-identical output.

---

## 5. Alternatives considered and rejected

### 5.1 Where to keep the once-flags — the full design space

This is the most-explored corner of the design. The options, in the order they
were considered, with why each was kept or dropped:

- **Patch the bytecode itself (no flags at all).** The most direct encoding of
  "record once" would be to rewrite the `record_type_once_` opcode into a no-op
  after it fires, so the check disappears entirely. **Rejected:** bytecode is
  shared across all invocations of a function, so self-modification would force a
  **per-invocation copy of the whole bytecode** — far more expensive than the
  check it removes. *This rejection is the reason the separate `fired` flags exist
  at all.*
- **`alloca`'d array, sized per Code object** — **what this branch does**
  (§2A.2). Sized to exactly `recordTypeOnceCount`, no fixed cap, zeroed by
  `memset`. Verdict: works, but **does not scale** — the frame cost grows with the
  number of once-slots, and it is a dynamic stack allocation on every activation.
  It can be byte-packed (as now) or bit-packed.
- **Fixed-size `bool[512]` frame array.** A static frame slot: predictable, no
  `alloca` call, zeroing only the used prefix. Costs up to 512 bytes of frame even
  when few slots are used. Measured *modestly* faster on spectralnorm / mandelbrot
  — **but inside the codegen-artifact noise floor** (§8), so not a confident
  result. **Unresolved**; revisit only with a measurement method that clears that
  floor.
- **One global vector.** A single process-wide vector plus an RAII guard to
  save/restore per activation. Drawbacks: an **offset must be computed on every
  access**, and it needs the same packing decision as the frame array. If
  bit-packed, each access additionally needs word-index arithmetic plus shift/mask
  to reach the exact bit — meaningful work on the hot gate.
- **Bit-packing (1 bit/flag), in any of the above.** Rejected in favour of one
  `bool` per flag: direct byte indexing avoids the shift/mask entirely, and the
  memory saved is irrelevant at these sizes.
- **`std::bitset` as the storage.** Rejected: it cannot cheaply zero only the
  *used* prefix, and the bounds-checked `.test()`/`.set()` add overhead. A
  `reinterpret_cast<std::bitset<512>&>` *view* over the `bool[512]` was briefly
  used purely for readability on TEST/SET/CLEAR — with unchecked `operator[]`,
  never `.test()`/`.set()` — and ultimately the plain array plus macros were kept.

**Preferred endpoint for the non-promise case (not yet implemented here):** keep a
**single `uint64_t`**, zeroed at function entry. 64 bits appears to be ample for
the once-slots that actually matter — the parameters read inside a loop — and it
collapses the whole storage question to one register-sized word with no
allocation, no offset computation, and a trivial zeroing step. This supersedes the
`alloca`-vs-fixed-array question above rather than resolving it.
### 5.2 Other alternatives considered

- **`record_type_dep_` / `record_type_once_dep_` as separate opcodes.** Merged
  this session into `record_type_leaf_notify_[once_]`. They had become
  byte-identical handlers once the notifier was made generic over
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
  (both inner opcodes route through `recordInner` + `markRelatedDirty`).
- **suppression check inside `record()`** (the plain-leaf recorder).
  Removed: leaves are never suppressed, so the check
  was provably dead for its callers and cost a load+branch on the hottest record
  opcode.
- **Env-bitmap force-behavior (`ForceBehaviorKind::EnvBit` +
  `record_type_once_promise_` + `ldvar_cached_envRecordFB_`).** Not rejected on
  merit — it is the *promise* half of the once-flag design, currently disabled and
  awaiting an `ENVSXP` change. See **§2A.2.3** for the lifetime argument that makes
  the environment the only viable storage, and for what remains in the tree.
  Promise-context free variables record FB via the normal path meanwhile.
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
- **Cross-invocation record-once for `immutable` captures.** Today record-once is
  strictly *per activation* (§2A.2.1) and only fires inside loops (§2A.1.1). But a
  capture in the `immutable` set is by construction stable for the whole lifetime
  of the inner closure — so its type could be observed **once across all
  executions**, not once per call, and even outside a loop. That needs a flag with
  a lifetime longer than the frame, i.e. the same environment-resident storage the
  promise scheme needs (§2A.2.3) — which is why the two are blocked on the same
  `ENVSXP` change. Currently `immutable` is computed and propagated but never
  consulted; this is the intended payoff. Start with parameters (the narrow, clearly
  safe case) and widen to any controlled-immutable variable, then to inner
  functions.
- **Run-time patch for the guarded-fast-path case (§3.1) — in progress.** The
  `v[[i]]`/`v[i]` two-branch compilation means an index load is recorded on only
  one path, which statically kills second-use subsumption and costs performance
  broadly. The intended remedy is a *run-time* mechanism that recovers those
  subsumptions while staying sound, rather than a static relaxation. This is the
  largest open performance item.
- **Promises are deliberately left unoptimized** — a scoping decision, plus the
  fact that the storage it needs (a `uint64` on `ENVSXP`) is not present in the
  current `custom-r` submodule. Full rationale, the designed scheme, and the exact
  vestiges left in the tree are in **§2A.2.3**. Re-enabling requires an R submodule
  change, so it is a heavier lift than a compiler-only feature. Revisit once the
  guarded-fast-path work (§3.1) settles.
- **Are the *auxiliary flags* inferable for all 14 elided operators?**
  Losslessness for inner-node elision (§1, mechanism 3) requires the result to be
  exactly reconstructible from the operands over the **whole** `ObservedValues` —
  not just the type set, but `notScalar`, `attribs`, `notFastVecelt` too. This
  obligation already forced `[` out of the inner-node set (§2B.3). The arithmetic
  ops look sound (result length = `max(operand lengths)` ⇒ `notScalar`
  derivable), and the comparisons yield a constant `LGLSXP`, but the remaining
  fields have **not** been checked operator-by-operator — in particular `attribs`
  and `notFastVecelt` propagation through arithmetic (attribute preservation in R
  arithmetic has its own rules, e.g. names/dim inheritance from the longer
  operand). This audit should be completed before claiming end-to-end
  losslessness, and it is the most likely place to find another `[`-style case.
- **Reconstruction is implemented but unwired.** `TypeFeedback::reconstructFeedback()`
  now recovers both dimensions (§4.6) and is verified, but it has **no callers**,
  so No-Record dependent slots are still all-zero at runtime. Calling it is a
  consumer-side decision: it belongs wherever PIR reads the feedback, and PIR has
  not been adapted to this branch at all (§7 scope boundary). Two things to know
  before wiring it: the FB dimension is *derived*, not copied — `Infer` needs the
  clamp `min(FB(source), evaluatedPromise)` and a plain copy of the source is
  wrong; and the type dimension only propagates the *observations*, so the
  auxiliary-flag question below (are the flags exactly inferable for the elided
  inner-node operators?) is untouched by this and remains the real gap.
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
- **Whether the generic `recordForceBehavior` should honor `fbKind`.** Non-cached
  loads currently ignore the compile-time FB strategy (they always attempt FB via
  the peek). This is inherited from the pre-recordless single-dispatcher design;
  harmless but asymmetric with the cached path. Since the split into
  `fbGenericRec` / `fbGenericOnceRec` the accounting at least shows how much
  traffic each arm carries, so the question can be settled on measurement.
- **`ldvar_for_update_cache_` is never FB-specialized, though it is cached.**
  `compileGetvar` records a `ldvarCachedPos` for the patch, but the complex-
  assignment path calls the three-argument `emitRecordTypeForVar`, whose
  `ldvarCachedPos` defaults to `kNoLdvarCached` — so the patch block is skipped
  even when a cache slot exists. This cannot cost a RecordOnce (see above: the
  target can never be classified `RecordOnce`), but it does cost the `NoRecord`
  cases: an `FBValue`/`Infer` target that would have become
  `ldvar_cached_noRecordFB_` instead pays the `noinline` generic call plus the
  opcode peek, only to hit `default:` and bail. Fixing it needs either a new
  `ldvar_for_update_cache_fbRecordOnce_`/`_noRecordFB_` pair or threading the
  position through the overload.
- **`recordForceBehaviorNoCheck` assumes profiling is on.** It blind-reads `pc+1`
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

## 7. Current implementation status (snapshot, 2026-09-02)

Working branch: `recordLessNew2-expressionsNewSchema`, at commit `b8076079`.
A sibling clone at `~/rsh-recordLess-baseline` (branch
`recordLess-baseline-outline`) is used for A/B binary builds.

**Scope boundary.** This work is **interpreter/compiler-side only**. PIR has not
been adapted to the new feedback-recording strategy — that is not started, and is
out of scope for this document. Everything below describes the producer side:
what the compiler emits and what the interpreter does at runtime.

**In the branch (verified present in the source on 2026-08-11).** Everything this
document describes was checked against the tree; the markers below were confirmed
by direct inspection, not recalled:

- The three recording classes (RecordAlways/RecordOnce/NoRecord) and the full
  `classifyUse` decision order (§2A.1.1); `typeDeps_` produced and stored.
- Record-once with the per-invocation `fired` array — **`alloca` form**.
- Once-bit clearing: `clear_record_type_once_bit_`,
  `clear_record_type_once_bits_range_`, `LoopScopeGuards.h`, and the
  `CodeStream::patchImmediate` / `patchOpcode` primitives.
- The opcode family `record_type_leaf_notify_[once_]` /
  `record_type_inner_` / `record_type_inner_notify_`; the old
  `record_type_dep_` / `record_type_once_dep_` are **gone**.
- `markRelatedDirty(slot, idx)`;
  `recordInner()`; `record()` is a plain `doRecord` with **no**
  suppression check; `recordSimple` and `notifyParent` are **gone**.
- `RECORDLESS_EXPTREE_ENABLED` **removed** — the expression-tree scheme is
  unconditionally compiled in (only `RECORDLESS_EXPTREE_DEBUG` remains).
- FB: `ldvar_cached_` / `ldvar_cached_noRecordFB_` /
  `ldvar_cached_fbRecordOnce_`; helpers named **`recordForceBehaviorNoCheck`**
  and **`recordForceBehaviorRecordOnceNoCheck`** (both carry a `SLOWASSERT` on
  the following opcode); `ldvar_super_` calls `recordForceBehaviorNoCheck`; the
  generic `recordForceBehavior` recognizes the `leaf_notify_` family. The
  soundness argument for each strategy and the reconstruction rule each implies
  are written up in §4.5 / §4.6; the reconstruction itself is **not**
  implemented (consumer side, out of scope).
- `RIR_RECORD_STATS` instrumentation incl. the force-behavior table
  (`fbgeneric` / `fbgeneric_once` / `always` / `record_once` / `no_record` +
  `fbgeneric_bail` reported separately). Compile-time toggle, **off** by
  default. *(As of this revision the toggle is `#define`d unconditionally in
  `record_stats.h` — i.e. currently **on** in the tree. Turn it back off before
  any timing run.)*

**Changes landed since the previous snapshot (2026-07-27 → 2026-08-11).**

1. **`[` (`Bracket`) demoted** from an elidable inner node to a tracked leaf —
   §2B.3. `x[i]`'s result is a plain always-recording leaf; `x + y` still yields
   a suppressed inner node.
2. **`:` (`Colon`) demoted** likewise, to `recordTypeOpaqueResult()`. It stays in
   the same `compileSpecialCall` binary block (so the bytecode shape is
   unchanged) but is routed away from `recordTypeTracked(true)` at the record
   site. `seq_colon` coerces both operands to `double` and derives the result
   type *and* length from the **values** — `1:1`, `1:5` and `1.5:3.5` all have
   identical operand feedback and differ in both `seen` and `notScalar`. This is
   also the one operator that breaks the otherwise-universal
   `notScalar = disjunction` rule, which comes from `R_binary`'s
   `n = max(n1, n2)`; `:` never goes through `R_binary`.
3. **`recordTypeTracked` degrades to untracked in default formal arguments**
   (§2C.3), so default-arg records both always record and are attributed to the
   `untracked` row — matching the baseline.
4. **New notification mechanism** — per-execution signature + `dirty` bit,
   replacing the object gate and one-shot latch (§2B.2, §2B.2.1). This is what
   makes inner-node feedback *lossless*: the node records its own observations
   instead of relying on a reconstruction that was never implemented and is
   provably impossible in general.
5. **`ObservedValues` shrunk 13 → 8 bytes** (§2C.1): `isLeaf`,
   `shouldNotRecord` and `hasPropagatedNotification` removed, `parent` pointer
   replaced by a biased 2-byte index, `dirty` and `lastSig` added.
6. **`slotsStack` and `valueRecord` moved from `CompilerContext` to
   `CodeContext`** (§2C.5), so a promise body can no longer register its leaves
   into whatever expression level its caller left open, nor leave a def-slot
   stamp whose position and scope id belong to a different counter space. Both
   are behaviour-neutral today — promises are excluded from the optimization,
   and the stamp leak was blocked only by the contingent fact that a `call_`
   always follows a `mk_promise_` — but they remove the trap by construction.
7. **Honest def-slot attribution** (§2A.4) — the positional
   `typeSlotCount() - 1` heuristic replaced by a stamp that must be proven to
   describe the stored value on every path. Fixes four measured
   under-approximations (`-a`, `!f()`, `is.null(f())`, `if/else`).
8. **Flat NoRecord dep graph** (§2A.4, §2C.2) — a NoRecord read stamps the
   ultimate source, so copy chains resolve in one hop. This also closed a
   pre-existing hole: with chains, a copy under a suppressed inner node was two
   hops from the source and `markRelatedDirty`'s one-level walk never reached
   it, leaving that node recording *nothing* where baseline records real types.
9. **Stats reporting brought in line** with all of the above: the inner-node
   "skipped" column now documents the `dirty` gate rather than the removed
   `shouldNotRecord` latch (and that it is re-armable); `[` and `:` are listed
   among the opaque-result leaves they became; and a new counter,
   `sigUnchangedNoOp`, reports how many records ran but *updated nothing*
   because the signature was unchanged. That last one exists because the
   signature early-out is otherwise invisible in the tables — the opcode still
   executes, so the work it avoids was being counted as "recorded".
10. **Force-behavior soundness written up** (§4.5). Each of the four live
    strategies now has an explicit soundness argument rather than an assertion in
    a comment: `Infer` turns out to be losslessly reconstructible via a clamp,
    and the `RecordOnce` "first observation is the maximum" claim reduces to the
    fact that forcing moves *down* the lattice. Two holes are recorded — a
    theoretical `lazyLoadDBfetch` inversion (no reachable instance found) and
    non-`stvar` rebinding via `delayedAssign`, which the type dimension already
    shares.
11. **`reconstructFeedback` now reconstructs both dimensions** (§4.6). It was a
    whole-struct copy that carried `stateBeforeLastForce` and clobbered
    `parentPlus1` / `lastSig` / `dirty` — fields added after it was written. Now
    the type half goes through the new
    `ObservedValues::copyTypeObservationsFrom` (observations only) and the FB
    half is *derived* per slot from `forceBehaviorKinds_`. Still deliberately
    **callerless**: it is the consumer-side step and PIR has not been adapted.
    Verified against a temporary env-gated call (since removed) on a
    formal/copy/loop mix, including that the `Infer` clamp yields
    `evaluatedPromise` where a copy would have said `promise`, that parents
    survive, and that repeated calls are idempotent.

**Changes landed since (2026-08-11 → 2026-09-02).**

12. **Two `<<-`-escape soundness holes fixed** (§2A.1.3). A `<<-`-escaped
    **formal** could reach NoRecord subsumption by two routes that both bypassed
    the `\ S_f` filter in `functionLocalOrParam_`, letting a use be subsumed
    against an earlier one across a `<<-` that had retyped the binding —
    narrowing the recorded feedback:
    - `computeCapturesForInner` inserted formals into the `controlled` capture
      set unconditionally (body-locals *were* filtered), so a *sibling* closure
      could subsume via `isOuterControlled`. Fixed by applying the same guard to
      the formals loop.
    - `classifyUse`'s eligibility predicate had `isFormal` as a **bare
      disjunct**, which readmitted exactly the names `isLocalOrParam` excludes.
      This needs no sibling closure at all — both uses can sit in the defining
      function's own body. Fixed by making `isLocalOrParam` a mandatory conjunct.
      The divergence was hidden by a **dead duplicate**: a helper
      `isOptimizable` had no callers while `classifyUse` carried its own
      differently-worded copy, so patching the helper would have changed
      nothing. The helper has been deleted; `classifyUse`'s expression is now
      the only copy.

    Both confirmed by disassembly (`ldvar_cached_noRecordFB_` +
    `NoRecord Type#1 (dep: #0)` where the runtime value had become `character`)
    and re-checked after the fix. Each guard instrumented separately across the
    `<<-`-containing benchmarks: 0 firings, so no optimization is lost in
    practice. **Neither is covered by a regression test** — the existing test
    shape exercises only the `functionLocalOrParam_` route, which was never
    broken.
13. **Documentation only — §2A.4.1 added**, writing up why rules 1 and 2 take
    different dominating-def queries (`findDominatingDef` vs the
    back-edge-vetoed `findReachingDef`). Includes the algebraic simplification of
    rule 1's gate, what a dominating def actually establishes for a body-local
    (that the binding exists in our frame at this point — *not* locality, which
    is a pre-scan set), the fact that NoRecord reads are absent from `useDefs_`,
    and an A/B disassembly showing the third disjunct is what makes the ordinary
    accumulator loop elide. §2A.1.1's *optimizable* formula was also stale
    (pre-fix, bare `isFormal`) and the `trackDef`/`trackUseDef` call-site line
    numbers had drifted; both corrected.

All build clean and every checked benchmark produces correct results.

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
  dispatcher to `recordForceBehaviorNoCheck` — literally one `callq` target)
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
- Recommended methodology (**now adopted** for the 2026-08-11 work): compare
  **retired instruction counts** (`perf stat -e instructions`, 3 reps) rather
  than wall clock — layout/frequency-invariant, so it measures work done rather
  than layout luck. Observed run-to-run spread with this method is ~0.004%, so
  sub-1% deltas are resolvable. Every optimization in items 4-8 of §7 was
  accepted or rejected on this basis, always against a rebuilt A/B pair of the
  *same tree* (one edit, recompile, measure, restore, verify the restored binary
  is byte-identical by md5).
- Prior confounds recorded in the author's memory notes: a "node-size confound"
  (the `ObservedValues` growth for the expression-tree fields inflates per-node
  cost vs. a clean master, muddying speedup claims — now +4 bytes over the
  baseline's 4, down from +12); several observed
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
   "skipped" means *suppressed* via the `dirty` gate (the elision), not gated:
   no operand's per-execution signature changed, so the result is the one
   already absorbed. Unlike the old latch this is re-armable, so a node can go
   back to being suppressed once its operands settle.
   Printed under table 2 is a non-row line, **"of all N records, M updated
   nothing (signature unchanged)"** — the share of records that ran but whose
   early-out found nothing to do. It *overlaps* the recorded counts rather than
   partitioning them, and only the `_notify_` paths can early-out (plain
   `record_type_` has no signature and always does the work). It exists because
   that work is otherwise invisible: the opcode still executes.
4. **force behavior (recordForceBehavior variants):** rows `fbgeneric`,
   `fbgeneric_once`, `always`, `record_once`, `no_record`, `TOTAL`; plus
   `fbgeneric_bail` reported *separately* below the table (loads where the
   generic dispatcher found no following record — a genuine "nothing to
   attribute FB to," not a skip, hence not a table row).
   The generic dispatcher occupies two rows because its two switch arms are
   different concepts: `fbgeneric` is the plain arm (no gate, never skipped —
   pairs with `always`) and `fbgeneric_once` the gate arm (pairs with
   `record_once`, and carries the whole of `fbGenericSkip`). Grouping the table
   this way lets each generic row be read alongside its cached equivalent.

**Recording-elision rates (what the stats *do* reliably show).** With
`RIR_RECORD_STATS` on, the fraction of baseline "record-everything" sites the
recordless build skips is substantial and stable across runs (these are counts,
not timings, so they are trustworthy). Representative figures:

Measured 2026-07-27, interpreter-only (`PIR_ENABLE=off PIR_OSR=0`), on the current
tree (i.e. **after** the `[` demotion and the default-arg degradation):

| benchmark (inner iters) | should | recorded | skipped | skip% |
|---|---|---|---|---|
| mandelbrot ×300 (areWeFast) | 79,025,387 | 18,962,092 | 60,063,295 | **76.0%** |
| nbody_naive_inner ×3000 | 6,790,298 | 3,232,082 | 3,558,216 | **52.4%** |
| fasta_naive_2 ×20000 | 9,093,124 | 4,820,319 | 4,272,805 | **47.0%** |
| spectralnorm ×3 | 1,466 | 922 | 544 | **37.1%** |
| binarytrees_naive ×3 | 164,292 | 147,343 | 16,949 | **10.3%** |

Re-measured 2026-08-11 at `8e31b041`, same conditions. Elision rates are
essentially unchanged by the new notification mechanism — the point of it was
losslessness, not more skipping — but inner nodes now hold **real recorded
feedback** rather than nothing:

| benchmark | site | should | recorded | skipped |
|---|---|---|---|---|
| mandelbrot ×500 | leaves | 126,614,090 | 52,388,596 | 58.6% |
| | inner nodes | 92,736,740 | **62** | ≈100% |
| | TOTAL | 219,665,552 | 52,703,380 | **76.0%** |
| nbody_naive ×20000 | leaves | 35,161,808 | 20,841,585 | 40.7% |
| | inner nodes | 8,000,219 | 134 | ≈100% |

Note the inner-node "recorded" column: 62 records out of 92.7 M executions on
mandelbrot. Under the old object-gated scheme that column would read 0 — and the
slots would be empty, with no way to reconstruct them. Losslessness costs 62
records.

The no-op line (§ stats output, table 3) shows how much of the *surviving* work
the signature early-out removes: **82.2%** of mandelbrot's 52.7 M records update
nothing, versus 35.4% on nbody_naive — the same workload-shape asymmetry visible
everywhere else here.

**Instruction-count deltas** for the individual 2026-08-11 optimizations,
`perf stat -e instructions`, 3 reps, A/B against a rebuilt copy of the same tree
(spread ~0.004%):

| change | mandelbrot | nbody_naive | storage |
|---|---|---|---|
| 13 → 8-byte slot | −1.25% | −0.74% | −0.33% |
| signature early-out (hoisted check) | −4.35% | −2.42% | −0.78% |
| readable signature encoding | +0.19% | +0.11% | +0.03% |
| honest def slot + flat deps | ±0% | ±0% | ±0% |
| **cumulative vs. no inner-node elision** | **−15.2%** | **−5.3%** | **−2.2%** |

Two entries worth reading carefully. The readable-encoding row is a *deliberate*
regression: biasing the type rather than the whole value makes the low bits
genuinely `isScalar`/`hasDim`, at the cost of an `lea` that no longer folds a
constant. And the honest-def-slot row being zero is the useful result — the
correctness fix costs nothing on these benchmarks.

Shape of the wins, by benchmark character:

- **mandelbrot** — the best case: leaves ~58% skipped (~45% of leaves NoRecord-
  elided, ~13% RecordOnce with ~97% of those gated away) and inner nodes ~100%
  suppressed.
- **nbody_naive_inner** — ~52% overall; the force-behavior table shows
  `no_record` as the dominant FB class (the compiler proved most cached loads
  statically value/inferable), `record_once` ~76% gated.
- **binarytrees_naive / storage** — much lower leaf skip% (~0–4%): object- and
  allocation-heavy code where most leaves are genuine RecordAlways.

**Measured cost of excluding promises (§2A.2.3).** Making every record inside a
promise body plain/untracked is the single most expensive scope restriction so
far, and its cost is *very* unevenly distributed — it lands entirely on code that
puts real expressions inside lazy arguments:

| benchmark | skip% before | skip% after | inner-node elisions lost |
|---|---|---|---|
| binarytrees_naive ×3 | 22.9% | **10.3%** | 20,632 |
| fasta_naive_2 ×20000 | 47.0% | 47.0% | 1,338 |
| mandelbrot ×300 | 76.0% | 76.0% | 300 |
| nbody_naive_inner ×3000 | 52.4% | 52.4% | 0 |
| spectralnorm ×3 | 37.0% | 37.0% | 0 |

`binarytrees_naive` loses **more than half its skip rate** (22.9% → 10.3%): it is
recursive with lazy arguments, so a large share of its expression nodes live in
promise bodies and were previously elidable inner nodes. Everywhere else the
change is pure re-attribution — records move from the `leaves` row into
`untracked` with no elision lost, and `TOTAL should` is unchanged in every case.
This quantifies what re-enabling the promise scheme would be worth, and says the
payoff is concentrated in recursive/lazy-argument-heavy code rather than spread
evenly.

**Effect of the two most recent changes: negligible on these programs.** Every
`TOTAL should` above is *identical* to the pre-change measurement, and each
benchmark's skip% moved by ≤0.5pp. The small per-row deltas (tens of records) are
the default-arg reattribution (records moving from leaves/inner into `untracked`),
not the `[` demotion. That the `[` demotion barely registers here is a property of
these benchmarks, not of the change: in a deliberately `[`-heavy loop
(`for (i in 1:n) s <- s + v[i]`, 5,000 iterations) the demotion is clearly
visible — inner nodes are exactly 5,000 (the `+` alone) with the 5,000 `v[i]`
results now counted as RecordAlways leaves, where previously both would have been
inner nodes.

**Speedups reported by the author (wall-clock; read with §8's noise caveat).**
These come from the author's own running notes rather than from a controlled
re-measurement, and they are wall-clock rather than instruction counts, so treat
the absolute values as indicative:

- **nbody_naive:** after the eligibility tightening of §2A.1.2, the speedup fell
  to **~12%**. The cause is diagnostic rather than incidental: the benchmark
  begins by assigning a batch of **global** vectors and then reads them
  repeatedly, and globals are exactly what the control requirement excludes (we
  cannot know whether reflection rebound them). A **rewritten variant that keeps
  those variables in scope** — so they become locals the function creates —
  recovers **~20%**. This is the cleanest available illustration that the
  optimization's reach is bounded by *scoping discipline in the benchmark*, not
  only by the analysis.
- **The `v[[i]]` guarded-fast-path tightening (§3.1)** cost a further broad
  regression, visible across many benchmarks rather than isolated to one, because
  guarded fast paths are pervasive in the emitted bytecode.

Both figures postdate the soundness work and predate the planned run-time patch
for §3.1, so they should be re-taken once that lands.

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
- `rir/src/runtime/TypeFeedback.h` — `ObservedValues` (flags; `doRecord` vs.
  `doRecordAndSign`, the latter carrying the signature and the early-out;
  `record` / `recordInner` / `recordInnerAndSign`; `signatureOf` and
  `SigAlwaysDirty`; `parentPlus1` and its accessors), `TypeFeedback`
  (`record_type_*` methods, `markRelatedDirty`, `typeDeps_`,
  `noRecordSourceToDeps_`, `buildNoRecordReverseMap`, `ForceBehaviorKind`).
- `rir/src/bc/Compiler.cpp` — `compileGetvar`, `emitRecordTypeForVar`,
  `setTypeFeedbackParents` (the post-pass that specializes opcodes and sets
  parents), once-bit assignment & clearing; the def-slot stamp
  (`recordTypeForSlot` / `recordTypeOnceForSlot` — the single place a record is
  built and stamped — plus `noteValueRecord[At]` and `valueRecordSlotHere`).
- `rir/src/bc/CodeContext.h` — the per-Code-object compile state that must not
  leak across a Code boundary: `slotsStack` and `valueRecord` (§2C.5).
- `rir/src/bc/CodeStream.h` — `lastValueInstructionPos()` and `isValueNeutral()`,
  which is how the def-slot check asks "did anything change the value since that
  record?".
- `rir/src/bc/DefUseAnalysis.h` — `classifyUse` (RecordAlways/RecordOnce/NoRecord
  + `ForceBehaviorKind`), dominance/postdominance conditions, `scopeStillOpen`.
- `rir/src/bc/insns.h` — opcode definitions (the value-type record family must
  stay contiguous for the stats range-check).
- `rir/src/bc/BC_inc.h` — once-immediate pack/unpack + gate macros.
- `rir/src/interpreter/record_stats.{h,cpp}` — `RIR_RECORD_STATS` instrumentation.
