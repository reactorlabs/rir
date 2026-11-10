# Recordless: reducing type-feedback recording overhead in the Ř interpreter

**Status:** working design snapshot, last revised **2026-11-09** (§2B–§2C
rewritten for the per-activation operand-pair design). The author is
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
  `record_test_`. There is **no** `record_type_once_`, no leaf/inner/publish
  variants, and no No-Record (every load that is profiled gets a `record_type_`).
- Baseline `ObservedValues` is **4 bytes**:
  ```cpp
  uint8_t numTypes:2, stateBeforeLastForce:2, notScalar:1, attribs:1, object:1, notFastVecelt:1;
  std::array<uint8_t,3> seen;              // → static_assert(sizeof==4)
  ```
  It has **none** of the expression-tree fields. The current **16-byte** layout
  adds `arity`, `lastSig`, the operand position, the inner node's operand
  positions and recorded operand pair, and the dependents range (§2C.1) —
  **+12 bytes/slot**. This is the "node-size confound" (§7): any cross-branch
  timing partly reflects the larger feedback node, not the algorithm. (The size
  has moved: 16 with a parent pointer, 8 with a parent index, 24 with the frame
  stamp of the `dirty` design, 16 since.)

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
3. teaching the generic dispatcher to recognize the `leaf_publish_` opcodes
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
  node type, one publish function, and one opcode family; and they interact
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

- **RecordAlways** → emit `record_type_` (or the publishing variant
  `record_type_leaf_publish_`, §2C). Records on every execution.
- **RecordOnce** → emit `record_type_once_` (or `record_type_leaf_publish_once_`).
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
| 0 | `isAssignedInPromise(name)` — assigned in a **promise-argument position**, or by a `while` whose condition contains a loop (§2A.4.1) | `RecordAlways` | `Always` |
| 1 | *(eligible)* ∧ `¬vetoedInCondition` ∧ ∃ recorded use `ud` with `dominates(ud) ∧ postDominates(ud)` | `NoRecord` → `ud.feedbackSlot` | `FBValue` if local-stvar-reach or for-loop var, else `Infer` |
| 2 | dominating def `dd` (`findDominatingDef`) ∧ `¬vetoedInCondition` ∧ `isLocalOrParam` ∧ `postDominates(*dd)` ∧ `dd` has a slot | `NoRecord` → `dd.feedbackSlot` | `FBValue` |
| 3 | `d` ∧ local/param ∧ `loopDepth_>0` ∧ `!assignedInInnermostLoop` ∧ `assignedInEnclosingLoop` | `RecordOnce` *(dynamic — bit gets cleared)* | `FBValue` |
| 4 | *optimizable* ∧ `loopDepth_>0` ∧ `!assignedInEnclosingLoop` | `RecordOnce` *(stable — bit excluded from clear range)* | `FBValue` if local-stvar-reach, else `RecordOnce` |
| 5 | `isRangeBasedForLoopVar` ∧ `loopDepth_>0` | `RecordOnce` | `FBValue` |
| 6 | *(fallthrough)* | `RecordAlways` | `Always` |

where **eligible** (rule 1) = *optimizable* ∨ `isForLoopVar` ∨ (local/param with
a dominating def), and ***optimizable*** = `isOuterControlled(name) ∨
(isLocalOrParam(name) ∧ (isFormal(name) ∨ reaching-def exists))`.

Note `isLocalOrParam` is a **conjunct**, not an alternative beside `isFormal` —
§2A.1.2 for why that is a soundness requirement and §2A.1.3 for what broke when
it was not. **eligible** simplifies; see §2A.4.1, which also explains why
rules 1 and 2 both take `findDominatingDef`, while the RecordOnce rules and
*optimizable* keep the back-edge-vetoed `findReachingDef` (rule 2 used the
vetoed one until `524907e2`). `vetoedInCondition` — the read is in a `while`
condition whose loop assigns the name — is also defined there (§2A.4.1, "The
`while` condition veto"), as is what happens to the condition's sites once the
loop closes (`exitWhileLoop`, "Uses after the loop").

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
  **Default arguments count too (fixed 2026-10-13, found in external review).**
  The rule-0 set is built per Code object from that Code's own AST
  (`push` → `collectPromiseAssignedVars(ast, …)`). A default argument is a
  promise evaluated in the function's frame, but it lives in the formals list,
  outside the body's AST, so the body's set used to miss what it assigns:
  ```r
  function(d = (a <- "x")) { a <- id(1L); d; a }   # last `a` was NoRecord: integer only, read "x"
  ```
  This is the same pattern as the already-handled call-argument case
  `a <- id(1L); g(a <- "x"); a` — only the promise's origin differs. It needs a
  recorded right-hand side to show: with `a <- 1L` the def has no slot and the
  read records anyway. `push` now merges the function-wide `promiseAssigned_`
  (§2A.1.4) — body promise arguments plus default expressions, filled by the
  pre-scan in `finalize` before the body is pushed — into the body's rule-0 set,
  so a name a default argument assigns is excluded from every leaf optimization
  in the function, exactly like a call-argument one. Controls with a default
  that assigns nothing, or assigns a *different* name, keep their NoRecord.
  In-context suite diff: zero changes.

  **What the scan treats as inline, and conditional definitions.**
  `collectPromiseAssignedVars` treats the arguments of `<-`/`=`/`<<-`, `if`,
  `while`, `for`, `repeat`, `{` and `switch` as compiled inline; the arguments
  of every other call are treated as promises, so a name assigned there falls
  under rule 0. Two questions follow from this, both checked against the
  compiler (external review asked whether the right operand of `&&`/`||` and the
  arms of `switch` are treated as branches, since they run conditionally — if
  not, `c && (x <- g()); y <- x` would pass the dominance test):
  - **`&&`, `||` and inline `switch` arms are branches.** The compiler wraps the
    right operand of `&&`/`||` and each inline `switch` arm in
    `enterBranch`/`exitBranch`, so a definition there does not dominate a later
    use. Verified: `x <- id(1L); switch(k, a = x <- id("s"), b = 0); x` keeps
    an early read NoRecord (rule 0 is not involved) and the final read records
    `integer, character`. For `&&`/`||` the scan is in fact *more* conservative
    than needed — see the last point.
  - **`switch` with a `...` alternative was a hole (fixed 2026-10-14).** With a
    `...` arm the compiler does not inline `switch` at all
    (`if (*arg == R_DotsSymbol) return false` in `compileSpecialCall`) but
    emits a call to the `switch` special, which evaluates `EXPR` and the chosen
    arm itself, in our frame, with no `stvar_` of ours. The scan still listed it
    as inline, so in
    `function(k, ...) { x <- id(1L); switch(k, a = x <- id("s"), ...); x }`
    the last `x` was NoRecord against `x <- id(1L)` while it read `"s"`. The
    scan now treats a `switch` with a `...` argument as non-inline, so names
    assigned in its arguments fall under rule 0, matching what the compiler
    does. The other inline forms never fall back without also compiling the
    code inline (a `for` over a non-fast sequence still compiles its fast-path
    body inline, so the analysis sees its definitions). In-context suite diff:
    zero changes.
  - **`(`, `&&` and `||` are now on the inline list (`6f5cfda8`)** — but
    only in exactly the shapes `compileSpecialCall` inlines: `(` with one
    non-`...` argument, `&&`/`||` with two (a `...` operand is treated as a
    fall-back, conservatively). Previously all three were scanned as promises
    although compiled inline, so a name assigned inside parentheses or a
    logical operand was excluded from **every** leaf optimization in the
    function. Inline, an assignment there is a plain `stvar_` the analysis
    sees: unconditional for `(` and the left operand, inside a branch scope
    for the right operand of `&&`/`||`. Witnesses: `x <- id(1L); x;
    (x <- id("s")); x` now keeps both reads NoRecord; `c && (x <- …)` still
    records both types for the final read; the fall-back shapes (3-argument
    `&&`, 2-argument `(`) stay under rule 0.

    **No effect on the suite (in-context diff: zero changes).** In every
    benchmark the assignment also sits inside a *comparison or arithmetic
    operator* — `(kk <- perm[[k]]) > 1L`, `(x <- …) == …` — and those are not
    on the inline list either, so the name stays under rule 0 because of the
    outer operator. Freeing e.g. fannkuch's `kk` would need the operators the
    compiler inlines on the list too. That must be checked operator by
    operator: some calls compile a guarded fast path *plus* a slow-path call
    whose arguments are promises (`length(x <- 1)` stores in both), and those
    must stay off the list. Not done. Prerequisite: the while-condition fix
    of §2A.4.1, without which `kk` would become exposed.
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
  - **`controlled`** — "in our realm": the outer captures $f$ itself inherited,
    $O(f) = C(\mathit{enc}(f)) \setminus (F_f \cup P_f)$, plus
    $(F_f \cup L_f^{\,\prec g}) \setminus (S_f \cup P_f)$, where $P_f$ is the
    set of names assigned inside a promise of $f$ (§2A.1.4) and
    $L_f^{\,\prec g}$ the locals of $f$ whose definition **dominates the point
    where `g` is created** (§2A.1.5). The inner function may rely on name lookup
    falling through to a controlled environment rather than to the global env —
    which is only true if the binding already exists in a controlled frame when
    `g` is created: formals always, inherited names by induction, and locals only
    under that dominance condition. This is what enables **RecordOnce** on such captures, and — via
    `isOuterControlled` in `classifyUse` — **NoRecord** subsumption too.

    **Controlled is enough; immutability is not required.** Every leaf rule
    reasons within *one activation* of the inner function `g` (useDefs dedup
    within its body, RecordOnce bits per call). While `g` runs, the activation
    of `f` that created it is either suspended below it on the stack or has
    already returned, so none of `f`'s own bytecode executes between two reads
    in `g`: a capture that `f` reassigns (`a <- "x"` after `g`'s definition)
    still cannot change *under `g`'s feet*. The only ways it could are
    excluded by the subtractions: a nested closure's `<<-` ($S_f$, §2A.1.3) and
    a promise created by `f` — or by any function between `g` and the owner of
    the capture — that assigns in its own frame ($P$, §2A.1.4). A plain `<-` in
    another nested closure binds in that closure's own frame, and a recursive
    call to `f` gets a fresh environment, so neither interferes.

    In one sentence: *a capture is safe for `g` when no frame on its lookup
    path, from `g` up to the frame that owns it, can gain or change that binding
    during one activation of `g`.* The owner's frame is protected by its $S$
    (a `<<-` at any depth below it) and its $P$; every frame in between by its
    own $P$. Frames in between need no $S$: a `<<-` only ever updates an
    existing binding further out, it never creates one in between. And the
    path must *end* at a controlled frame: if the owner's binding does not exist
    yet when `g` runs, the lookup continues past it — to the global environment,
    which anything can rebind (§2A.1.5).
    Reflection (`assign(…, envir=)`, `delayedAssign`, `environment(g)$a <-`) is
    out of scope, as it is for every local subsumption.
  - **`immutable`** — a strict subset whose *value* cannot change during the
    inner function's lifetime: formals never body-assigned, and body-locals
    assigned exactly once, not for-loop variables, with a dominating def at the
    point the closure is created. (Both start from the
    $\setminus (S_f \cup P_f)$ set, so `<<-`-escaped and promise-assigned names
    are already gone.) Reserved for future cross-invocation optimizations; not
    yet exploited — immutability only matters for facts carried *across*
    activations of `g`, which no current rule does.
- The carry-through of an *outer* function's `controlled`/`immutable` needs no
  `S_f` filter of its own. `collectInnerSuperAssigned` → `scanForSuperAssigns`
  recurses through **all** nested functions at any depth, so a `<<-` anywhere in
  a subtree is already in the `innerSuperAssigned_` of *every* ancestor whose
  subtree contains it. Filtering where a name **enters** the capture sets is
  therefore complete; carry-through only ever sees already-filtered names.
  **This argument does not carry over to $P_f$.** $P_f$ is collected over $f$'s
  own promises only — the scan stops at nested function bodies — so no
  enclosing function's sets can contain it. A name $f$ inherits must therefore
  be filtered by $f$'s own $P_f$ when $f$ stores it: that is the
  $\setminus (F_f \cup P_f)$ in $O(f)$ above, and its absence was a real hole
  (§2A.1.4, "The inherited route").
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

### 2A.1.4 A third capture hole: promises of the outer function (found and fixed 2026-10-13)

The "controlled is enough" argument of §2A.1.2 says none of `f`'s bytecode runs
while an inner closure `g` executes. That holds for `f`'s instruction stream —
but not for **promises `f` created**. A promise is code of `f`, evaluated in
`f`'s frame, at whatever moment it is first forced, which can be in the middle
of `g`:

```r
run <- function(p, g) g(function() p)
f <- function() {
  a <- 1L
  g <- function(q) { a; q(); a }   # 2nd read subsumed against the 1st
  run(a <- "x", g)                 # `a <- "x"` is a promise bound to f's frame
}                                  # g's q() forces it: `a` is retyped mid-g
```

Before the fix, `g`'s second read compiled to `ldvar_cached_noRecordFB_` with
`NoRecord Type#1 (dep: #0)` while `g` returned `"x"`: the `character`
observation was lost, and the read reported only the `integer` of the first.
A **default argument** is the same hole by another route — it is a promise
evaluated in `f`'s frame:

```r
f <- function(d = (a <- "x")) { a <- 1L; g <- function(q) { a; q(); a }; g(function() d) }
```

Both reproduced on the pre-fix build. The control — `f` reassigning `a` with
its own bytecode *after* `g` returns — was, and remains, correctly NoRecord.

**Why nothing caught it.** Rule 0 of `classifyUse` (`isAssignedInPromise`)
handles promise-assigned names for **`f`'s own uses** only; the per-Code
`promiseAssignedVars_` set it reads was never consulted when building the
captures handed to `g`. And `collectAssignedVars` counts `a <- "x"` inside
`run(...)` as an ordinary body assignment, so `a` entered `controlled`. Default
arguments were not scanned at all: they live in the formals list, not the body.

**Fix.** `Compiler.cpp` gains a function-wide pre-scan, `promiseAssigned_`
($P_f$), alongside `innerSuperAssigned_`: `collectPromiseAssignedVars` over the
body, plus each formal's default expression scanned as already inside a promise.
`computeCapturesForInner` excludes $P_f$ from both capture sets, in the formals
loop and the body-locals loop, exactly where $S_f$ is excluded. It is a
function-wide set rather than `code.top()->defUseAnalysis`'s on purpose: a
closure literal compiled inside one of `f`'s promises would otherwise see only
that promise's expression.

That covers names that enter the capture sets as **`f`'s own** formals and
locals. It does *not* cover names `f` merely passes on from further out — see
"The inherited route" below; an earlier version of this section wrongly said
carry-through needed no filter.

How default arguments themselves are compiled is unchanged (they remain
unoptimized): the scan only removes the names they may *assign* from what inner
closures may rely on. `f`'s **own** reads are protected separately, by adding
the same set to `f`'s rule-0 set (§2A.1.1).

#### The inherited route (found in external review, fixed 2026-10-13)

The filter above sits where `f`'s own formals and locals enter the capture sets.
A name `f` *inherits* from an enclosing function arrives by a different route —
`f` stores the incoming sets in `finalize`, and `computeCapturesForInner` copies
them to `g` unconditionally — and that route had no $P_f$ filter:

```r
h <- function() {
  x <- 1L
  f <- function() {
    g <- function(q) { x; q; x }   # x reaches g as h's capture, via f
    g(x <- "a")                    # promise of f
  }
  f()
}
```

`g`'s first read finds `h`'s `x` (`1L`). Reading `q` forces the promise, which
runs in **`f`'s frame** and creates a *new* `x` there; the second read stops at
`f` and finds `"a"`. `h`'s binding never changed — a new binding appeared
*between* `g` and `h` and shadowed it. The second read was NoRecord against the
first: `g` returned `"a"` with only `integer` recorded.

Why the $S$ argument does not transfer: a `<<-` anywhere below `h` is in `h`'s
own scan, so `h` never offers the name. `f`'s promises are invisible to `h`
(the promise scan stops at nested function bodies), so only `f` can drop the
name, and it did not: `finalize` filtered inherited names by `f`'s formals
alone. Note that `x <- "a"` *is* counted in `f`'s `bodyAssignedCount_`, which
removed `x` from `f`'s **immutable** set — but leaf rules read the
**controlled** set, which deliberately kept body-assigned names.

**Fix.** In `finalize`, inherited names are also dropped if `f` assigns them in
a promise: $O(f) = C(\mathit{enc}(f)) \setminus (F_f \cup P_f)$ for controlled;
immutable keeps its body-assigned filter and gains $P_f$ too (default arguments
are not in `bodyAssignedCount_`). Plain body assignments still do not drop the
name, and need not: they cannot run while `g` does.

**A stricter rule was tried and rejected.** Dropping an inherited name whenever
`f` binds it *in any way* (plain assignment included) is also sound and slightly
simpler to state, and inner closures lose nothing by it — a name `f` assigns
reaches them as `f`'s own local anyway. But it removes the inherited flag from
`f`'s **own** reads, and that flag was sometimes the only thing making them
eligible. In `prg/cholesky.R` the inner `cholesky` assigns `U` while its
enclosing `execute` also has a `U`; six reads of `U` within single statements
of the `j` loop (lines 16–17: the second `U` of
`sum(U[1:(i-1), i] * U[1:(i-1), j])` and the target load of each `U[i, j] <- …`,
two loop copies) went from NoRecord to recorded. They were eligible only via the
inherited flag because the analysis tracks the *most recent* def of `U` — a
subassignment inside an `if`/`else`, which does not dominate — rather than
`U <- matrix(…)` on line 8, which does. (That last point is a separate
imprecision worth fixing on its own.) The promise-only rule keeps all six.

**Cost: none — measured in context.** An earlier measurement here was invalid:
it compiled every function literal *standalone*, where inner functions receive no
captures at all, so it could not observe any capture change. The valid method
enables the closure `rirDisassemble(what, R_TrueValue)` call in `rirCompile`
(`api.cpp`; the expression one must stay off — it rejects non-closure code and
halts R). Its verbose output includes a `== nested closures ==` section: inner
functions compiled *inside* their enclosing function, with the captures it hands
down. All 656 function literals in the suite (98 with nested closures) were
compiled this way and their load classifications diffed:

| comparison | loads changed |
|---|---|
| before §2A.1.4 → own-locals $P_f$ filter | 0 |
| own-locals filter → + inherited-route filter | 0 |
| stricter "any binding" rule → inherited-route filter | 6 (cholesky, restored) |

No benchmark assigns a captured name inside a promise, so the generated bytecode
is unchanged by either fix.

### 2A.1.5 A fourth hole: falling through to the global environment (found in external review, fixed 2026-10-13)

A local of `f` used to enter the inner closure's `controlled` set
unconditionally (minus $S_f$, $P_f$). The justification was that when `f`'s
definition has not run yet, lookup falls through to a *controlled* outer
environment. That is false when nothing controlled lies beyond `f` — e.g. `f` is
defined at top level, so the fall-through reaches the **global** environment,
which any function can rebind:

```r
x <- 1L
h <- function() x <<- if (is.integer(x)) 2.5 else "s"   # rebinds the GLOBAL x
f <- function() {
  g <- function() for (i in 1:3) { y <- x; h() }
  g()          # f has no x yet: g's reads fall through to the global x
  x <- 1L      # x in L_f, so x was handed to g as controlled
}
```

`h`'s `<<-` is outside `f`'s subtree, so $S_f$ cannot see it. `g`'s read was
RecordOnce: with peeling, iteration 1 (peeled copy) recorded `integer`, and the
loop copy fired once on iteration 2 (`double`) and skipped iteration 3
(`character`) — recording `double` where baseline records `double, character`.
(The reviewer's two-iteration version does not show it: peeling gives each of the
two iterations its own once-slot.)

**Fix.** In `computeCapturesForInner`, a body-local enters `controlled` only if a
definition of it **dominates the point where the inner closure is created**
(`findDominatingDef` at the function literal). Then the binding exists in `f`'s
frame on every path before `g` exists, so `g`'s lookup always stops at `f` and
never reaches the global. The query is `findDominatingDef`, not the
loop-vetoed `hasDominatingDef` that `immutable` uses: only the binding's
*existence* matters, and a later reassignment in an enclosing loop does not
unbind it. Formals need no condition (bound from entry). Inherited names compose:
an ancestor admitted them under the same rule, so its frame is bound. A closure
literal compiled inside one of `f`'s promises sees no body definitions there and
drops the name — conservative. A control with `x <- 1L` *before* `g` is created
keeps its RecordOnce.

**Cost: 7 loads, in-context suite diff.** All come from one common idiom —
helper closures defined at the top of a function, before the locals they read:

| program | load | why |
|---|---|---|
| `prg/Qlearn.R` | `n.states` in `world2m`, line 63 (`s = s + bit*n.states/2`), previously subsumed against line 43's read | `world2m` is created at line 41, `execute` assigns `n.states = 10*2` only at line 72 |
| `prg/cholesky.R` | the six `U` reads of lines 16–17 (§2A.1.4) | `cholesky` is created at line 2, `execute`'s `U` is assigned at line 28 |

The two are different in kind. **Qlearn's** loss is the price of the rule: the
closures only run at line 76, after line 72, so the subsumption was correct in
this program — but a closure created at line 41 *could* be called before line 72,
and the compiler cannot tell which without knowing call times. One record per
`world2m` call, i.e. per simulation step. **cholesky's** loss is recoverable:
`cholesky` binds its *own* `U` at line 8 before any read and never reads
`execute`'s; those reads were only eligible through the inherited flag because of
the "most recent def only" imprecision of §2A.1.4. Fixing that imprecision for a
function's own locals would restore them on their own merits.

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
   - calls `registerLeafSlot(slot)` so the elided use is still a pending
     operand in the expression tree — if it is an operand of an inner node it
     gets an **operand position**, and the source publishes into it (§2C.2).
   No opcode is emitted.
2. **`Compiler::finalize`.** After the post-pass assigned positions,
   `buildNoRecordReverseMap()` inverts `typeDeps_` into `depPositions_`: for
   each source, the operand positions of its dependents, as a range
   (`depsBegin`, `depsCount`) stored in the source's slot.
3. **Runtime.** The dependent slot is never written (no opcode). The *publish*
   half is live: on every execution the source writes its signature into its
   dependents' positions in the activation's `scratch` (`TypeFeedback::publish`).
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

There are six call sites (line numbers re-verified 2026-09-22):

| site | construct | what it records |
|---|---|---|
| `Compiler.cpp:607` | `recordTypeTracked(name)` — a **RecordAlways** leaf | `trackUseDef(name, slot)` — this use is now a subsumption candidate |
| `Compiler.cpp:2716` | a **RecordOnce** leaf | `trackUseDef(name, slot)` — same |
| `Compiler.cpp:1214` | **plain assignment** `x <- expr` | `trackDef(lhs, defSlot)` **with a real slot** |
| `Compiler.cpp:1432` | **subassignment** `x[i] <- v` | `trackDef(target, kNoSlot)` — **slot-less** |
| `Compiler.cpp:1623` | **replacement function** `f(x) <- v` | `trackDef(root, kNoSlot)` — slot-less |
| `Compiler.cpp:1284` | **any bail-out** (`bailToGnuR`) | `trackDef(root, kNoSlot)` — slot-less |

The last three are one family and are the subject of "Complex assignment is a
type barrier" below.

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
`publish`'s one-level dependents list requires (§2C.2).

**Complex assignment is a type barrier.** Every form of `f(x) <- v` stores a
value that nothing observed, so all of them reseat the def with `kNoSlot`. This
matters because such a store can change the type *or the flags*:

```r
x <- c(1, 2, 3)     # REALSXP, no attributes
x[1] <- "a"         # x is now STRSXP
dim(x) <- c(2, 2)   # x now has attributes  -> the `attribs` flag
class(x) <- "k"     # x is now an object    -> the `object` flag, which gates dispatch
```

Without invalidation a later read of `x` is classified NoRecord against a def or
use from *before* the assignment and inherits a stale description — and since
NoRecord is a correctness commitment (§3), that is simply wrong feedback.

There are **three routes** into a complex assignment, and they fail differently:

1. **Subassignment** (`x[i] <- v`, `x[[i]] <- v`, multi-dim). Emits
   `subassign1_1/…; stvar[Cached]` with **no `record_type_`** after it. The
   store is visible, its result is not.
2. **Replacement function** (`dim(x) <- v`, `names<-`, `attr<-`, `levels<-`, …).
   Emits `named_call_ \`dim<-\`; stvar[Cached]`. Also visible-but-unobserved —
   and note the record that *does* follow in non-void context describes the RHS
   left on the stack, not what was stored.
3. **Bail-out** (`class(x) <- v`, `slot<-`, `$<-`, `@<-`, and any LHS with more
   than one level of nesting). The compiler gives up and emits a plain call to
   GNU R's `<-` special, which performs the store itself — so there is **no
   `stvar_` in the instruction stream at all**. Not merely an unobserved store,
   an *invisible* one.

Route 1 was always guarded. Routes 2 and 3 were not, and produced exactly the
errors above: `x <- c(1,2,3,4); dim(x) <- c(2,2); x` elided the final read and
reported `double ()` where the baseline records `double (a)`; `class(x) <- "k"`
reported `object = false` for an object. Route 3 has six separate exits, so they
are funnelled through one `bailToGnuR` helper rather than guarded individually —
a bail added later inherits the barrier by construction.

**The name invalidated is the ROOT of the LHS chain**, not the immediate target.
`dim(a$b) <- v` rebuilds and re-stores `a`:

```r
`*tmp*` <- a
a <- `$<-`(`*tmp*`, "b", `dim<-`(`*tmp*`$b, v))
```

so the walk peels one container per step — `dim(a$b)` → `a$b` → `a` — using the
fact that in every replacement form the object being modified is the *first*
argument (`a$b` is `` `$`(a, b) ``). This must be the **same walk** that
`collectAssignedVars` performs when it builds the `expected` counts. If the two
disagree, `trackDef`'s `bumpSeen` never increments the name the pre-scan counted,
`seen` never catches up to `expected`, and `hasUnseenLoopDef` pins itself true
for that variable for the rest of the enclosing loop (§2A.4.1) — silently
disabling `findReachingDef` there. Using the compiler's immediate `dest` instead
of the root is what caused this: `dest` is `a$b`, not a symbol, so the guard
skipped it. A non-symbol root (`dim(g()) <- v`, invalid R) walks to
`R_NilValue` and is skipped — it compiles, then fails at runtime with R's own
`invalid (NULL) left side of assignment`.

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

### 2A.4.1 How the subsumption rules treat later in-loop definitions

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

**The pre-scan must cover the `while` condition too (fixed 2026-10-14).** The
`expected` counts are collected before anything of the loop is compiled —
including the condition, so in `x <- f(); while (x > 0) x <- g()` the
condition's read already sees the body's store as pending and is not subsumed
against `x <- f()` (an external-review question; verified, with and without
peeling). But `compileWhile` scanned only the **body**, and the condition also
runs every iteration:

```r
k <- id(5L); while ({ k <- k - 1.5; k > 0 }) n <- n + 1L
```

The read of `k` in `k - 1.5` precedes the condition's own store, so from the
second check on it sees the double the previous check stored — but with the
store unregistered it was NoRecord against `k <- id(5L)` (`integer` only).
`compileWhile` now also scans the condition's AST (`for` desugaring passes none:
its condition is compiler-generated). The condition's stores are tracked when
compiled, so `seen` still catches up. With peeling, the peeled first check now
records where only the pre-loop value can reach it — a small, deliberate
over-approximation. Fannkuch's `while ((kk <- perm[[k]]) > 1L)` was never
exposed, only because the `>` puts `kk` under rule 0 (§2A.1.1). In-context suite
diff: zero changes.

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

**Rule 2 (def-to-use) does not care either — once post-dominance is required.**
It used to take the vetoed `d`, on the reasoning that its claim — *this read's
value is the one **that store** wrote* — is broken if a later in-loop store
reaches the read around the back edge. That is true only when the store is
*outside* the loop:

```r
a <- f(); while (…) { y <- a; a <- g() }    # 2nd iteration: y sees g()'s value
```

But rule 2 also requires `postDominates(def)`, i.e. the def and the read in the
**same innermost scope** with no `return`/`break`/`next` between them. In that
case the def is inside the loop body before the read, so every path into the
read — the back edge included — passes through the def first and overwrites
whatever a later store left:

```r
while (…) { a <- f(); y <- a; a <- g() }    # y always sees f()'s value
```

Inside a loop **body** the shape the back-edge veto exists for never satisfies
`postDominates` (different scopes), so there the veto added nothing to rule 2
and only refused the safe shape. Since `524907e2` rule 2 takes
`findDominatingDef` as rule 1 does — but gated by the condition veto below,
which `524907e2` lacked:

```cpp
const Def* dd = findDominatingDef(name);
if (dd && !vetoedInCondition && isLocalOrParam(name) &&
    postDominates(*dd) && dd->feedbackSlot != kNoSlot)
```

`defs_` keeps only the most recent def, so nothing rebinds the name between
`dd` and the read; a complex-assignment barrier leaves `dd` with no slot. The
**RecordOnce** rules (3, 4) and the *optimizable* gate keep the vetoed
`findReachingDef`: they reason about values across iterations, where a pending
in-loop store does matter.

| | claim | effect of a later in-loop def |
|---|---|---|
| rule 1, use-to-use | *these two **reads** see the same value as each other* | none in a body — both re-execute each iteration and see whatever is live; in a `while` condition, and after the loop for a condition read, see below |
| rule 2, def-to-use | *this read's value is the one **that store** wrote* | none in a body, given post-dominance — the def re-executes before the read every iteration; in a `while` condition, and after the loop for a store in the condition, see below |
| rules 3–4, RecordOnce | *the value is invariant across iterations of this loop* | decisive — hence the back-edge veto |

#### The `while` condition veto (found in external review, 2026-10-23)

The argument above — "post-dominance confines the def to the read's own scope,
so it runs before the read on every iteration" — holds for a loop **body**,
which has its own scope. It fails for a `while` **condition**: `compileWhile`
compiles the condition in the *enclosing* scope (`enterLoopContext`, not
`enterLoopScope`), the same scope as a def before the loop. So `524907e2` was
unsound:

```r
f <- function() {
  x <- id(1L)        # integer
  while (x < 5) {    # read of x: same scope as the def, post-dominates it
    x <- x + 2.5     # double
  }
  x
}
```

The read in `x < 5` was NoRecord against `x <- id(1L)` and its feedback said
`integer` only, while from the second check on it reads a double. This also
undid the condition pre-scan above (#25): `while ({ k <- k - 1.5; k > 0 })`
was again subsumed against the pre-loop `k`. Rule 1 had the **same hole before
`524907e2`**: a condition read could be subsumed against an earlier recorded
read of the same name before the loop (`x; while (x < 5) x <- x + 2.5`), for
the same reason.

**Fix.** `LoopBodyInfo` carries an `inCondition` flag, set by
`enterWhileCondition`/`exitWhileCondition` around both `compileCond()` calls
in `compileWhile` (peeled copy and loop copy). `classifyUse` computes, before
rule 1:

```cpp
bool vetoedInCondition = false;
if (!loopBodyDefs_.empty() && loopBodyDefs_.back().inCondition) {
    const auto& expected = loopBodyDefs_.back().expected;
    auto it = expected.find(name);
    vetoedInCondition = it != expected.end() && it->second > 0;
}
```

and both rule 1 and rule 2 require `!vetoedInCondition`. The read then falls to
rules 3–4 and records. The veto fires only if the loop **assigns the name**
(`expected` comes from the pre-scan of body *and* condition), so a condition
read of a name the loop never writes stays NoRecord.

**A loop inside a condition: rule 0, not a stack walk (2026-11-02).** Two loops
can be in their condition at once only if a loop is written *inside* another
loop's `while (…)` — in practice a `{ }` condition block, e.g. do-while
emulation containing a loop:

```r
x <- id(1L)
while ({
  while (x > 100) NULL   # inner condition: inner loop does not assign x
  x <- x + 2.5           # the outer loop does
  x < 5
}) NULL
```

Both conditions compile in the function's scope, so `x > 100` post-dominates
`x <- id(1L)`; checking only the innermost loop finds no def of `x` and
subsumes it. Until 2026-11-02 the veto walked `loopBodyDefs_` outward while
each loop was in its condition, reaching the outer loop. That walk existed for
this shape alone, so it was replaced by putting the shape under rule 0:
`collectPromiseAssignedVars`, on a `while` whose condition `containsLoop`, adds
**every** name the loop assigns (condition and body) to the rule-0 set. With
that, a read in a condition can only be reached by a store of the innermost
loop, and `back()` suffices. The cost is function-wide: those names record at
every read (e.g. `u <- t` in `while ({ for (j in 1:2) NULL; i < 3L }) { …;
t <- id(i); u <- t }`). No corpus program has a loop in a condition.

Why a flag and not something on `Def`: the question is not *where the def was
made* but *whether this read re-executes after the body*, and that is a
property of the read's position. Earlier attempts stamped defs with the loop
depth at creation (broken: sequential loops reuse stack positions) and then
with a per-loop entry timestamp (sound, same results as the flag, more state);
both were removed.

**Cost.** A name read in a condition after being assigned earlier in the
*same* condition now records: in `while ({ k <- k - 1.5; k > 0 })` the `k` in
`k > 0` would be safe to subsume against the store just before it, but the veto
does not distinguish it. With peeling, the peeled first check is vetoed too,
although only the pre-loop value reaches it.

**Witnesses** (current build, peeling on and off): the example above records
`integer, double`; #25's `k` reads record; the rule-1 case records; the
do-while-with-inner-loop case records `integer, double` at `x > 100`. Controls
stay NoRecord: a condition reading a name the loop does not assign; `while (…)
{ a <- f(); a; a <- g() }` (the motivating case of `524907e2`); a def in an
outer body read in an inner loop's condition; the swap's `t`. Rewriting f7-style
code as `repeat { …; if (!c) break; … }` puts the inner loop in a body scope,
where neither the problem nor its rule-0 fallback arises.

**Measured** on the paper corpus (43 programs, stats builds, `02c05117` → the
fixed version): mean NoRecord share 8.94% → 9.07%, mean leaf skip 31.77% →
31.91%. The gain is mostly `fannkuchredux_naive` (+5.27 pp, the swap's `t`),
with small gains in `fasta*`, `fastaredux*`, `Mandelbrot` and
`reversecomplement_naive`. In-context classification diff: 61 loads record →
NoRecord vs `02c05117`; 17 NoRecord → record vs `524907e2`. Zero changes between
the `back()`-only and walking versions (the walk was later replaced by rule 0,
above). **The `524907e2` numbers (8.94% →
9.81%; `sieve1` +14.2 pp, `fasta_naive_2` +10.0 pp, `fannkuchredux` +4.2 pp,
`pidigits`) were the unsound condition reads and are withdrawn** — e.g.
`sieve1`'s `while (j <= n)` after `j <- …`, with `j` assigned in the body.

#### Uses after the loop: condition sites are not sources (external review, 2026-11-02)

The veto protects reads **in** the condition. A read **after** the loop has the
same problem from the other side: the condition's sites keep the enclosing
scope's id, so they still pass `dominates` and `postDominates` against a use
after the loop, though they ran on every check and the use runs once. This
breaks the re-execution clause of §3 (the source must not run again before the
use):

```r
b <- function(f) {
  while (x <- f()) NULL   # store in the condition: every check
  c1 <- x                 # was NoRecord via rule 2, from that store
}
while ({ x <- f(); x > 0 }) NULL; x   # rule 1, from the condition's recorded read
```

With `f` returning `1.5`, `1.5`, `1.5`, `0L`, the source's summary is
`double, integer` and the use only ever sees `integer`: an over-approximation —
a precision loss, not unsoundness, because the last check reads or stores
exactly the value the use sees. The operator form `while ((x <- f()) > 0)`
escaped by accident (the `>` puts `x` under rule 0); the bare form, `(…)`,
`&&`/`||` operands and `{ }` blocks were affected.

**Fix.** `compileWhile` closes the loop with `exitWhileLoop()` instead of
`exitLoop()` + `clearLoopBodyDefs()`. For each name the loop assigns
(`expected > 0`, condition and body), it

- sets the name's last def's `feedbackSlot` to `kNoSlot`, the barrier a complex
  assignment leaves, so it is no rule-2 source;
- erases `useDefs_[name]`, so no condition read is a rule-1 source.

The def itself **stays**: a store in the condition does dominate what follows
the loop (the condition always runs once), and that keeps `findReachingDef`
non-null, which the RecordOnce rules need. A name only the body assigns already
has its last def in the closed body scope, and the body's store had already
erased the reads before it, so for it the hook changes nothing. Classification
on both sides of the loop is otherwise untouched:

```r
b <- function(f, n) {
  x <- id(1L)
  a1 <- x                    # NoRecord (from the def)
  a2 <- x                    # NoRecord
  while (x <- f()) {
    b1 <- x                  # records (body scope: no source yet)
    b2 <- x                  # NoRecord (from b1)
  }
  c1 <- x                    # records — the fix
  s <- 0L
  for (i in 1:n) s <- s + x  # RecordOnce (needs the dominating def kept)
}
```

**Rejected first.** Putting every name a condition assigns under rule 0 was
simpler but function-wide: in the function above it records `a1`, `a2`, `b2`
and turns the later loop's RecordOnce into a per-iteration record. A first cut
that also put the *body's* names under rule 0 cost `fannkuchredux_naive`
(RecordAlways 46.1M → 72.6M leaves), `reversecomplement_naive` (16.9M → 23.0M),
`fannkuchredux` (`k_range`) and `fasta_2` (`lo`). Moving the def into the closed
loop scope instead of making it a barrier also closes the hole, but loses the
later loop's RecordOnce.

**Measured.** Paper corpus (43 programs, stats builds, `b231d619` vs fixed):
every stats CSV byte-identical; the disassembly of all 710 function literals in
the benchmark sources is identical. No corpus program has a condition that
assigns a name the analysis can see. All earlier reviewer witnesses (95
scripts, peeling on and off) classify as before, except the reads after the
loop above and the loop-in-condition rule-0 case.

#### What rule 1's third disjunct buys, measured

Removing
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
  NoRecord *source* even when its NoRecord dependent had no inner node above it
  — pointless work and a classification muddle. Now a def is a publishing
  source only if one of its dependents received an operand position (§2C.5).
- **Reentrancy** — handled by the per-invocation `fired` array (§2A.2.1); would
  be a correctness bug if the bitmap were global. The inner nodes' former
  `dirty` gate *was* global in exactly this sense and had exactly this bug; the
  current operand signatures now live in a per-activation array too (§2B.5).
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

An inner node records only when its operands give it something it has not
absorbed yet. Otherwise its record instruction is one 16-bit compare and
nothing else:

```cpp
if (slot.operandsUnchanged(scratch)) skip; else record and remember the operands;
```

There is no separate `shouldNotRecord` flag: "is this an elidable inner node" is
carried by the opcode (§2C.3), and the operand pair the node last recorded
(`tuple`, §2B.2) is its whole runtime state.

### 2B.2 Record when the operand tuple changes

**This section describes the mechanism as of 2026-11-04 (`e995aa09`,
"sigpair"), refined through `bcd013f3`. It replaced a per-slot `dirty` bit with
an ownership stamp and a depth stamp; §2B.5 explains why.**

An inner node must re-record whenever its result could differ from the ones it
has already absorbed. Its result's `ObservedValues` contribution is a function
of its operands' **signatures**:

```
sig(v) = ((TYPEOF + 1) << 2) | isScalar << 1 | hasDim      (see signatureOf)
sig(v) = 0  (SigAlwaysDirty) for objects, attributes beyond a lone `dim`,
            S4, and length 0 — values that cannot be summarised
```

(58 = integer scalar, 62 = double scalar, 46 = logical scalar.) So the node
needs to know exactly one thing: *is this execution's pair of operand
signatures one I have recorded before?* The state that answers it is split by
who owns it:

| state | lives in | written by | read by |
|---|---|---|---|
| the **current** operand signatures | `scratch`, a byte array per **activation** (§2B.5), one byte per *operand position* | each operand, when it executes (`publish`) | its inner node |
| the **recorded** operand pair, `tuple` | the node's `ObservedValues` (shared by all activations) | the node, at the end of each record | the node |
| `lastSig`, the signature of the value last recorded in full | the node's `ObservedValues` | the record | the node, when it publishes upward |

Each inner node owns two consecutive operand positions, `tupleBase` and
`tupleBase + 1`; each of its operands owns one of them (`posPlus1`). An operand
writes its signature to `scratch[pos]` on *every* execution, changed or not —
a position holds this activation's current value, not a delta. The node then
compares the two bytes at `scratch + tupleBase` with `tuple` as one `uint16_t`:

```cpp
bool operandsUnchanged(const uint8_t* scratch) const {
    uint16_t cur; memcpy(&cur, scratch + tupleBase, sizeof(cur));   // one cmpw
    return cur == tuple;
}
```

On a mismatch it records and stores the current pair as `tuple`
(`storeTuple`) — unless the pair is incomplete (an operand position holds 0:
unwritten in this activation, or an unsummarisable value), in which case it
stores `kNoTuple` (`0xFFFF`, which no pair of real signatures can equal, the
widest being 131). A node with one operand (`x + 1`: constants record nothing)
still gets two positions; the second is never written, reads 0 in both
`scratch` and `tuple`, and the 16-bit compare needs no arity.

**Soundness** is one invariant: *`tuple` is always a complete operand pair whose
result this slot has absorbed.* It is written only at the end of a record, from
the pair that record ran on. A skip therefore means "this execution's operands
have the signatures of an execution already recorded here", so — the result
being a function of those signatures, and the accumulated state only growing —
recording again would change nothing. The base case holds because a fresh slot
has `tuple = kNoTuple`, which nothing matches, so the first execution always
records.

The critical property, unchanged since the first signature design, is that the
signatures describe **this execution**, not the accumulated state. The
accumulated state is monotone and stops changing while the live value keeps
varying underneath it: after operands have separately been seen as `int` and
`double`, an accumulated-state watcher sees nothing when the first `(int, int)`
pair arrives, and misses the `integer` result.

An unsummarisable operand publishes 0, so the pair is incomplete, `tuple` stays
unmatchable, and the node records on every execution while such values keep
arriving. It re-suppresses as soon as two summarisable operands repeat a
recorded pair.

### 2B.2.1 What this replaced, and why

**First: an object gate (until 2026-08-11).** The original design un-suppressed
a parent when a child observed an **object**, via `notifyRelatedNodes` —
object-gated, monotonic, and latched once per activation. It was cheaper (one
test on a value `doRecord` had already computed) but not lossless, for two
independent reasons:

1. **The type set is unreconstructible when both operands are polymorphic.**
   `h(1L,1.5); h(1.5,1L)` and the same plus `h(1L,1L)` leave *byte-identical*
   operand feedback — `[integer,double]`, `[double,integer]`, both scalar — but
   ground truths of `{double}` and `{double,integer}`. No function of the
   operand feedback can be right for both. This is an impossibility, not a
   missing rule.
2. **`attribs` is unreconstructible**, because `copyMostAttrib` gates on the
   operand's actual length while the feedback carries only `notScalar`
   (§2B.3).

Inner-node reconstruction was never implemented anyway — `reconstructFeedback`
only does the leaf copy, and is itself never called (§2A.3, §4.6) — so
suppressed inner slots simply stayed empty (`numTypes == 0`, which
`PirType::merge` asserts against). And when the old gate *did* fire it recorded
a biased sample: only post-object executions, missing every one before the
latch. One object anywhere also meant the parent recorded on *every*
subsequent execution for the life of the function.

**Then: a `dirty` bit (2026-08-11 → 2026-11-04).** Each child compared its
signature with its own previous one (`lastSig`) and, on a change, set its
parent's `dirty` bit; the parent recorded if `dirty` and cleared it. This made
inner nodes lossless and re-armable, but both pieces of state — the bit and the
child's previous signature — were per *Code*, while the protocol they
implement spans an activation. Two reentrancy holes followed, and the fixes
(an owner stamp on `dirty`, a live-depth stamp on `lastSig`, depth restoration
on every non-local exit) were the most intricate part of the system. §2B.5
covers them and why the tuple design removes them rather than patching them.

What survives from both predecessors: signatures, not objects, decide; the node
holds its own observations (no reconstruction); suppression re-arms as soon as
operands settle; and a value outside the domain keeps the consumer recording
for as long as it keeps arriving.

### 2B.3 What actually becomes an inner node

There is exactly **one** `recordTypeTracked(true)` call site in the compiler
(verified 2026-08-07) — that call is the *only* way a slot can become an inner
node, and it covers a fixed list of **13 binary operators**:

> `Add Sub Mul Div Idiv Mod Pow` (arithmetic), `Eq Ne Lt Le Gt Ge` (comparison).

It *becomes* one only if the post-pass gives it operand positions, i.e. if at
least one operand registered under it and none of them was opaque (§2C.5). The
corpus (710 functions, 43 programs) has 4,616 inner nodes with one recorded
operand, 5,790 with two, and none with more.

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

Unary `-x`, `+x`, `!x` share the `Add`/`Sub`/`Not` symbols but are compiled
without a record of their own, so they are not inner nodes (and not
transparent either, §2C.5).

**Why `[` is *not* an inner node (it was, until it was demoted).** From
2026-06-03 (`b659827e`, "optimnize for extract1") until 2026-08, `[`
(`Bracket`) *was* emitted as an elidable inner node, on the justification:
"`x[...]` has the same SEXPTYPE as `x` for non-object `x`, so its result type is
inferable from the lhs leaf." The SEXPTYPE half is true; the conclusion does not
follow, because eliding the node discards the **entire** `ObservedValues`, and
`notScalar` is not inherited. Measured on the baseline (record-everything) build
with `f <- function(x,i) x[i]`, `x = c(1,2,3)`, `i = 1L`:

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

`[` has therefore been **demoted to `recordTypeOpaqueResult()`** — an
always-recording leaf, the same treatment as `[[`. Its operands carry no
operand position, while `x + y` still yields a suppressed inner node.

`:` is excluded for the same reason: its result type and length come from the
operand *values*, not their types, so the feedback cannot be reconstructed. It
is emitted as `recordTypeOpaqueResult()`.

The general lesson, worth carrying into any future inner-node candidate: **the
inference obligation is over the whole `ObservedValues`, not just the SEXPTYPE.**
An operator qualifies only if *every* recorded field — type set, `notScalar`,
`attribs`, `notFastVecelt` — is a function of the operands' signatures. `[`
fails on `notScalar`; the arithmetic ops pass because result length is
`max(operand lengths)`.

`[[` (`DoubleBracket`) extracts an *element* whose type genuinely varies (e.g.
`list(3, "hello")[[i]]`), so it was never a candidate. The colon *operand casts*
(`colonCastLhs/Rhs`) emitted by `compileSimpleFor` are internal and untracked —
distinct from the `Colon` result node.

### 2B.4 Problems specific to inner nodes

- **Determining inferability** is per-operator, conservative, and easy to get
  wrong. `[[`, general calls, and anything whose recorded feedback isn't a
  function of the operands' signatures must *not* be made an elidable inner
  node. The obligation covers the **whole `ObservedValues`**, not just the
  SEXPTYPE — `[` was demoted for exactly this reason (§2B.3), having been an
  inner node for ~7 weeks on a SEXPTYPE-only argument.
- **The operands must be exactly the operator's operands.** The node skips when
  its operand pair is unchanged, so every value that flows into the operator
  must be described by exactly one operand slot that publishes on every
  execution — no more (a slot from *inside* an operand would make the skip
  depend on something that is not an operand) and no less (an operand nobody
  describes could change while the pair stays equal). The unsound case this
  rules out, present until `b070b332`: `(if (c) a else b) + d`. Neither branch
  records the `if`'s value, so `c`, `a`, `b` and `d` all used to propagate up as
  "operands" of the `+`. With `a` and `b` each keeping its own type, a flip of
  `c` changed the `+`'s operand type while every published signature stayed
  the same. The compile-time rules that guarantee "exactly one slot per
  operand, or none" are in §2C.5; an operand no single slot can describe makes
  the operator an ordinary always-recording leaf. Witness: `a` double once,
  `b` always integer, `c` selecting `a` once — the `+` now records
  `integer, double` on every execution (plain `record_type_`).
- **Graceful degradation.** Anything that does not fit — an opaque operand, more
  than two operands, positions past the 16-bit range, a promise-context default
  argument — leaves the operator a leaf, which always records. Nothing ever
  degrades into an unsound skip.
- **The producer of an operand must be the thing that publishes it.** That is
  why opaque value results are tracked (`recordTypeOpaqueResult`) rather than
  untracked; see the worked `x[[i]] + 1` example in §2C.4.
- **Reentrancy.** The protocol spans several instructions (each operand
  publishes, then the node compares), so where its state lives decides whether
  a nested activation can interfere. §2B.5.
- **Root inner nodes that are also sources.** The top of an assigned expression
  (`a <- f(x)+1`) is an inner node that may *also* be a NoRecord source for later
  reads of `a`; it must publish its result's signature into its *dependents'*
  operand positions, not a position of its own. This is why the inner opcodes
  split on the "publishes-or-not" axis (§2C.3), not the root-vs-non-root axis.

---

### 2B.5 Reentrancy

The skip test compares *current* operand signatures with a *recorded* pair. The
first are per-execution state; the second is persistent. Every reentrancy
problem the inner nodes have had came from keeping per-execution state in the
Code-wide `TypeFeedback`, which all activations of the Code share. The current
design keeps each kind of state where its owner lives, and has no reentrancy
protocol at all.

#### 2B.5.1 The two holes of the `dirty`-bit design (history)

Both are kept here because they are the witnesses any future design must pass.

**A stolen flag.** In the `dirty` design a child set its parent's flag and the
parent consumed it at its own record instruction. Anything in between that
re-entered the same Code could consume a flag meant for an activation further
out:

```r
f <- function(a, n) {
  if (n == 0) return(1L)
  a + f(1L, n - 1)          # leaf FIRST, then the recursive call
}
f(1.5, 2)
```

| step | activation | action |
|---|---|---|
| 1 | A | `ldvar a` (double) records, sets the `+` node dirty |
| 2 | B | nested call; its `ldvar a` (integer) records |
| 3 | B | `add_` sees dirty, records **integer**, clears |
| 4 | A | `add_` sees a clean slot, **skips** — A's `double` is lost |

The `add_` slot ended with `[integer]`; `f(1L, n-1) + a`, or hoisting the call
into a temporary, recorded both. So the recorded feedback depended on operand
order — not merely lost, but unstable under semantics-preserving edits.
`binarytrees_naive`'s hot line `tree[[1]] + check(tree[[2]]) -
check(tree[[3]])` has exactly this shape.

**A shared signature.** Whether to set the flag at all was decided by comparing
with the child's previous signature, also shared. An interleaved activation
could rewrite it so that each of the outer activation's operands compared
"unchanged" against a write the outer never made:

```r
f <- function(x, ret, n) {
  if (n == 0) return(ret)
  w <- x + f(1+0i, ret, n - 1)     # leaf first, then the recursive call
  ret
}
f(1L, 2.5,  1)      # A1:  1L + 2.5  -> double
f(1L, TRUE, 2)      # B:   1+0i + TRUE -> complex ;  A: 1L + TRUE -> integer
```

The `+` ended with `[double, complex]`; the truth is
`{double, complex, integer}`. Both operands *were* observed — the node never
learned they were paired that way: a cross-product of two independently stale
signatures.

**The patches, and why they were abandoned.** The flag got an owner (the frame
address of the activation that set it: first setter wins, only the owner
clears, the skip path never looks at the owner). The signature got a stamp:
the count of live activations of the Code (`Code::liveDepth`), 6 bits with an
asymmetric clamp, and an early-out only if `lastSigDepth <= depth`. Keeping
`liveDepth` exact then required restoring it on every non-local exit
(`return()` in a loop or promise, `break`/`next` in a promise, trampolined
loops) via a guard in `rirCallTrampoline` and `loopTrampoline` and R's `cend`
hook — and promise Codes, deopt and caught errors in nested activations were
still open. A stamp leaked by an error disabled suppression for that node
permanently. An external review (point 2) found further missed records and
orphaned flags under recursion. Each fix was correct on its own terms, but the
protocol had become the most complex part of the system for a problem that is
not inherent: the current operand signatures *are* per-activation facts.

#### 2B.5.2 Current operand signatures per activation, recorded pair shared

`evalRirCode` allocates the activation's `scratch` array on entry (`alloca`,
`Code::innerScratchSize` bytes, `memset` to 0), next to the record-once `fired`
array (§2A.2.1), which is per-invocation for the same reason. A trampolined
loop body (`loopTrampoline`) is the same activation and is handed the
caller's array (`sharedScratch`). Operands publish into it; inner nodes read
it.

Why this needs no further protocol:

- **Current signatures cannot be disturbed.** A nested activation of the same
  Code has its own array. Between an operand's publish and its node's compare,
  nothing but this activation writes those bytes. Both witnesses of §2B.5.1
  pass: the outer `+` compares the outer activation's own pair.
- **The recorded pair can be disturbed, harmlessly.** A nested activation may
  replace `tuple` while the outer one is between operands — but only with
  another pair it recorded. By the invariant of §2B.2, whatever `tuple` holds,
  matching it proves a record would add nothing. The worst an interleaving can
  do is make the outer activation record a pair it would otherwise have
  skipped: over-recording, never under-recording.
- **`lastSig` is read only together with `tuple`.** A suppressed
  `inner_publish_` node publishes `lastSig` as its result's signature. Both are
  written at the end of the same record and read by the same instruction, with
  no other code in between, so they always describe the same pair.
- **Nothing to restore on a non-local exit.** The array dies with the frame.
  `liveDepth`, `LiveDepthGuard`, the owner stamp, the depth clamp, `pin()` and
  the `cend` hook are all gone.
- **No first-execution cost per activation.** `tuple` is shared, so a new
  activation whose operands have the signatures recorded by an earlier one
  skips on its first execution. (Making the recorded pair per-activation too
  would be simpler still, but would record each inner node at least once per
  call.) A position is read as 0 only if no operand wrote it in this
  activation, which within one Code happens only for a NoRecord dependent
  whose source has not run — and that is the conservative outcome.

**Measured**, deterministic record counts (stats build), vs the depth-stamp
design:

| | depth stamp (`b070b332`) | per-activation pair (`e995aa09`) |
|---|---|---|
| recursion to depth 20, 30,500 executions of `n + r(n - 1L)` | 22 recorded | **3** |
| `gcd` inner-node records | 1.30M | **61** |
| `binarytrees` inner-node records | ≈37,000 | **≈50** |

(`gcd` was one of the programs whose leaked `liveDepth` saturated; the depth
design then recorded those functions' inner nodes on every execution.)

**Cost.** One `alloca` + `memset` of `innerScratchSize` bytes per activation of
a Code that has inner nodes (two bytes per inner node, plus one per source that
is not itself an operand). In exchange the hot paths lost: the depth clamps
computed per activation, the depth increment/decrement and its exit guards, and
the per-record ownership tests. The skip itself is one 16-bit load and
compare, in place of a 1-bit test.

---

## 2C. Bringing the two strategies together

### 2C.1 One node type: `ObservedValues`

Both strategies are carried by extra fields on the single feedback node
(`runtime/TypeFeedback.h`), 16 bytes in all:

```cpp
// byte 0: baseline flags (numTypes, stateBeforeLastForce, notScalar, attribs, object, notFastVecelt)
uint8_t  arity : 3;     // inner node: number of operand positions (1 or 2); 0 otherwise
uint8_t  unused : 5;
std::array<uint8_t, 3> seen;   // bytes 2-4, baseline
uint8_t  lastSig;       // signature of the value last recorded in full
uint16_t posPlus1;      // operand position this site publishes to, biased by one (0 = none)
uint16_t tupleBase;     // inner node: its first operand position
uint16_t tuple;         // inner node: operand signatures it last recorded (kNoTuple = none)
uint16_t depsBegin;     // NoRecord source: its dependents' positions are
uint16_t depsCount;     //   TypeFeedback::depPositions_[depsBegin, +depsCount)
```

vs. the baseline **4 bytes** (§1.1). All tree fields are compile-time
(`setTypeFeedbackParents`, `buildNoRecordReverseMap`) except `lastSig` and
`tuple`, which the record writes. The size is a power of two on purpose:
`types_[idx]` is then a scaled index, and it is indexed on every record.

Layout choices:

- **Positions, not slot indices.** An operand does not name its parent slot; it
  names the byte of the activation's `scratch` its parent reads. Positions are
  dense per Code (two per inner node), which keeps `scratch` small, and they
  are what make a node's two operands adjacent, so the compare is one 16-bit
  load. `tupleBase` is therefore **not** a slot index, and `pos()` is not the
  operand's slot index either.
- **`posPlus1` is biased by one** so that an all-zero slot publishes nothing: the
  array is `memcpy`'d from a vector and the constructor `memset`s, so a `0xFFFF`
  sentinel would be one forgotten initializer away from silently making
  position 0 everyone's. A position that will not fit in 16 bits is *dropped* at
  compile time, not truncated — the operator then stays a leaf.
- **`lastSig` gets a whole byte**: no read-modify-write to preserve neighbouring
  bits, and no range guard on `TYPEOF` (5 bits, so the widest encoding is
  `((31 + 1) << 2) | 3 = 131`).
- **`depsBegin`/`depsCount` replaced padding** (`bcd013f3`): bytes 12–15 used to
  be unused, so the dependents list cost no size.

Fields earlier versions carried and that are **gone**: `isLeaf` /
`shouldNotRecord` (the opcode says it); `hasPropagatedNotification` (the
object-gate latch, §2B.2.1); `parentPlus1` (the parent slot index of the
`dirty` design); `dirty`, the owner stamp and `lastSigDepth` (§2B.5.1);
`hasDeps` (replaced by `depsCount`).

### 2C.2 One publish function for both edges

The single mechanism that serves *both* "an operand feeds its inner node" and
"a source stands in for its NoRecord dependents":

```cpp
// TypeFeedback::publish — called on EVERY execution of a publishing site
void publish(const ObservedValues& slot, uint8_t sig, uint8_t* scratch) {
    scratch[slot.pos()] = sig;                      // own operand position
    if (unsigned n = slot.depsCount) {              // NoRecord dependents' positions
        const uint16_t* p = depPositions_.data() + slot.depsBegin;
        for (unsigned i = 0; i < n; i++)
            scratch[p[i]] = sig;
    }
}
```

Callers: `record_type_leaf_publish[_once]_` (with the signature
`doRecordAndSign` just computed), `record_type_inner_publish_` after recording
(with the new `lastSig`) and when skipping (with the stored `lastSig`, the
result of the pair that just matched).

Design points:

- **Unconditional, not on change.** A position holds this activation's value, so
  it is rewritten on every execution. In steady state the store writes the
  value already there, but no test is needed to know that.
- **Every publishing site has a position of its own**, so the first store needs
  no test: a source that is not itself an operand gets a dummy position nobody
  reads (`setTypeFeedbackParents`). That store is wasted; §6 has how to drop it.
- **Dependents are a flat list of positions** (`bcd013f3`). The dependents emit
  no opcode, so their source writes their positions for them. All sources'
  lists are concatenated in `TypeFeedback::depPositions_`, and each source
  keeps its range in the slot (`depsBegin`, `depsCount`). Only dependents that
  are operands (have a position) are listed; the rest have nothing to receive.
  This replaced a `std::vector` of dependent *slot indices* per slot
  (`noRecordSourceToDeps_`), whose loop loaded each dependent's
  `ObservedValues` to test `publishes()` and read `pos()`. A list longer than
  the 16-bit range would be left empty — its dependents' nodes then always
  record — but it cannot occur: each dependent has one source and its own
  position, and positions are already 16-bit.
- **The loop is inline, behind the `depsCount` test.** Dependents are not rare in
  arithmetic code (a variable or temporary read again under another operator):
  8.71% of all publishes in the corpus have dependents (1.44 on average),
  30.4% in mandelbrot, 48.7% in sieve1. The test keeps sites without
  dependents from touching `depPositions_`.
- **The count is read once** (`06f8a4e5`). `scratch` is `uint8_t*`, so as far
  as the compiler knows every store may alias the slot, and `slot.depsCount` in
  the loop condition was reloaded on each iteration. With the count in a local,
  the loop is five instructions per dependent (load position, advance, store,
  compare, branch). Bytecode and record stats unchanged.
- **Push, not pull.** The source writes into each dependent's position rather
  than the dependent's inner node reading the source's own position. That
  keeps every inner node's two operands adjacent, so its test stays one 16-bit
  load (§5.2 has the rejected alternative).
- **One level deep, and that is load-bearing.** A source reaches its
  dependents' positions but not a dependent's dependents'. That is correct only
  because the dep graph is kept **flat**: a NoRecord read stamps the *ultimate
  source* slot rather than the dep slot just allocated, so `b <- a; c <- b`
  yields `#2→#0, #3→#0` rather than a `#3→#2→#0` chain (§2A.4).
- **Only leaves that need it pay.** A non-operand, non-source leaf keeps the
  plain `record_type_` opcode and never publishes nor computes a signature.
- **Deserialized feedback has no dependents.** `serialize` writes neither
  `typeDeps_` nor `depPositions_`, so `deserialize` zeroes `depsBegin` /
  `depsCount`. Operand positions and `innerScratchSize` do travel; a
  dependent's position is then never written, reads 0, and its inner node
  records on every execution. Conservative.

### 2C.3 One unified opcode family

After the post-pass (`setTypeFeedbackParents`), the value-type record opcodes are
(contiguous in the `Opcode` enum — the stats range-check relies on it):

| opcode | strategy | meaning |
|---|---|---|
| `record_type_` | leaf | plain leaf, RecordAlways; also every *untracked* record, the RecordOnce fallback, and an operator that did not become an inner node |
| `record_type_once_` | leaf | plain leaf, RecordOnce (fired-gated) |
| `record_type_leaf_publish_` | leaf | leaf that **publishes**: an operand of an inner node, and/or a NoRecord *source* |
| `record_type_leaf_publish_once_` | leaf | RecordOnce variant of the above |
| `record_type_inner_` | inner | **standalone** inner node: root, no dependents — nothing to publish |
| `record_type_inner_publish_` | inner | inner node that **publishes**: an operand itself (non-root), **or** a root source (dependents) |

The two families are parallel: each has a plain form and a `*_publish_` form,
and the publish form of both routes through the same `publish`. (Until
`a3181731` these were the `*_notify_` opcodes, which §5 and §7 still use for
history.) `record_test_`/`record_call_`
(branch/callee feedback) sit outside this range.

The inner opcodes test `operandsUnchanged` **inline in the dispatch loop**; only
the record itself is out of line (`record_type_inner_when_changed`,
`record_type_inner_publish_when_changed`, both `noinline`). Passing the
already-resolved slot reference into those handlers was measured 0.23% slower
on mandelbrot than recomputing `types(idx)`.

The `ObservedValues` recording primitives:

- `doRecord(e)` — shared core: updates `numTypes/seen[]`, `notScalar`, `object`,
  `attribs`, `notFastVecelt`.
- `doRecordAndSign(e)` — the same updates plus the signature, from a single
  inspection of `e`; returns early, touching nothing, when the signature equals
  `lastSig` (every update is then provably a no-op). Sets `lastSig`.
- `record(e)` → just `doRecord(e)`. Used by plain `record_type_`/`_once_`.
- `recordInner(e, scratch)` — `doRecord` + `storeTuple`. Used by `inner_`, which
  publishes nothing and so needs no result signature.
- `recordInnerAndSign(e, scratch)` — `doRecordAndSign` + `storeTuple`. Used by
  `inner_publish_`.

**Untracked records** (a leaf concern that touches the shared opcode). Not every
`record_type_` participates in the analysis. `recordTypeUntracked()` emits a
plain `record_type_` and registers nowhere, so the post-pass leaves it untouched:
always records, never publishes, never suppressed, never NoRecord/RecordOnce. The
truly-untracked sites: the colon `m:n` operand casts (×2), the super-assign
target read-for-update, and the `ldvar` fallback while compiling default formal
args (no main-body context).

`recordTypeTracked` and `recordTypeOpaqueResult` also **degrade to untracked
while compiling default formal arguments** (`!mainBodyCtx_`) and when the leaf
optimization is off. Every record there records always and is attributed to the
untracked row — matching the baseline, which has no suppression anywhere. The
degraded record still marks its level as recorded
(`markValueRecordedUntracked`) and leaves no slot pending, so the enclosing
operator sees no operand and no opaque marker for it: an operator *outside* the
default argument cannot be suppressed on the strength of a slot inside it.

Not registering guarantees the slot has no position. It does **not**
structurally guarantee `!isSource`: the post-pass derives `sourceSlots` from
`typeDeps_` targets, and `trackDef` records whatever slot the value-record stamp
names, without regard to tracking (§2A.4), so an untracked slot could in
principle be named as a def's feedback slot and then be specialized to
`record_type_leaf_publish_`. In practice it never is, but *contingently*, for a
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
  leaves depending on it. When one of those reads is an operand of another
  operator, the source publishes its result signature (recorded or, when
  skipping, `lastSig`) into that read's position.
- **An inner node's elision is sound only if the node that *produces* its operand
  publishes it.** This is why opaque value results (call return, `[`, `[[`,
  `for` element, replacement-fn) use `recordTypeOpaqueResult()` rather than
  `recordTypeUntracked()`: the opaque result stays pending at its level as the
  one slot standing for that operand, so the enclosing operator gives it a
  position. An untracked slot never gets one, so it could never tell its
  operator anything.

  Why that is not academic — `g <- function(x, i) x[[i]] + 1` run with
  `x = list(structure(1, class = "myclass"))` and a `+.myclass` method
  (measured 2026-07-27, under the `dirty` design; the argument is unchanged):

  ```
  x       → [ list    (s)    … ] Type#0   ← NOT an object
  i       → [ integer (s)    … ] Type#2   ← NOT an object
  x[[i]]  → [ double  (oavs) … ] Type#3   ← IS an object;  operand of Type#4
  add_    → [ double  (oavs) … ] Type#4   ← records
  ```

  `x[[i]]` is an object while `x` and `i` are not, so the `[[` result is the
  *only* site that can tell `+` its inference has broken. It publishes 0, the
  pair is incomplete, and `+` records the S3-dispatched result. Had `x` and `i`
  been the `+`'s operands instead, both would publish unchanged signatures and
  the `+` would skip while dispatching to `+.myclass` and returning an object.

  **Runtime cost of tracking: none** when the result is not an operand: a
  tracked slot with no position and no dependents is left plain
  `record_type_` by the post-pass. (Only other effect: stats attribution.)

  Acting as a *def's feedback slot* does **not** require tracking: `trackDef`
  records whatever slot the value-record stamp names, regardless of whether that
  slot was ever registered in the tree (§2A.4).
- **Outside the signature domain, the operand's producer must publish on *every*
  execution of the consumer (found in external review, fixed 2026-10-29).** A
  value outside the domain (an object, attributes richer than a lone `dim`, S4,
  length 0) publishes 0, so the consumer records — but only while the producer
  keeps publishing it. The two leaf optimizations stop that:

  ```r
  `+.ctr` <- function(e1, e2) { e1$n <- e1$n + 1L; if (e1$n > 2L) 2.5 else 1L }
  f1 <- function(x, n) { y <- 0; for (i in 1:n) { y <- x + 1L }; y }            # RecordOnce leaf
  f2 <- function() { x <- mk(); i <- 0L; while ((x + 1L) < 2) i <- i + 1L; i }   # NoRecord leaf
  ```

  `x` is an environment-backed S3 object whose `+` method changes its state,
  so the result type changes although `x` is never reassigned. The loss needs a
  dispatched method whose result type changes for an unchanged binding
  (reference-semantics state, globals, randomness) — or, for a non-object
  outside the domain, another operand that changes *within* its signature
  (`c(a=1,b=2) + y` keeps or drops `names` as `y`'s length goes 2 → 4). It is
  lost feedback, not a miscompile: PIR guards its speculation, so the cost is a
  deopt.

  **RecordOnce (`f1`) — one publish is enough, no special case
  (`3a04a9f5`).** `record_type_leaf_publish_once_` records, publishes, and
  sets its once-bit unconditionally, like plain `record_type_once_`. Outside
  the domain it publishes 0, and that 0 stays: `scratch` and `fired` are both
  per activation and only this leaf writes its positions, so while the bit is
  set they still hold what it published when it fired. A pair with a 0 is
  stored as `kNoTuple` (`storeTuple`) and never matches, so the `+` records on
  every iteration. Clearing the bit (an enclosing loop, §2A.2.2) re-records and
  republishes.

  Until `3a04a9f5` the leaf stayed armed while its value was unsummarisable
  (set the bit only if `lastSig != SigAlwaysDirty`) and recorded on every
  execution. The `dirty` design needed that, since the parent re-armed only on
  a signalled change. Under sigpair it re-recorded the same binding for
  nothing. The one thing lost is an object mutated through an alias (e.g. an
  environment's attributes changed by reference) between the fire and a later
  execution: the leaf's own feedback misses it, but its parent still records.
  Witness, three calls with `n = 5`: RecordOnce leaf records 15 → 6 (skips
  0 → 9), FB `record_once` the same (it shares the bit); inner-node counts
  unchanged (29 executions / 16 recorded / 13 skipped); result `2.5` both.
  Corpus: record stats byte-identical on all 43 programs (no RecordOnce leaf
  there is ever outside the domain).

  **NoRecord (`f2`) — handled by the per-activation array, no special case.**
  The source (`mk()`'s result) runs once, before the loop, and publishes 0 into
  the dependent's position. Nothing overwrites it in this activation, so the
  `+` sees an incomplete pair and records on every iteration; the next call
  starts from a zeroed array and the source publishes again. The `dirty` design
  needed a dedicated mechanism here ("Fix B1": *pinning* the dependents' parents
  permanently, since a per-Code flag could not tell when the source's value had
  left the domain). Re-measured on `bcd013f3`: the in-loop `+` records
  `integer, double` with `tuple -` (`kNoTuple`), and the pre-loop first check
  `integer` (it only ever returns `1L`). On `3a04a9f5`, three calls: the
  NoRecord `x` 9 executions / 0 recorded, the `+` (`inner_publish_`) 9 / 9;
  result `2`. No pinning code remains.

  Both review cases now rest on one fact: a published 0 stays in `scratch`
  for the rest of the activation.

  A NoRecord use can run more often than its source only when the source is a
  pre-loop def read in a `while` condition (the condition compiles in the
  enclosing scope, §2A.4.1), or when the source is itself RecordOnce — rules
  1 and 2 otherwise keep source and use in the same scope.
- **The `tracked` vs `untracked` distinction is the bridge.**
  `recordTypeTracked(isParent)` / `recordTypeOpaqueResult()` register a slot in
  the expression-tree structure so the post-pass can give it a position and
  specialize it; `recordTypeUntracked()` opts a slot out entirely. The genuinely
  untracked sites (§2C.3) are the ones that can never be the operand of a
  tracked expression, so they have no position to lose.

### 2C.5 Compile-time construction and the specialization post-pass

**Building the tree during a single compile pass.** The expression tree is never
materialized as a data structure; it is discovered with a **stack of pending
child-slot lists** (`slotsStack`), a parallel stack of "did this level record
its own value" flags (`slotsRecorded`), and a flat `parents` map (operand slot
→ operator slot):

- Both stacks live on the **`CodeContext`**, i.e. one per Code object, not per
  function. An expression tree never spans a Code boundary: a promise body is a
  separate Code object whose evaluation is decoupled in time from the expression
  that created it, so its operands are not operands of that expression. When
  this lived on the function-wide `CompilerContext`, a promise's leaves landed
  in whatever level the *enclosing* function had open — in `f(x) + g(x)` the
  promise loads of `x` became children of the `+`. It is also what makes
  operand positions per-Code: an operator and its operands always run in the
  same activation's `scratch`.

  **`valueRecord` (§2A.4's def-slot stamp) lives there for the same reason**, and
  the reason is worth stating because it is not the obvious one. Both quantities
  the stamp compares are *per Code object*: `insnPos` is an offset into that
  `CodeContext`'s `CodeStream`, and `scopeId` comes from that `CodeContext`'s
  `DefUseAnalysis`. Both counters restart — `pos` at 0, `nextScopeId_` at 1 — so
  a stamp carried across a Code boundary would be compared against a different
  counter space and could match by *coincidence* rather than by meaning. A
  top-level record stamps `scopeId == 0`, which `scopeStillOpen` accepts
  unconditionally, so the scope half would offer no protection at all there.

  Note this is **not** covered by promises being excluded from the leaf
  optimization. That exclusion stops a stamp made in a promise from being
  *consumed* in that promise (`emitRecordTypeForVar` returns early, so
  `classifyUse` never runs), but the degradation path is
  `recordTypeUntracked()`, which still stamps — and the consumer at the
  assignment is guarded only by the global `isRecordlessLeafEnabled()`. What
  made the leak unreachable in practice was unrelated and contingent: every
  `mk_promise_` happens to be followed by a `call_` whose result re-stamps.
  Putting the state on the `CodeContext` makes it unreachable by construction
  instead, which is why there is deliberately no "same Code object?" check in
  `valueRecordSlotHere`.
- `compileExpr`'s `LANGSXP` case brackets every call with
  `pushNewNodeForSlots()` … `compileCall(...)` … `finishNodeForSlots(fun, nargs,
  voidContext)` … `popNodeForSlots()`. One stack level per nested call;
  `compileExpr` is the *single owner* of the push/pop balance.
- **Leaves register as pending operands**: `registerLeafSlot` (RecordAlways,
  RecordOnce, and NoRecord uses via `registerNoRecordDep`) pushes the slot onto
  the current level. Constants register nothing — they never change, so they
  need no position.
- **An operator adopts what is pending** — `registerSlot(slot, isParent=true)`,
  from `recordTypeTracked(true)`:
  ```cpp
  bool opaque = any(currentSlots == kOpaqueOperand);
  assert(opaque || currentSlots.size() <= 2);   // one slot per operand, binary op
  if (!opaque) for (child : currentSlots) parents[child] = slot;
  currentSlots = { slot };  slotsRecorded.top() = true;
  ```
  The operator then stands in for its own value at this level, for the
  enclosing operator to adopt. That is how `a <- f(x) + 1` links the call
  result to `+` without an explicit tree.
- **An opaque result consumes what is pending** — `recordTypeOpaqueResult()`
  (call return, `[`, `[[`, `:`, `for`, replacement-fn) clears the level, pushes
  only itself and marks the level recorded. Its operands say nothing about its
  type, so they must not become anyone's operands: in `v[i] + 1`, `v` and `i`
  stay parent-less (plain `record_type_`, no publish) and `v[i]` is the `+`'s
  one operand.
- **`finishNodeForSlots` reduces each call to what its *value* contributes** —
  at most one slot, or the marker `kOpaqueOperand` — before `popNodeForSlots`
  lifts whatever is left to the enclosing level (`b070b332`). In order:

  | the call | leaves pending | why |
  |---|---|---|
  | in void context | nothing | its value goes nowhere |
  | recorded its own value (operator or opaque result) | its own slot | already reduced |
  | `&&`, `\|\|`, `while`, `repeat`, `for`, `function`, `quote`, `return`, `missing`, `is.null`, `is.list`, `is.pairlist` (compiled inline, no record) | nothing | value's signature is fixed (logical scalar, NULL, a closure, …) — a constant |
  | `(`, `{`, `<-`, `=`, `<<-` with ≤ 1 pending slot | that slot | transparent: its value *is* that subexpression's, exactly as recorded |
  | unary `-`, `+`, `!` with nothing pending (`-1L`) | nothing | a constant |
  | anything else (`if`, `switch`, unary op around a recorded operand, a transparent form with leftovers, …) | `kOpaqueOperand` | no single slot describes the value |

  An operator with `kOpaqueOperand` among its pending operands links nothing,
  gets no positions, and stays an always-recording leaf. So `-x + y` records the
  `+` every time — `-x` was never recorded, so `x`'s slot must not stand for
  it — and `(if (c) a else b) + d` too (§2B.4). Two sites trim before the
  reduction so that transparency holds: `{` clears the pending list before its
  last expression (earlier statements contribute nothing), and a complex
  assignment keeps only the right-hand side's slots (`keepPendingSlots`), not
  those of the target and index reads.

  Effect vs `6465ef10` (the last commit without these rules): 278 of the
  corpus's 70,752 record opcodes changed kind; record counts changed only in
  `fannkuchredux` (+362,880 each version), `pidigits` (+248,185) and `nbody`
  (+3/+1) — inner nodes that used to skip on operands that were not theirs.
- `popNodeForSlots()` still lifts unadopted slots to the enclosing level, which
  is now at most one slot or the marker per call; at the outermost level of a
  Code they are dropped (the Code boundary). `pop()` asserts the stack is empty
  when a Code is finished.
- **The post-pass**, `CompilerContext::setTypeFeedbackParents` (called from
  `Compiler::finalize`, after all Codes are emitted):
  1. Maps every placeholder record to its Code (`codeOf`).
  2. Inverts `parents` into each operator's operand list, in slot order, and
     gives the operator two consecutive positions in its Code
     (`setInner(base, n)`, `tuple = kNoTuple`) and each operand one
     (`setPos(base + i)`). More than `MaxArity` operands, or a position past the
     16-bit range: the operator stays a leaf.
  3. Collects `sourceSlots`: the sources of NoRecord dependents that received a
     position. A source with no position of its own gets a dummy one (so
     `publish` needs no test); one that cannot is dropped from the set, and its
     dependents' positions read 0.
  4. Stores each Code's position count in `Code::innerScratchSize` (serialized
     with the Code).
  5. **Specializes each placeholder `record_type_`/`record_type_once_` opcode**
     by `(isInner = arity > 0, publishes = has a position or is a source)` into
     `inner_` / `inner_publish_` / `leaf_publish_[once_]`, or leaves it plain.
     Inner nodes only ever come from `record_type_` (RecordOnce only classifies
     leaves), which an `assert` checks.
- `buildNoRecordReverseMap()` (called right after, since it needs the
  positions) fills `depPositions_` and each source's `depsBegin` /
  `depsCount` from `typeDeps_` (§2C.2).
- `bc/CodeContext.h` holds the per-Code compile state (including
  `recordTypeOnceBitmapSize`, `slotsStack`, `slotsRecorded`, `valueRecord`);
  `bc/CompilerCFG.{h,cpp}` supplies the CFG/scope structure the dominance
  approximation reads (§3).

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
`fbRecordOnce_`. A `SLOWASSERT(*pc == record_type_ || *pc == record_type_leaf_publish_)`
pins this in debug builds.

**Non-cached loads use a generic runtime dispatcher.** `ldvar_`,
`ldvar_for_update_`, `ldvar_for_update_cache_`, `ldddvar_` call the generic
`recordForceBehavior`, which peeks the following opcode and dispatches.
(`ldvar_super_` does **not**: it calls `recordForceBehaviorNoCheck`, which is
safe because a super-assign target is always followed by
`recordTypeUntracked()` — a plain record with a raw immediate.)

```cpp
switch (*pc) {
case record_type_:  case record_type_leaf_publish_:            idx = raw;          // always
    REC_STAT(fbGenericRec++);                                                     break;
case record_type_once_: case record_type_leaf_publish_once_:                        // once
    RECORD_TYPE_ONCE_GATE(fired, raw, { REC_STAT(fbGenericSkip++); return; });
    idx = RECORD_TYPE_ONCE_SLOT_IDX(raw);
    REC_STAT(fbGenericOnceRec++);                                                 break;
default: REC_STAT(fbGenericBail++); /* no record follows this load */  return;
}
recordFbAtSlot(idx, s);
```

Note the dispatcher recognizes **all four** value-type leaf opcodes, including
the `leaf_publish_` variants. Recognizing only the plain `record_type_`/`_once_`
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
FBValue    → value, if the slot's type record is non-empty; else unknown
Infer      → min(FB(typeDeps_[i]), evaluatedPromise)     // resolve source first
```

**Unexecuted sites stay `unknown` (`8fccf0af`, external review).** `FBValue`
used to be written unconditionally, so a def-to-use NoRecord in a branch never
taken, or a RecordOnce use in a loop run zero times, became `value` where the
baseline leaves `unknown` — a formal break of losslessness, though harmless in
practice (an empty type record gives the compiler nothing to speculate on). Every
execution records at least one type, so after the type copy an empty record
(`numTypes == 0`) is exactly "never ran" (for a dependent: its source never
ran). `Infer` needed no change: an unexecuted source is `unknown` (0), and
`min(unknown, evaluatedPromise)` stays `unknown`.

One gap remains, shared with the type copy: post-dominance excludes
`return`/`break`/`next` between a source and its NoRecord use, but not an R
error. If the source ran and an error aborted the function before the use, the
baseline leaves the use's slot empty while reconstruction copies the source's
types (and now `value`). Losslessness holds on error-free paths.

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
  signature state into a slot that never records for itself. (Those fields are
  now `posPlus1` / `arity` / `tupleBase` / `tuple` / `lastSig`, §2C.1; the
  type-only copy excludes all of them.)

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
  this session into `record_type_leaf_notify_[once_]` (now
  `record_type_leaf_publish_[once_]`). They had become
  byte-identical handlers once the notifier was made generic over
  parent-and-dependents; the split was residual. The former name `leafWithParent`
  was renamed to `leaf_notify` because the merged opcode also covers a parentless
  source.
- **Splitting inner nodes on the root-vs-non-root axis** (`root_inner_` /
  `inner_node_`). Replaced this session by the **notifies-vs-standalone** axis
  (`record_type_inner_` / `record_type_inner_notify_`, now `_inner_publish_`).
  The old axis lumped a
  root that *is* a source (must notify its dependents) together with a root that
  is not (nothing to notify), forcing the common no-notify case to still run the
  notify gate every execution. The new axis moves that cost off the common path;
  a root+source now correctly lands in the notifying bucket. This also let
  `notifyParent()` and the separate `recordInnerNode()` primitive be deleted
  (both inner opcodes then routed through `recordInner` + `markRelatedDirty`;
  today through `recordInner[AndSign]` + `publish`, §2C.3).
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
- **Dependents read their source's position ("pull") instead of the source
  writing theirs ("push").** Sound: each dependent has exactly one source, and
  the source's own position holds the same byte that push would copy. But an
  inner node's operands are then no longer adjacent, so its test needs two
  1-byte loads, a shift and an or instead of one 16-bit load (two position
  fields replacing `tupleBase`, `depsBegin`, `depsCount`, still 16 bytes).
  Where source and use run equally often (same scope), it is roughly a wash per
  dependent: about 5 loop instructions saved, about 3 added to the test, and
  only if the scattered test is a separate opcode — otherwise every inner node
  pays. It loses where the use runs more often than its source: a pre-loop def
  read in a `while` condition, and a NoRecord use of a RecordOnce source
  (§2C.4). There push stores once per activation and every later check finds
  the byte in place; pull pays the wider test on every iteration. Rejected.
- **Suppressed operands not republishing.** A suppressed `inner_publish_` node
  stores `lastSig` to its position on every execution. Reading the child's
  shared `lastSig` from the parent instead reintroduces the stale-signature
  hole of §2B.5.1 (a nested activation can re-record the child between its skip
  and the parent's test). Relying on the byte already in `scratch` fails too: it
  is 0 on the first execution in an activation, and stale when the child's pair
  changed and then matched a `tuple` re-recorded by a nested activation.
  Storing only when `scratch[pos] != lastSig` is a load, compare and branch to
  save one store. Kept as is.
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
  - **Ř-specific in detail:** the exact opcode taxonomy, the suppress/publish
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
- **A source's dead position could be one of its dependents'.** A source that
  is no operand (nearly every one: it is an assignment's RHS) gets a position
  nobody reads, and `publish` stores to it on every execution before the
  dependents' loop (§2C.2). Giving it one of its dependents' positions instead,
  and leaving that one out of `depPositions_`, makes a one-dependent source a
  single store with no loop iteration. Compile-time only (the post-pass and
  `buildNoRecordReverseMap`); opcodes and record stats would be unchanged,
  `innerScratchSize` smaller. Not done.
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

## 7. Current implementation status (snapshot, 2026-11-10)

Working branch: `recordLessNew2-expressionsNewSchema-sigPair`, at commit
`3a04a9f5` (entries 31–38 below). `recordLessNew2-expressionsNewSchema` holds
the `dirty`-bit design up to `6465ef10`, merged in at `1f41af40`.
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
- The opcode family `record_type_leaf_publish_[once_]` /
  `record_type_inner_` / `record_type_inner_publish_` (the `_notify_` names
  until `a3181731`); the old `record_type_dep_` / `record_type_once_dep_` are
  **gone**.
- `TypeFeedback::publish`, `ObservedValues::operandsUnchanged` /
  `storeTuple`, the per-activation `scratch` array (`Code::innerScratchSize`);
  `recordInner()` / `recordInnerAndSign()`; `record()` is a plain `doRecord`
  with **no** suppression check. `markRelatedDirty`, `dirty`, the owner
  stamp, `liveDepth` / `LiveDepthGuard`, `pin()`, `parentPlus1`, `hasDeps`,
  `noRecordSourceToDeps_`, `recordSimple` and `notifyParent` are **gone**.
- `RECORDLESS_EXPTREE_ENABLED` **removed** — the expression-tree scheme is
  unconditionally compiled in (only `RECORDLESS_EXPTREE_DEBUG` remains).
- FB: `ldvar_cached_` / `ldvar_cached_noRecordFB_` /
  `ldvar_cached_fbRecordOnce_`; helpers named **`recordForceBehaviorNoCheck`**
  and **`recordForceBehaviorRecordOnceNoCheck`** (both carry a `SLOWASSERT` on
  the following opcode); `ldvar_super_` calls `recordForceBehaviorNoCheck`; the
  generic `recordForceBehavior` recognizes the `leaf_publish_` family. The
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

**Changes landed since (2026-09-02 → 2026-09-14).**

14. **Inner-node dirty gate given an owner** (superseded by 31; history in
    §2B.5.1). The suppress/notify
    handshake spans two instructions and the flag was global to the Code, so a
    nested activation landing in that window consumed notifications meant for an
    outer one — `a + f(...)` inside `f` lost the outer activation's type, while
    `f(...) + a` did not, making recorded observations depend on operand order.
    Fixed by stamping the flag with the arming activation: first setter wins,
    only the owner clears, and the skip path never consults the stamp (it must
    not — that would delete the ≈100% inner-node suppression). Verified on both
    orderings, on `binarytrees`' `check()`, and against the full benchmark suite;
    stats byte-identical, so recording work is unchanged. Costs +0.13% to +0.23%
    of executed instructions. **Open:** a stamp leaked by a non-local exit
    disables suppression for that node permanently — over-records, never
    under-records; the stack-direction reclaim is not implemented.

**Changes landed since (2026-09-14 → 2026-09-22).**

15. **Complex assignment made a type barrier on all three routes** (§2A.4).
    Only subassignment was guarded; replacement functions (`dim<-`, `names<-`,
    `attr<-`, `levels<-`, `dimnames<-`) and every bail-out to GNU R's `<-`
    (`class<-`, `slot<-`, `$<-`, `@<-`, nested LHS) stored without reseating the
    def, so a later read was subsumed against the pre-assignment value.
    Symptoms: `dim(x) <- c(2,2); x` reported `double ()` where baseline records
    `double (a)`, and `class(x) <- "k"; x` reported `object = false` for an
    object — the flag that gates dispatch. The six bail exits now funnel through
    one `bailToGnuR` helper, and the invalidated name is the **root of the LHS
    chain**, matching `collectAssignedVars` so `bumpSeen` stays in step with the
    pre-scan's `expected` counts. Verified against baseline on `dim<-`,
    `dimnames<-`, `names<-`, `class<-`, `attr<-`, `levels<-`, `x$a <-`,
    `dim(a$b) <-`, `names(a$b) <-` and `dim(x)[1] <-`; full suite passes. No
    measurable cost on the benchmarks, none of which use replacement functions
    in hot code.

**Changes landed since (2026-09-22 → 2026-09-24).**

16. **Second reentrancy hole closed: the shared signature** (superseded by
    31; history in §2B.5.1). The
    owner-stamp protects a flag that *has* been armed; it does
    nothing for a flag that is **never armed**, because `lastSig` — which
    decides whether to arm — is itself shared across activations. An
    interleaved activation can rewrite it so that each of an outer
    activation's operands compares "unchanged" against a write the outer never
    made, and the pair they form is recorded by nobody.

    Fixed by stamping `lastSig` with a count of live activations **of this
    Code** (`Code::liveDepth`) and taking the early-out only when
    `lastSigDepth <= depth`. Three things were load-bearing and each was
    measured, not assumed:
    - **Depth, not frame address.** An address distinguishes *sequential*
      activations too, so suppression became a property of the caller's shape:
      `{ f(); (function() f())(); }` in a loop armed **10,000 / 10,000** times
      with an address stamp and **1 / 10,000** with depth.
    - **`<=`, not `==`.** `==` makes every new recursion depth re-arm:
      recursion to depth 20 recorded 20,501 of 30,500 executions under `==`
      and **22** under `<=`. (`<` would break the flat case outright — it all
      runs at depth 1 and `1 < 1` is false.)
    - **Asymmetric clamp.** The 6-bit depth must saturate, and guarding that
      with explicit tests cost **+0.6%** on mandelbrot (two extra conjuncts on
      a comparison it runs ~126M times per iteration). Saturating writer→63 and
      reader→0 makes the ordinary comparison fail safe in both directions, so
      the test is three comparisons — fewer than the address version's four.

    `ObservedValues` drops **24 → 16 bytes** (the depth reuses spare bits in
    the `dirty` byte). Versus the address stamp: binarytrees **−4.23%**,
    storage **−0.91%**, mandelbrot −0.17%, nbody +0.00%, fasta +0.27%. All
    three witnesses and the full benchmark suite pass. **Superseded:** the
    `depthAnchor` self-correction assumed only errors skip the decrement;
    ordinary `return()` inside a loop does too, see entry 18.

**Changes landed since (2026-09-24 → 2026-09-29).**

17. **`hasDeps` bit** (`b6ef9156`; superseded by 35). Gates `markRelatedDirty`'s walk of
    the reverse-dependency map, which is empty for ~97% of slots. Free (spare
    bit, no size change), measured at −0.001% instructions on nbody, i.e. no
    measurable effect. Also stops deserialized feedback from indexing an empty
    `noRecordSourceToDeps_`.

18. **`liveDepth` leak on non-local exits fixed** (`69c05f03`; superseded by
    31, `liveDepth` no longer exists). The
    depth was restored only at `eval_done`, so `return()` inside a loop or a
    promise (`return_`), `break`/`next` inside a promise, and the trampolined
    `endloop_` all leaked one level per exit. No recursion needed: a flat loop
    of calls climbed by one per call (200 calls → depth 200) because each call
    reuses the same frame address and the anchor test did not fire. Past 31 the
    Code saturates and recordless is off for it permanently: over-recording,
    feedback still correct. A static scan finds this saturating `gcd`,
    `primes` and `pidigits` in the suite (`fannkuch` marginally); earlier
    recordless numbers for those three were measured in that state. Fixed by
    snapshotting the depth where the closure and loop contexts are opened
    (`LiveDepthGuard`) and restoring on landing, after `endloop_`, and via R's
    `cend` hook for pass-through, plus a raise-site restore in `return_` for
    promises. Anchor test left strict. Costs +0.01% to +0.16% instructions.
    **Open:** promise Codes abandoned by a longjmp raised inside them (and
    deopt, once per deopt) still leak; over-records only. Suite impact not yet
    confirmed dynamically.

**Changes landed since (2026-09-29 → 2026-10-13).**

19. **Third capture hole closed: promises of the outer function** (§2A.1.4).
    Captures handed to an inner closure only need to be *controlled*, not
    immutable, because none of the enclosing function's bytecode runs while
    the closure executes (§2A.1.2). Promises the enclosing function created
    break that: `run(a <- "x", g)` or a default argument `d = (a <- "x")`
    assigns in its frame whenever forced, possibly between two reads of `a`
    inside `g`, and `g`'s second read was NoRecord against the first —
    reproduced on both routes, losing the `character` observation. Fixed by a
    function-wide `promiseAssigned_` pre-scan (body promise arguments plus
    default expressions) subtracted from both capture sets in
    `computeCapturesForInner` (`1bb94569`). The "zero changes across 656
    functions" first reported for it was measured invalidly — standalone
    compilation hands inner functions no captures — and has since been
    re-measured in context: still zero (§2A.1.4).

20. **Inherited route of the same hole closed** (§2A.1.4, from external
    review). The fix in 19 filtered `f`'s *own* formals and locals but not the
    captures `f` passes on from further out: `x` from `h` reached `g` through
    `f`, and `f`'s promise `g(x <- "a")` created a shadowing `x` in `f`'s frame
    between `g`'s two reads. Fixed in `finalize`:
    $O(f) = C(\mathit{enc}(f)) \setminus (F_f \cup P_f)$. A stricter "drop on
    any binding" rule was rejected after costing six NoRecords in
    `prg/cholesky.R`. In-context suite diff against the previous commit: zero
    changes.

21. **Rule 0 covers default arguments** (§2A.1.1, same review). The rule-0
    set ("assigned in a promise → never optimize") was built from the body's
    AST only, so a default argument assigning into the frame was invisible to
    the function's own reads: `function(d = (a <- "x")) { a <- id(1L); d; a }`
    subsumed the last `a` while it read `"x"`. `push` now merges the
    function-wide `promiseAssigned_` into the body's set. Controls unaffected;
    in-context suite diff: zero changes.

22. **Fall-through to the global environment closed** (§2A.1.5, external
    review). A local of `f` was handed to inner closures as controlled even if
    the closure could run before `f` bound it; the lookup then reached the
    global environment, which a called function rebound mid-loop, and a
    RecordOnce read missed a type. A body-local now enters `controlled` only if
    its definition dominates the closure's creation point. Costs 7 loads: one
    `n.states` read in Qlearn (inherent to the rule) and cholesky's six `U`
    reads (recoverable by fixing the "most recent def only" imprecision).

23. **`switch` with a `...` arm handled by rule 0** (§2A.1.1, found while
    checking an external-review question). With a `...` alternative the
    compiler calls the `switch` special instead of inlining it, so an
    assignment in an arm ran in our frame with no `stvar_`, while the promise
    scan still treated `switch` as inline: `switch(k, a = x <- id("s"), ...); x`
    subsumed the last `x` against an earlier def. The scan now treats such a
    `switch` as non-inline. The review's actual question — are `&&`/`||`
    operands and inline `switch` arms branches? — checked out: both are
    wrapped in `enterBranch`/`exitBranch`. Noted as open precision work: `(`,
    `&&`, `||` are scanned as promises although compiled inline (costs e.g.
    fannkuch's `kk`). In-context suite diff: zero changes.

24. **`(`, `&&`, `||` treated as inline by the promise scan** (`6f5cfda8`,
    §2A.1.1), in exactly the shapes the compiler inlines. Sound by witnesses;
    no suite effect, because the remaining assignments also sit inside
    comparison/arithmetic operators that are still scanned as promises.
    Widening to those operators needs a per-operator check for slow-path
    promise fallbacks.

25. **`while` condition scanned for loop-carried stores** (§2A.4.1, found
    while checking an external-review question). Only the body was pre-scanned,
    so a condition that re-assigns its variable (`while ({ k <- k - 1.5;
    k > 0 })`) had its read subsumed against the pre-loop def while it saw the
    condition's own stores. `compileWhile` now scans the condition too.
    Controls unaffected; in-context suite diff: zero changes.

26. **Rule 2 uses the dominating def** (`524907e2`, §2A.4.1). It took the
    loop-vetoed `findReachingDef`, so `while (…) { a <- f(); a; a <- g() }`
    recorded the read every iteration because of the later `a <- g()` — which
    cannot reach it: post-dominance already confines rule 2 to a def in the
    read's own scope, executed before it on every iteration. **That argument
    holds for loop bodies only; `524907e2` was unsound for `while`-condition
    reads** — see entry 27. Its reported gains (mean 8.94% → 9.81%, `sieve1`
    +14.2 pp, …) are withdrawn.

27. **`while` condition veto** (§2A.4.1, external review, uncommitted on top of
    `4862cbc7`). A `while` condition compiles in the enclosing scope, so a
    condition read post-dominated a pre-loop def: `x <- id(1L); while (x < 5)
    x <- x + 2.5` was NoRecord with `integer`-only feedback (and #25's case
    regressed). Rule 1 had the same hole even before `524907e2`. Now both rules
    are skipped when the read is in a `while` condition whose loop assigns the
    name; the check walked outward through every loop currently in its
    condition, which mattered only for a loop written inside a condition block
    (replaced by rule 0, entry 30).
    Corrected corpus effect of `524907e2` + veto vs `02c05117`: mean NoRecord
    8.94% → 9.07%, `fannkuchredux_naive` +5.27 pp; 61 loads record → NoRecord.
    Committed as `9fb4e4d4`. On areWeFast mandelbrot, bytecode and record
    stats (1 and 15 iterations) are byte-identical to `4862cbc7`, as are
    instructions per steady-state iteration; the wall-time gap seen there
    flipped sign between runs (shared machine), i.e. noise.

28. **Reconstruction leaves unexecuted `FBValue` sites `unknown`** (`8fccf0af`,
    §4.6, external review). `value` is now written only when the slot's type
    record is non-empty.

29. **Operands outside the signature domain keep re-arming their parent**
    (§2C.4, external review, uncommitted). A RecordOnce leaf, or a NoRecord
    use whose source ran less often, stopped notifying, so `x + 1L` on an
    object whose `+` method changes its result type recorded `integer` only.
    Fix A: a RecordOnce leaf does not set its once-bit while its value has
    `SigAlwaysDirty`. Fix B1: a `SigAlwaysDirty` source pins its NoRecord
    dependents' parents permanently. Runtime-only. Paper corpus (43
    programs, stats builds, before vs after): every CSV byte-identical, all
    runs correct — no corpus site hits either case, so the fixes cost no
    recording there; any timing difference is code layout.

30. **Condition sites stop being sources after the loop; the condition walk
    is gone** (§2A.4.1, external review point 5(a), uncommitted). A store or a
    recorded read in a `while` condition kept the enclosing scope and was a
    rule-2/rule-1 source for a use after the loop, though it ran on every
    check: `while (x <- f()) NULL; x` subsumed the last `x` against the store
    (precision loss, `double, integer` for an `integer`-only use).
    `exitWhileLoop` makes each name the loop assigns lose its sources at loop
    exit — the def stays as a `kNoSlot` barrier, its recorded reads are
    dropped — so reads before and inside the loop, and RecordOnce in later
    loops, are unchanged. The outward walk of the veto is replaced by rule 0
    for every name of a `while` whose condition contains a loop; the veto now
    checks only the innermost loop. Paper corpus vs `b231d619`: stats CSVs and
    the bytecode of all 710 benchmark functions byte-identical.

**Changes landed since (2026-11-03 → 2026-11-09), branch
`recordLessNew2-expressionsNewSchema-sigPair`.** Entries 14, 16, 17, 18 and the
B1 half of 29 are superseded by 31.

31. **Inner nodes compare operand pairs; current signatures per activation**
    (`e995aa09`, §2B.2, §2B.5). Each operand publishes its signature into the
    running activation's `scratch` array; each inner node compares its two
    positions, as one `uint16_t`, with the pair it last recorded (`tuple`,
    shared). Removes `dirty`, the owner stamp, `liveDepth` with its clamp and
    exit guards, and pinning; the reentrancy witnesses of §2B.5.1 and external
    review point 2 pass with no protocol. Inner records: recursion witness 22 →
    3 of 30,500; `gcd` 1.30M → 61; `binarytrees` ≈37k → ≈50.

32. **An operator's operands are exactly its operands** (`b070b332`, §2B.4,
    §2C.5). `finishNodeForSlots` reduces every call to at most one pending slot
    or `kOpaqueOperand`: fixed-signature constructs count as constants,
    `(`/`{`/assignments are transparent, a unary operator around a recorded
    operand or any other unrecorded construct (`if`, `switch`, …) is opaque and
    makes the operator an always-recording leaf. Closes the
    `(if (c) a else b) + d` hole, where a branch flip changed the operand while
    every published signature stayed the same (present on the `dirty` branch
    too). `registerSlot` asserts ≤ 2 operands. Corpus vs `6465ef10`: 278 of
    70,752 record opcodes changed kind; record counts changed in
    `fannkuchredux` (+362,880 each version), `pidigits` (+248,185) and `nbody`
    (+3/+1) only.

33. **Merge** of `recordLessNew2-expressionsNewSchema` (`1f41af40`), resolved in
    favour of 32 wherever the two touched the slot tree.

34. **Hot-path tightening** (`0d2b174b`): two positions per inner node always
    (no arity in the compare), `kNoTuple` instead of a validity bit,
    `operandsUnchanged` as a single 16-bit `memcpy` compare, `publish` without
    a slot index. Opcodes and record stats byte-identical.

35. **Flat dependents list** (`bcd013f3`, §2C.2). `depPositions_` holds each
    source's dependents' *positions* contiguously, indexed from the slot by
    `depsBegin` / `depsCount` (in former padding); only dependents with a
    position are listed. Replaces `noRecordSourceToDeps_` and `hasDeps`; the
    loop stays inline behind the `depsCount` test (dependents occur in 8.71% of
    publishes, 30% in mandelbrot). Also removes the `RL_SLOTDBG` debug hook.
    Opcodes of all 710 corpus functions and record stats of all 43 programs
    byte-identical to `0d2b174b`. Strictly less runtime work; the only
    additions are compile-time (one extra pass in `buildNoRecordReverseMap`).

36. **`publish` reads the dependents count once** (`06f8a4e5`, §2C.2). The
    byte stores into `scratch` may alias the slot, so `depsCount` was reloaded
    on every loop iteration; now five instructions per dependent. Bytecode of
    all 710 corpus functions and record stats of all 43 programs unchanged.

37. **`notify` renamed to `publish`** (`a3181731`), C++ only:
    `record_type_leaf_publish_[once_]`, `record_type_inner_publish_`,
    `record_type_leaf_publish`, `record_type_inner_publish_when_changed`, stats
    counters `innerPublishRec` / `Skip` / `Should`. The stats row label stays
    `inner_notify`, because `recordless/analysis/helpers.R` and existing data
    key on it. Record stats byte-identical on all 43 programs.

38. **RecordOnce publishing leaves set their bit unconditionally**
    (`3a04a9f5`, §2C.4). Outside the domain the published 0 stays in `scratch`
    for the activation, so the parent keeps recording without the leaf
    re-recording the same binding. Corpus record stats byte-identical (no
    RecordOnce leaf there leaves the domain); the `f1` witness drops its leaf
    records 15 → 6 with inner-node counts unchanged.

All build clean and every checked benchmark produces correct results.

To collect stats: configure with `cmake -DRECORD_STATS=ON .` in the build
directory (it defines `RIR_RECORD_STATS` everywhere), rebuild, run; **set it
back to `OFF` for any timing.**

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
3. **inner nodes by opcode:** `inner (standalone)`, `inner_notify` (the
   `record_type_inner_publish_` opcode; the label kept its old name) — here
   "skipped" means *suppressed* (the elision), not gated: this activation's
   operand pair equals the pair last recorded (§2B.2), so the result is one
   already absorbed. Re-armable: a node goes back to being suppressed as soon
   as its operands repeat a recorded pair.
   Printed under table 2 is a non-row line, **"of all N records, M updated
   nothing (signature unchanged)"** — the share of records that ran but whose
   early-out found nothing to do. It *overlaps* the recorded counts rather than
   partitioning them, and only the `_publish_` paths can early-out (plain
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

- `rir/src/interpreter/interp.cpp` — `evalRirCode`; the `fired` and `scratch`
  arrays (and `sharedScratch` for `loopTrampoline`); the
  `recordFbAtSlot` / `recordForceBehavior*` lambdas; all `record_type_*` and
  `ldvar*` handlers.
- `rir/src/runtime/TypeFeedback.h` — `ObservedValues` (flags; `doRecord` vs.
  `doRecordAndSign`, the latter carrying the signature and the early-out;
  `record` / `recordInner` / `recordInnerAndSign`; `signatureOf` and
  `SigAlwaysDirty`; `posPlus1` / `tupleBase` / `tuple`, `operandsUnchanged`,
  `storeTuple`), `TypeFeedback`
  (`record_type_*` methods, `publish`, `depPositions_`, `typeDeps_`,
  `buildNoRecordReverseMap`, `ForceBehaviorKind`).
- `rir/src/runtime/Code.h` — `innerScratchSize` (the per-activation operand
  array's size, serialized with the Code).
- `rir/src/bc/Compiler.cpp` — `compileGetvar`, `emitRecordTypeForVar`,
  `finishNodeForSlots` / `registerSlot` (the slot tree),
  `setTypeFeedbackParents` (the post-pass that assigns operand positions and
  specializes opcodes), once-bit assignment & clearing; the def-slot stamp
  (`recordTypeForSlot` / `recordTypeOnceForSlot` — the single place a record is
  built and stamped — plus `noteValueRecord[At]` and `valueRecordSlotHere`).
- `rir/src/bc/CodeContext.h` — the per-Code-object compile state that must not
  leak across a Code boundary: `slotsStack`, `slotsRecorded` and `valueRecord`
  (§2C.5).
- `rir/src/bc/CodeStream.h` — `lastValueInstructionPos()` and `isValueNeutral()`,
  which is how the def-slot check asks "did anything change the value since that
  record?".
- `rir/src/bc/DefUseAnalysis.h` — `classifyUse` (RecordAlways/RecordOnce/NoRecord
  + `ForceBehaviorKind`), dominance/postdominance conditions, `scopeStillOpen`.
- `rir/src/bc/insns.h` — opcode definitions (the value-type record family must
  stay contiguous for the stats range-check).
- `rir/src/bc/BC_inc.h` — once-immediate pack/unpack + gate macros.
- `rir/src/interpreter/record_stats.{h,cpp}` — `RIR_RECORD_STATS` instrumentation.
