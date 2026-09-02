#include "bc/Compiler.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "Rinternals.h"
#include "bc/BC.h"
#include "bc/CodeStream.h"
#include "bc/CodeVerifier.h"
#include "bc/CompilerCFG.h"
#include "bc/DefUseAnalysis.h"
#include "bc/LoopScopeGuards.h"
#include "bc/recordless.h"
#include "interpreter/cache.h"
#include "interpreter/interp.h"
#include "interpreter/interp_incl.h"
#include "interpreter/safe_force.h"
#include "runtime/TypeFeedback.h"
#include "simple_instruction_list.h"
#include "utils/Pool.h"

#include <set>
#include <stack>

namespace rir {

namespace {

static bool isConstant(SEXP exp) {
    // Dispatch on the current type of AST node
    switch (TYPEOF(exp)) {
    case LANGSXP:
    case SYMSXP:
    case PROMSXP:
        return false;
    default:
        return true;
    }
}

static bool containsLoop(SEXP exp) {
    if (TYPEOF(exp) != LANGSXP)
        return false;

    auto fun = CAR(exp);
    auto args_ = CDR(exp);

    if (TYPEOF(fun) != SYMSXP) {
        return false;
    } else if (fun == symbol::Repeat) {
        return true;
    } else if (fun == symbol::While) {
        return true;
    } else if (fun == symbol::For) {
        return true;
    }

    RList args(args_);
    bool res = false;
    for (RListIter e = args.begin(); e != args.end(); ++e) {
        res = res || containsLoop(*e);
    }
    return res;
}

class CompilerContext {
  public:
    using LoopContext = rir::LoopContext;
    using CodeContext = rir::CodeContext;

    using PromiseContext = rir::PromiseContext;

    std::stack<CodeContext*> code;
    CodeContext* mainBodyCtx_ = nullptr;

    CodeStream& cs() { return code.top()->cs; }
    DefUseAnalysis& defUseAnalysis() { return code.top()->defUseAnalysis; }
    const DefUseAnalysis& defUseAnalysis() const {
        return code.top()->defUseAnalysis;
    }
    // Expression-tree levels of the Code object being compiled. Scoped to that
    // Code object (see CodeContext::slotsStack), so a promise body can never
    // register into the level its caller left open.
    std::stack<std::vector<uint32_t>>& slotsStack() {
        return code.top()->slotsStack;
    }

    FunctionWriter& fun;
    Preserve& preserve;
    TypeFeedback::Builder typeFeedbackBuilder;
    CompilerCFGBuilder cfgBuilder;
    uint32_t recordTypeOncePromiseBitmapSize = 0;

    // Formals + body-assigned variables (minus inner-<<--assigned ones).
    // Populated once in Compiler::finalize() before any context is pushed.
    std::unordered_set<SEXP> functionLocalOrParam_;

    // Variables from enclosing scopes in our "realm": when a local def has not
    // run, ldvar falls through to a controlled env, not to global. Superset
    // of outerImmutable_. Enables RecordOnce. Names that are also locally
    // assigned by formals of this function are excluded (formal completely
    // shadows the outer name).
    std::unordered_set<SEXP> outerControlled_;

    // Strict subset of outerControlled_: values that truly do not change during
    // this function's lifetime. Names locally body-assigned here are excluded.
    // Reserved for future cross-invocation optimizations.
    std::unordered_set<SEXP> outerImmutable_;

    // Pre-scan results retained for use at inner-function call sites when
    // computing the `safeForInner` capture set to hand to nested compilations.
    std::unordered_set<SEXP> formalNames_;
    std::unordered_map<SEXP, int> bodyAssignedCount_;
    std::unordered_set<SEXP> innerSuperAssigned_;
    std::unordered_set<SEXP> forLoopVars_;

    // // Variables that appear in pure-read (value) position in the function
    // body.
    // // Used to gate the post-subassign record_type_ emission.
    // std::unordered_set<SEXP> readVars_;

    // Expression-tree child->parent edges accumulated over the whole function.
    // Function-wide on purpose, unlike CodeContext::slotsStack: slot indices
    // are allocated from the single per-function typeFeedbackBuilder, so they
    // are unique across every Code object and cannot collide here, and the
    // consumer (setTypeFeedbackParents) needs the union of all edges because
    // there is one TypeFeedback per function. Scoping slotsStack per Code
    // object is what guarantees no edge in here ever crosses a Code boundary.
    std::map<uint32_t, uint32_t> parents;
    std::vector<Code*> allCodes_;
#ifdef RIR_RECORD_STATS
    // Stats only: slot indices emitted via recordTypeUntracked(), handed to the
    // TypeFeedback at finalize so the record_type_ handler can attribute them
    // to the "untracked" row rather than to RecordAlways leaves.
    std::set<uint32_t> untrackedStatsSlots_;
#endif

    CompilerContext(FunctionWriter& fun, Preserve& preserve)
        : fun(fun), preserve(preserve) {}

    ~CompilerContext() { assert(code.empty()); }

    bool inLoop() const { return code.top()->inLoop(); }

    DefUseAnalysis::UseClassification classifyUse(SEXP name) const {
        return defUseAnalysis().classifyUse(name);
    }

    LoopContext& loop() { return code.top()->loops.top(); }

    bool loopNeedsContext() {
        assert(inLoop());
        return code.top()->loops.top().context_needed_;
    }

    bool loopIsLocal() { return code.top()->loopIsLocal(); }

    BC::Label loopNext() { return code.top()->loopNext(); }

    BC::Label loopBreak() { return code.top()->loopBreak(); }

    void pushLoop(BC::Label next_, BC::Label break_) {
        code.top()->loops.emplace(next_, break_);
    }

    void pushFakeLoop() {
        code.top()->loops.emplace(-1, -1);
        code.top()->setContextNeeded();
    }

    void popLoop() { code.top()->loops.pop(); }

    void push(SEXP ast, SEXP env) {
        std::unordered_set<SEXP> promiseAssigned;
        if (Compiler::recordLess_Leaf_Enabled)
            DefUseAnalysis::collectPromiseAssignedVars(ast, promiseAssigned);
        DefUseAnalysis dua(&functionLocalOrParam_, &outerControlled_,
                           &outerImmutable_, &formalNames_,
                           std::move(promiseAssigned));
        code.push(new CodeContext(ast, fun, code.empty() ? nullptr : code.top(),
                                  std::move(dua)));
        if (!mainBodyCtx_)
            mainBodyCtx_ = code.top();
    }

    bool isInPromise() { return pushedPromiseContexts > 0; }

    void pushPromiseContext(SEXP ast) {
        pushedPromiseContexts++;
        std::unordered_set<SEXP> promiseAssigned;
        if (Compiler::recordLess_Leaf_Enabled)
            DefUseAnalysis::collectPromiseAssignedVars(ast, promiseAssigned);
        DefUseAnalysis dua(&functionLocalOrParam_, &outerControlled_,
                           &outerImmutable_, &formalNames_,
                           std::move(promiseAssigned));
        // Inherit the enclosing loop depth so the first use of a local/param
        // inside a promise compiled within a loop gets RecordOnce rather than
        // RecordAlways.
        // if (!code.empty())
        //    dua.loopDepth_ = code.top()->defUseAnalysis.loopDepth_;
        code.push(new PromiseContext(
            ast, fun, code.empty() ? nullptr : code.top(), std::move(dua)));
    }

    struct CaptureInfo {
        std::unordered_set<SEXP> immutable;
        std::unordered_set<SEXP> controlled;
    };

    // Build the two capture sets to hand off to a function literal being
    // compiled at the current point of this function's body.
    //
    // `controlled`: everything in our realm — the inner function can rely on
    // fallthrough going to a controlled env rather than global.
    // `immutable`: strict subset — values that won't change during the inner
    // function's lifetime (enables future cross-invocation optimizations).
    CaptureInfo computeCapturesForInner() const {
        CaptureInfo result;
        const auto& dua = code.top()->defUseAnalysis;

        // Carry-through: outer captures remain valid for the inner function.
        for (SEXP s : outerImmutable_) {
            result.immutable.insert(s);
            result.controlled.insert(s);
        }
        for (SEXP s : outerControlled_)
            result.controlled.insert(s);

        // Own formals: always bound at call time → controlled, UNLESS
        // <<-escaped from an inner function. A `<<-` in a nested closure skips
        // that closure's own frame and lands in ours, so the binding can be
        // retyped between two uses in a *sibling* closure with no stvar of ours
        // in between — the sibling would subsume the second use against the
        // first and narrow the feedback. Same guard as the body-locals loop
        // below; together they give (formals ∪ body-locals) \
        // innerSuperAssigned. Immutable additionally requires the binding never
        // be body-assigned.
        for (SEXP f : formalNames_) {
            if (innerSuperAssigned_.count(f))
                continue;
            result.controlled.insert(f);
            if (!bodyAssignedCount_.count(f))
                result.immutable.insert(f);
        }

        // Own body-locals: live in the current function's local env →
        // controlled. Immutable only if assigned exactly once, not a for-loop
        // variable, not
        // <<-escaped, and the single def dominates this compilation point.
        for (auto& kv : bodyAssignedCount_) {
            if (!innerSuperAssigned_.count(kv.first))
                result.controlled.insert(kv.first);
            if (kv.second == 1 && !forLoopVars_.count(kv.first) &&
                !innerSuperAssigned_.count(kv.first) &&
                dua.hasDominatingDef(kv.first))
                result.immutable.insert(kv.first);
        }

        return result;
    }

    Code* pop() {
        // compileExpr's LANGSXP case is the sole owner of the push/pop balance,
        // and it is balanced within a single call, so every level opened while
        // compiling this Code object has been closed again. A non-empty stack
        // here means some node's children escaped their Code object.
        assert(code.top()->slotsStack.empty() &&
               "expression-tree levels leaked across a Code boundary");
        Code* res = cs().finalize(0, code.top()->loadsSlotInCache.size());
        res->recordTypeOnceCount =
            (uint16_t)code.top()->recordTypeOnceBitmapSize;
        if (code.top()->isPromiseContext())
            pushedPromiseContexts--;
        delete code.top();
        code.pop();
        allCodes_.push_back(res);
        return res;
    }

    void emitError(const char* msg, SEXP ast) {
        cs() << BC::push(R_TrueValue) << BC::push(Rf_mkString(msg))
             << BC::callBuiltin(2, ast, getBuiltinFun("stop")) << BC::return_();
    }
    void emitWarning(const char* msg, SEXP ast) {
        cs() << BC::push(R_TrueValue) << BC::push(R_FalseValue)
             << BC::push(R_FalseValue) << BC::push(Rf_mkString(msg))
             << BC::callBuiltin(4, ast, getBuiltinFun("warning")) << BC::pop();
    }

    void popNodeForSlots() {
        // Always pop the inner vector for this LANGSXP. If it still has
        // unhandled child slots (non-profiled call — no recordType(true) was
        // emitted), propagate them to the enclosing level so they are not
        // orphaned and don't corrupt the stack. At the outermost level of a
        // Code object there is no enclosing level and they are dropped — which
        // is the correct behaviour at a Code boundary: they belong to no node
        // above.
        auto& stack = slotsStack();
        auto slots = std::move(stack.top());
        stack.pop();
        if (!slots.empty() && !stack.empty()) {
            for (auto s : slots)
                stack.top().push_back(s);
        }
    }

    void pushNewNodeForSlots() { slotsStack().push(std::vector<uint32_t>()); }

    void registerSlot(uint32_t slotIdx, bool isParent) {

#ifdef RECORDLESS_EXPTREE_DEBUG
        std::cerr << "\n slotsStack size: " << slotsStack().size() << "\n";
        std::cerr << "\n registerSlot " << slotIdx << "\n";
#endif

        auto& currentSlots = slotsStack().top();
        if (!isParent) {
            currentSlots.push_back(slotIdx);
        } else {
            // Link all current children to this parent slot, then replace the
            // current level with just this slot. Do NOT pop — popNodeForSlots()
            // in compileExpr is the single owner of the push/pop balance.
            for (auto child : currentSlots) {
                parents[child] = slotIdx;
            }
            currentSlots.clear();
            currentSlots.push_back(slotIdx);
        }
    }

    void setTypeFeedbackParents(TypeFeedback& tf) {
        // An edge is only usable if the parent's index fits the biased 16-bit
        // parentPlus1. An unrepresentable edge must be DROPPED, not truncated:
        // the parent would otherwise be classified as an inner node (starts
        // clean, records only once a child marks it dirty) with no child able
        // to reach it, so it would never record at all. Dropping the edge
        // leaves it a leaf, which always records — conservative and sound.
        // Needs >65534 type slots in one function, so effectively unreachable.
        std::map<uint32_t, uint32_t> usable;
        for (const auto& kv : parents)
            if (ObservedValues::canReference(kv.second))
                usable.emplace(kv.first, kv.second);

        for (const auto& kv : usable)
            tf.types(kv.first).setParent(kv.second);

        // parentSlots: slots that have children (not leaves).
        // childSlots:  slots that have a parent (not roots).
        // Leaf-vs-inner is not stored on the slot — it is carried by the
        // opcode the loop below patches in. An inner node starts clean and
        // records nothing until a child signals a changed value; the first
        // execution always signals, since a child's initial signature is 0 and
        // 0 never compares equal.
        std::set<uint32_t> parentSlots;
        std::set<uint32_t> childSlots;
        for (const auto& kv : usable) {
            childSlots.insert(kv.first);
            parentSlots.insert(kv.second);
        }

        // Source slots: those a NoRecord (elided) use depends on AND whose
        // dependent has a parent to un-suppress. When such a slot records an
        // object it must propagate to its dependents' parents (via
        // record_type_leaf_notify_'s notifyRelatedNodes). A source whose
        // dependents are all parentless leaves (e.g. `a <- f(); a`, where the
        // elided read of `a` is a standalone leaf) has nothing to propagate to,
        // so it stays a plain record_type_ rather than a specialized opcode.
        std::set<uint32_t> sourceSlots;
        for (size_t d = 0; d < tf.types_size(); d++)
            if (tf.hasTypeDep(d) && childSlots.find(d) != childSlots.end())
                sourceSlots.insert(tf.typeDep(d));

        // Specialize each record_type_ / record_type_once_ by (isLeaf, isRoot,
        // isSource). Inner nodes come only from record_type_ (RecordOnce only
        // classifies leaves). Leaf-with-parent + no source keeps the original
        // opcode (record_type_ / record_type_once_). The packed immediate is
        // preserved; only the opcode byte changes.
        for (Code* code : allCodes_) {
            Opcode* pc = code->code();
            Opcode* end = code->endCode();
            while (pc < end) {
                Opcode op = *pc;
                bool once = op == Opcode::record_type_once_;
                if (op == Opcode::record_type_ || once) {
                    uint32_t imm;
                    memcpy(&imm, pc + 1, sizeof(imm));
                    uint32_t slot = once ? RECORD_TYPE_ONCE_SLOT_IDX(imm) : imm;
                    bool isLeaf = parentSlots.find(slot) == parentSlots.end();
                    bool isRoot = childSlots.find(slot) == childSlots.end();
                    bool isSrc = sourceSlots.find(slot) != sourceSlots.end();
                    if (!once && !isLeaf) {
                        // Split inner nodes on whether they notify anything: a
                        // standalone inner node (root, no deps) notifies
                        // nothing; a non-root notifies its parent and a root
                        // source
                        // notifies its dependents — both via record_type_
                        // inner_notify_. Notification is one-time (latched), so
                        // the shared notifier's usually-empty branch is free.
                        bool notifies = !isRoot || isSrc;
                        *pc = notifies ? Opcode::record_type_inner_notify_
                                       : Opcode::record_type_inner_;
                    } else if (isRoot && !isSrc) {
                        // Non-source simple leaf (root + leaf) — and every
                        // untracked record, which is also isLeaf && isRoot &&
                        // !isSrc — stays plain record_type_ / record_type_once_
                        // (doRecord only, no notification).
                    } else {
                        // A leaf that must notify related nodes: a source
                        // (propagate to its NoRecord dependents' parents), a
                        // leaf with a parent (un-suppress its own parent), or
                        // both. record_type_leaf_notify_ handles all three —
                        // notifyRelatedNodes does own-parent + any deps
                        // together (the usually-empty branch is a no-op) — so
                        // there is no separate _dep_ opcode.
                        *pc = once ? Opcode::record_type_leaf_notify_once_
                                   : Opcode::record_type_leaf_notify_;
                    }
                }
                pc = BC::next(pc);
            }
        }
    }

    // Tracked: participates in the expression-tree optimization. Registered in
    // the slot tree, so the post-pass specializes it (inner_/inner_notify_ for
    // inner nodes, leaf_notify_* for leaves that notify a parent and/or
    // dependents). A tracked but parent-less non-source leaf stays plain
    // record_type_ (same as untracked).
    //
    // Degrades to recordTypeUntracked() outside the analysis's domain, so that
    // such records both always record (matching the baseline, which has no
    // suppression anywhere) and are attributed to the "untracked" stats row:
    //   * isInPromise() — recordless does not optimize inside promises, so a
    //     promise body records plainly throughout. emitRecordTypeForVar bails
    //     for the same reason, which is what also suppresses
    //     NoRecord/RecordOnce classification there. Note this guard is only
    //     about *not optimizing* promises: it is no longer what keeps a
    //     promise's leaves out of the enclosing expression's tree, since
    //     slotsStack now lives on the CodeContext. Relaxing the guard
    //     re-enables promise-local trees without reintroducing cross-Code
    //     parenting.
    //   * !mainBodyCtx_ — compiling default formal arguments (themselves
    //     promises, but reached before any main-body context exists).
    //   * leaf optimization off — likewise, all leaves are untracked.
    // In each case the operand leaves are untracked and hold no parent pointer,
    // so a tracked inner node above them could never be un-suppressed: e.g.
    // `a + f(1)` would otherwise suppress the `+` on the strength of the
    // tracked call result while `a` (untracked, possibly an object) had no way
    // to revoke the elision.
    // ---- "value record" tracking -------------------------------------------
    // A def may only reference a feedback slot that provably records exactly
    // the value being stored. The record helpers below stamp the slot they are
    // about to emit, together with where its instruction starts and which scope
    // it is emitted in; valueRecordSlotHere() then decides whether that stamp
    // still describes the value on top of the stack.
    // Lives on the CodeContext, so a promise's stamp is invisible to the
    // enclosing function — see CodeContext::valueRecord.
    CodeContext::ValueRecord& valueRecord() { return code.top()->valueRecord; }

    // Called from a helper *before* the record is streamed, so currentPos() is
    // where it will be written. A stamp that turns out to be inaccurate (a
    // helper whose result is streamed in the middle of a `cs << a << b << c`
    // chain, where C++ leaves the argument evaluation order unsequenced) can
    // only ever make the check below fail, never wrongly succeed — so being
    // approximate here costs an elision, not correctness.
    void noteValueRecord(int slot) {
        valueRecord() = {slot, cs().currentPos(),
                         defUseAnalysis().scopeIdHere()};
    }

    // The one place a value-type record is turned into bytecode: builds the BC
    // and stamps it, for a slot the caller has already allocated. Every helper
    // below funnels through this, so the stamp lives in exactly one place and a
    // new emission site cannot silently forget it.
    //
    // Note this is NOT recordTypeUntracked: that one allocates its own slot and
    // asserts a property about it ("never a def, a source, or an inner-node
    // operand", recorded in untrackedStatsSlots_). The RecordOnce paths in
    // emitRecordTypeForVar have already allocated their slot and registered it
    // as a tree leaf and a use-def, so it is very much tracked.
    BC recordTypeForSlot(int slot) {
        noteValueRecord(slot);
        return BC::recordType(slot);
    }
    BC recordTypeOnceForSlot(int slot, uint32_t bit) {
        noteValueRecord(slot);
        return BC::recordTypeOnce((uint32_t)slot, bit);
    }

    // Same, for a value whose type is already described by an existing slot so
    // that no record instruction is emitted at all — a NoRecord variable read.
    // `pos` is the instruction that pushed the value (the ldvar), which is what
    // the "still the last value-changing instruction" test matches against.
    //
    // Callers pass the *source* slot, not the dep slot allocated for the read,
    // so `b <- a` points straight at whatever recorded `a` rather than adding a
    // hop. Chains of deps do resolve — reconstructFeedback iterates forward and
    // a dep always references an earlier, lower-numbered slot — but staying
    // flat keeps that property from being load-bearing.
    void noteValueRecordAt(int slot, unsigned pos) {
        valueRecord() = {slot, pos, defUseAnalysis().scopeIdHere()};
    }

    // The slot describing the value currently on top of the stack, or kNoSlot
    // when that cannot be established. Two conditions, each catching a failure
    // the other misses:
    //
    //  * the record must be the last VALUE-CHANGING instruction emitted (a
    //    trailing visible_ / ensure_named_ does not count, since the value is
    //    still the recorded one). Otherwise something
    //    after it replaced the value — `x <- -f()` records the call, then
    //    uminus_ produces a different value (and for a logical operand a
    //    different *type*); likewise `!f()`, `is.null(f())`, a constant RHS.
    //
    //  * its scope must still be open. Otherwise the record lies on only one of
    //    several paths — in `x <- if (c) f() else g()` each branch records its
    //    own outcome, and the second one is textually adjacent to the store yet
    //    reached only half the time. A position check alone accepts this, since
    //    labels emit no bytes.
    //
    // Anything unproven gives kNoSlot, so the def carries no slot and later
    // reads of the variable record for real rather than depending on a slot
    // that describes something else.
    //
    // There is deliberately no third check for "was this stamped in the same
    // Code object": the stamp lives on the CodeContext, so a promise's stamp is
    // simply not reachable from here. That matters because both quantities
    // compared above are per-Code-object — `insnPos` indexes this CodeStream
    // and `scopeId` comes from this DefUseAnalysis — so a stamp from another
    // Code object could match by coincidence rather than by meaning.
    int valueRecordSlotHere() {
        const auto& vr = valueRecord();
        if (vr.slot == DefUseAnalysis::kNoSlot)
            return DefUseAnalysis::kNoSlot;
        if (cs().lastValueInstructionPos() != vr.insnPos)
            return DefUseAnalysis::kNoSlot;
        if (!defUseAnalysis().scopeStillOpen(vr.scopeId))
            return DefUseAnalysis::kNoSlot;
        return vr.slot;
    }

    BC recordTypeTracked(bool isParent) {
        if (isInPromise() || !mainBodyCtx_ ||
            !Compiler::isRecordlessLeafEnabled())
            return recordTypeUntracked();
        auto slotIdx = typeFeedbackBuilder.addType();
        if (!slotsStack().empty())
            registerSlot(slotIdx, isParent);
        return recordTypeForSlot((int)slotIdx);
    }

    // An opaque value result: a call return, `[`, `[[`, a `for` result, or a
    // replacement-function result. Its type is NOT a function of its operands,
    // so it is a recording *leaf*, never an elidable inner node — same as
    // recordTypeTracked(false) in that respect.
    //
    // The difference is that it **consumes** the operands pending at this
    // level. What flows to the enclosing expression is this result, not the
    // operands, and the operands say nothing about this result's type. Leaving
    // them pending would let popNodeForSlots propagate them upward and have the
    // *enclosing* inner node adopt them — e.g. in `v[i] + 1`, `v` and `i` would
    // become children of the `+` even though its only operand is `v[i]`. They
    // would then be specialized to record_type_leaf_notify_ and pay the
    // notification check on every execution, and an object-valued `v` would
    // un-suppress the `+` redundantly (the `[` result records the object and
    // notifies anyway). Consuming them leaves them parent-less, so they stay
    // plain record_type_.
    BC recordTypeOpaqueResult() {
        if (isInPromise() || !mainBodyCtx_ ||
            !Compiler::isRecordlessLeafEnabled())
            return recordTypeUntracked();
        auto slotIdx = typeFeedbackBuilder.addType();
        if (!slotsStack().empty()) {
            auto& currentSlots = slotsStack().top();
            currentSlots.clear(); // operands consumed by this operation
            currentSlots.push_back(slotIdx); // this result flows onward
        }
        return recordTypeForSlot((int)slotIdx);
    }

    // Untracked: genuinely outside the analysis — the slot is never a def, a
    // source, or an inner-node operand. Just emits a record_type_ and registers
    // nowhere, so the post-pass leaves it untouched (isLeaf && isRoot &&
    // !isSource). It records on every execution, never notifies a parent, is
    // never suppressed, and is never NoRecord/RecordOnce. Reserved for the few
    // records that are truly internal: the colon (`m:n`) operand casts, the
    // super-assign target read-for-update, and the ldvar fallback while
    // compiling default formal args (no main-body context to analyse).
    // NB: opaque *value results* (call / `[[` / `for` / replacement-fn) are NOT
    // untracked — they are tracked always-record leaves (recordTypeTracked) so
    // they can be defs/sources and so an enclosing inner node can lean on them.
    BC recordTypeUntracked() {
        auto slotIdx = typeFeedbackBuilder.addType();
#ifdef RIR_RECORD_STATS
        untrackedStatsSlots_.insert(slotIdx);
#endif
        return recordTypeForSlot((int)slotIdx);
    }

    // Register a slot that the leaf optimization allocated directly (emitting
    // record_type_ / record_type_once_) as a leaf child in the expression tree,
    // so its parent pointer is wired and its record notifies the parent.
    // Mirrors the registerSlot(false) that recordType() does for the non-leaf-
    // opt path.
    void registerLeafSlot(uint32_t slotIdx) {
        if (!slotsStack().empty())
            registerSlot(slotIdx, /*isParent=*/false);
    }

    BC recordTypeTracked(SEXP name) {
        int slot = typeFeedbackBuilder.addType();
        defUseAnalysis().trackUseDef(name, slot);
        registerLeafSlot((uint32_t)slot); // RecordAlways leaf → tree child
        return recordTypeForSlot(slot);
    }

    unsigned typeSlotCount() const { return typeFeedbackBuilder.typeCount(); }

    // Allocate a slot for a NoRecord use and register its dependency on
    // `sourceSlot` so the type info can be propagated before JIT compilation.
    // Returns the new slot index.
    uint32_t registerNoRecordDep(int sourceSlot) {
        assert(sourceSlot != DefUseAnalysis::kNoSlot &&
               "NoRecord must always reference a valid feedback slot");
        uint32_t slot = typeFeedbackBuilder.addType();
        typeFeedbackBuilder.setTypeDep(slot, (uint32_t)sourceSlot);
        // Register the elided use as a leaf in the expression tree so it gets a
        // parent pointer: the source's propagation enables that parent.

        registerLeafSlot(slot);
        return slot;
    }

    BC recordCall() { return BC::recordCall(typeFeedbackBuilder.addCallee()); }

    BC recordTest() { return BC::recordTest(typeFeedbackBuilder.addTest()); }

  private:
    unsigned int pushedPromiseContexts = 0;
};

struct LoadArgsResult {
    bool hasNames = false;
    bool hasDots = false;
    std::vector<SEXP> names;
    Context assumptions;
    int numArgs = 0;
};

Code* compilePromise(CompilerContext& ctx, SEXP exp);
Code* compilePromiseNoRir(CompilerContext& ctx, SEXP exp);
// If we are in a void context, then compile expression will not leave a value
// on the stack. For example in `{a; b}` the expression `a` is in a void
// context, but `b` is not. In `while(...) {...}` all loop body expressions are
// in a void context, since the loop as an expression is always nil.
void compileExpr(CompilerContext& ctx, SEXP exp, bool voidContext = false);
void compileCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                 bool voidContext);
static constexpr unsigned kNoLdvarCached = (unsigned)-1;
static void emitRecordTypeForVar(CompilerContext& ctx, CodeStream& cs,
                                 SEXP name,
                                 unsigned ldvarCachedPos = kNoLdvarCached);

// EAGER_PROMISE_FROM_TOS is for the special case when the expression has
// already been evaluated: wrap the value at TOS into a promise. This is used in
// particular for the complex assignment: the expression
//    f(x) <- z
// returns z, z must be evaluated first, and z must be passed as en eager
// promise to `f<-` as its last argument.
enum class ArgType {
    PROMISE,
    EAGER_PROMISE,
    RAW_VALUE,
    EAGER_PROMISE_FROM_TOS
};

static void compileLoadOneArg(CompilerContext& ctx, SEXP arg, ArgType arg_type,
                              LoadArgsResult& res);

static void compileLoadArgs(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                            LoadArgsResult& info, bool voidContext,
                            int skipArgs = 0, int eager = 0);

void compileWhile(CompilerContext& ctx, std::function<void()> compileCond,
                  std::function<void()> compileBody, SEXP bodyAst,
                  bool peelLoop = false) {
    CodeStream& cs = ctx.cs();

    BC::Label nextBranch = cs.mkLabel();
    BC::Label breakBranch = cs.mkLabel();
    ctx.pushLoop(nextBranch, breakBranch);

    unsigned beginLoopPos = cs.currentPos();
    cs << BC::beginloop(breakBranch);

    if (Compiler::isRecordlessLeafEnabled()) {
        std::unordered_map<SEXP, int> bodyDefs;
        DefUseAnalysis::collectAssignedVars(bodyAst, bodyDefs);
        ctx.defUseAnalysis().setLoopBodyDefs(std::move(bodyDefs));
        // enterLoopContext: track that we're in a loop (for RecordOnce) but
        // keep the scope stack at the outer scope — so the condition can be
        // classified as NoRecord when a pre-loop def post-dominates it.
        ctx.defUseAnalysis().enterLoopContext();
    }

    // loop peel is a copy of the condition and body, with no backwards jumps
    if (Compiler::loopPeelingEnabled && peelLoop) {
        auto savedDefs = (Compiler::isRecordlessLeafEnabled())
                             ? ctx.defUseAnalysis().saveState()
                             : DefUseAnalysis::DefsSnapshot{};
        compileCond();
        cs << ctx.recordTest() << BC::brfalse(breakBranch);
        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().enterLoopScope();
        compileBody();
        if (Compiler::isRecordlessLeafEnabled()) {
            ctx.defUseAnalysis().exitLoop();
            ctx.defUseAnalysis().restoreState(std::move(savedDefs));
        }
    }

    cs << nextBranch;
    compileCond();
    cs << BC::brfalse(breakBranch);

    if (Compiler::isRecordlessLeafEnabled())
        ctx.defUseAnalysis().enterLoopScope();
    compileBody();
    if (Compiler::isRecordlessLeafEnabled()) {
        ctx.defUseAnalysis().exitLoop();
        ctx.defUseAnalysis().clearLoopBodyDefs();
    }
    cs << BC::br(nextBranch) << breakBranch;

    if (ctx.loopNeedsContext()) {
        cs << BC::endloop();
    } else {
        cs.remove(beginLoopPos);
    }

    ctx.popLoop();
}

void emitGuardForNamePrimitive(CodeStream& cs, SEXP fun) {
    if (!Compiler::unsoundOpts) {
        cs << BC::guardNamePrimitive(fun);
    }
}

// True when `seq` is an AST call to `:`, `seq_len`, or `seq_along` — these are
// the seq forms whose elements all have the same SEXP type, so a for-loop's
// iter var has stable type across iterations.
static bool isRangeBasedSeq(SEXP seq) {
    if (TYPEOF(seq) != LANGSXP)
        return false;
    SEXP fun = CAR(seq);
    if (TYPEOF(fun) != SYMSXP)
        return false;
    return fun == symbol::Colon || fun == symbol::seq_len ||
           fun == symbol::seq_along;
}

// RAII-style helper for range-based for-loop iter-var instrumentation.
// Construction does the "before loop body" work (emit clear placeholder if
// nested, push range-based var, push dynamic-bit tracking entry); finish()
// does the "after loop body" work (patch or remove placeholder using the
// tracked dynamic bit range, pop range-based var).
//
// clearActive    — emit a clear_record_type_once_bits_range_ placeholder before
//                  the loop body when nested. True for ALL loop kinds when
//                  recordLess is enabled, so bits for outer-scope-def variables
//                  are cleared on each outer iteration.
// rangeVarActive — push/pop sym as a range-based for-loop iter var. True only
//                  for `:` / seq_len / seq_along sequences.
//
// Stable bits are excluded from the clear range: when compileGetvar allocates
// a RecordOnce bit, it tells DefUseAnalysis whether the bit is dynamic
// (variable re-assigned in some enclosing loop / range-based iter var) or
// stable. The clear range covers only [first dynamic bit, last+1), so stable
// bits at either end persist for the whole invocation.

/**
 * Try to convert this loop into a C-style for loop. If it fails or must compile
 * a regular loop, it will use the given function.
 */
bool compileSimpleFor(CompilerContext& ctx, SEXP fullAst, SEXP sym, SEXP seq,
                      SEXP body, bool voidContext) {
    if (TYPEOF(seq) != LANGSXP)
        return false;

    auto fun = CAR(seq);
    auto argsSexp = CDR(seq);

    RList args(argsSexp);
    if (fun != symbol::Colon || args.length() != 2) {
        return false;
    }

    // for(i in m:n) {
    //   ...
    // }
    // =>
    // m' <- m
    // n' <- n
    // if (!colonInputEffects(m, n)) {
    //    <regular for>
    // } else {
    //   m' <- colonCastLhs(m')
    //   n' <- colonCastRhs(m', n')
    //   step <- if (m' <= n') 1L else -1L
    //   i' <- m'
    //   while (i' != n') {
    //     i <- i'
    //     i' <- i' + step
    //     ...
    //   }
    // }

    SEXP start = args[0];
    SEXP end = args[1];
    CodeStream& cs = ctx.cs();

    BC::Label skipRegularForBranch = cs.mkLabel();
    BC::Label stepElseBranch = cs.mkLabel();
    BC::Label stepEndBranch = cs.mkLabel();
    BC::Label endBranch = cs.mkLabel();

    // m' <- m
    compileExpr(ctx, start);
    cs << BC::force();
    // n' <- n
    compileExpr(ctx, end);
    cs << BC::force();

    // if (!colonInputEffects(m, n)) {
    cs << BC::colonInputEffects();
    cs.addSrc(seq);
    bool staticFastcase = TYPEOF(start) != LANGSXP && TYPEOF(start) != SYMSXP &&
                          TYPEOF(end) != LANGSXP && TYPEOF(end) != SYMSXP &&
                          isColonFastcase(start, end);
    if (staticFastcase) {
        // We statically know that colonInputEffects is true, so we can
        // just pop the result and don't need to compile the slowcase
        // branch
        cs << BC::pop();
    } else {
        cs << ctx.recordTest() << BC::brtrue(skipRegularForBranch);
        //   <regular for>
        // Note that we call the builtin `for` and pass the body as a
        // promise to lower the bytecode size

        // 1) Finish creating the seq, and add its SEXP as a promise
        // (it's eager but it needs to be a promise to be an arg)
        cs << BC::colon();
        cs.addSrc(seq);
        Code* seqProm = compilePromise(ctx, seq);
        size_t seqPromIdx = cs.addPromise(seqProm);

        // 2) Create a promise with the body
        ctx.pushFakeLoop();
        Code* bodyProm = compilePromise(ctx, body);
        ctx.popLoop();
        size_t bodyPromIdx = cs.addPromise(bodyProm);

        // 3) Add the function, arguments, and call
        Context assumptions;
        assumptions.setEager(0);
        assumptions.add(Assumption::CorrectOrderOfArguments);
        assumptions.add(Assumption::NotTooManyArguments);

        cs << BC::ldfun(symbol::For) << BC::swap()
           << BC::mkEagerPromise(seqPromIdx) << BC::mkPromise(bodyPromIdx)
           << BC::call(2, fullAst, assumptions);
        if (voidContext)
            cs << BC::pop();
        else if (Compiler::profile)
            // `for` result is an opaque value that may be assigned (a def
            // candidate) and may feed an enclosing inner node, so it is a
            // tracked always-record leaf, not untracked.
            cs << ctx.recordTypeOpaqueResult();

        cs << BC::br(endBranch);
        cs << skipRegularForBranch;
    }
    // } else {

    // m' <- colonCastLhs(m')
    cs << BC::swap() << BC::colonCastLhs() << ctx.recordTypeUntracked()
       << BC::ensureNamed() << BC::swap();

    // n' <- colonCastRhs(m', n')
    cs << BC::colonCastRhs() << BC::ensureNamed() << ctx.recordTypeUntracked();

    // step <- if (m' <= n') 1L else -1L
    cs << BC::dup2() << BC::le();
    cs.addSrc(R_NilValue);
    cs << ctx.recordTest() << BC::brfalse(stepElseBranch) << BC::push(1)
       << BC::br(stepEndBranch) << stepElseBranch << BC::push(-1)
       << stepEndBranch;

    // i' <- m' (we just reuse m', but we need to fix the stack as the
    //           following bytecode expects: lhs :: rhs :: step :: ...)
    cs << BC::swap() << BC::pick(2);

    if (Compiler::isRecordlessLeafEnabled())
        ctx.defUseAnalysis().pushForLoopVar(sym);
    // compileSimpleFor always handles `:` — always range-based.
    RangeBasedIterVarScope rangeScope(
        ctx.code.top(), sym,
        /*active=*/Compiler::isRecordlessLeafEnabled());
    ClearableScopeGuard clearScope(ctx.code.top(),
                                   Compiler::isRecordlessLeafEnabled());

    // while
    compileWhile(
        ctx,
        [&cs]() {
            // (i' != n')
            cs << BC::dup2() << BC::ne();
            cs.addSrc(R_NilValue);
        },
        [&ctx, &cs, &sym, &body]() {
            // {
            // i <- i'
            cs << BC::dup();
            if (ctx.code.top()->isCached(sym))
                cs << BC::stvarCached(sym, ctx.code.top()->cacheSlotFor(sym));
            else
                cs << BC::stvar(sym);
            // i' <- i' + step
            cs << BC::pull(2) << BC::ensureNamed() << BC::add();
            cs.addSrc(R_NilValue);
            // ...
            compileExpr(ctx, body, true);
            // }
        },
        body, !containsLoop(body));

    rangeScope.finish();
    clearScope.finish();
    if (Compiler::isRecordlessLeafEnabled())
        ctx.defUseAnalysis().popForLoopVar(sym);

    cs << BC::popn(3);
    if (!voidContext)
        cs << BC::push(R_NilValue) << BC::invisible();
    cs << endBranch;
    return true;
}

// A very conservative estimation if the ast could contain an assignment, or
// subset into sym
static bool maybeChanges(SEXP sym, SEXP ast) {
    if (TYPEOF(ast) != LANGSXP)
        return false;
    if (CADR(ast) == sym)
        return true;
    for (auto s : RList(CDR(ast))) {
        if (maybeChanges(sym, s))
            return true;
    }
    return false;
}

// Inline some specials
// TODO: once we have sufficiently powerful analysis this should (maybe?) go
//       away and move to an optimization phase.
bool compileSpecialCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args_,
                        bool voidContext) {
    // `true` if an argument isn't labeled, or `...`.
    auto isRegularArg = [](RListIter arg) {
        return *arg != R_DotsSymbol && !arg.hasTag();
    };

    RList args(args_);
    CodeStream& cs = ctx.cs();

    // TODO: this is not sound... There are other ways to call remove... What we
    // should do instead is trap do_remove in gnur and clear the cache!
    if (fun == symbol::remove || fun == symbol::rm) {
        CodeContext::CacheSlotNumber min = MAX_CACHE_SIZE;
        CodeContext::CacheSlotNumber max = 0;
        for (auto c : ctx.code.top()->loadsSlotInCache) {
            auto i = c.second;
            if (i == CodeContext::BindingCacheDisabled)
                continue;
            if (i < min)
                min = i;
            if (i > max)
                max = i;
        }
        if (min < max)
            cs << BC::clearBindingCache(min, max - min);
        return false;
    }

    if (fun == symbol::Function && args.length() == 3) {
        if (!voidContext) {
            CompilerContext::CaptureInfo captures;
            if (Compiler::recordLess_Leaf_Enabled)
                captures = ctx.computeCapturesForInner();
            auto dt = Compiler::compileFunction(args[1], args[0],
                                                std::move(captures.immutable),
                                                std::move(captures.controlled));
            Protect p(dt);
            // Mark this as an inner function to prevent the optimizer from
            // assuming a stable environment
            DispatchTable::check(dt)->baseline()->flags.set(
                Function::InnerFunction);
            assert(TYPEOF(dt) == EXTERNALSXP);
            cs << BC::push(args[0]) << BC::push(dt) << BC::push(args[2])
               << BC::close();
        }
        return true;
    }

    if (args.length() == 2 &&
        (fun == symbol::Add || fun == symbol::Sub || fun == symbol::Mul ||
         fun == symbol::Div || fun == symbol::Idiv || fun == symbol::Mod ||
         fun == symbol::Pow || fun == symbol::Eq || fun == symbol::Ne ||
         fun == symbol::Lt || fun == symbol::Le || fun == symbol::Gt ||
         fun == symbol::Ge || fun == symbol::Colon)) {
        emitGuardForNamePrimitive(cs, fun);

        compileExpr(ctx, args[0]);
        compileExpr(ctx, args[1]);

        if (fun == symbol::Add)
            cs << BC::add();
        else if (fun == symbol::Sub)
            cs << BC::sub();
        else if (fun == symbol::Mul)
            cs << BC::mul();
        else if (fun == symbol::Div)
            cs << BC::div();
        else if (fun == symbol::Idiv)
            cs << BC::idiv();
        else if (fun == symbol::Mod)
            cs << BC::mod();
        else if (fun == symbol::Pow)
            cs << BC::pow();
        else if (fun == symbol::Eq)
            cs << BC::eq();
        else if (fun == symbol::Ne)
            cs << BC::ne();
        else if (fun == symbol::Lt)
            cs << BC::lt();
        else if (fun == symbol::Le)
            cs << BC::le();
        else if (fun == symbol::Gt)
            cs << BC::gt();
        else if (fun == symbol::Ge)
            cs << BC::ge();
        else if (fun == symbol::Colon)
            cs << BC::colon();
        cs.addSrc(ast);

        if (voidContext)
            cs << BC::pop();
        else if (Compiler::profile) {
            // `:` is excluded from the inner-node optimization: its result type
            // and length come from the operand values, not their types, so it
            // is an opaque result leaf like `[` and call results.
            if (fun == symbol::Colon)
                cs << ctx.recordTypeOpaqueResult();
            else
                cs << ctx.recordTypeTracked(true);
        }

        return true;
    }

    if (fun == symbol::And && args.length() == 2) {
        emitGuardForNamePrimitive(cs, fun);

        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);

        cs << BC::aslogical();
        cs.addSrc(args[0]);
        cs << BC::dup() << BC::brfalse(nextBranch);

        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().enterBranch();
        compileExpr(ctx, args[1]);
        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().exitBranch();

        cs << BC::aslogical();
        cs.addSrc(args[1]);
        cs << BC::lglAnd();

        cs << nextBranch;

        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (fun == symbol::Or && args.length() == 2) {
        emitGuardForNamePrimitive(cs, fun);

        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);

        cs << BC::aslogical();
        cs.addSrc(ast);
        cs << BC::dup() << BC::brtrue(nextBranch);

        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().enterBranch();
        compileExpr(ctx, args[1]);
        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().exitBranch();

        cs << BC::aslogical();
        cs.addSrc(ast);
        cs << BC::lglOr();

        cs << nextBranch;

        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (args.length() == 1 &&
        (fun == symbol::Add || fun == symbol::Sub || fun == symbol::Not)) {
        emitGuardForNamePrimitive(cs, fun);

        compileExpr(ctx, args[0]);

        if (fun == symbol::Add)
            cs << BC::uplus();
        else if (fun == symbol::Sub)
            cs << BC::uminus();
        else if (fun == symbol::Not)
            cs << BC::not_();
        cs.addSrc(ast);

        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (fun == symbol::quote && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        if (!voidContext)
            cs << BC::push(args[0]);
        return true;
    }

    if (fun == symbol::Assign || fun == symbol::Assign2 ||
        fun == symbol::SuperAssign) {
        assert(args.length() == 2);

        bool superAssign = fun == symbol::SuperAssign;

        SEXP lhs = args[0];
        SEXP rhs = args[1];

        // 1) Verify lhs is valid
        SEXP l = lhs;
        while (l) {
            switch (TYPEOF(l)) {
            case LANGSXP: {
                auto fun = CAR(l);
                auto args = CDR(l);
                if (TYPEOF(fun) == SYMSXP) {
                    l = CAR(args);
                } else {
                    // Cant rewrite this statically...
                    return false;
                }
                break;
            }
            case SYMSXP: {
                l = nullptr;
                break;
            }
            case STRSXP: {
                l = nullptr;
                break;
            }
            default: {
                // Probably broken assignment
                return false;
            }
            }
        }

        if (!superAssign)
            MARK_ASSIGNMENT_CALL(ast);

        // 2) Specialcase normal assignment (ie. "i <- expr")
        if (TYPEOF(lhs) == SYMSXP) {
            emitGuardForNamePrimitive(cs, fun);
            compileExpr(ctx, rhs);
            // Decide the def's slot HERE, before anything else is emitted: the
            // test is that the RHS's record is still the last instruction.
            int defSlot = Compiler::isRecordlessLeafEnabled()
                              ? ctx.valueRecordSlotHere()
                              : DefUseAnalysis::kNoSlot;
            if (!voidContext) {
                // No ensureNamed needed, stvar already ensures named
                cs << BC::dup() << BC::invisible();
            }
            if (superAssign) {
                cs << BC::stvarSuper(lhs);
            } else {
                if (ctx.code.top()->isCached(lhs))
                    cs << BC::stvarCached(lhs,
                                          ctx.code.top()->cacheSlotFor(lhs));
                else
                    cs << BC::stvar(lhs);
                if (Compiler::isRecordlessLeafEnabled())
                    ctx.defUseAnalysis().trackDef(lhs, defSlot);
            }
            return true;
        }

        // Find all parts of the lhs
        SEXP target = nullptr;
        l = lhs;
        std::vector<SEXP> lhsParts;
        while (!target) {
            switch (TYPEOF(l)) {
            case LANGSXP: {
                auto fun = CAR(l);
                auto args = CDR(l);
                assert(TYPEOF(fun) == SYMSXP);
                lhsParts.push_back(l);
                l = CAR(args);
                break;
            }
            case SYMSXP: {
                target = l;
                lhsParts.push_back(target);
                break;
            }
            case STRSXP: {
                assert(Rf_length(l) == 1);
                target = Rf_install(CHAR(STRING_ELT(l, 0)));
                lhsParts.push_back(target);
                break;
            }
            default: {
                Rf_errorcall(ast,
                             "invalid (do_set) left-hand side to assignment");
                break;
            }
            }
        }

        // 3) Special case f(a) <- b

        // Only allow one level of nesting:
        //     f(x) <- 1         ok
        //     f(g(x)) <- 1      not supported
        // TODO: compile nested complex assignments
        if (lhsParts.size() != 2) {
            return false;
        }

        RList g(lhs);
        // If assignment is
        //       f(x, 2, 3) <- y
        // g = `f`, `x`, 2, 3

        // If we are here, it means that a complex assignment was requested
        // i.e. g != `x`
        assert(g.length() >= 2);

        SEXP fun2 = g[0]; // symbol `f`
        SEXP dest = g[1]; // symbol `x`

        // 3.a) Special case [ and [[
        if (fun2 == symbol::Bracket || fun2 == symbol::DoubleBracket) {
            int dims = g.length() - 2;
            if (dims < 1 || dims > 3) {
                return false;
            }

            SEXP fun2 = *g.begin();
            RListIter idx = g.begin() + 2;
            if (!isRegularArg(idx) || (dims > 1 && !isRegularArg(idx + 1)) ||
                (dims > 2 && !isRegularArg(idx + 2))) {
                return false;
            }
            if (dims == 3 && fun2 == symbol::DoubleBracket)
                return false;

            emitGuardForNamePrimitive(cs, fun);

            if (maybeChanges(target, rhs)) {
                if (ctx.code.top()->isCached(target))
                    cs << BC::ldvarForUpdateCached(
                        target, ctx.code.top()->cacheSlotFor(target));
                else
                    cs << BC::ldvarForUpdate(target);
                cs << BC::setShared() << BC::pop();
            }

            // First rhs (assign is right-associative)
            compileExpr(ctx, rhs);
            if (!voidContext) {
                // Keep a copy of rhs since it's the result of this expression
                cs << BC::dup();
                if (!isConstant(rhs))
                    cs << BC::setShared();
            }

            // Again, subassign bytecodes override objects with named count
            // of 1. If the target is from the outer scope that would be wrong.
            // For example
            //
            //     a <- 1
            //     f <- function()
            //         a[[1]] <- 2
            //
            // the f function should not override a.
            // The ldvarForUpdate BC increments the named count if the target is
            // not local to the current environment.

            if (superAssign) {
                cs << BC::ldvarSuper(target);
                if (Compiler::profile)
                    cs << ctx.recordTypeUntracked();
            } else {
                if (ctx.code.top()->isCached(target)) {
                    cs << BC::ldvarForUpdateCached(
                        target, ctx.code.top()->cacheSlotFor(target));
                } else {
                    cs << BC::ldvarForUpdate(target);
                }
                if (Compiler::profile) {
                    if (Compiler::recordLess_Leaf_Enabled)
                        emitRecordTypeForVar(ctx, cs, target);
                    else
                        cs << ctx.recordTypeUntracked();
                }
            }

            if (maybeChanges(target, *idx) ||
                (dims > 1 && maybeChanges(target, *(idx + 1))) ||
                (dims > 2 && maybeChanges(target, *(idx + 2))))
                cs << BC::setShared();

            // And index
            compileExpr(ctx, *idx);
            if (dims > 1)
                compileExpr(ctx, *(idx + 1));
            if (dims > 2)
                compileExpr(ctx, *(idx + 2));

            if (dims == 3) {
                assert(fun2 == symbol::Bracket);
                cs << BC::subassign1_3();
            } else if (dims == 2) {
                if (fun2 == symbol::DoubleBracket) {
                    cs << BC::subassign2_2();
                } else {
                    cs << BC::subassign1_2();
                }
            } else {
                if (fun2 == symbol::DoubleBracket) {
                    cs << BC::subassign2_1();
                } else {
                    cs << BC::subassign1_1();
                }
            }
            cs.addSrc(ast);

            // store the result as "target"
            if (superAssign) {
                cs << BC::stvarSuper(target);
            } else {

                if (ctx.code.top()->isCached(target)) {
                    cs << BC::stvarCached(target,
                                          ctx.code.top()->cacheSlotFor(target));
                } else {
                    cs << BC::stvar(target);
                }
                if (Compiler::isRecordlessLeafEnabled())
                    ctx.defUseAnalysis().trackDef(target,
                                                  DefUseAnalysis::kNoSlot);
            }

            if (!voidContext)
                cs << BC::invisible();
        } else {
            /*
                3.b) Deal with all the other functions:
                    f(x,y) <- z
                i.e.
                    <-(f(x,y), value=z)
                will (almost) get rewritten into
                    <-( x, value=f<-(x,y,value=z) )

                This rewriting is theoretical. Indeed, there are some
                specificities to complex assignments:
                    - z is evaluated eagerly, followed by x
                    - the other arguments are passed as promises, as usual
                    - the complex assignment returns the value of z
            */

            std::string const fun2name = CHAR(PRINTNAME(fun2));

            // "slot<-" ignores value semantics and modifies shared objects
            // in-place, our implementation does not deal with this case.
            if (fun2name == "slot" || fun2name == "class") {
                return false;
            }

            // We need to get the SEXP for `f<-` from the SEXP for `f`
            std::string const fun2_replacement_name = fun2name + "<-";
            SEXP farrow_sym = Rf_install(fun2_replacement_name.c_str());

            /* Deal with special functions.
             The issue with special functions is that they do not use the
             arguments passed on the stack, but evaluate the arguments through
             the AST. For normal functions, in the assignment
                 f(x,a,b) <- z
             we emit the bytecode that will lead to the evaluation of z and x,
             and pass these values in evaluated promises on the object stack.

             If we use the same strategy for special function, the arguments
             will be evaluated a second time.

             There are only a couple special assignment functions
                - [[<-   (handled above)
                - [ <-   (handled above)
                - <-     (will not appear in a rewriting)
                - <<-    (will not appear in a rewriting)
                - @<-
                - $<-
             This leaves only two to deal with.

             The simple solution is to give up trying to compile the complex
             assignments for the two special assignment functions. In that case
             we lose the opportunity of compiling the RHS ; it will get
             interpreted by GNU R.

             It would still be interesting to compile the RHS and somehow pass
             the value to the special. The approach used in the GnuR BC compiler
             is to add the special instruction SETTER_CALL to deal with this
             situation at runtime: the AST of the RHS is replaced at runtime by
             an AST containing just the value obtained from the evaluation of
             the RHS. See
                https://github.com/reactorlabs/gnur/blob/R-3-6-2-branch-rir-patch/src/main/eval.c#L7128
            */

            bool const maybe_special = (fun2name == "$" || fun2name == "@");
            if (maybe_special) {
                return false;
            }

            // Get the LISTSXP of args for f
            SEXP f_args = CDR(CAR(args_));

            // Make the ast for the call : f<-, x, y, value=z
            // and protect it from GC
            SEXP farrow_ast;
            Protect farrow_ast_protect{farrow_ast =
                                           Rf_lcons(farrow_sym, R_NilValue)};
            // duplicate the args from the AST of the call to f into the AST for
            // the call to `f<-` (directly linked in the AST so that everything
            // is protected)
            SETCDR(farrow_ast, Rf_duplicate(f_args));

            SEXP last_farrow_cell = farrow_ast;
            while (CDR(last_farrow_cell) != R_NilValue) {
                last_farrow_cell = CDR(last_farrow_cell);
            }
            MARK_ASSIGNMENT_CALL(farrow_ast);

            // We need to append "value = z" to the list of args for f<-
            // Let's create the corresponding cell (directly linked in AST so
            // that it is protected)
            SETCDR(last_farrow_cell, Rf_lcons(rhs, R_NilValue));
            SEXP new_z_cell = CDR(last_farrow_cell);
            SET_TAG(new_z_cell, Rf_install("value"));

            // The RHS must be evaluated before the LHS
            // Additionnaly, the value of the RHS must be returned after the
            // assignment (in non-void contexts). It will be kept on the stack
            // before the call to `f<-`.
            // A copy will be wrapped in an evaluated promise and passed to f<-.
            compileExpr(ctx, rhs);

            // Prepare the call to f<-(x, y1, <...>, yn, z)
            cs << BC::ldfun(farrow_sym);

            if (Compiler::profile)
                cs << ctx.recordCall();

            // prepare x, yk, z as promises
            LoadArgsResult load_arg_res;
            SEXP farrow_args = CDR(farrow_ast);

            // Load the value of x as a raw value
            // Passing x as a raw value instead of an evaluated promise is valid
            // in this case since R code is already discouraged from doing
            // non-standard evaluation on the destination of a complex
            // assignment. See "A Byte Code Compiler for R" p.76 for a
            // discussion on how one package used to do NSE on the destination
            // of complex assignments (by modifying `*tmp*` in the evaluation
            // environment) but was asked to abandon this practice.
            compileLoadOneArg(ctx, farrow_args, ArgType::RAW_VALUE,
                              load_arg_res);

            // load y1, <...>, yn

            for (SEXP cur_arg_cell = CDR(farrow_args);
                 cur_arg_cell != new_z_cell; cur_arg_cell = CDR(cur_arg_cell)) {
                compileLoadOneArg(ctx, cur_arg_cell, ArgType::PROMISE,
                                  load_arg_res);
            }

            // now, the value stack looks like this:

            // N+2      N+1       N   N-1                 0
            //  ??, z (raw),  `f<-`,    x,   y1,  <...>, yn

            // where N is the number of arguments _already_ passed to `f<-`
            // (load_arg_res.numArgs)
            if (voidContext) {
                // move the value of z to TOS
                cs << BC::pick(load_arg_res.numArgs + 1);
            } else {
                // keep a copy before `f<-` to return after the assignment
                cs << BC::pull(load_arg_res.numArgs + 1);
            }

            // after this instruction:

            // N+2     N+1     N   N-1           1        0
            //  ??,  `f<-`,    x,   y1,  <...>, yn, z (raw)

            // Wrap the value of z in an evaluated promise:
            compileLoadOneArg(ctx, new_z_cell, ArgType::EAGER_PROMISE_FROM_TOS,
                              load_arg_res);

            // call f<- with the arguments
            if (load_arg_res.hasDots) {
                cs << BC::callDots(load_arg_res.numArgs, load_arg_res.names,
                                   farrow_ast, load_arg_res.assumptions);
            } else {
                cs << BC::call(load_arg_res.numArgs, load_arg_res.names,
                               farrow_ast, load_arg_res.assumptions);
            }

            // Bind the result to x
            if (superAssign) {
                cs << BC::stvarSuper(dest);
            } else {
                if (ctx.code.top()->isCached(dest))
                    cs << BC::stvarCached(dest,
                                          ctx.code.top()->cacheSlotFor(dest));
                else
                    cs << BC::stvar(dest);
            }

            if (!voidContext) {
                // The return value, RHS, is TOS
                cs << BC::invisible();
                if (Compiler::profile) {
                    // Replacement-function result is an opaque value that may
                    // be assigned (a def candidate) and may feed an enclosing
                    // inner node, so it is a tracked always-record leaf.
                    cs << ctx.recordTypeOpaqueResult();
                }
            }

            return true;
        }

        return true;
    }

    if (fun == symbol::Block) {
        emitGuardForNamePrimitive(cs, fun);

        if (args.length() == 0) {
            if (!voidContext)
                cs << BC::push(R_NilValue);
            return true;
        }

        for (RListIter e = args.begin(); e != args.end(); ++e) {
            if (e + 1 != args.end()) {
                compileExpr(ctx, *e, true);
            } else {
                compileExpr(ctx, *e, voidContext);
            }
        }

        return true;
    }

    if (fun == symbol::If) {
        if (args.length() < 2 || args.length() > 3)
            return false;

        emitGuardForNamePrimitive(cs, fun);
        BC::Label trueBranch = cs.mkLabel();
        BC::Label nextBranch = cs.mkLabel();

        compileExpr(ctx, args[0]);
        cs << BC::asbool() << BC::brtrue(trueBranch);

        if (args.length() < 3) {
            if (!voidContext) {
                cs << BC::push(R_NilValue);
                cs << BC::invisible();
            }
        } else {
            if (Compiler::isRecordlessLeafEnabled())
                ctx.defUseAnalysis().enterBranch();
            compileExpr(ctx, args[2], voidContext);
            if (Compiler::isRecordlessLeafEnabled())
                ctx.defUseAnalysis().exitBranch();
        }
        cs << BC::br(nextBranch);

        cs << trueBranch;
        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().enterBranch();
        compileExpr(ctx, args[1], voidContext);
        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().exitBranch();

        cs << nextBranch;
        return true;
    }

    if (fun == symbol::Parenthesis) {
        if (args.length() != 1 || args[0] == R_DotsSymbol)
            return false;

        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (!voidContext)
            cs << BC::visible();
        else
            cs << BC::pop();

        return true;
    }

    if (fun == symbol::Return && args.length() < 2) {
        emitGuardForNamePrimitive(cs, fun);

        if (args.length() == 0)
            cs << BC::push(R_NilValue);
        else
            compileExpr(ctx, args[0]);

        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().markReturn();
        if (ctx.inLoop() || ctx.isInPromise())
            cs << BC::return_();
        else
            cs << BC::ret();
        return true;
    }

    if (fun == symbol::isnull && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(BC::RirTypecheck::isNILSXP);
        return true;
    }

    if (fun == symbol::islist && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(BC::RirTypecheck::isVECSXP);
        return true;
    }

    if (fun == symbol::ispairlist && args.length() == 1) {
        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, args[0]);
        if (voidContext)
            cs << BC::pop();
        else
            cs << BC::is(BC::RirTypecheck::isLISTSXP);
        return true;
    }

    if (fun == symbol::DoubleBracket || fun == symbol::Bracket) {
        int dims = args.length() - 1;
        if (dims < 1 || dims > 3) {
            return false;
        }

        SEXP lhs = *args.begin();
        RListIter idx = args.begin() + 1;

        if (!isRegularArg(idx) || (dims > 1 && !isRegularArg(idx + 1)) ||
            (dims > 2 && !isRegularArg(idx + 2)))
            return false;
        if (dims == 3 && fun == symbol::DoubleBracket)
            return false;

        emitGuardForNamePrimitive(cs, fun);
        compileExpr(ctx, lhs);

        BC::Label objBranch = cs.mkLabel();
        BC::Label nonObjBranch = cs.mkLabel();
        BC::Label contBranch = cs.mkLabel();

        cs << BC::dup() << BC::is(BC::RirTypecheck::isNonObject)
           << ctx.recordTest() << BC::brfalse(objBranch)
           << BC::br(nonObjBranch);

        cs << objBranch;

        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().enterBranch();
        {
            LoadArgsResult dummy;
            compileLoadArgs(ctx, ast, fun, args_, dummy, voidContext, 1);
        }
        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().exitBranch();
        cs << BC::br(contBranch);

        cs << nonObjBranch;

        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().enterBranch();
        compileExpr(ctx, *idx);
        if (dims == 3) {
            compileExpr(ctx, *(idx + 1));
            compileExpr(ctx, *(idx + 2));
        } else if (dims == 2) {
            compileExpr(ctx, *(idx + 1));
        }
        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().exitBranch();
        cs << BC::br(contBranch);

        cs << contBranch;

        if (dims == 3) {
            assert(fun != symbol::DoubleBracket);
            cs << BC::extract1_3();
        } else if (dims == 2) {
            if (fun == symbol::DoubleBracket)
                cs << BC::extract2_2();
            else
                cs << BC::extract1_2();
        } else {
            if (fun == symbol::DoubleBracket)
                cs << BC::extract2_1();
            else
                cs << BC::extract1_1();
        }
        cs.addSrc(ast);
        if (!voidContext) {
            if (Compiler::profile) {
                // Neither `[` nor `[[` is an elidable inner node: both are
                // opaque always-record leaves (def candidates / inner-node
                // operands), but tracked rather than untracked.
                //
                // `[` was previously treated as an elidable inner node on the
                // rationale that "x[...] has the same SEXPTYPE as x for
                // non-object x, so the result is inferable from the lhs leaf".
                // The SEXPTYPE part is true, but eliding the node discards the
                // whole ObservedValues, and `notScalar` is not inherited:
                //     x <- c(1,2,3); x[1L]
                //     lhs slot    -> double ()   (notScalar)
                //     result slot -> double (s)  (scalar)
                // Nor can it be recovered from the operands' feedback, since it
                // depends on the *length of the index value* (x[1] is scalar,
                // x[1:2] is not — i is INTSXP in both). Contrast the arithmetic
                // ops, where result length is max(operand lengths), so
                // scalar-ness IS derivable from the operands.
                //
                // `[[` extracts an element whose type varies outright
                // (e.g. list(3,"hello")[[i]]), so it was never inferable.
                cs << ctx.recordTypeOpaqueResult();
            }
            cs << BC::visible();
        } else {
            cs << BC::pop();
        }

        return true;
    }

    if (fun == symbol::Missing && args.length() == 1 &&
        TYPEOF(args[0]) == SYMSXP && !DDVAL(args[0])) {
        emitGuardForNamePrimitive(cs, fun);
        if (!voidContext) {
            cs << BC::missing(args[0]) << BC::visible();
        }
        return true;
    }

    if (fun == symbol::While) {
        assert(args.length() == 2);

        SEXP cond = args[0];
        SEXP body = args[1];

        emitGuardForNamePrimitive(cs, fun);

        ClearableScopeGuard clearScope(ctx.code.top(),
                                       Compiler::isRecordlessLeafEnabled());
        compileWhile(
            ctx,
            [&ctx, &cs, &cond]() {
                compileExpr(ctx, cond);
                cs << BC::asbool();
            },
            [&ctx, &body]() { compileExpr(ctx, body, true); }, body,
            !containsLoop(body));
        clearScope.finish();

        if (!voidContext)
            cs << BC::push(R_NilValue) << BC::invisible();

        return true;
    }

    if (fun == symbol::Repeat) {
        assert(args.length() == 1);

        SEXP body = args[0];

        emitGuardForNamePrimitive(cs, fun);

        BC::Label nextBranch = cs.mkLabel();
        BC::Label breakBranch = cs.mkLabel();
        ctx.pushLoop(nextBranch, breakBranch);

        unsigned beginLoopPos = cs.currentPos();
        cs << BC::beginloop(breakBranch);

        if (Compiler::isRecordlessLeafEnabled()) {
            ctx.defUseAnalysis().enterLoop();
            std::unordered_map<SEXP, int> bodyDefs;
            DefUseAnalysis::collectAssignedVars(body, bodyDefs);
            ctx.defUseAnalysis().setLoopBodyDefs(std::move(bodyDefs));
        }

        ClearableScopeGuard clearScope(ctx.code.top(),
                                       Compiler::isRecordlessLeafEnabled());

        // loop peel is a copy of the body, with no backwards jumps
        if (Compiler::loopPeelingEnabled && !containsLoop(body)) {
            auto savedDefs = (Compiler::isRecordlessLeafEnabled())
                                 ? ctx.defUseAnalysis().saveState()
                                 : DefUseAnalysis::DefsSnapshot{};
            compileExpr(ctx, body, true);
            if (Compiler::isRecordlessLeafEnabled())
                ctx.defUseAnalysis().restoreState(std::move(savedDefs));
        }

        cs << nextBranch;
        compileExpr(ctx, body, true);
        clearScope.finish();
        if (Compiler::isRecordlessLeafEnabled()) {
            ctx.defUseAnalysis().clearLoopBodyDefs();
            ctx.defUseAnalysis().exitLoop();
        }
        cs << BC::br(nextBranch) << breakBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endloop();
        } else {
            cs.remove(beginLoopPos);
        }

        if (!voidContext)
            cs << BC::push(R_NilValue) << BC::invisible();

        ctx.popLoop();
        return true;
    }

    if (fun == symbol::For) {
        // TODO: if the seq is not a vector, we need to throw an error!
        assert(args.length() == 3);

        SEXP sym = args[0];
        SEXP seq = args[1];
        SEXP body = args[2];

        assert(TYPEOF(sym) == SYMSXP);

        emitGuardForNamePrimitive(cs, fun);

        if (compileSimpleFor(ctx, ast, sym, seq, body, voidContext)) {
            return true;
        }

        BC::Label nextBranch = cs.mkLabel();
        BC::Label breakBranch = cs.mkLabel();
        ctx.pushLoop(nextBranch, breakBranch);

        // Compile the seq expression (vector) and initialize the loop
        compileExpr(ctx, seq);
        cs << BC::forSeqSize() << BC::push((int)0);

        auto compileIndexOps = [&](bool record) {
            // Increment the index and compare to the seq upper bound
            cs << BC::inc() << BC::ensureNamed() << BC::dup2() << BC::lt();
            // We know this is an int and won't do dispatch.
            // TODO: add a integer version of lt_
            cs.addSrc(R_NilValue);

            if (record)
                cs << ctx.recordTest();

            // If outside bound, branch, otherwise index into the vector
            cs << BC::brtrue(breakBranch) << BC::pull(2) << BC::pull(1)
               << BC::extract2_1();
            // We know this is a loop sequence and won't do dispatch.
            // TODO: add a non-object version of extract2_1
            cs.addSrc(R_NilValue);

            // Set the loop variable
            if (ctx.code.top()->isCached(sym))
                cs << BC::stvarCached(sym, ctx.code.top()->cacheSlotFor(sym));
            else
                cs << BC::stvar(sym);
        };

        bool rangeBased =
            Compiler::isRecordlessLeafEnabled() && isRangeBasedSeq(seq);
        RangeBasedIterVarScope rangeScope(ctx.code.top(), sym,
                                          /*active=*/rangeBased);
        ClearableScopeGuard clearScope(ctx.code.top(),
                                       Compiler::isRecordlessLeafEnabled());

        unsigned int beginLoopPos = cs.currentPos();
        cs << BC::beginloop(breakBranch);

        if (Compiler::isRecordlessLeafEnabled()) {
            ctx.defUseAnalysis().enterLoop();
            ctx.defUseAnalysis().pushForLoopVar(sym);
            std::unordered_map<SEXP, int> bodyDefs;
            DefUseAnalysis::collectAssignedVars(body, bodyDefs);
            // The for loop also assigns sym on each iteration
            bodyDefs[sym]++;
            ctx.defUseAnalysis().setLoopBodyDefs(std::move(bodyDefs));
        }

        // loop peel is a copy of the body (including indexing ops), with no
        // backwards jumps
        if (Compiler::loopPeelingEnabled && !containsLoop(body)) {
            auto savedDefs = (Compiler::isRecordlessLeafEnabled())
                                 ? ctx.defUseAnalysis().saveState()
                                 : DefUseAnalysis::DefsSnapshot{};
            compileIndexOps(true);
            compileExpr(ctx, body, true);
            if (Compiler::isRecordlessLeafEnabled())
                ctx.defUseAnalysis().restoreState(std::move(savedDefs));
        }

        cs << nextBranch;
        compileIndexOps(false);

        // Compile the loop body
        compileExpr(ctx, body, true);
        rangeScope.finish();
        clearScope.finish();
        if (Compiler::isRecordlessLeafEnabled()) {
            ctx.defUseAnalysis().clearLoopBodyDefs();
            ctx.defUseAnalysis().popForLoopVar(sym);
            ctx.defUseAnalysis().exitLoop();
        }
        cs << BC::br(nextBranch) << breakBranch;

        if (ctx.loopNeedsContext()) {
            cs << BC::endloop();
        } else {
            cs.remove(beginLoopPos);
        }

        cs << BC::popn(3);
        if (!voidContext) {
            cs << BC::push(R_NilValue) << BC::invisible();
        }

        ctx.popLoop();

        return true;
    }

    if (fun == symbol::Next) {
        assert(args.length() == 0);

        if (!ctx.inLoop()) {
            // notify wrong next
            return false;
        }

        if (ctx.loopIsLocal()) {
            emitGuardForNamePrimitive(cs, fun);
            if (Compiler::isRecordlessLeafEnabled())
                ctx.defUseAnalysis().markLoopExit();
            cs << BC::br(ctx.loopNext()) << BC::push(R_NilValue);
            return true;
        }
    }

    if (fun == symbol::Break) {
        assert(args.length() == 0);

        if (!ctx.inLoop()) {
            // notify wrong break
            return false;
        }

        if (ctx.loopIsLocal()) {
            emitGuardForNamePrimitive(cs, fun);
            if (Compiler::isRecordlessLeafEnabled())
                ctx.defUseAnalysis().markLoopExit();
            cs << BC::br(ctx.loopBreak()) << BC::push(R_NilValue);
            return true;
        }
    }

    if (fun == symbol::Switch) {
        /* # A high level overview
         * # Assume argLen > 1:
         *   compile arg[0]
         *   if (arg[0] is not length-1 vector) br vecECont
         *   error(...)
         *   # unreachable
         * vecECont:
         *   # this check is currently skipped
         *   if (not isFactor(arg[0])) br facWCont
         *   warning(...)
         * facWCont:
         *   if (isString(arg[0])) br str
         *   asInteger(arg[0])
         *   if (value == 1) br label[0]
         *   ...
         *   if (value == n) br label[n-1]
         *   br nil
         *
         * str:
         *   if (not is.na(value)) br strNACont
         *   value <- "NA"
         * strNACont:
         *   if (value == group[0]) br groupLabels[0]
         *   ...
         *   if (value == group[k]) br groupLabels[k]
         *   br default label[dftLabelIdx] # or nil if no default
         *
         * label[i]:
         *   # if arg[i+1] is missing, we must came from integer case
         *   # error(...)
         *   pop # pop evaluated 1st arg
         *   compile expression[i]
         *   br cont
         * ...
         *
         * nil:
         *   # stack is [1stArg]
         *   push R_NilValue
         *
         * cont:
         *   # stack is [retval]
         */
        int argLen = args.length();
        // argLen error/warning is statically determinable
        if (argLen == 0) {
            ctx.emitError("'EXPR' is missing", ast);
            return true;
        }
        auto expr = args.begin();
        if (expr.hasTag()) {
            auto supplied = CHAR(PRINTNAME(expr.tag()));
            auto ns = strlen(supplied);
            if (ns > strlen("EXPR") || strncmp(supplied, "EXPR", ns)) {
                ctx.emitError(std::string("supplied argument name '")
                                  .append(supplied)
                                  .append("' does not match 'EXPR'")
                                  .c_str(),
                              ast);
                return true;
            }
        }
        // when 1st arg is string, switch behaves like C/C++ switch/case.
        // Cases like `x=, y=, z=20` are grouped together.
        // groups[-1] is not used, for impl convenience
        std::vector<std::vector<SEXP>> groups = {{}};
        std::vector<BC::Label> groupLabels; // eval/return for each group
        std::vector<SEXP> expressions;      // return value ast for each group
        std::vector<BC::Label> labels;      // eval/return for each arg
        std::vector<bool> argMissing(argLen - 1, true);
        int dftLabelIdx = -1; // index into `labels` for default return arg
        bool dupDflt = false;
        BC::Label vecArityBr = cs.mkLabel();
        BC::Label vecErrorBr = cs.mkLabel();
        BC::Label vecEContBr = cs.mkLabel();
        BC::Label facWContBr = cs.mkLabel();
        BC::Label strBr = cs.mkLabel();
        BC::Label strNAContBr = cs.mkLabel();
        BC::Label nilBr = cs.mkLabel();
        BC::Label contBr = cs.mkLabel();

        // find default and group args
        int argIdx = 0;
        for (auto arg = args.begin() + 1; arg != args.end(); ++arg, ++argIdx) {
            auto label = cs.mkLabel();
            labels.push_back(label);
            if (!arg.hasTag()) {
                dupDflt |= (dftLabelIdx != -1);
                dftLabelIdx = argIdx;
            } else {
                groups.back().push_back(arg.tag());
            }
            if (*arg != R_MissingArg) { // tag must be present if value is not
                argMissing[argIdx] = false;
                expressions.push_back(*arg);
                groupLabels.push_back(label);
                groups.push_back({}); // start new group
            }
            if (*arg == R_DotsSymbol)
                return false;
        }

        /******************* INSTRUCTIONS START HERE *********************/
        compileExpr(ctx, args[0]);

        // !isVector(x)
        cs << BC::dup() << BC::is(BC::RirTypecheck::isVector)
           << ctx.recordTest() << BC::brtrue(vecArityBr);
        cs << BC::br(vecErrorBr);

        // ... || LENGTH(x) != 1
        cs << vecArityBr << BC::dup() << BC::length_() << BC::push(1)
           << BC::eq();
        cs.addSrc(R_NilValue); // to make code verifier happy
        cs << ctx.recordTest() << BC::brtrue(vecEContBr);

        cs << vecErrorBr;
        ctx.emitError("EXPR must be a length 1 vector", ast);

        // isFactor(x)
        cs << vecEContBr;

        cs << BC::dup() << BC::is(BC::RirTypecheck::isFactor)
           << ctx.recordTest() << BC::brfalse(facWContBr);

        ctx.emitWarning("EXPR is a \"factor\", treated as integer.\n Consider "
                        "using 'switch(as.character( * ), ...)' instead.",
                        ast);

        cs << facWContBr;

        if (argLen == 1) { // inserted here to mimic behavior of builtin impl
            ctx.emitWarning("'switch' with no alternatives", ast);
            cs << BC::br(nilBr);
        }
        cs << BC::dup() << BC::is(BC::RirTypecheck::isSTRSXP)
           << ctx.recordTest() << BC::brtrue(strBr);
        cs << BC::asSwitchIdx();

        // currently stack is [arg[0]] (converted to integer)
        for (size_t i = 0; i < labels.size(); ++i) {
            cs << BC::dup() << BC::push(Rf_ScalarInteger(i + 1)) << BC::eq();
            cs.addSrc(R_NilValue); // call argument for builtin
            cs << BC::asbool() << ctx.recordTest() << BC::brtrue(labels[i]);
        }
        cs << BC::br(nilBr) << strBr;
        if (dupDflt) {
            ctx.emitError("duplicate 'switch' defaults", ast);
        } else {

            // If value is NA, set it to the string "NA". This solves two
            // problems: 1) In an R switch(), NA_character_ should match the
            // string "NA". 2) It ensures that BC:eq will always return a
            // boolean instead of NA. This allows us to use BC::eq and
            // BC::asbool to compare the cases.
            cs << BC::dup()
               << BC::callBuiltin(1, R_NilValue, getBuiltinFun("is.na"))
               << BC::asbool() << ctx.recordTest() << BC::brfalse(strNAContBr)
               << BC::pop() << BC::push(Rf_mkString("NA")) << strNAContBr;

            for (size_t i = 0; i < expressions.size(); ++i) {
                for (auto& n : groups[i]) {
                    cs << BC::dup() << BC::push(n) << BC::eq();
                    cs.addSrc(R_NilValue); // call argument for builtin
                    cs << BC::asbool() << ctx.recordTest()
                       << BC::brtrue(groupLabels[i]);
                }
            }

            auto fallbackLabel =
                (dftLabelIdx == -1) ? nilBr : labels[dftLabelIdx];
            cs << BC::br(fallbackLabel);
        }

        for (size_t i = 0, j = 0; i < labels.size(); ++i) {
            cs << labels[i];
            if (argMissing[i]) {
                ctx.emitError("empty alternative in numeric switch", ast);
                continue;
            } else {
                cs << BC::pop();
                if (Compiler::isRecordlessLeafEnabled())
                    ctx.defUseAnalysis().enterBranch();
                compileExpr(ctx, expressions[j++]);
                if (Compiler::isRecordlessLeafEnabled())
                    ctx.defUseAnalysis().exitBranch();
                cs << BC::br(contBr);
            }
        }

        cs << nilBr << BC::pop() << BC::push(R_NilValue);
        cs << contBr;
        if (voidContext)
            cs << BC::pop();
        return true;
    }

    if (fun == symbol::Internal) {
        SEXP inAst = args[0];
        SEXP args_ = CDR(inAst);
        RList args(args_);
        SEXP fun = CAR(inAst);

        if (TYPEOF(fun) == SYMSXP) {
            SEXP internal = fun->u.symsxp.internal;

            // Check if the .Internal call is malformed:
            //      .Internal(undefined_function())
            // This can occur in normal R code as some internal functions are
            // not defined on all platforms (see names.c). E.g.
            //      .Internal(tzone_name())
            // only works on win32.
            if (internal == R_NilValue) {
                return false;
            }

            int i = getBuiltinNr(internal);
            // If the .Internal call goes to a builtin, then we call eagerly
            if (R_FunTab[i].eval % 10 == 1) {
                emitGuardForNamePrimitive(cs, symbol::Internal);

                bool hasDots = false;
                for (RListIter arg = args.begin(); arg != RList::end(); ++arg)
                    if (*arg == R_DotsSymbol)
                        hasDots = true;

                if (hasDots)
                    cs << BC::push(internal);

                std::vector<SEXP> names;
                for (RListIter arg = args.begin(); arg != RList::end(); ++arg) {

                    if (*arg == R_DotsSymbol) {
                        // The name is ignored, eg. foo(x = ...) ~~~ foo(...),
                        // so we can use it to mark that we require dots
                        // expansion. The actual value pushed means that we at
                        // runtime should look up the ellipsis, as opposed to it
                        // being already on the stack (which is what pir does).
                        cs << BC::push(symbol::expandDotsTrigger);
                        names.push_back(symbol::expandDotsTrigger);
                        continue;
                    }

                    if (hasDots)
                        names.push_back(arg.tag());

                    if (*arg == R_MissingArg) {
                        cs << BC::push(R_MissingArg);
                        continue;
                    }

                    compileExpr(ctx, *arg);
                }

                if (hasDots) {
                    cs << BC::callDots(args.length(), names, inAst, Context());
                } else {
                    cs << BC::callBuiltin(args.length(), inAst, internal);
                }
                if (voidContext)
                    cs << BC::pop();

                return true;
            }

            // .Internal(lapply(X, FUN))
            if (fun == symbol::lapply && args.length() == 2) {

                BC::Label loopBranch = cs.mkLabel();
                BC::Label nextBranch = cs.mkLabel();

                compileExpr(ctx, args[0]); // [X]

                // get length and names of the vector X
                cs << BC::dup() << BC::names() << BC::swap()
                   << BC::length_() // [names(X), length(X)]
                   << BC::dup() << BC::push(Rf_mkString("list")) << BC::swap()
                   << BC::callBuiltin(
                          2, symbol::tmp,
                          getBuiltinFun("vector")) // [names(X), length(X), ans]
                   << BC::pick(2) << BC::setNames() << BC::swap()
                   << BC::push((int)0); // [ans, length(X), i]

                // loop invariant stack layout: [ans, length(X), i]

                // check end condition
                cs << loopBranch << BC::inc() << BC::dup2() << BC::lt();
                cs.addSrc(ast);

                SEXP isym = Rf_install("i");
                cs << BC::brtrue(nextBranch) << BC::dup() << BC::stvar(isym);

                // construct ast for FUN(X[[i]], ...)
                SEXP tmp = PROTECT(
                    Rf_lcons(symbol::DoubleBracket,
                             Rf_lcons(args[0], Rf_lcons(isym, R_NilValue))));
                SEXP call = Rf_lcons(
                    args[1], Rf_lcons(tmp, Rf_lcons(R_DotsSymbol, R_NilValue)));

                PROTECT(call);
                compileCall(ctx, call, CAR(call), CDR(call), false);
                UNPROTECT(2);

                // store result
                cs << BC::pull(1) << BC::pick(4)
                   << BC::swap() // [length(X), i, fun(X[[i]], ...), ans, i]
                   << BC::set_vec_elt();

                cs << BC::put(2) // [ans, length(X), i]
                   << BC::br(loopBranch);

                // put ans to the top and remove rest
                cs << nextBranch << BC::pop() << BC::pop() << BC::visible();

                if (voidContext)
                    cs << BC::pop();

                return true;
            }
        }
    }

#define V(NESTED, name, Name)                                                  \
    if (fun == symbol::name) {                                                 \
        cs << BC::push(R_NilValue) << BC::name();                              \
        cs.addSrc(ast);                                                        \
        return true;                                                           \
    }
    SIMPLE_INSTRUCTIONS(V, _)
#undef V

    return false;
}

static void compileLoadOneArg(CompilerContext& ctx, SEXP arg, ArgType arg_type,
                              LoadArgsResult& res) {
    // Prepare the argument arg for a function call.
    // The bytecode generated will return the result either as a promise, an
    // evaluated promise, or a raw value.

    CodeStream& cs = ctx.cs();
    int i = res.numArgs;
    res.numArgs += 1;

    if (CAR(arg) == R_DotsSymbol) {
        // The name is ignored, eg. foo(x = ...) ~~~ foo(...), so we can use it
        // to mark that we require dots expansion. The actual value pushed means
        // that we at runtime should look up the ellipsis, as opposed to it
        // being already on the stack (which is what pir does).
        cs << BC::push(symbol::expandDotsTrigger);
        res.names.push_back(symbol::expandDotsTrigger);
        res.hasDots = true;
        return;
    }

    // remember if the argument had a name associated (for missing too)
    res.names.push_back(TAG(arg));
    if (TAG(arg) != R_NilValue)
        res.hasNames = true;

    if (CAR(arg) == R_MissingArg) {
        cs << BC::push(R_MissingArg);
        return;
    }

    if (arg_type == ArgType::RAW_VALUE) {
        compileExpr(ctx, CAR(arg), false);
        return;
    }

    // Constant arguments do not need to be promise wrapped
    if (arg_type != ArgType::EAGER_PROMISE_FROM_TOS)
        switch (TYPEOF(CAR(arg))) {
        case LANGSXP:
        case SYMSXP:
            break;
        default:
            auto eager = CAR(arg);
            res.assumptions.setEager(i);
            if (!Rf_isObject(eager)) {
                res.assumptions.setNotObj(i);
                if (IS_SIMPLE_SCALAR(eager, REALSXP))
                    res.assumptions.setSimpleReal(i);
                if (IS_SIMPLE_SCALAR(eager, INTSXP))
                    res.assumptions.setSimpleInt(i);
            }
            cs << BC::push(eager);
            return;
        }

    Code* prom;
    if (arg_type == ArgType::EAGER_PROMISE) {
        // Compile the expression to evaluate it eagerly, and
        // wrap the return value in a promise without rir code
        compileExpr(ctx, CAR(arg), false);
        prom = compilePromiseNoRir(ctx, CAR(arg));
    } else if (arg_type == ArgType::EAGER_PROMISE_FROM_TOS) {
        // The value we want to wrap in the argument's promise is
        // already on TOS, no nead to compile the expression.
        // Wrap it in a promise without rir code.
        prom = compilePromiseNoRir(ctx, CAR(arg));
    } else { // ArgType::PROMISE
        // Compile the expression as a promise.
        prom = compilePromise(ctx, CAR(arg));
    }

    size_t idx = cs.addPromise(prom);

    if (arg_type == ArgType::EAGER_PROMISE ||
        arg_type == ArgType::EAGER_PROMISE_FROM_TOS) {
        res.assumptions.setEager(i);
        cs << BC::mkEagerPromise(idx);
    } else {
        cs << BC::mkPromise(idx);
    }
}

static void compileLoadArgs(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                            LoadArgsResult& info, bool voidContext,
                            int skipArgs, int eager) {
    // Process arguments:
    // Arguments can be optionally named

    SEXP cur_cell = args;
    int i = 0;
    while (cur_cell != R_NilValue) {
        if (i >= skipArgs) {
            ArgType t = (i < eager) ? ArgType::RAW_VALUE : ArgType::PROMISE;
            compileLoadOneArg(ctx, cur_cell, t, info);
        }
        cur_cell = CDR(cur_cell);
        i++;
    }
}

// function application
void compileCall(CompilerContext& ctx, SEXP ast, SEXP fun, SEXP args,
                 bool voidContext) {

    CodeStream& cs = ctx.cs();

    // application has the form:
    // LHS ( ARGS )

    // LHS can either be an identifier or an expression
    bool speculateOnBuiltin = false;
    BC::Label eager = 0;
    BC::Label theEnd = 0;

    if (TYPEOF(fun) == SYMSXP) {
        if (compileSpecialCall(ctx, ast, fun, args, voidContext))
            return;

        if (!ctx.isInPromise()) {

            auto callHasDots = false;
            for (RListIter arg = RList(args).begin(); arg != RList::end();
                 ++arg) {

                if (*arg == R_DotsSymbol) {
                    callHasDots = true;
                    break;
                }
            }

            if (!callHasDots) {
                auto builtin = Rf_findVar(fun, R_BaseEnv);
                assert(builtin != R_NilValue);
                auto likelyBuiltin = TYPEOF(builtin) == BUILTINSXP;
                speculateOnBuiltin = likelyBuiltin;

                if (speculateOnBuiltin) {
                    eager = cs.mkLabel();
                    theEnd = cs.mkLabel();
                    cs << BC::push(builtin) << BC::dup()
                       << BC::ldvarNoForce(fun) << BC::identicalNoforce()
                       << ctx.recordTest() << BC::brtrue(eager);

                    cs << BC::pop();
                }
            }
        }

        cs << BC::ldfun(fun);
    } else {
        compileExpr(ctx, fun);
        cs << BC::checkFunction();
    }

    if (Compiler::profile)
        cs << ctx.recordCall();

    auto compileCall = [&](LoadArgsResult& info) {
        if (info.hasDots) {
            cs << BC::callDots(info.numArgs, info.names, ast, info.assumptions);
        } else if (info.hasNames) {
            cs << BC::call(info.numArgs, info.names, ast, info.assumptions);
        } else {
            info.assumptions.add(Assumption::CorrectOrderOfArguments);
            cs << BC::call(info.numArgs, ast, info.assumptions);
        }
    };

    LoadArgsResult info;
    if (speculateOnBuiltin && Compiler::isRecordlessLeafEnabled())
        ctx.defUseAnalysis().enterBranch();
    if (fun == symbol::forceAndCall) {
        // forceAndCall is a special with signature `function(n, FUN, ...)`
        // The first two args are eager
        compileLoadOneArg(ctx, args, ArgType::RAW_VALUE, info);
        compileLoadOneArg(ctx, CDR(args), ArgType::RAW_VALUE, info);
        if (Compiler::profile)
            cs << ctx.recordCall();
        // Load the rest of the args
        compileLoadArgs(ctx, ast, fun, args, info, voidContext, 2, 0);
    } else {
        compileLoadArgs(ctx, ast, fun, args, info, voidContext);
    }
    compileCall(info);
    if (speculateOnBuiltin && Compiler::isRecordlessLeafEnabled())
        ctx.defUseAnalysis().exitBranch();

    if (speculateOnBuiltin) {
        cs << BC::br(theEnd) << eager;

        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().enterBranch();
        LoadArgsResult infoEager;
        compileLoadArgs(ctx, ast, fun, args, infoEager, voidContext, 0,
                        RList(args).length());

        compileCall(infoEager);
        if (Compiler::isRecordlessLeafEnabled())
            ctx.defUseAnalysis().exitBranch();

        cs << theEnd;
    }

    if (voidContext)
        cs << BC::pop();
    else if (Compiler::profile)
        // Call result is an opaque value (type not inferable from operands)
        // that may be assigned (a def candidate) and may feed an enclosing
        // inner node, so it is a tracked always-record leaf, not untracked.
        cs << ctx.recordTypeOpaqueResult();
}

// Classify the use of `name` and emit the appropriate recording instruction.
// Falls back to plain recordType() when called from inside a promise context.
// Sentinel for `ldvarCachedPos` (declared above): no ldvar_cached_ was
// emitted, don't patch.
static void emitRecordTypeForVar(CompilerContext& ctx, CodeStream& cs,
                                 SEXP name, unsigned ldvarCachedPos) {
    // Recordless does not optimize inside promises: every record emitted in a
    // promise body is a plain untracked record_type_. Returning here (rather
    // than relying on the degradation in recordTypeTracked) is what suppresses
    // the *classification* as well — otherwise classifyUse could still make
    // this use NoRecord (no opcode at all) or RecordOnce, both of which are
    // optimizations. Covers default formal arguments too, which are compiled as
    // promises; the !mainBodyCtx_ test additionally catches them before any
    // main-body context exists.
    if (ctx.isInPromise() || !ctx.mainBodyCtx_) {
        cs << ctx.recordTypeUntracked();
        return;
    }

    using UseKind = DefUseAnalysis::UseKind;
    using ForceBehaviorKind = DefUseAnalysis::ForceBehaviorKind;
    auto uc = ctx.classifyUse(name);
    ForceBehaviorKind fbKind = uc.forceBehavior;
    // Slot allocated by this call (sentinel = no slot allocated). Filled in by
    // each branch below; used at the end to register the FB kind in one place.
    static constexpr int kNoAllocatedSlot = -1;
    int allocatedSlot = kNoAllocatedSlot;
    // Whether a record_type_once_ (not the record_type_ fallback) was emitted.
    // Only the once-variant carries the per-code bitmap the fbRecordOnce ldvar
    // gates on; the fallback records every time, so FB must record every time.
    bool emittedRecordTypeOnce = false;
    if (uc.kind == UseKind::NoRecord) {
        allocatedSlot = (int)ctx.registerNoRecordDep(uc.defSlot);
        // No record instruction is emitted, so nothing stamps a value record —
        // yet the value IS on the stack (the ldvar just above pushed it) and
        // its type IS already described by a slot: uc.defSlot, by the very
        // definition of NoRecord. Stamp that, so an enclosing `x <- y` can give
        // x the same source and keep chains of copies (`b <- a; c <- b; ...`)
        // fully elided. The position is the ldvar's; it is the last
        // value-changing instruction, and later patching it to
        // ldvar_cached_noRecordFB_ rewrites the opcode in place without moving
        // the instruction.
        ctx.noteValueRecordAt(uc.defSlot, cs.lastValueInstructionPos());
        // record_type_once_promise_ / ldvar_cached_envRecordFB_ disabled:
        // } else if (ctx.code.top()->isPromiseContext() && ...) { EnvBit ... }
    } else {
        switch (uc.kind) {
        case UseKind::NoRecord:
            assert(false && "no record unreachable");
            break; // unreachable: handled above
        case UseKind::RecordOnce: {
            int slot = ctx.typeFeedbackBuilder.addType();
            allocatedSlot = slot;
            ctx.defUseAnalysis().trackUseDef(name, slot);
            // Wire this leaf into the expression tree so its single recording
            // notifies the parent (RecordOnce records an invariant type, so one
            // notification is sufficient — see merge analysis).
            ctx.registerLeafSlot((uint32_t)slot);
            auto& bitmapSize = ctx.code.top()->recordTypeOnceBitmapSize;
            if (ctx.defUseAnalysis().isRangeBasedForLoopVar(name)) {
                // Deferred: bit assigned later when outermost
                // range-based scope finishes.
                int total = (int)bitmapSize +
                            ctx.defUseAnalysis().rangeVarTotalPending();
                if (RECORD_TYPE_ONCE_VALID_SLOT_IDX(slot) &&
                    total < (int)RECORD_TYPE_ONCE_MAX_IIDX) {
                    unsigned bcPos = cs.currentPos();
                    cs << ctx.recordTypeOnceForSlot(slot, 0);
                    ctx.defUseAnalysis().registerRangeVarUse(name, bcPos, slot);
                    emittedRecordTypeOnce = true;
                } else {
                    cs << ctx.recordTypeForSlot(slot);
                }
            } else if (ctx.defUseAnalysis().assignedInEnclosingLoop(name) &&
                       ctx.defUseAnalysis().hasClearableScope()) {
                // Dynamic: re-assigned in an enclosing loop.
                // Defer bit assignment to the innermost clearable
                // scope so stable bits never interleave with the
                // clear range.
                int total = (int)bitmapSize +
                            ctx.defUseAnalysis().rangeVarTotalPending();
                if (RECORD_TYPE_ONCE_VALID_SLOT_IDX(slot) &&
                    total < (int)RECORD_TYPE_ONCE_MAX_IIDX) {
                    unsigned bcPos = cs.currentPos();
                    cs << ctx.recordTypeOnceForSlot(slot, 0);
                    ctx.defUseAnalysis().registerClearableUse(name, bcPos,
                                                              slot);
                    emittedRecordTypeOnce = true;
                } else {
                    cs << ctx.recordTypeForSlot(slot);
                }
            } else if (RECORD_TYPE_ONCE_VALID_SLOT_IDX(slot) &&
                       bitmapSize < RECORD_TYPE_ONCE_MAX_IIDX) {
                // Stable: assign bit immediately.
                cs << ctx.recordTypeOnceForSlot(slot, bitmapSize++);
                emittedRecordTypeOnce = true;
            } else {
                cs << ctx.recordTypeForSlot(slot);
            }
            break;
        }
        case UseKind::RecordAlways:
            cs << ctx.recordTypeTracked(name);
            break;
        }
    }

    // Patch the ldvar_cached_ opcode (if one was emitted) based on the
    // force-behavior strategy. FBValue and Infer both skip runtime FB
    // recording — distinguishing them is needed for later JIT-time decisions,
    // not at the interpreter level.
    if (ldvarCachedPos != kNoLdvarCached) {
        switch (fbKind) {
        case ForceBehaviorKind::FBValue:
        case ForceBehaviorKind::Infer:
            cs.patchOpcode(ldvarCachedPos, Opcode::ldvar_cached_noRecordFB_);
            break;
        case ForceBehaviorKind::EnvBit: // disabled — falls through to Always
                                        // (record every time)
            break;
        case ForceBehaviorKind::RecordOnce:
            // Gate FB recording on the per-code bitmap — but only when a
            // record_type_once_ actually carries that bitmap. If the
            // record_type_ fallback was emitted (no bit available), the type
            // is recorded every time, so leave the default ldvar_cached_ to
            // record FB every time too.
            if (emittedRecordTypeOnce)
                cs.patchOpcode(ldvarCachedPos,
                               Opcode::ldvar_cached_fbRecordOnce_);
            break;
        case ForceBehaviorKind::Always:
            // Default ldvar_cached_ already emitted; no patch.
            break;
        }
    }

    // Remember the FB decision on the allocated slot (if any) so the JIT can
    // reconstruct it later. Skipped for the implicit Always default.
    assert(fbKind == ForceBehaviorKind::Always ||
           allocatedSlot != kNoAllocatedSlot);
    if (fbKind != ForceBehaviorKind::Always &&
        allocatedSlot != kNoAllocatedSlot) {
        ctx.typeFeedbackBuilder.setForceBehaviorKind((uint32_t)allocatedSlot,
                                                     fbKind);
    }
}

// Lookup
void compileGetvar(CompilerContext& ctx, SEXP name) {
    CodeStream& cs = ctx.cs();
    if (DDVAL(name)) {
        cs << BC::ldddvar(name);
    } else if (name == R_MissingArg) {
        cs << BC::push(R_MissingArg);
    } else {
        unsigned ldvarCachedPos = kNoLdvarCached;
        if (ctx.code.top()->isCached(name)) {
            auto const cache_slot = ctx.code.top()->cacheSlotFor(name);
            ldvarCachedPos = cs.currentPos();
            cs << BC::ldvarCached(name, cache_slot);
        } else {
            cs << BC::ldvar(name);
        }
        if (Compiler::profile) {
            if (Compiler::recordLess_Leaf_Enabled)
                emitRecordTypeForVar(ctx, cs, name, ldvarCachedPos);
            else
                cs << ctx.recordTypeUntracked();
        }
    }
}

// Constant
void compileConst(CodeStream& cs, SEXP constant) {
    SET_NAMED(constant, 2);
    cs << BC::push(constant) << BC::visible();
}
void compileExpr(CompilerContext& ctx, SEXP exp, bool voidContext) {

    // Dispatch on the current type of AST node
    switch (TYPEOF(exp)) {
        // Function application
    case LANGSXP: {

#ifdef RECORDLESS_EXPTREE_DEBUG
        std::cerr << "pushing slot node for expr: \n";
        Rf_PrintValue(exp);
        std::cerr << "\n\n";
#endif

        ctx.pushNewNodeForSlots();

        auto fun = CAR(exp);
        auto args = CDR(exp);
        compileCall(ctx, exp, fun, args, voidContext);

        ctx.popNodeForSlots();

    } break;
        // Variable lookup
    case SYMSXP:

        compileGetvar(ctx, exp);
        if (voidContext)
            ctx.cs() << BC::pop();
        break;
    case PROMSXP: {
        auto expr = PREXPR(exp);
        // TODO: honestly I do not know what should be the semantics of
        //       this shit.... For now force it here and see what
        //       breaks...
        //       * One of the callers that does this is eg. print.c:1013
        //       * Another (a bit more sane) producer of this kind of ast
        //         is eval.c::applydefine (see rhsprom). At least there
        //         the prom is already evaluated and only used to attach
        //         the expression to the already evaled value
        if (!voidContext) {
            SEXP val = evaluatePromise(exp);
            Protect p(val);
            compileConst(ctx.cs(), val);
            ctx.cs().addSrc(expr);
        }
    } break;
    case BCODESXP:
    case EXTERNALSXP:
        assert(false);
        break;
        // TODO : some code (eg. serialize.c:2154) puts closures into asts...
        //        not sure how we want to handle it...
        // Case(CLOSXP) {
        //     assert(false);
        // }

    default:
        if (!voidContext)
            compileConst(ctx.cs(), exp);
        break;
    }
}

Code* compilePromise(CompilerContext& ctx, SEXP exp) {
    ctx.pushPromiseContext(exp);
    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    return ctx.pop();
}

/* Create a promise code object without compiling the AST to RIR bytecode.
   This is useful for evaluated promises: the bytecode is never used since
   the value is already stored in the promise.
*/
Code* compilePromiseNoRir(CompilerContext& ctx, SEXP exp) {
    ctx.pushPromiseContext(exp);
    ctx.cs() << BC::push(R_NilValue) << BC::ret();
    return ctx.pop();
}

} // anonymous namespace

SEXP Compiler::finalize() {
    FunctionWriter function;
    CompilerContext ctx(function, preserve);

    FunctionSignature signature(FunctionSignature::Environment::CallerProvided,
                                FunctionSignature::OptimizationLevel::Baseline);

    if (Compiler::recordLess_Leaf_Enabled) {
        ctx.cfgBuilder.configure(formals, exp);

        // Pre-scan the function body. Stored on ctx for reuse at inner-
        // function call sites when computing safe captures to hand off.
        DefUseAnalysis::collectAssignedVars(exp, ctx.bodyAssignedCount_);
        DefUseAnalysis::collectInnerSuperAssigned(exp, ctx.innerSuperAssigned_);
        DefUseAnalysis::collectForLoopVars(exp, ctx.forLoopVars_);
        // DefUseAnalysis::collectPureReadVars(exp, ctx.readVars_);

        for (RListIter arg = RList(formals).begin(); arg != RList::end(); ++arg)
            if (arg.tag() != R_NilValue && TYPEOF(arg.tag()) == SYMSXP)
                ctx.formalNames_.insert(arg.tag());

        // outerImmutable_: incoming immutable captures, minus names shadowed by
        // this function's own formals (formal completely hides the outer name)
        // or body-assigned here (local assignment breaks immutability).
        for (SEXP s : outerImmutable)
            if (!ctx.formalNames_.count(s) && !ctx.bodyAssignedCount_.count(s))
                ctx.outerImmutable_.insert(s);

        // outerControlled_: incoming controlled captures (superset of
        // immutable), minus names shadowed by this function's own formals.
        // Body-assigned names stay: when the local def hasn't run, ldvar falls
        // through to the controlled outer env, not to global.
        for (SEXP s : outerControlled)
            if (!ctx.formalNames_.count(s))
                ctx.outerControlled_.insert(s);

        // functionLocalOrParam_: own formals + body-assigned, minus any var
        // <<-assigned in an inner function (those can mutate without a
        // visible local stvar).
        for (SEXP f : ctx.formalNames_)
            if (!ctx.innerSuperAssigned_.count(f))
                ctx.functionLocalOrParam_.insert(f);
        for (auto& kv : ctx.bodyAssignedCount_)
            if (!ctx.innerSuperAssigned_.count(kv.first))
                ctx.functionLocalOrParam_.insert(kv.first);
    }

    // Compile formals (if any) and create signature
    for (RListIter arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (*arg == R_MissingArg) {
            function.addArgWithoutDefault();
        } else {
            Code* compiled = compilePromise(ctx, *arg);
            function.addDefaultArg(compiled);
        }
        signature.pushFormal(*arg, arg.tag());
    }

    ctx.push(exp, closureEnv);

    // Prepopulate all binding cache numbers for all variables occuring in the
    // function.
    std::function<void(SEXP)> scanNames = [&](SEXP e) {
        if (TYPEOF(e) == LANGSXP)
            for (auto n : RList(CDR(e))) {
                if (CAR(e) == symbol::rm) {
                    ctx.code.top()->loadsSlotInCache[n] =
                        CodeContext::BindingCacheDisabled;
                } else if (TYPEOF(n) == SYMSXP) {
                    ctx.code.top()->cacheSlotFor(n);
                } else {
                    scanNames(n);
                }
            }
    };
    scanNames(exp);

    compileExpr(ctx, exp);
    ctx.cs() << BC::ret();
    Code* body = ctx.pop();
    TypeFeedback* feedback = ctx.typeFeedbackBuilder.build();
    ctx.setTypeFeedbackParents(*feedback);
    feedback->buildNoRecordReverseMap();
#ifdef RIR_RECORD_STATS
    feedback->setStatsUntrackedSlots(ctx.untrackedStatsSlots_);
#endif

    PROTECT(feedback->container());
    function.finalize(body, signature, Context(), feedback);
    // function.function()->recordTypeOncePromiseCount =
    //     (uint16_t)ctx.recordTypeOncePromiseBitmapSize;  // disabled
    UNPROTECT(1);

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(function.function()->container());
#endif

    return function.function()->container();
}

bool Compiler::unsoundOpts =
    !(getenv("UNSOUND_OPTS") &&
      std::string(getenv("UNSOUND_OPTS")).compare("off") == 0);

bool Compiler::profile =
    !(getenv("RIR_PROFILING") &&
      std::string(getenv("RIR_PROFILING")).compare("off") == 0);

bool Compiler::loopPeelingEnabled = true;

// The ldvar-leaf optimization toggle (runtime). Defined here (single TU) rather
// than in recordless.h, which is a macros-only header included broadly.
bool Compiler::recordLess_Leaf_Enabled = true;

} // namespace rir
