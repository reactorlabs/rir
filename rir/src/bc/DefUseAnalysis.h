#ifndef RIR_BC_DEF_USE_ANALYSIS_H
#define RIR_BC_DEF_USE_ANALYSIS_H

#include "R/Symbols.h"
#include "R/r.h"
#include "runtime/TypeFeedback.h"

#include <cstdint>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace rir {

// Tracks variable definitions during bytecode compilation and classifies each
// use site into one of three recording strategies:
//
//   NoRecord   — unique dominating def D, U post-dominates D.
//                Type info at D == type info at U: skip use-site recording,
//                redirect the JIT to the def's feedback slot.
//
//   RecordOnce — unique dominating def D, but U does NOT post-dominate D
//                (D can execute without U executing, so D's slot accumulates
//                more samples). Record at U only on the first invocation.
//
//   RecordAlways — no unique dominating def, or other ambiguous cases.
//                  Standard record-every-time behaviour.

class DefUseAnalysis {
  public:
    // ---- types ----

    // A def records the innermost open scope at the time it was made, plus
    // the monotonic return/loop-exit counters snapshot. The scope id alone
    // is enough to check dominance and same-scope post-dominance because
    // scopes nest strictly: if the innermost is still open, every enclosing
    // one is too; if it's closed, D is not reachable from the current point.
    struct Def {
        int scopeId; // innermost enclosing scope id; 0 = top-level
        int closedReturnCount;
        int closedLoopExitCount;
        int feedbackSlot; // TypeFeedback slot of the def's record_type_, or
                          // kNoSlot
    };

    static constexpr int kNoSlot = -1;

    enum class UseKind { NoRecord, RecordOnce, RecordAlways };

    // ForceBehaviorKind is shared with the persistent TypeFeedback layer and
    // is defined in runtime/TypeFeedback.h. classifyUse never returns EnvBit
    // directly — emitRecordTypeForVar overrides the kind when the env-bitmap
    // promise path applies.
    using ForceBehaviorKind = rir::ForceBehaviorKind;

    struct UseClassification {
        UseKind kind;
        int defSlot; // valid when kind == NoRecord; kNoSlot otherwise
        ForceBehaviorKind forceBehavior;
    };

    // A single unified stack of open control-flow scopes (branches AND loops),
    // ordered by actual source/AST nesting. Branches and loops share one id
    // namespace (`nextScopeId_`); `kind` is kept only where the algorithm
    // truly needs to distinguish them (`markLoopExit` targets branches;
    // `enterLoop`/`exitLoop` track `loopDepth_`).
    struct ScopeEntry {
        enum class Kind : uint8_t { Branch, Loop };
        Kind kind;
        int id;
        int loopDepthAtEntry; // loopDepth_ right before this scope was pushed
        bool hasReturn = false;
        bool hasLoopExit = false; // Branch only; ignored for Loop
    };

    // Per currently-open loop: the expected count of assignments to each
    // variable in the loop body (from an AST pre-scan) and a running count
    // of assignments actually seen so far during compilation. When
    // `seen < expected`, the back-edge can still feed an as-yet-unprocessed
    // def into earlier uses, so we cannot rely on dominance.
    struct LoopBodyInfo {
        int scopeId;
        std::unordered_map<SEXP, int> expected;
        std::unordered_map<SEXP, int> seen;
    };

    struct DefsSnapshot {
        std::unordered_map<SEXP, Def> defs;
        std::unordered_map<SEXP, std::vector<Def>> useDefs;
        int closedReturnCount;
        std::vector<int> closedLoopExitByDepth;
        std::vector<ScopeEntry> scopeStack;
        std::vector<LoopBodyInfo> loopBodyDefs;
        int loopDepth;
    };

    // ---- data ----

    DefUseAnalysis() = default;
    DefUseAnalysis(const std::unordered_set<SEXP>* localOrParam,
                   const std::unordered_set<SEXP>* outerControlled,
                   const std::unordered_set<SEXP>* outerImmutable,
                   const std::unordered_set<SEXP>* formalNames,
                   std::unordered_set<SEXP> argAssignedVars = {})
        : localOrParam_(localOrParam), outerControlled_(outerControlled),
          outerImmutable_(outerImmutable), formalNames_(formalNames),
          argAssignedVars_(std::move(argAssignedVars)) {}

    // bool hasRead(SEXP name) const {
    //     return readVars_ && readVars_->count(name);
    // }

    // // Collect all symbols that appear in pure-read (value) position in
    // `ast`.
    // // Excludes: direct symbol LHS of plain assignments (`v <- expr`), and
    // the
    // // container chain of subscript LHS (`v[i] <- expr` — v is
    // ldvarForUpdate,
    // // not a value read). Index expressions inside subscript LHS are
    // included.
    // // Does not recurse into nested function bodies.
    // static void collectPureReadVars(SEXP ast, std::unordered_set<SEXP>& out)
    // {
    //     if (!ast || ast == R_NilValue)
    //         return;
    //     if (TYPEOF(ast) == SYMSXP) {
    //         out.insert(ast);
    //         return;
    //     }
    //     if (TYPEOF(ast) != LANGSXP)
    //         return;
    //     SEXP fun = CAR(ast);
    //     if (TYPEOF(fun) == SYMSXP && fun == symbol::Function)
    //         return;
    //     if (TYPEOF(fun) == SYMSXP &&
    //         (fun == symbol::Assign || fun == symbol::Assign2 ||
    //          fun == symbol::SuperAssign)) {
    //         // Skip the LHS entirely — it is a write target, not a read.
    //         // If the LHS is a subscript expression, scan its index args
    //         (they
    //         // are reads), but not the container itself.
    //         SEXP lhs = CADR(ast);
    //         if (TYPEOF(lhs) == LANGSXP) {
    //             for (SEXP s = CDDR(lhs); s != R_NilValue; s = CDR(s))
    //                 collectPureReadVars(CAR(s), out);
    //         }
    //         // Scan the RHS.
    //         collectPureReadVars(CADDR(ast), out);
    //         return;
    //     }
    //     for (SEXP s = CDR(ast); s != R_NilValue; s = CDR(s))
    //         collectPureReadVars(CAR(s), out);
    // }

    // Pointer to the function-wide set of local/param variables (owned by
    // CompilerContext::functionLocalOrParam_). Set once per CodeContext; never
    // mutated. Null when recordLessEnabled is off.
    const std::unordered_set<SEXP>* localOrParam_ = nullptr;

    // Pointer to the set of variables captured from enclosing scopes that are
    // within our "realm" — when the local def has not yet run, an ldvar for
    // one of these falls through to a controlled (sentinel-monitorable) env,
    // not to global. Superset of outerImmutable_. Enables RecordOnce.
    // (owned by CompilerContext::outerControlled_)
    const std::unordered_set<SEXP>* outerControlled_ = nullptr;

    // Pointer to the strict subset of outerControlled_ whose values truly do
    // not change during this function's lifetime (formals of outer never
    // reassigned, body-locals of outer assigned once with dominating def).
    // Reserved for future cross-invocation optimizations.
    // (owned by CompilerContext::outerImmutable_)
    const std::unordered_set<SEXP>* outerImmutable_ = nullptr;

    // Pointer to this function's formal parameter names (owned by
    // CompilerContext::formalNames_). Formals are always bound at call time,
    // so RecordOnce is sound for them even without a dominating stvar def.
    const std::unordered_set<SEXP>* formalNames_ = nullptr;

    // // Pointer to the set of variables that appear in pure-read (value)
    // position
    // // in the function body (owned by CompilerContext::readVars_). Used to
    // gate
    // // the post-subassign record_type_ so we don't record when no ldvar will
    // // ever consume the slot.
    // const std::unordered_set<SEXP>* readVars_ = nullptr;

    // Variables assigned inside promise-argument positions of this code
    // context's AST. Assignments there run at an unpredictable point (when
    // the promise is forced), invisible to the DFA — so we exclude them from
    // all optimizations (RecordAlways).
    std::unordered_set<SEXP> argAssignedVars_;

    bool isArgAssigned(SEXP name) const {
        return argAssignedVars_.count(name) > 0;
    }

    std::unordered_map<SEXP, Def> defs_;
    std::unordered_map<SEXP, std::vector<Def>> useDefs_;
    int loopDepth_ = 0;

    std::vector<ScopeEntry> scopeStack_;
    int nextScopeId_ = 1;
    std::vector<LoopBodyInfo> loopBodyDefs_;
    int closedReturnCount_ = 0;
    std::vector<int> closedLoopExitByDepth_;

    // Per-name nesting count of for-loop iteration variables currently being
    // compiled. The for-loop's implicit stvar reassigns this each iteration
    // with a single, type-stable value extracted from the seq, so within one
    // iteration the type is consistent — useDefs dedup is safe even though
    // there is no trackDef-tracked dominating def.
    std::unordered_map<SEXP, int> forLoopVarDepth_;

    // Stack of currently-active range-based for-loop iteration variables
    // (seq is `:`, `seq_len`, or `seq_along` — type stable across iterations).
    //
    // For nested loops a clear_record_type_once_bits_range_ is emitted before
    // each loop body.  Bit indices are assigned lazily: each use site is
    // emitted with a placeholder (bitIdx=0) and registered here; when the
    // outermost range-based scope finishes (rangeBasedForLoopVars_ becomes
    // empty) all pending use sites and clear templates are patched in
    // outermost-first order so each var's bits form a contiguous range.
    struct RangeBasedLoopVarEntry {
        struct UseSite {
            unsigned pos; // bytecode position of record_type_once_
            int slot;     // TypeFeedback slot
        };
        SEXP sym;
        bool nested;               // true when a clear template was emitted
        unsigned clearTemplatePos; // bytecode position of the clear template
        int pendingCount = 0;      // number of use sites recorded so far
        std::vector<UseSite> useSites;
    };
    std::vector<RangeBasedLoopVarEntry> rangeBasedForLoopVars_;
    // Entries that have been popped but not yet assigned final bit indices.
    // Ordered innermost-first (outermost is at the back after all pops).
    std::vector<RangeBasedLoopVarEntry> pendingRangeVarEntries_;

    // Stack of open clearable loop scopes (while/repeat/non-range-based for).
    // Each entry collects deferred RecordOnce use sites for vars assigned in
    // an enclosing loop. Bits are assigned when the scope finishes
    // (innermost-first), so they always come after stable bits, avoiding the
    // interleaving problem of a simple [first, last+1) range approach.
    struct ClearableScopeEntry {
        unsigned clearTemplatePos;
        // Size of loopBodyDefs_ at the moment this scope was pushed. Used to
        // map a loopBodyDefs_ assignment index to the right clearable scope:
        // the first scope with loopBodyDefsIdxAtPush > found is the loop
        // directly inside the loop that assigns the var.
        int loopBodyDefsIdxAtPush;
        std::vector<RangeBasedLoopVarEntry::UseSite> useSites;
    };
    std::vector<ClearableScopeEntry> clearableScopeStack_;

    // ---- compile-time state updates ----

    bool isLocalOrParam(SEXP name) const {
        return localOrParam_ && localOrParam_->count(name) > 0;
    }
    bool isOuterControlled(SEXP name) const {
        return outerControlled_ && outerControlled_->count(name) > 0;
    }
    bool isOuterImmutable(SEXP name) const {
        return outerImmutable_ && outerImmutable_->count(name) > 0;
    }
    bool isFormal(SEXP name) const {
        return formalNames_ && formalNames_->count(name) > 0;
    }
    // A use is optimizable when the variable is provably "under our control":
    //   isFormal          — always bound at call time
    //   isOuterControlled — fallthrough goes to a controlled env, not global
    //   hasDominatingDef  — a local stvar dominates this point (no path reads
    //                       an unbound value from an uncontrolled outer scope)
    bool isOptimizable(SEXP name) const {
        return isFormal(name) || isOuterControlled(name) ||
               hasDominatingDef(name);
    }

    // True when `name` is assigned in the body of any currently-open loop.
    // Such a variable can change type between iterations, so one recording
    // per invocation is not representative — RecordOnce is unsafe.
    bool assignedInEnclosingLoop(SEXP name) const {
        for (const auto& info : loopBodyDefs_) {
            auto it = info.expected.find(name);
            if (it != info.expected.end() && it->second > 0)
                return true;
        }
        return false;
    }

    // True when `name` is assigned in the body of the INNERMOST currently-open
    // loop. Used to guard the "dominating def in outer scope → RecordOnce"
    // case: if the def is from the same loop body as the use, the value can
    // change each iteration and no per-iteration clear mechanism exists.
    bool assignedInInnermostLoop(SEXP name) const {
        if (loopBodyDefs_.empty())
            return false;
        const auto& inner = loopBodyDefs_.back();
        auto it = inner.expected.find(name);
        return it != inner.expected.end() && it->second > 0;
    }

    // True when defs_ contains a def of `name` that dominates the current
    // compilation point (no closed-scope or unseen-loop-def issues). Public
    // wrapper around findReachingDef for use at inner-function call sites.
    bool hasDominatingDef(SEXP name) const {
        return findReachingDef(name) != nullptr;
    }

    // Like findReachingDef but ignores hasUnseenLoopDef. Use when a dominating
    // local assignment is sufficient to establish control — even if the loop
    // back-edge has an unseen def that might shadow it later.
    const Def* findDominatingDef(SEXP name) const {
        auto it = defs_.find(name);
        if (it == defs_.end())
            return nullptr;
        return dominates(it->second) ? &it->second : nullptr;
    }

    void trackDef(SEXP name, int feedbackSlot = kNoSlot) {
        useDefs_.erase(name);
        defs_[name] = {currentScopeId(), closedReturnCount_,
                       currentLoopExitCount(), feedbackSlot};
        bumpSeen(name);
    }

    void pushClearableScope(unsigned clearTemplatePos) {
        clearableScopeStack_.push_back(
            {clearTemplatePos, (int)loopBodyDefs_.size(), {}});
    }
    ClearableScopeEntry popClearableScope() {
        auto e = std::move(clearableScopeStack_.back());
        clearableScopeStack_.pop_back();
        return e;
    }
    // Register a deferred dynamic use site. The var is assigned in some
    // enclosing loop L; we find the first loopBodyDefs_[k] that contains
    // `name` and register with clearableScopeStack_[k] — the clearable scope
    // for the loop directly inside L.
    // Register a deferred dynamic use site with the correct clearable scope.
    // collectAssignedVars recurses into nested loops, so loopBodyDefs_[k]
    // contains vars from all nested sub-loops too. We therefore scan all
    // entries and take the LAST (deepest/innermost) match — that is the
    // innermost enclosing loop that directly assigns `name`, whose directly
    // inner loop's clearable scope (clearableScopeStack_[k]) is the right one.
    void registerClearableUse(SEXP name, unsigned bcPos, int slot) {
        int found = -1;
        for (int k = 0; k < (int)loopBodyDefs_.size(); ++k) {
            auto it = loopBodyDefs_[k].expected.find(name);
            if (it != loopBodyDefs_[k].expected.end() && it->second > 0)
                found = k;
        }
        assert(found >= 0);
        // Find the first clearable scope whose loop is directly inside the
        // loop at loopBodyDefs_[found]. Range-based for-loops push to
        // loopBodyDefs_ but NOT to clearableScopeStack_, so we cannot index
        // directly. Instead find the first entry with loopBodyDefsIdxAtPush >
        // found.
        for (auto& e : clearableScopeStack_) {
            if (e.loopBodyDefsIdxAtPush > found) {
                e.useSites.push_back({bcPos, slot});
                return;
            }
        }
        assert(false && "no clearable scope inside loop that assigns this var");
    }
    bool hasClearableScope() const { return !clearableScopeStack_.empty(); }

    void trackUseDef(SEXP name, int slot) {
        useDefs_[name].push_back({currentScopeId(), closedReturnCount_,
                                  currentLoopExitCount(), slot});
    }

    void enterBranch() {
        scopeStack_.push_back(
            {ScopeEntry::Kind::Branch, nextScopeId_++, loopDepth_});
    }
    void exitBranch() {
        const auto& entry = scopeStack_.back();
        if (entry.hasReturn)
            ++closedReturnCount_;
        if (entry.hasLoopExit) {
            int d = entry.loopDepthAtEntry;
            if (d >= (int)closedLoopExitByDepth_.size())
                closedLoopExitByDepth_.resize(d + 1, 0);
            ++closedLoopExitByDepth_[d];
        }
        scopeStack_.pop_back();
    }

    void enterLoop() {
        enterLoopContext();
        enterLoopScope();
    }
    void exitLoop() {
        const auto& entry = scopeStack_.back();
        if (entry.hasReturn)
            ++closedReturnCount_;
        loopDepth_--;
        scopeStack_.pop_back();
    }

    // For while loops: call before compiling the condition. Increments
    // loopDepth (so uses without a dominating def classify as RecordOnce)
    // but does NOT push a scope entry — post-dominance queries against
    // pre-loop defs still see the condition as same-scope, enabling
    // NoRecord for uses that have a unique reaching def before the loop.
    void enterLoopContext() { loopDepth_++; }

    // For while loops: call after the condition, before the body.
    // Pushes the loop scope so uses in the body see a distinct scope id.
    void enterLoopScope() {
        scopeStack_.push_back(
            {ScopeEntry::Kind::Loop, nextScopeId_++, loopDepth_ - 1});
    }

    // A `return` is attributed to the innermost open scope — whichever was
    // pushed most recently. When that scope closes, `closedReturnCount_` is
    // bumped, making the return visible to all downstream compilation points.
    void markReturn() {
        if (!scopeStack_.empty())
            scopeStack_.back().hasReturn = true;
    }

    // A `break`/`next` is attributed to the innermost enclosing Branch at the
    // current loop depth. The counter it bumps (`closedLoopExitByDepth_`) is
    // consumed by later code at the same loop depth; `break`/`next` outside
    // any branch leaves only dead code afterward.
    void markLoopExit() {
        for (auto it = scopeStack_.rbegin(); it != scopeStack_.rend(); ++it) {
            if (it->kind == ScopeEntry::Kind::Branch &&
                it->loopDepthAtEntry == loopDepth_) {
                it->hasLoopExit = true;
                return;
            }
        }
    }

    void setLoopBodyDefs(std::unordered_map<SEXP, int> expected) {
        loopBodyDefs_.push_back({currentScopeId(), std::move(expected), {}});
    }
    void clearLoopBodyDefs() { loopBodyDefs_.pop_back(); }

    void pushForLoopVar(SEXP sym) { ++forLoopVarDepth_[sym]; }
    void popForLoopVar(SEXP sym) {
        auto it = forLoopVarDepth_.find(sym);
        if (it != forLoopVarDepth_.end() && --it->second == 0)
            forLoopVarDepth_.erase(it);
    }
    bool isForLoopVar(SEXP name) const {
        return forLoopVarDepth_.count(name) > 0;
    }

    void pushRangeBasedForLoopVar(SEXP sym, bool nested,
                                  unsigned clearTemplatePos) {
        rangeBasedForLoopVars_.push_back(
            {sym, nested, clearTemplatePos, 0, {}});
    }
    // Move the innermost active entry to pendingRangeVarEntries_.
    void moveRangeVarToPending() {
        pendingRangeVarEntries_.push_back(
            std::move(rangeBasedForLoopVars_.back()));
        rangeBasedForLoopVars_.pop_back();
    }
    bool isRangeBasedForLoopVar(SEXP name) const {
        for (const auto& e : rangeBasedForLoopVars_)
            if (e.sym == name)
                return true;
        return false;
    }
    // Register a deferred RecordOnce use site for a range-based loop var.
    void registerRangeVarUse(SEXP name, unsigned bcPos, int slot) {
        for (auto& e : rangeBasedForLoopVars_) {
            if (e.sym == name) {
                e.pendingCount++;
                e.useSites.push_back({bcPos, slot});
                return;
            }
        }
    }
    // True when all range-based scopes have finished and the final bit
    // assignment can be performed.
    bool rangeVarAssignmentReady() const {
        return rangeBasedForLoopVars_.empty() &&
               !pendingRangeVarEntries_.empty();
    }
    // Total deferred bits across all active and pending entries (for budget
    // check before emitting a new deferred use site).
    int rangeVarTotalPending() const {
        int total = 0;
        for (const auto& e : rangeBasedForLoopVars_)
            total += e.pendingCount;
        for (const auto& e : pendingRangeVarEntries_)
            total += e.pendingCount;
        return total;
    }

    int loopDepth() const { return loopDepth_; }

    // ---- save / restore for loop peeling ----

    DefsSnapshot saveState() const {
        return {defs_,
                useDefs_,
                closedReturnCount_,
                closedLoopExitByDepth_,
                scopeStack_,
                loopBodyDefs_,
                loopDepth_};
    }
    void restoreState(DefsSnapshot&& s) {
        defs_ = std::move(s.defs);
        useDefs_ = std::move(s.useDefs);
        closedReturnCount_ = s.closedReturnCount;
        closedLoopExitByDepth_ = std::move(s.closedLoopExitByDepth);
        scopeStack_ = std::move(s.scopeStack);
        loopBodyDefs_ = std::move(s.loopBodyDefs);
        loopDepth_ = s.loopDepth;
    }

    // ---- query ----

    // Classify the use of `name` at the current compilation point.
    UseClassification classifyUse(SEXP name) const {
        // Variables assigned in promise-argument positions are not tracked by
        // the DFA (the assignment runs when the promise is forced, which is
        // invisible to the main code's stvar sequence). Exclude from all
        // optimizations.
        if (isArgAssigned(name))
            return {UseKind::RecordAlways, kNoSlot, ForceBehaviorKind::Always};

        // Only locals/params and stable outer captures are eligible for
        // NoRecord-via-useDefs and RecordOnce. Free variables from outer
        // scopes (not captured-stable) must always be recorded.
        const Def* d = findReachingDef(name);
        const bool optimizable = isFormal(name) || isOuterControlled(name) ||
                                 (isLocalOrParam(name) && d != nullptr);

        // A reaching def from a local stvar means the binding currently holds
        // the result of an expression evaluation — always a value, never a
        // promise. Used to pick FBValue vs Infer.
        const bool hasLocalStvarReach = d != nullptr && isLocalOrParam(name);

        if (optimizable || isForLoopVar(name) ||
            (isLocalOrParam(name) && findDominatingDef(name))) {
            // useDefs dedup: a previously recorded use that dominates and
            // post-dominates this point has the same value — skip re-recording.
            // For-loop iter vars and locals with a dominating def (even when
            // hasUnseenLoopDef blocks findReachingDef) are also admitted: the
            // variable is under local control so prior recordings are valid.
            auto udIt = useDefs_.find(name);
            if (udIt != useDefs_.end()) {
                for (const Def& ud : udIt->second) {
                    if (dominates(ud) && postDominates(ud))
                        return {UseKind::NoRecord, ud.feedbackSlot,
                                hasLocalStvarReach || isForLoopVar(name)
                                    ? ForceBehaviorKind::FBValue
                                    : ForceBehaviorKind::Infer};
                }
            }
        }

        if (d && isLocalOrParam(name) && postDominates(*d) &&
            d->feedbackSlot != kNoSlot)
            return {UseKind::NoRecord, d->feedbackSlot,
                    ForceBehaviorKind::FBValue};

        // Unique dominating def from an enclosing loop (doesn't post-dominate):
        // the value changes per enclosing-loop iteration.  RecordOnce is sound;
        // the enclosing loop clears this bit before each inner-loop execution
        // via clear_record_type_once_bits_range_.
        // Guards:
        //   !assignedInInnermostLoop — same-loop def can change each iteration,
        //     no per-iteration clear mechanism for branches within the same
        //     loop.
        //   assignedInEnclosingLoop  — only fire for vars re-assigned in some
        //     enclosing loop; truly stable vars (assigned before all loops)
        //     must NOT land in the clear range.
        if (d != nullptr && isLocalOrParam(name) && loopDepth_ > 0 &&
            !assignedInInnermostLoop(name) && assignedInEnclosingLoop(name))
            return {UseKind::RecordOnce, kNoSlot, ForceBehaviorKind::FBValue};

        // Stable RecordOnce: var has a dominating def / is formal / outer-
        // controlled, AND is not re-assigned in any enclosing loop.  The
        // bit allocated for this use is excluded from the clear range —
        // compileGetvar tracks dynamic vs stable bits and the inner loop
        // clears only [first dynamic bit, last+1).
        if (optimizable && loopDepth_ > 0 && !assignedInEnclosingLoop(name))
            return {UseKind::RecordOnce, kNoSlot,
                    hasLocalStvarReach ? ForceBehaviorKind::FBValue
                                       : ForceBehaviorKind::Infer};

        // Range-based for-loop iter var with no prior useDefs hit: type is
        // stable across iterations of THIS loop (seq is `:`, `seq_len`, or
        // `seq_along`), so RecordOnce is sound even though the for-loop
        // implicitly reassigns sym every iteration. For nested loops the
        // pre-allocated bit gets cleared on each entry to the loop, so a
        // different outer iteration can re-record if the seq's element type
        // changed (queried via rangeBasedForLoopVarBit at compileGetvar).
        if (isRangeBasedForLoopVar(name) && loopDepth_ > 0)
            return {UseKind::RecordOnce, kNoSlot, ForceBehaviorKind::FBValue};

        return {UseKind::RecordAlways, kNoSlot, ForceBehaviorKind::Always};
    }

    // ---- AST pre-scan ----

    // Collect all variables that are <<-assigned inside any inner function
    // (at any nesting depth) of `ast`. These can be modified from a nested
    // closure without a local stvar, so they must not be optimised.
    static void collectInnerSuperAssigned(SEXP ast,
                                          std::unordered_set<SEXP>& out) {
        if (!ast || ast == R_NilValue || TYPEOF(ast) != LANGSXP)
            return;
        SEXP head = CAR(ast);
        if (head == symbol::Function) {
            // Found an inner function: scan its body for <<- at any depth.
            scanForSuperAssigns(CADDR(ast), out);
            return;
        }
        for (SEXP s = CDR(ast); s != R_NilValue; s = CDR(s))
            collectInnerSuperAssigned(CAR(s), out);
    }

    // Collect variables bound by `for (sym in ...)` loops anywhere in `ast`
    // (not recursing into inner functions). For-loop variables are reassigned
    // on every iteration, so they must never be treated as "stable / assigned
    // exactly once" when computing safe captures for inner functions.
    static void collectForLoopVars(SEXP ast, std::unordered_set<SEXP>& out) {
        if (!ast || ast == R_NilValue || TYPEOF(ast) != LANGSXP)
            return;
        SEXP fun = CAR(ast);
        if (TYPEOF(fun) == SYMSXP && fun == symbol::For) {
            SEXP sym = CADR(ast);
            if (TYPEOF(sym) == SYMSXP)
                out.insert(sym);
        }
        if (TYPEOF(fun) == SYMSXP && fun == symbol::Function)
            return;
        for (SEXP s = CDR(ast); s != R_NilValue; s = CDR(s))
            collectForLoopVars(CAR(s), out);
    }

    static void collectAssignedVars(SEXP ast,
                                    std::unordered_map<SEXP, int>& out) {
        if (ast == R_NilValue || TYPEOF(ast) != LANGSXP)
            return;
        SEXP fun = CAR(ast);
        if (TYPEOF(fun) == SYMSXP &&
            (fun == symbol::Assign || fun == symbol::Assign2)) {
            SEXP lhs = CADR(ast);
            while (TYPEOF(lhs) == LANGSXP)
                lhs = CADR(lhs);
            if (TYPEOF(lhs) == SYMSXP)
                out[lhs]++;
        }
        if (TYPEOF(fun) == SYMSXP && fun == symbol::For) {
            SEXP sym = CADR(ast);
            if (TYPEOF(sym) == SYMSXP)
                out[sym]++;
        }
        if (TYPEOF(fun) == SYMSXP && fun == symbol::Function)
            return;
        for (SEXP s = CDR(ast); s != R_NilValue; s = CDR(s)) {
            collectAssignedVars(CAR(s), out);
        }
    }

    // Collect variables assigned inside promise-argument positions of regular
    // function calls. Control-flow forms (if/while/for/repeat/{/switch) and
    // assignment forms compile sub-expressions inline — not promise boundaries.
    // Does not cross inner function definitions.
    static void collectArgAssignedVars(SEXP ast, std::unordered_set<SEXP>& out,
                                       bool inPromise = false) {
        if (!ast || ast == R_NilValue || TYPEOF(ast) != LANGSXP)
            return;
        SEXP fun = CAR(ast);
        if (TYPEOF(fun) == SYMSXP && fun == symbol::Function)
            return;
        const bool isAssign =
            TYPEOF(fun) == SYMSXP &&
            (fun == symbol::Assign || fun == symbol::Assign2 ||
             fun == symbol::SuperAssign);
        const bool isInline =
            isAssign || (TYPEOF(fun) == SYMSXP &&
                         (fun == symbol::If || fun == symbol::While ||
                          fun == symbol::For || fun == symbol::Repeat ||
                          fun == symbol::Block || fun == symbol::Switch));
        if (inPromise && isAssign) {
            SEXP lhs = CADR(ast);
            while (TYPEOF(lhs) == LANGSXP)
                lhs = CADR(lhs);
            if (TYPEOF(lhs) == SYMSXP)
                out.insert(lhs);
        }
        for (SEXP s = CDR(ast); s != R_NilValue; s = CDR(s))
            collectArgAssignedVars(CAR(s), out, inPromise || !isInline);
    }

  private:
    // Recursively scan `ast` for <<- assignments at any depth, including
    // inside further nested functions (conservative: their <<- may bubble up).
    static void scanForSuperAssigns(SEXP ast, std::unordered_set<SEXP>& out) {
        if (!ast || ast == R_NilValue || TYPEOF(ast) != LANGSXP)
            return;
        SEXP head = CAR(ast);
        if (head == symbol::SuperAssign) {
            SEXP lhs = CADR(ast);
            // Walk subscript chain (`x[[i]] <<-`, `x$f <<-`, etc.) to root.
            while (TYPEOF(lhs) == LANGSXP)
                lhs = CADR(lhs);
            if (TYPEOF(lhs) == SYMSXP)
                out.insert(lhs);
        }
        for (SEXP s = CDR(ast); s != R_NilValue; s = CDR(s))
            scanForSuperAssigns(CAR(s), out);
    }

    int currentScopeId() const {
        return scopeStack_.empty() ? 0 : scopeStack_.back().id;
    }

    int currentLoopExitCount() const {
        return (loopDepth_ < (int)closedLoopExitByDepth_.size())
                   ? closedLoopExitByDepth_[loopDepth_]
                   : 0;
    }

    // Increment the "seen" counter for `name` in every currently-open loop
    // body. This lets `hasUnseenLoopDef` compare running totals against the
    // AST pre-scan's expected counts, without needing to re-scan defs_ or
    // track ancestor chains.
    void bumpSeen(SEXP name) {
        for (auto& info : loopBodyDefs_) {
            auto it = info.expected.find(name);
            if (it != info.expected.end())
                ++info.seen[name];
        }
    }

    // Returns true when D dominates the current compilation point — every
    // path from function entry to here passes through D. Equivalent to:
    // D's innermost scope is still on the stack (or D was at top level).
    bool dominates(const Def& d) const {
        if (d.scopeId == 0)
            return true;
        for (const auto& entry : scopeStack_)
            if (entry.id == d.scopeId)
                return true;
        return false;
    }

    // Returns the unique reaching def of `name` at the current compilation
    // point, or nullptr if none exists.
    //
    // We store only the most recent def per name. If it dominates the
    // current point, return it. Otherwise it's in a closed scope: on some
    // paths it overwrote an earlier value, on others it didn't — we can't
    // tell which, so bail out. (We never reset on scope close, so the
    // lingering closed-scope def is precisely what makes that bail-out
    // happen.)
    const Def* findReachingDef(SEXP name) const {
        auto it = defs_.find(name);
        if (it == defs_.end())
            return nullptr;
        if (hasUnseenLoopDef(name))
            return nullptr;
        return dominates(it->second) ? &it->second : nullptr;
    }

    // Returns true when U post-dominates D — every path from D to function
    // exit passes through U.
    //
    // D and U must be in the same innermost scope (which also implies the
    // same loop depth, since scope nesting determines depth), and no return
    // or break/next may have fired in a closed scope between D and U.
    bool postDominates(const Def& d) const {
        if (currentScopeId() != d.scopeId)
            return false;
        if (closedReturnCount_ != d.closedReturnCount)
            return false;
        if (currentLoopExitCount() != d.closedLoopExitCount)
            return false;
        return true;
    }

    bool hasUnseenLoopDef(SEXP name) const {
        for (const auto& info : loopBodyDefs_) {
            auto expIt = info.expected.find(name);
            if (expIt == info.expected.end())
                continue;
            auto seenIt = info.seen.find(name);
            int seen = (seenIt != info.seen.end()) ? seenIt->second : 0;
            if (seen < expIt->second)
                return true;
        }
        return false;
    }

};

} // namespace rir

#endif // RIR_BC_DEF_USE_ANALYSIS_H
