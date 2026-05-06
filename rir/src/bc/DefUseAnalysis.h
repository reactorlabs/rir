#ifndef RIR_BC_DEF_USE_ANALYSIS_H
#define RIR_BC_DEF_USE_ANALYSIS_H

#include "R/Symbols.h"
#include "R/r.h"

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

    struct UseClassification {
        UseKind kind;
        int defSlot; // valid when kind == NoRecord; kNoSlot otherwise
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

    std::unordered_map<SEXP, Def> defs_;
    std::unordered_map<SEXP, std::vector<Def>> useDefs_;
    int loopDepth_ = 0;

    // Stack of (firstDynamicBit, lastDynamicBit) — one entry per currently-open
    // clearable loop scope. As compileGetvar allocates RecordOnce bits, the
    // ones that are "dynamic" (variable re-assigned in some enclosing loop or
    // a range-based for-loop iter var) update every entry on this stack.
    // RangeBasedIterVarScope::finish() pops the top entry and patches the
    // clear placeholder to cover only [first, last+1] — stable bits on either
    // side fall outside the cleared range and persist for the whole invocation.
    // -1 sentinels mean "no dynamic bit recorded yet."
    std::vector<std::pair<int, int>> dynamicBitTracking_;

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
    // Each RecordOnce use-site of the iter var gets its own bit (allocated via
    // bitmapSize++ in compileGetvar). For nested loops, a
    // clear_record_type_once_bits_range_ is emitted before the loop so all
    // per-position bits are cleared on each outer iteration. Peel and main
    // share slot+bit at each position via typeCount/bitmapSize reset after
    // peel body compilation.
    struct RangeBasedLoopVarEntry {
        SEXP sym;
    };
    std::vector<RangeBasedLoopVarEntry> rangeBasedForLoopVars_;

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

    // True when defs_ contains a def of `name` that dominates the current
    // compilation point (no closed-scope or unseen-loop-def issues). Public
    // wrapper around findReachingDef for use at inner-function call sites.
    bool hasDominatingDef(SEXP name) const {
        return findReachingDef(name) != nullptr;
    }

    void trackDef(SEXP name, int feedbackSlot = kNoSlot) {
        useDefs_.erase(name);
        defs_[name] = {currentScopeId(), closedReturnCount_,
                       currentLoopExitCount(), feedbackSlot};
        bumpSeen(name);
    }

    // Push/pop a dynamic-bit tracking entry. Called by RangeBasedIterVarScope
    // when entering/leaving a nested clearable loop scope.
    void pushDynamicBitTracking() { dynamicBitTracking_.push_back({-1, -1}); }
    std::pair<int, int> popDynamicBitTracking() {
        auto p = dynamicBitTracking_.back();
        dynamicBitTracking_.pop_back();
        return p;
    }

    // Record that a "dynamic" RecordOnce bit was just allocated at `bitIdx`.
    // Updates first/last for every currently-open clearable scope: a bit
    // allocated inside an inner loop is also part of every enclosing scope's
    // range and must be cleared by each.
    void recordDynamicBit(uint32_t bitIdx) {
        int idx = (int)bitIdx;
        for (auto& e : dynamicBitTracking_) {
            if (e.first == -1) {
                e.first = idx;
                e.second = idx;
            } else {
                if (idx < e.first)
                    e.first = idx;
                if (idx > e.second)
                    e.second = idx;
            }
        }
    }

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

    void pushRangeBasedForLoopVar(SEXP sym) {
        rangeBasedForLoopVars_.push_back({sym});
    }
    void popRangeBasedForLoopVar() { rangeBasedForLoopVars_.pop_back(); }
    bool isRangeBasedForLoopVar(SEXP name) const {
        for (const auto& e : rangeBasedForLoopVars_)
            if (e.sym == name)
                return true;
        return false;
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
        // Only locals/params and stable outer captures are eligible for
        // NoRecord-via-useDefs and RecordOnce. Free variables from outer
        // scopes (not captured-stable) must always be recorded.
        const Def* d = findReachingDef(name);
        const bool optimizable =
            isFormal(name) || isOuterControlled(name) || d != nullptr;

        if (optimizable || isForLoopVar(name)) {
            // useDefs dedup: a previously recorded use that dominates and
            // post-dominates this point has the same value — skip re-recording.
            // For-loop iter vars are admitted here even though
            // optimizable=false (no trackDef'd reaching def): the for-loop's
            // implicit stvar sets the same type each iteration, so within an
            // iteration any prior RecordAlways recording is current.
            auto udIt = useDefs_.find(name);
            if (udIt != useDefs_.end()) {
                for (const Def& ud : udIt->second) {
                    if (dominates(ud) && postDominates(ud))
                        return {UseKind::NoRecord, ud.feedbackSlot};
                }
            }
        }

        if (d && postDominates(*d))
            return {UseKind::NoRecord, d->feedbackSlot};

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
        if (d != nullptr && loopDepth_ > 0 && !assignedInInnermostLoop(name) &&
            assignedInEnclosingLoop(name))
            return {UseKind::RecordOnce, kNoSlot};

        // Stable RecordOnce: var has a dominating def / is formal / outer-
        // controlled, AND is not re-assigned in any enclosing loop.  The
        // bit allocated for this use is excluded from the clear range —
        // compileGetvar tracks dynamic vs stable bits and the inner loop
        // clears only [first dynamic bit, last+1).
        if (optimizable && loopDepth_ > 0 && !assignedInEnclosingLoop(name))
            return {UseKind::RecordOnce, kNoSlot};

        // Range-based for-loop iter var with no prior useDefs hit: type is
        // stable across iterations of THIS loop (seq is `:`, `seq_len`, or
        // `seq_along`), so RecordOnce is sound even though the for-loop
        // implicitly reassigns sym every iteration. For nested loops the
        // pre-allocated bit gets cleared on each entry to the loop, so a
        // different outer iteration can re-record if the seq's element type
        // changed (queried via rangeBasedForLoopVarBit at compileGetvar).
        if (isRangeBasedForLoopVar(name) && loopDepth_ > 0)
            return {UseKind::RecordOnce, kNoSlot};

        return {UseKind::RecordAlways, kNoSlot};
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

  private:
    // Recursively scan `ast` for <<- assignments at any depth, including
    // inside further nested functions (conservative: their <<- may bubble up).
    static void scanForSuperAssigns(SEXP ast, std::unordered_set<SEXP>& out) {
        if (!ast || ast == R_NilValue || TYPEOF(ast) != LANGSXP)
            return;
        SEXP head = CAR(ast);
        if (head == symbol::SuperAssign) {
            SEXP lhs = CADR(ast);
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
};

} // namespace rir

#endif // RIR_BC_DEF_USE_ANALYSIS_H
