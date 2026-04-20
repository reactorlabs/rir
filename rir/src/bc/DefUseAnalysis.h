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

    std::unordered_map<SEXP, Def> defs_;
    std::unordered_map<SEXP, std::vector<Def>> useDefs_;
    int loopDepth_ = 0;
    std::vector<ScopeEntry> scopeStack_;
    int nextScopeId_ = 1;
    std::vector<LoopBodyInfo> loopBodyDefs_;
    int closedReturnCount_ = 0;
    std::vector<int> closedLoopExitByDepth_;

    // ---- compile-time state updates ----

    void trackDef(SEXP name, int feedbackSlot = kNoSlot) {
        useDefs_.erase(name);
        defs_[name] = {currentScopeId(), closedReturnCount_,
                       currentLoopExitCount(), feedbackSlot};
        bumpSeen(name);
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
        scopeStack_.push_back(
            {ScopeEntry::Kind::Loop, nextScopeId_++, loopDepth_});
        loopDepth_++;
    }
    void exitLoop() {
        const auto& entry = scopeStack_.back();
        if (entry.hasReturn)
            ++closedReturnCount_;
        loopDepth_--;
        scopeStack_.pop_back();
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
        // Check use-defs first: a previously recorded use of `name` that
        // dominates this point and is post-dominated by it can serve as the
        // type-info source, avoiding another recording.
        if (!hasUnseenLoopDef(name)) {
            auto udIt = useDefs_.find(name);
            if (udIt != useDefs_.end()) {
                for (const Def& ud : udIt->second) {
                    if (dominates(ud) && postDominates(ud))
                        return {UseKind::NoRecord, ud.feedbackSlot};
                }
            }
        }

        const Def* d = findReachingDef(name);
        if (d && postDominates(*d))
            return {UseKind::NoRecord, d->feedbackSlot};
        if (loopDepth_ > 0 && !assignedInEnclosingLoop(name))
            return {UseKind::RecordOnce, kNoSlot};
        return {UseKind::RecordAlways, kNoSlot};
    }

    // ---- AST pre-scan ----

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
};

} // namespace rir

#endif // RIR_BC_DEF_USE_ANALYSIS_H
