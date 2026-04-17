#ifndef RIR_BC_DEF_USE_ANALYSIS_H
#define RIR_BC_DEF_USE_ANALYSIS_H

#include "R/Symbols.h"
#include "R/r.h"

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

    struct Def {
        int loopDepth;
        int loopId;   // ID of the innermost loop at def time; 0 = top-level
        int branchId; // 0 = unconditional, >0 = branch ID
        int closedReturnCount;   // snapshot of closedReturnCount_ at def time
        int closedLoopExitCount; // snapshot of
                                 // closedLoopExitByDepth_[loopDepth] at def
                                 // time
        int feedbackSlot; // TypeFeedback slot of the def's record_type_, or -1
    };

    enum class UseKind { NoRecord, RecordOnce, RecordAlways };

    struct UseClassification {
        UseKind kind;
        int defSlot; // valid when kind == NoRecord; -1 otherwise
    };

    struct LoopEntry {
        int loopId;
        bool hasReturn = false;
    };

    struct DefsSnapshot {
        std::unordered_map<SEXP, std::vector<Def>> defs;
        std::unordered_map<SEXP, Def> useDefs;
        int closedReturnCount;
        std::vector<int> closedLoopExitByDepth;
        std::vector<LoopEntry> loopStack;
    };

    struct LoopBodyInfo {
        int loopDepth;
        int loopId;
        std::unordered_map<SEXP, int> defCounts;
    };

    struct BranchEntry {
        int branchId;
        int loopDepthAtEntry;
        bool hasReturn = false;
        bool hasLoopExit = false;
    };

    // ---- data ----

    std::unordered_map<SEXP, std::vector<Def>> defs_;
    std::unordered_map<SEXP, Def> useDefs_;
    int loopDepth_ = 0;
    std::vector<LoopEntry> loopStack_;
    int nextLoopId_ = 1;
    std::vector<BranchEntry> branchStack_;
    int nextBranchId_ = 1;
    std::vector<LoopBodyInfo> loopBodyDefs_;
    int closedReturnCount_ = 0;
    std::vector<int> closedLoopExitByDepth_;

    // ---- compile-time state updates ----

    void trackDef(SEXP name, int feedbackSlot = -1) {
        useDefs_.erase(name);
        int loopExitCount = (loopDepth_ < (int)closedLoopExitByDepth_.size())
                                ? closedLoopExitByDepth_[loopDepth_]
                                : 0;
        defs_[name].push_back(
            {loopDepth_, loopStack_.empty() ? 0 : loopStack_.back().loopId,
             branchStack_.empty() ? 0 : branchStack_.back().branchId,
             closedReturnCount_, loopExitCount, feedbackSlot});
    }

    void trackUseDef(SEXP name, int slot) {
        int loopExitCount = (loopDepth_ < (int)closedLoopExitByDepth_.size())
                                ? closedLoopExitByDepth_[loopDepth_]
                                : 0;
        useDefs_[name] = {loopDepth_,
                          loopStack_.empty() ? 0 : loopStack_.back().loopId,
                          branchStack_.empty() ? 0
                                               : branchStack_.back().branchId,
                          closedReturnCount_,
                          loopExitCount,
                          slot};
    }

    void enterBranch() {
        branchStack_.push_back({nextBranchId_++, loopDepth_, false, false});
    }
    void exitBranch() {
        const auto& entry = branchStack_.back();
        if (entry.hasReturn)
            ++closedReturnCount_;
        if (entry.hasLoopExit) {
            int d = entry.loopDepthAtEntry;
            if (d >= (int)closedLoopExitByDepth_.size())
                closedLoopExitByDepth_.resize(d + 1, 0);
            ++closedLoopExitByDepth_[d];
        }
        branchStack_.pop_back();
    }

    void enterLoop() {
        loopStack_.push_back({nextLoopId_++, false});
        loopDepth_++;
    }
    void exitLoop() {
        const auto& entry = loopStack_.back();
        if (entry.hasReturn)
            ++closedReturnCount_;
        loopDepth_--;
        loopStack_.pop_back();
    }

    // A `return` marks the innermost open scope so that when that scope
    // closes, `closedReturnCount_` is incremented, signalling to downstream
    // code that some path bypassed it. Branches take priority; if no branch
    // is open but a loop is, mark the loop (the return still escapes through
    // the loop body).
    void markReturn() {
        if (!branchStack_.empty())
            branchStack_.back().hasReturn = true;
        else if (!loopStack_.empty())
            loopStack_.back().hasReturn = true;
    }

    void markLoopExit() {
        for (auto it = branchStack_.rbegin(); it != branchStack_.rend(); ++it) {
            if (it->loopDepthAtEntry == loopDepth_) {
                it->hasLoopExit = true;
                return;
            }
        }
    }

    void setLoopBodyDefs(std::unordered_map<SEXP, int> counts) {
        loopBodyDefs_.push_back(
            {loopDepth_, loopStack_.empty() ? 0 : loopStack_.back().loopId,
             std::move(counts)});
    }
    void clearLoopBodyDefs() { loopBodyDefs_.pop_back(); }

    // ---- save / restore for loop peeling ----

    DefsSnapshot saveState() const {
        return {defs_, useDefs_, closedReturnCount_, closedLoopExitByDepth_,
                loopStack_};
    }
    void restoreState(DefsSnapshot&& s) {
        defs_ = std::move(s.defs);
        useDefs_ = std::move(s.useDefs);
        closedReturnCount_ = s.closedReturnCount;
        closedLoopExitByDepth_ = std::move(s.closedLoopExitByDepth);
        loopStack_ = std::move(s.loopStack);
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
                const Def& ud = udIt->second;
                if (dominates(ud)) {
                    if (postDominates(ud))
                        return {UseKind::NoRecord, ud.feedbackSlot};
                }
            }
        }

        const Def* d = findReachingDef(name);
        if (!d)
            return {UseKind::RecordAlways, -1};
        if (postDominates(*d))
            return {UseKind::NoRecord, d->feedbackSlot};
        return {UseKind::RecordOnce, -1};
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
    // Returns true when D dominates the current compilation point — every path
    // from function entry to here passes through D.
    bool dominates(const Def& d) const {
        if (d.loopDepth > loopDepth_)
            return false;
        if (d.loopId != 0) {
            bool loopOpen = false;
            for (const auto& entry : loopStack_) {
                if (entry.loopId == d.loopId) {
                    loopOpen = true;
                    break;
                }
            }
            if (!loopOpen)
                return false;
        }
        if (d.branchId == 0)
            return true;
        for (const auto& entry : branchStack_) {
            if (entry.branchId == d.branchId)
                return true;
        }
        return false;
    }

    // Returns a pointer to the unique dominating def of `name` at the current
    // compilation point, or nullptr if none exists.
    //
    // A def D is "dominating" if its loop is still open, its branch is still
    // open, and it is the last def overall (no later def could have
    // overwritten the variable).
    const Def* findReachingDef(SEXP name) const {
        auto it = defs_.find(name);
        if (it == defs_.end())
            return nullptr;

        if (hasUnseenLoopDef(name))
            return nullptr;

        const auto& ds = it->second;
        const Def* lastDefinite = nullptr;
        const Def* last = &ds.back();
        for (const auto& d : ds) {
            if (!dominates(d))
                continue;
            if (d.branchId == 0) {
                lastDefinite = &d;
            } else {
                for (const auto& entry : branchStack_) {
                    if (entry.branchId == d.branchId) {
                        lastDefinite = &d;
                        break;
                    }
                }
            }
        }
        if (!lastDefinite)
            return nullptr;

        // Must be the last def overall: any later def could have overwritten.
        if (lastDefinite != last)
            return nullptr;

        return lastDefinite;
    }

    // Returns true when U post-dominates D — every path from D to function
    // exit passes through U.
    //
    // Approximation (sound — no false positives):
    //   (1) same loop: a def from a different loop does not post-dominate.
    //   (2) branch check: U must not be inside a branch that D is not in.
    //   (3) no return() in a closed branch between D and U.
    //   (4) no break/next at the current loop depth in a closed branch
    //       between D and U.
    bool postDominates(const Def& d) const {
        if (d.loopDepth != loopDepth_)
            return false;

        int currentBranchId =
            branchStack_.empty() ? 0 : branchStack_.back().branchId;
        if (currentBranchId != 0 && currentBranchId != d.branchId)
            return false;

        if (closedReturnCount_ != d.closedReturnCount)
            return false;

        int curLoopExit = (loopDepth_ < (int)closedLoopExitByDepth_.size())
                              ? closedLoopExitByDepth_[loopDepth_]
                              : 0;
        if (curLoopExit != d.closedLoopExitCount)
            return false;

        return true;
    }

    bool hasUnseenLoopDef(SEXP name) const {
        for (const auto& info : loopBodyDefs_) {
            auto it = info.defCounts.find(name);
            if (it == info.defCounts.end())
                continue;
            int expected = it->second;
            int seen = 0;
            auto defsIt = defs_.find(name);
            if (defsIt != defs_.end()) {
                for (const auto& d : defsIt->second) {
                    if (d.loopId == info.loopId)
                        seen++;
                }
            }
            if (seen < expected)
                return true;
        }
        return false;
    }
};

} // namespace rir

#endif // RIR_BC_DEF_USE_ANALYSIS_H
