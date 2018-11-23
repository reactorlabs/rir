#ifndef PIR_GENERIC_STATIC_ANALYSIS
#define PIR_GENERIC_STATIC_ANALYSIS

#include "../debugging/stream_logger.h"
#include "../pir/bb.h"
#include "../pir/closure.h"
#include "../pir/instruction.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "abstract_result.h"

#include <stack>
#include <unordered_map>

namespace rir {
namespace pir {

/*
 * Generic implementation of a (forward) static analysis.
 *
 * In "mergepoint" we keep a list of abstract states for every basic block. The
 * first state is the abstract state at the beginning of the basic block. We
 * add additional entries after every function return.
 *
 * To implement a concrete static analysis, the "apply" method needs to be
 * implemented, which supplies the implementation for every instruction. Apply
 * is supposed to modify the abstract state, but not (!) the analysis itself
 * (that is why it is marked const). The reason is, that after we reached a
 * fixed-point, it should be possible to reconstruct the state of the analysis
 * at every instruction. To do so, a dominating state is loaded from
 * "mergepoint" and then "apply" is used to seek to the desired instruction
 * pointer (see "collect" for an example).
 *
 * AbstractState basically has to have a merge function.
 * Anything else depends on the requirements of the apply function, which is
 * provided by the subclass that specializes StaticAnalysis.
 */

enum class AnalysisDebugLevel {
    None,
    Taint,
    Exit,
    Merge,
    BB,
    Instruction,
};

template <class AbstractState,
          AnalysisDebugLevel DEBUG_LEVEL = AnalysisDebugLevel::None>
class StaticAnalysis {
  public:
    enum class PositioningStyle { BeforeInstruction, AfterInstruction };

  private:
    const std::string name;

    std::vector<std::vector<AbstractState>> mergepoint;
    virtual AbstractResult apply(AbstractState&, Instruction*) const = 0;
    AbstractState exitpoint;
    bool done = false;

  protected:
    const std::vector<std::vector<AbstractState>>& getMergepoints() {
        return mergepoint;
    }

    LogStream& log;

    Closure* closure;
    Code* code;
    BB* entry;

  public:
    StaticAnalysis(const std::string& name, Closure* cls, Code* code,
                   LogStream& log)
        : name(name), log(log), closure(cls), code(code), entry(code->entry) {
        mergepoint.resize(code->nextBBId);
        mergepoint[entry->id].resize(1);
    }
    StaticAnalysis(const std::string& name, Closure* cls, Code* code,
                   const AbstractState& initialState, LogStream& log)
        : name(name), log(log), closure(cls), code(code), entry(code->entry) {
        mergepoint.resize(code->nextBBId);
        mergepoint[entry->id].push_back(initialState);
    }

    const AbstractState& result() {
        assert(done);
        return exitpoint;
    }

    template <PositioningStyle POS>
    const AbstractState& at(Instruction* i) {
        assert(done);

        BB* bb = i->bb();
        size_t segment = 0;
        const AbstractState& state = mergepoint[bb->id][segment];
        for (auto j : *bb) {
            if (POS == PositioningStyle::BeforeInstruction && i == j)
                return state;

            if (CallInstruction::CastCall(i))
                state = mergepoint[bb->id][++segment];
            else
                apply(state, i);

            if (POS == PositioningStyle::AfterInstruction && i == j)
                return state;
        }

        assert(false);
        return state;
    }

    typedef std::function<void(const AbstractState&, Instruction*)> Collect;

    template <PositioningStyle POS>
    void foreach (Collect collect) const {
        assert(done);

        Visitor::run(entry, [&](BB* bb) {
            size_t segment = 0;
            AbstractState state = mergepoint[bb->id][segment];
            for (auto i : *bb) {
                if (POS == PositioningStyle::BeforeInstruction)
                    collect(state, i);

                if (CallInstruction::CastCall(i))
                    state = mergepoint[bb->id][++segment];
                else
                    apply(state, i);

                if (POS == PositioningStyle::AfterInstruction)
                    collect(state, i);
            }
        });
    }

    void operator()() {
        bool reachedExit = false;

        std::vector<bool> changed(mergepoint.size(), false);
        changed[entry->id] = true;

        if (DEBUG_LEVEL > AnalysisDebugLevel::None) {
            log << "=========== Starting " << name << " Analysis on "
                << closure->name << " ";
            if (code == closure)
                log << "body";
            else
                log << "Prom(" << closure->promiseId(code) << ")";
            log << "\n";
        }

        do {
            done = true;
            Visitor::run(entry, [&](BB* bb) {
                size_t id = bb->id;

                if (!changed[id])
                    return;

                size_t segment = 0;
                assert(mergepoint[id].size() > 0);
                AbstractState state = mergepoint[id][segment];

                if (DEBUG_LEVEL >= AnalysisDebugLevel::BB) {
                    log << "======= Entering BB" << bb->id
                        << ", initial state\n";
                    log(state);
                }

                for (auto i : *bb) {
                    AbstractState old;
                    if (DEBUG_LEVEL == AnalysisDebugLevel::Taint)
                        old = state;
                    auto res = apply(state, i);
                    if ((DEBUG_LEVEL >= AnalysisDebugLevel::Instruction &&
                         res >= AbstractResult::None) ||
                        (DEBUG_LEVEL >= AnalysisDebugLevel::Taint &&
                         res >= AbstractResult::Tainted)) {
                        if (res == AbstractResult::Tainted) {
                            log << "===== Before applying instruction ";
                            log(i);
                            log << " we have\n";
                            log(old);
                        }
                        log << "===== After applying instruction ";
                        if (res == AbstractResult::Tainted) {
                            log << " (State got tainted)";
                        } else {
                            log(i);
                        }
                        log << " we have\n";
                        log(state);
                    }

                    if (CallInstruction::CastCall(i)) {
                        segment++;
                        if (mergepoint[id].size() <= segment) {
                            mergepoint[id].resize(segment + 1);
                        }
                        mergepoint[bb->id][segment] = state;
                    }
                }

                if (bb->isExit()) {
                    if (!Deopt::Cast(bb->last())) {
                        if (DEBUG_LEVEL >= AnalysisDebugLevel::Exit) {
                            log << "===== Exit state is\n";
                            log(state);
                        }
                        if (reachedExit) {
                            exitpoint.merge(state);
                        } else {
                            exitpoint = state;
                            reachedExit = true;
                        }
                    }
                    return;
                }

                mergeBranch(bb, bb->trueBranch(), state, changed);
                mergeBranch(bb, bb->falseBranch(), state, changed);

                changed[id] = false;
            });
        } while (!done);
    }

    void mergeBranch(BB* in, BB* branch, const AbstractState& state,
                     std::vector<bool>& changed) {
        if (!branch)
            return;

        auto id = branch->id;
        if (mergepoint[id].empty()) {
            mergepoint[id].push_back(state);
            done = false;
            changed[id] = true;
        } else {
            AbstractState old;
            if (DEBUG_LEVEL >= AnalysisDebugLevel::Taint) {
                old = mergepoint[id][0];
            }
            AbstractResult mres = mergepoint[id][0].merge(state);
            if (mres > AbstractResult::None) {
                if (DEBUG_LEVEL >= AnalysisDebugLevel::Merge ||
                    (mres == AbstractResult::Tainted &&
                     DEBUG_LEVEL == AnalysisDebugLevel::Taint)) {
                    log << "===== Merging BB" << in->id << " into BB" << id
                        << (mres == AbstractResult::Tainted ? " tainted" : "")
                        << (mres == AbstractResult::LostPrecision
                                ? " lost precision"
                                : "")
                        << " updated state:\n";
                    log << "===- In state is:\n";
                    log(state);
                    log << "===- Old state is:\n";
                    log(old);
                    log << "===- Merged state is:\n";
                    log(mergepoint[id][0]);
                }
                done = false;
                changed[id] = true;
            } else if (DEBUG_LEVEL >= AnalysisDebugLevel::Merge) {
                log << "===== Merging into trueBranch BB" << id
                    << " reached fixpoint\n";
            }
        }
    }
};
}
}

#endif
