#ifndef PIR_GENERIC_STATIC_ANALYSIS
#define PIR_GENERIC_STATIC_ANALYSIS

#include "../pir/bb.h"
#include "../pir/function.h"
#include "../pir/instruction.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"

#include <functional>
#include <set>
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
 */
template <class AbstractState>
class StaticAnalysis {
  protected:
    std::vector<std::vector<AbstractState>> mergepoint;

  public:
    AbstractState exitpoint;
    BB* entry;
    CFG cfg;

    StaticAnalysis(BB* entry) : entry(entry), cfg(entry) {
        mergepoint.resize(cfg.size());
        mergepoint[entry->id].resize(1);
    }
    StaticAnalysis(BB* entry, const AbstractState& initialState)
        : entry(entry), cfg(entry) {
        mergepoint.resize(cfg.size());
        mergepoint[entry->id].push_back(initialState);
    }

    virtual void apply(AbstractState&, Instruction*) const = 0;

    typedef std::function<void(const AbstractState&, Instruction*)> collect_res;

    template <bool before = true>
    void collect(collect_res collect) {
        Visitor::run(entry, [&](BB* bb) {
            size_t segment = 0;
            AbstractState state = mergepoint[bb->id][segment];
            for (auto i : *bb) {
                if (before)
                    collect(state, i);
                if (Call::Cast(i))
                    state = mergepoint[bb->id][++segment];
                else
                    apply(state, i);
                if (!before)
                    collect(state, i);
            }
        });
    }

    void operator()() {
        bool reachedExit = false;
        bool done;

        std::vector<bool> changed(cfg.size(), false);
        changed[entry->id] = true;

        do {
            done = true;
            Visitor::run(entry, [&](BB* bb) {
                size_t id = bb->id;

                if (!changed[id])
                    return;

                size_t segment = 0;
                assert(mergepoint[id].size() > 0);
                AbstractState state = mergepoint[id][segment];

                for (auto i : *bb) {
                    apply(state, i);

                    if (Call::Cast(i)) {
                        segment++;
                        if (mergepoint[id].size() <= segment) {
                            mergepoint[id].resize(segment + 1);
                        }
                        mergepoint[bb->id][segment] = state;
                    }
                }

                if (!bb->next0 && !bb->next1) {
                    if (!Deopt::Cast(bb->last())) {
                        if (reachedExit) {
                            exitpoint.merge(state);
                        } else {
                            exitpoint = state;
                            reachedExit = true;
                        }
                    }
                    return;
                }

                if (bb->next0) {
                    if (mergepoint[bb->next0->id].empty()) {
                        mergepoint[bb->next0->id].push_back(state);
                        done = false;
                        changed[bb->next0->id] = true;
                    } else {
                        if (mergepoint[bb->next0->id][0].merge(state)) {
                            done = false;
                            changed[bb->next0->id] = true;
                        }
                    }
                }
                if (bb->next1) {
                    if (mergepoint[bb->next1->id].empty()) {
                        mergepoint[bb->next1->id].push_back(state);
                        done = false;
                        changed[bb->next1->id] = true;
                    } else {
                        if (mergepoint[bb->next1->id][0].merge(state)) {
                            done = false;
                            changed[bb->next1->id] = true;
                        }
                    }
                }

                changed[id] = false;
            });
        } while (!done);
    }
};
}
}

#endif
