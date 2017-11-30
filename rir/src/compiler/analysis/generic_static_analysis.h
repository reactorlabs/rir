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

        do {
            done = true;
            std::vector<bool> unchanged(cfg.size(), false);

            Visitor::run(entry, [&](BB* bb) {
                size_t id = bb->id;

                if (unchanged[id])
                    return;

                size_t segment = 0;
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
                    } else {
                        if (mergepoint[bb->next0->id][0].merge(state))
                            done = false;
                        else
                            unchanged[bb->next0->id] = true;
                    }
                }
                if (bb->next1) {
                    if (mergepoint[bb->next1->id].empty()) {
                        mergepoint[bb->next1->id].push_back(state);
                    } else {
                        if (mergepoint[bb->next1->id][0].merge(state))
                            done = false;
                        else
                            unchanged[bb->next1->id] = true;
                    }
                }
            });
        } while (!done);
    }
};

}
}

#endif
