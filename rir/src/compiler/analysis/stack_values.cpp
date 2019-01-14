#include "stack_values.h"
#include "../pir/pir_impl.h"

namespace {
using namespace rir::pir;

struct StackValuesAnalysisState {
    std::unordered_set<Value*> stackValues;

    AbstractResult merge(const StackValuesAnalysisState& other) {
        AbstractResult res;

        for (auto v : other.stackValues) {
            if (stackValues.count(v) == 0) {
                res.update();
                stackValues.insert(v);
            }
        }

        return res;
    }

    void print(std::ostream& out, bool tty) const {
        out << "== Result: { ";
        for (auto v : stackValues) {
            v->printRef(out);
            out << " ";
        }
        out << "}\n";
    }
};

class TheStackValuesAnalysis
    : public BackwardStaticAnalysis<StackValuesAnalysisState,
                                    AnalysisDebugLevel::None> {
  public:
    CFG const& cfg;

    TheStackValuesAnalysis(Closure* cls, Code* code, CFG const& cfg,
                           LogStream& log)
        : BackwardStaticAnalysis("Stack Values", cls, code, cfg, log),
          cfg(cfg) {}

    AbstractResult apply(StackValuesAnalysisState& state,
                         Instruction* i) const override;
};

AbstractResult TheStackValuesAnalysis::apply(StackValuesAnalysisState& state,
                                             Instruction* i) const {
    AbstractResult effect;

    // dumb, don't always search everything...
    std::unordered_set<Value*> toInsert, toDelete;
    for (auto v : state.stackValues) {
        if (auto phi = Phi::Cast(v)) {
            phi->eachArg([&](BB* bb, Value* vv) {
                if (i == vv) {
                    // something more involved?
                    toInsert.insert(vv);
                    toDelete.insert(phi);
                }
            });
        }
    }
    for (auto v : toDelete)
        state.stackValues.erase(state.stackValues.find(v));
    for (auto v : toInsert)
        state.stackValues.insert(v);

    if (auto phi = Phi::Cast(i)) {
        if (state.stackValues.count(phi) == 0) {
            effect.update();
            state.stackValues.insert(phi);
        }
    } else {
        if (state.stackValues.count(i) > 0) {
            effect.update();
            state.stackValues.erase(state.stackValues.find(i));
        }
        i->eachArg([&](Value* v) {
            if (!v->isInstruction() || Env::isAnyEnv(v))
                return;
            if (state.stackValues.count(v) == 0) {
                effect.update();
                state.stackValues.insert(v);
            }
        });
    }

    return effect;
}

} // namespace

namespace rir {
namespace pir {

StackValuesAnalysis::StackValuesAnalysis(Closure* function, Code* code,
                                         LogStream& log, CFG const& cfg) {
    TheStackValuesAnalysis analysis(function, code, cfg, log);
    analysis();

    analysis
        .foreach<TheStackValuesAnalysis::PositioningStyle::AfterInstruction>(
            [&](const StackValuesAnalysisState& state, Instruction* i) {
                BB* bb = i->bb();
                if (i == bb->first()) {
                    for (auto pred : cfg.immediatePredecessors(bb)) {
                        for (auto v : state.stackValues) {
                            stackValues[pred].insert(v);
                        }
                    }
                }
            });
}

} // namespace pir
} // namespace rir
