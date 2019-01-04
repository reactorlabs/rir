#include "stack_values.h"
#include "../pir/pir_impl.h"

namespace {
using namespace rir::pir;

struct StackValuesAnalysisState {
    std::unordered_set<Value*> stackValues;
    std::unordered_map<BB*, std::unordered_set<Value*>> phiAlternatives;

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
        out << "}, phi: { ";
        for (auto p : phiAlternatives) {
            out << "BB" << p.first->id << ": { ";
            for (auto v : p.second) {
                v->printRef(out);
                out << " ";
            }
            out << "} ";
        }
        out << "}\n";
    }
};

class TheStackValuesAnalysis
    : public BackwardStaticAnalysis<StackValuesAnalysisState,
                                    AnalysisDebugLevel::None> {
  public:
    TheStackValuesAnalysis(Closure* cls, Code* code, CFG const& cfg,
                           LogStream& log)
        : BackwardStaticAnalysis("Stack Values", cls, code, cfg, log) {}

    AbstractResult apply(StackValuesAnalysisState& state,
                         Instruction* i) const override;
};

AbstractResult TheStackValuesAnalysis::apply(StackValuesAnalysisState& state,
                                             Instruction* i) const {
    AbstractResult effect;

    for (auto v : state.phiAlternatives[i->bb()]) {
        state.stackValues.insert(v);
    }
    state.phiAlternatives.clear();

    if (state.stackValues.count(i) > 0) {
        effect.update();
        state.stackValues.erase(state.stackValues.find(i));
    }

    if (auto phi = Phi::Cast(i)) {
        phi->eachArg([&](BB* bb, Value* vv) {
            if (state.phiAlternatives.count(bb) == 0 ||
                state.phiAlternatives[bb].count(vv) == 0) {
                if (vv->isInstruction() && !Env::isAnyEnv(vv)) {
                    effect.update();
                    state.phiAlternatives[bb].insert(vv);
                }
            }
        });
    } else {
        i->eachArg([&](Value* v) {
            if (!v->isInstruction() || Env::isAnyEnv(v))
                return;
            if (state.stackValues.count(v) == 0) {
                effect.update();
                state.stackValues.insert(v);
            }
        });
    }

    // TODO: also track how many uses of the given instruction?

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
                if (i == bb->first() &&
                    cfg.immediatePredecessors(bb).size() == 1) {
                    BB* pred = cfg.immediatePredecessors(bb).front();
                    for (auto v : state.stackValues) {
                        stackValues[pred].insert(v);
                    }
                }
            });
}

} // namespace pir
} // namespace rir
