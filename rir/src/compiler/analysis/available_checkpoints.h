#ifndef PIR_AVAILABLE_CHECKPOINTS_H
#define PIR_AVAILABLE_CHECKPOINTS_H

#include "abstract_value.h"
#include "compiler/analysis/cfg.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct AvailableCheckpointsApply {
    static AbstractResult apply(AbstractUnique<Checkpoint>& state,
                                Instruction* i) {
        if (state.get()) {
            if (i->isDeoptBarrier()) {
                state.clear();
                return AbstractResult::Updated;
            }
        } else {
            if (auto cp = Checkpoint::Cast(i)) {
                state.set(cp);
                return AbstractResult::Updated;
            }
        }
        return AbstractResult::None;
    }
};

class FwdAvailableCheckpoints
    : public StaticAnalysis<AbstractUnique<Checkpoint>> {
  public:
    FwdAvailableCheckpoints(ClosureVersion* cls, Code* code, AbstractLog& log)
        : StaticAnalysis("FwdAvailableCheckpoints", cls, code, log) {}

    AbstractResult apply(AbstractUnique<Checkpoint>& state,
                         Instruction* i) const override {
        return AvailableCheckpointsApply::apply(state, i);
    }

    Checkpoint* reaching(Instruction* i) const { return before(i).get(); }
};

class RwdAvailableCheckpoints
    : public StaticAnalysis<AbstractUnique<Checkpoint>, DummyState, false> {
  public:
    RwdAvailableCheckpoints(ClosureVersion* cls, Code* code, AbstractLog& log)
        : StaticAnalysis("RwdAvailableCheckpoints", cls, code, log) {}

    AbstractResult apply(AbstractUnique<Checkpoint>& state,
                         Instruction* i) const override {
        return AvailableCheckpointsApply::apply(state, i);
    }

    Checkpoint* reachingThrough(Instruction* i) const { return after(i).get(); }
    Checkpoint* reaching(Instruction* i) const { return before(i).get(); }
};

class AvailableCheckpoints {
    FwdAvailableCheckpoints fwd;
    RwdAvailableCheckpoints rwd;

  public:
    AvailableCheckpoints(ClosureVersion* cls, Code* code, AbstractLog& log)
        : fwd(cls, code, log), rwd(cls, code, log) {}

    Checkpoint* at(Instruction* i) const { return fwd.reaching(i); }
    Checkpoint* next(Instruction* i, Instruction* dependency,
                     const DominanceGraph& dom) const {
        Checkpoint* res = next(i);
        if (res && dom.dominates(dependency->bb(), res->bb()))
            return res;
        return nullptr;
    }
    Checkpoint* next(Instruction* i) const {
        // Search for the next cp only in main path, not the deopt branch
        if (auto cp = Checkpoint::Cast(i)) {
            auto n = cp->nextBB();
            if (n->isEmpty())
                n = n->next();
            if (n->isEmpty())
                return nullptr;
            return rwd.reachingThrough(*n->begin());
        }
        return rwd.reaching(i);
    }
};

} // namespace pir
} // namespace rir

#endif
