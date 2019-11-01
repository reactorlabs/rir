#ifndef PIR_AVAILABLE_CHECKPOINTS_H
#define PIR_AVAILABLE_CHECKPOINTS_H

#include "../util/cfg.h"
#include "abstract_value.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct AvailableCheckpointsApply {
    static AbstractResult apply(AbstractUnique<Checkpoint>& state,
                                Instruction* i) {
        if (state.get() && i->isDeoptBarrier()) {
            state.clear();
            return AbstractResult::Updated;
        }
        if (auto cp = Checkpoint::Cast(i)) {
            if (cp != state.get()) {
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
    FwdAvailableCheckpoints(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("FwdAvailableCheckpoints", cls, cls, log) {}

    AbstractResult apply(AbstractUnique<Checkpoint>& state,
                         Instruction* i) const override {
        return AvailableCheckpointsApply::apply(state, i);
    }

    Checkpoint* reaching(Instruction* i) {
        return StaticAnalysis::at<PositioningStyle::BeforeInstruction>(i).get();
    }
};

class RwdAvailableCheckpoints
    : public BackwardStaticAnalysis<AbstractUnique<Checkpoint>> {
  public:
    RwdAvailableCheckpoints(ClosureVersion* cls, const CFG& cfg, LogStream& log)
        : BackwardStaticAnalysis("RwdAvailableCheckpoints", cls, cls, cfg,
                                 log) {}

    AbstractResult apply(AbstractUnique<Checkpoint>& state,
                         Instruction* i) const override {
        return AvailableCheckpointsApply::apply(state, i);
    }

    Checkpoint* reachingThrough(Instruction* i) {
        return BackwardStaticAnalysis::at<PositioningStyle::AfterInstruction>(i)
            .get();
    }
    Checkpoint* reaching(Instruction* i) {
        return BackwardStaticAnalysis::at<PositioningStyle::BeforeInstruction>(
                   i)
            .get();
    }
};

class AvailableCheckpoints {
    CFG cfg;
    FwdAvailableCheckpoints fwd;
    RwdAvailableCheckpoints rwd;

  public:
    AvailableCheckpoints(ClosureVersion* cls, LogStream& log)
        : cfg(cls), fwd(cls, log), rwd(cls, cfg, log) {}

    Checkpoint* at(Instruction* i) { return fwd.reaching(i); }
    Checkpoint* next(Instruction* i, Instruction* dependency,
                     const DominanceGraph& dom) {
        Checkpoint* res = next(i);
        if (res && dom.dominates(dependency->bb(), res->bb()))
            return res;
        return nullptr;
    }
    Checkpoint* next(Instruction* i) {
        // Search for the next cp only in main path, not the deopt branch
        if (auto cp = Checkpoint::Cast(i)) {
            auto n = cp->nextBB();
            while (n->isEmpty())
                n = n->next();
            return rwd.reachingThrough(*n->begin());
        }
        return rwd.reaching(i);
    }
};

} // namespace pir
} // namespace rir

#endif
