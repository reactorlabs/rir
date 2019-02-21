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
        if (state.get()) {
            if (i->hasEffectIgnoreVisibility()) {
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
    FwdAvailableCheckpoints(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("FwdAvailableCheckpoints", cls, cls, log) {}

    AbstractResult apply(AbstractUnique<Checkpoint>& state,
                         Instruction* i) const override {
        return AvailableCheckpointsApply::apply(state, i);
    }

    Checkpoint* at(Instruction* i) {
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

    Checkpoint* at(Instruction* i) {
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

    Checkpoint* at(Instruction* i) { return fwd.at(i); }
    Checkpoint* next(Instruction* i) { return rwd.at(i); }
};

} // namespace pir
} // namespace rir

#endif
