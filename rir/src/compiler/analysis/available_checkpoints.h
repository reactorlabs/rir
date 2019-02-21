#ifndef PIR_AVAILABLE_CHECKPOINTS_H
#define PIR_AVAILABLE_CHECKPOINTS_H

#include "../util/cfg.h"
#include "abstract_value.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

class AvailableCheckpoints : public StaticAnalysis<AbstractUnique<Checkpoint>> {
  public:
    ClosureVersion* code;
    AvailableCheckpoints(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("AvailableCheckpoints", cls, cls, log), code(cls) {}

    AbstractResult apply(AbstractUnique<Checkpoint>& state,
                         Instruction* i) const override {
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
    };

    Checkpoint* at(Instruction* i) {
        return StaticAnalysis::at<PositioningStyle::BeforeInstruction>(i).get();
    }
};

} // namespace pir
} // namespace rir

#endif
