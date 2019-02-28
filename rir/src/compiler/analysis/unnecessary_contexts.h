#ifndef PIR_UNNECESSARY_CONTEXTS_H
#define PIR_UNNECESSARY_CONTEXTS_H

#include "../util/cfg.h"
#include "abstract_value.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct UnnecessaryContextsState : public AbstractUnique<PushContext> {
    bool needed = false;
    AbstractResult merge(const UnnecessaryContextsState& other) {
        AbstractResult res = AbstractUnique<PushContext>::merge(other);
        if (get() && !needed && other.needed) {
            needed = true;
            res.update();
        }
        return res;
    }
};

class UnnecessaryContexts : public StaticAnalysis<UnnecessaryContextsState> {
  public:
    ClosureVersion* code;
    UnnecessaryContexts(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("UnnecessaryContexts", cls, cls, log), code(cls) {}

    AbstractResult apply(UnnecessaryContextsState& state,
                         Instruction* i) const override {
        if (auto p = PushContext::Cast(i)) {
            state.set(p);
            return AbstractResult::Updated;
        } else if (CallInstruction::CastCall(i) || CallBuiltin::Cast(i)) {
            // Contexts are needed for non-local returns and reflection. On
            // deoptimization we can synthesize them, thus none needed for
            // checkpoints.
            state.needed = true;
            return AbstractResult::Updated;
        } else if (auto p = PopContext::Cast(i)) {
            if (state.get()) {
                assert(state.get() == p->push());
                state.clear();
                return AbstractResult::Updated;
            }
        }
        return AbstractResult::None;
    };

    bool canRemove(PopContext* i) {
        auto res = StaticAnalysis::at<PositioningStyle::BeforeInstruction>(i);
        return res.get() && res.get() == i->push() && !res.needed;
    }
};

} // namespace pir
} // namespace rir

#endif
