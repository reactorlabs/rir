#ifndef PIR_LAST_ENV_H
#define PIR_LAST_ENV_H

#include "abstract_value.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

class LastEnv : public StaticAnalysis<AbstractUnique<Value>> {
  public:
    LastEnv(ClosureVersion* cls, Code* code, LogStream& log)
        : StaticAnalysis("Last Env", cls, code, log) {}

    static bool explicitEnvValue(Instruction* instr) {
        return MkEnv::Cast(instr) || IsEnvStub::Cast(instr);
    }

    AbstractResult apply(AbstractUnique<Value>& state,
                         Instruction* i) const override {
        Value* env = nullptr;
        if (i->mayHaveEnv())
            env = i->env();
        if (CallSafeBuiltin::Cast(i))
            env = Env::elided();
        if (env && !explicitEnvValue(i) && env != state.get()) {
            state.set(env);
            return AbstractResult::Updated;
        }
        // pop_context_ does not restore env
        if (state.get() && PopContext::Cast(i)) {
            state.clear();
            return AbstractResult::Updated;
        }
        return AbstractResult::None;
    };

    bool envStillValid(Instruction* i) { return currentEnv(i) == i->env(); }

    Value* currentEnv(Instruction* i) {
        return StaticAnalysis::at<PositioningStyle::BeforeInstruction>(i).get();
    }
};

} // namespace pir
} // namespace rir

#endif
