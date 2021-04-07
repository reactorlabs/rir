#ifndef PIR_UNNECESSARY_CONTEXTS_H
#define PIR_UNNECESSARY_CONTEXTS_H

#include "abstract_value.h"
#include "compiler/analysis/cfg.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct EnvsInContextsState {
    std::vector<MkEnv*> envs;
    AbstractResult merge(const EnvsInContextsState& other) {
        assert(envs.size() == other.envs.size() && "stack imbalance");
        return AbstractResult::None;
    }
    AbstractResult mergeExit(const EnvsInContextsState& other) {
        return merge(other);
    }
    void print(std::ostream& out, bool tty) const {
        out << "Envs in contexts: ";
        for (auto e : envs) {
            e->printRef(out);
            out << " ";
        }
        out << "\n";
    }
};

class EnvsInContexts : public StaticAnalysis<EnvsInContextsState> {
  public:
    EnvsInContexts(ClosureVersion* cls, Code* code, LogStream& log)
        : StaticAnalysis("EnvsInContexts", cls, code, log) {}

    AbstractResult apply(EnvsInContextsState& state,
                         Instruction* i) const override {

        if (auto p = PushContext::Cast(i)) {
            if (auto mk = MkEnv::Cast(p->env()))
                state.envs.push_back(mk);
            return AbstractResult::Updated;
        }

        if (auto p = PopContext::Cast(i)) {
            assert(PushContext::Cast(p->push()));
            assert(state.envs.back() == MkEnv::Cast(p->push()->env()));
            state.envs.pop_back();
            return AbstractResult::Updated;
        }

        if (Deopt::Cast(i) || Unreachable::Cast(i)) {
            state.envs.clear();
            return AbstractResult::Updated;
        }

        return AbstractResult::None;
    };
};

} // namespace pir
} // namespace rir

#endif
