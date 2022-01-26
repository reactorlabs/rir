#ifndef PIR_UNNECESSARY_CONTEXTS_H
#define PIR_UNNECESSARY_CONTEXTS_H

#include "abstract_value.h"
#include "compiler/analysis/cfg.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct ContextStackState {
    std::vector<PushContext*> contextStack;
    void eachLeakedEnvRev(std::function<void(MkEnv*)> it) const {
        for (auto i = contextStack.rbegin(); i != contextStack.rend(); ++i)
            if (auto mk = MkEnv::Cast((*i)->env()))
                it(mk);
    }
    size_t context() const { return contextStack.size(); }
    AbstractResult merge(const ContextStackState& other) {
        assert(contextStack.size() == other.contextStack.size() &&
               "stack imbalance");
        return AbstractResult::None;
    }
    AbstractResult mergeExit(const ContextStackState& other) {
        return merge(other);
    }
    void print(std::ostream& out, bool tty) const {
        out << "Contexts: ";
        for (auto c : contextStack) {
            c->printRef(out);
            out << " ";
        }
        out << "\n";
    }
};

class ContextStack : public StaticAnalysis<ContextStackState> {
  public:
    ContextStack(ClosureVersion* cls, Code* code, AbstractLog& log)
        : StaticAnalysis("ContextStack", cls, code, log) {}

    AbstractResult apply(ContextStackState& state,
                         Instruction* i) const override {

        if (auto push = PushContext::Cast(i)) {
            state.contextStack.push_back(push);
            return AbstractResult::Updated;
        }

        if (auto pop = PopContext::Cast(i)) {
            assert(PushContext::Cast(pop->push()));
            assert(state.contextStack.back() == pop->push());
            state.contextStack.pop_back();
            return AbstractResult::Updated;
        }

        if (Deopt::Cast(i) || Unreachable::Cast(i)) {
            state.contextStack.clear();
            return AbstractResult::Updated;
        }

        return AbstractResult::None;
    };
};

} // namespace pir
} // namespace rir

#endif
