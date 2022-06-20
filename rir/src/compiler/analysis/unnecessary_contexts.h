#ifndef PIR_UNNECESSARY_CONTEXTS_H
#define PIR_UNNECESSARY_CONTEXTS_H

#include "abstract_value.h"
#include "compiler/analysis/cfg.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct UnnecessaryContextsState : public AbstractUnique<PushContext> {
    std::unordered_set<MkEnv*> affected;
    bool needed = false;
    AbstractResult merge(const UnnecessaryContextsState& other) {
        AbstractResult res = AbstractUnique<PushContext>::merge(other);
        if (get() && !needed && other.needed) {
            needed = true;
            res.update();
        }
        if (get()) {
            for (auto& a : other.affected) {
                if (!affected.count(a)) {
                    affected.insert(a);
                    res.update();
                }
            }
        } else {
            affected.clear();
        }
        return res;
    }
    void clear() override final {
        AbstractUnique::clear();
        needed = false;
        affected.clear();
    }
    void print(std::ostream& out, bool tty) const {
        out << "Context : ";
        AbstractUnique<PushContext>::print(out, tty);
        out << "Needed: " << needed << "\nAffected: ";
        for (auto a : affected) {
            a->printRef(out);
            out << " ";
        }
        out << "\n";
    }
};

class UnnecessaryContexts : public StaticAnalysis<UnnecessaryContextsState> {
  public:
    UnnecessaryContexts(ClosureVersion* cls, Code* code, AbstractLog& log)
        : StaticAnalysis("UnnecessaryContexts", cls, code, log) {}

    AbstractResult apply(UnnecessaryContextsState& state,
                         Instruction* i) const override {
        if (auto p = PushContext::Cast(i)) {
            state.clear();
            state.set(p);
            return AbstractResult::Updated;
        }

        if (!state.needed) {
            // We use an opaque branch in the inliner to make pop-context
            // reachable for non-local returns. In this case we need to
            // preserve the context as the return target.
            if (Branch::Cast(i) &&
                Branch::Cast(i)->arg(0).val() == OpaqueTrue::instance()) {
                state.needed = true;
                return AbstractResult::Updated;
            }
            for (auto e : state.affected) {
                if (i->mayObserveContext(e)) {
                    // Contexts are needed for non-local returns and reflection.
                    // On deoptimization we can synthesize them, thus none
                    // needed for checkpoints.
                    state.needed = true;
                    return AbstractResult::Updated;
                }
            }
        }

        if (PopContext::Cast(i)) {
            if (state.get()) {
                assert(state.get() == PopContext::Cast(i)->push());
                state.clear();
                return AbstractResult::Updated;
            }
        }

        if (auto mk = MkEnv::Cast(i)) {
            if (state.get()) {
                state.affected.insert(mk);
                return AbstractResult::Updated;
            }
        }

        if (i->exits()) {
            // pushcontexts always needs to dominate a popcontext. Only
            // exception are deopt exits, where we rewrite contexts on the fly.
            // If this assert triggers, we inserted a return instruction into
            // an inlined function.
            assert((!state.get() || Deopt::Cast(i) || Unreachable::Cast(i)) &&
                   "Exit with missing pop context");
        }
        return AbstractResult::None;
    }

    PushContext* canRemove(PopContext* i) const {
        auto res = before(i);
        if (res.get() && res.get() == i->push() && !res.needed)
            return res.get();
        return nullptr;
    }

    std::unordered_set<MkEnv*> affectedEnvs(PopContext* pop) const {
        PushContext* push = canRemove(pop);
        assert(push);

        std::unordered_set<MkEnv*> res;
        // Affected are all envs between push and pop, which includes envs that
        // are in a deopt branch.
        foreach
            <PositioningStyle::BeforeInstruction>(
                [&](const UnnecessaryContextsState& state, Instruction* i) {
                    if (i == pop)
                        res.insert(state.affected.begin(),
                                   state.affected.end());
                    else if (Deopt::Cast(i) && state.get() == push)
                        res.insert(state.affected.begin(),
                                   state.affected.end());
                });
        return res;
    }
};

} // namespace pir
} // namespace rir

#endif
