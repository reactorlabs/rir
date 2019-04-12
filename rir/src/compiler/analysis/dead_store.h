#ifndef PIR_ENV_ESCAPE_ANALYSIS_H
#define PIR_ENV_ESCAPE_ANALYSIS_H

#include "../analysis/generic_static_analysis.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

class DeadStoreAnalysis {

    // 1. perfom an escape analysis on the environment.
    class EnvSet {
      public:
        typedef std::unordered_set<Value*> Envs;
        Envs envs;
        AbstractResult mergeExit(const EnvSet& other) { return merge(other); }
        AbstractResult merge(const EnvSet& other) {
            AbstractResult res;
            for (const auto& f : other.envs) {
                if (!envs.count(f)) {
                    envs.insert(f);
                    res.update();
                }
            }
            return res;
        }
        void print(std::ostream& out, bool tty) const {
            // TODO
        }
    };

    class EnvLeakAnalysis : public StaticAnalysis<EnvSet> {
      public:
        EnvLeakAnalysis(ClosureVersion* cls, LogStream& log)
            : StaticAnalysis("envLeak", cls, cls, log) {}

        EnvSet leakedAt(Instruction* i) const {
            return at<StaticAnalysis::PositioningStyle::BeforeInstruction>(i);
        }

      protected:
        AbstractResult apply(EnvSet& state, Instruction* i) const override {
            AbstractResult effect;
            if (i->leaksEnv()) {
                if (auto mk = MkEnv::Cast(i->env())) {
                    // stubs cannot leak, or we deopt
                    if (mk->stub)
                        return effect;
                }
                if (!state.envs.count(i->env())) {
                    state.envs.insert(i->env());
                    effect.update();
                }
            }
            return effect;
        }
    };

    /* 2. find all possible observations for stores.
     *
     * This is a backwards analysis with the following rules:
     *
     *   * LdVar observes load of one variable
     *   * StVar masks observability of previous stores to the same variable
     *   * other instruction with "reads env" observes all variables (in that
     *     env and potential parents)
     *   * other instruction with "executes code" observes all variables in all
     *     leaked environments
     *
     */
    typedef std::pair<SEXP, Value*> Variable;
    class ObservedStores {
      public:
        std::unordered_set<Value*> completelyObserved;
        std::unordered_set<Variable, pairhash> partiallyObserved;
        std::unordered_set<Variable, pairhash> ignoreStore;

        AbstractResult mergeExit(const ObservedStores& other) {
            return merge(other);
        }
        AbstractResult merge(const ObservedStores& other) {
            AbstractResult res;
            for (const auto& f : other.completelyObserved) {
                if (!completelyObserved.count(f)) {
                    completelyObserved.insert(f);
                    res.update();
                }
            }
            for (const auto& f : other.partiallyObserved) {
                if (!partiallyObserved.count(f)) {
                    partiallyObserved.insert(f);
                    res.update();
                }
            }
            for (auto it = ignoreStore.begin(); it != ignoreStore.end();) {
                if (other.ignoreStore.count(*it)) {
                    it++;
                } else {
                    it = ignoreStore.erase(it);
                    res.update();
                }
            }
            return res;
        }
        void print(std::ostream& out, bool tty) const {
            // TODO
        }
    };

    class ObservedStoreAnalysis
        : public BackwardStaticAnalysis<ObservedStores> {
        const EnvLeakAnalysis& leaked;

      public:
        ObservedStoreAnalysis(ClosureVersion* cls, const CFG& cfg,
                              const EnvLeakAnalysis& leaked, LogStream& log)
            : BackwardStaticAnalysis("observedEnv", cls, cls, cfg, log),
              leaked(leaked) {}

      private:
        static std::unordered_set<Value*> withPotentialParents(Value* env) {
            std::unordered_set<Value*> res;
            assert(env);
            for (;;) {
                if (!MkEnv::Cast(env))
                    break;
                res.insert(env);
                env = Env::parentEnv(env);
            }
            return res;
        }

      protected:
        AbstractResult apply(ObservedStores& state,
                             Instruction* i) const override {
            AbstractResult effect;
            auto observeFullEnv = [&](Value* env) {
                for (auto& e : withPotentialParents(env)) {
                    if (!state.completelyObserved.count(e)) {
                        state.completelyObserved.insert(e);
                        effect.update();
                    }
                    for (auto it = state.ignoreStore.begin();
                         it != state.ignoreStore.end();) {
                        if (it->second != e) {
                            it++;
                        } else {
                            it = state.ignoreStore.erase(it);
                            effect.update();
                        }
                    }
                }
            };

            if (auto ld = LdVar::Cast(i)) {
                for (auto& e : withPotentialParents(i->env())) {
                    Variable var({ld->varName, e});
                    if (!state.partiallyObserved.count(var)) {
                        state.partiallyObserved.insert(var);
                        effect.update();
                    }
                    auto i = state.ignoreStore.find(var);
                    if (i != state.ignoreStore.end()) {
                        state.ignoreStore.erase(i);
                        effect.update();
                    }
                }
            } else if (auto st = StVar::Cast(i)) {
                Variable var({st->varName, st->env()});
                // Two consecutive stores between observations => the first one
                // can be removed, since the second one overrides.
                if (!state.ignoreStore.count(var)) {
                    state.ignoreStore.insert(var);
                    effect.update();
                }
            } else if (i->readsEnv()) {
                observeFullEnv(i->env());
            } else if (i->exits() || i->maxEffect() >= Effect::ExecuteCode) {
                auto leakedEnvs = leaked.leakedAt(i);
                for (auto& l : leakedEnvs.envs)
                    observeFullEnv(l);
            }
            return effect;
        }

      public:
        bool isObserved(StVar* st) const {
            auto state = at<PositioningStyle::BeforeInstruction>(st);
            Variable var({st->varName, st->env()});
            if (state.ignoreStore.count(var))
                return false;
            if (state.completelyObserved.count(st->env()))
                return true;
            return state.partiallyObserved.count(var);
        }
    };

    EnvLeakAnalysis leak;
    ObservedStoreAnalysis observed;

    static bool isLocal(Value* env) { return MkEnv::Cast(env); }

  public:
    DeadStoreAnalysis(ClosureVersion* cls, const CFG& cfg, LogStream& log)
        : leak(cls, log), observed(cls, cfg, leak, log) {}

    bool isDead(StVar* st) const {
        if (!isLocal(st->env()))
            return false;
        return !observed.isObserved(st);
    };
};
} // namespace pir
} // namespace rir

#endif
