#ifndef PIR_ENV_ESCAPE_ANALYSIS_H
#define PIR_ENV_ESCAPE_ANALYSIS_H

#include "../analysis/generic_static_analysis.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

class DeadStoreAnalysis {
    template <class State>
    class SubAnalysis {
      protected:
        SubAnalysis() {}

        void applyRecurse(AbstractResult& effect, State& state, Instruction* i,
                          Value* promEnv) const {
            i->eachArg([&](Value* v) {
                if (auto mk = MkArg::Cast(v->followCasts())) {
                    if (mk->usesPromEnv() && !state.visited.count(mk)) {
                        // The promise could get forced here and
                        // use variables in the parent environment
                        state.visited.insert(mk);
                        auto env = mk->promEnv();
                        if (LdFunctionEnv::Cast(env)) {
                            assert(promEnv);
                            env = promEnv;
                        }
                        effect.max(handleRecurse(state, i, mk->prom(), env));
                    }
                }
            });
        }

        virtual AbstractResult handleRecurse(State& state, Instruction* i,
                                             Promise* prom,
                                             Value* env) const = 0;
    };

    // perfom an escape analysis on the environment.
    class EnvSet {
      public:
        typedef std::unordered_set<Value*> Envs;
        std::unordered_set<MkArg*> visited;
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
            for (const auto& a : other.visited) {
                if (!visited.count(a)) {
                    visited.insert(a);
                    res.update();
                }
            }
            return res;
        }
        void print(std::ostream& out, bool tty) const {
            // TODO
        }
    };

    class EnvLeakAnalysis : public StaticAnalysis<EnvSet>,
                            private SubAnalysis<EnvSet> {
        Value* promEnv = nullptr;

      public:
        EnvLeakAnalysis(ClosureVersion* cls, Code* code, Value* promEnv,
                        LogStream& log)
            : EnvLeakAnalysis(cls, code, EnvSet(), promEnv, log) {}

        EnvLeakAnalysis(ClosureVersion* cls, Code* code,
                        const EnvSet& initialState, Value* promEnv,
                        LogStream& log)
            : StaticAnalysis("envLeak", cls, code, initialState, NULL, log),
              SubAnalysis(), promEnv(promEnv) {}

        EnvSet leakedAt(Instruction* i) const {
            return at<StaticAnalysis::PositioningStyle::BeforeInstruction>(i);
        }

      protected:
        AbstractResult apply(EnvSet& state, Instruction* i) const override {
            AbstractResult effect;
            applyRecurse(effect, state, i, promEnv);
            if (i->leaksEnv()) {
                Value* env = i->env();
                if (LdFunctionEnv::Cast(env)) {
                    assert(promEnv);
                    env = promEnv;
                }
                if (auto m = MaterializeEnv::Cast(env))
                    env = m->env();
                if (auto mk = MkEnv::Cast(env)) {
                    // stubs cannot leak, or we deopt
                    if (mk->stub)
                        return effect;
                }
                if (!state.envs.count(env)) {
                    state.envs.insert(env);
                    effect.update();
                }
            }
            return effect;
        }

        AbstractResult handleRecurse(EnvSet& state, Instruction* i,
                                     Promise* prom, Value* env) const override {
            EnvLeakAnalysis analysis(closure, prom, env, log);
            analysis();
            return state.merge(analysis.result());
        }
    };

    /* 3. find all possible observations for stores.
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
        std::unordered_set<MkArg*> visited;

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
            for (const auto& a : other.visited) {
                if (!visited.count(a)) {
                    visited.insert(a);
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
        : public StaticAnalysis<ObservedStores, DummyState, false>,
          private SubAnalysis<ObservedStores> {
        Value* promEnv = nullptr;
        const EnvLeakAnalysis& leaked;

      public:
        ObservedStoreAnalysis(ClosureVersion* cls, Code* code, Value* promEnv,
                              const EnvLeakAnalysis& leaked, LogStream& log)
            : StaticAnalysis("observedEnv", cls, code, log), SubAnalysis(),
              promEnv(promEnv), leaked(leaked) {}

      private:
        static std::unordered_set<Value*> withPotentialParents(Value* env) {
            std::unordered_set<Value*> res;
            assert(env);
            for (;;) {
                if (auto e = MaterializeEnv::Cast(env)) {
                    env = e->env();
                    continue;
                }
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
            return apply(state, i, NULL);
        }

        Value* resolveEnv(Value* env) const {
            if (LdFunctionEnv::Cast(env)) {
                assert(promEnv);
                env = promEnv;
            }
            if (auto m = MaterializeEnv::Cast(env)) {
                return m->env();
            }
            return env;
        }

        AbstractResult apply(ObservedStores& state, Instruction* i,
                             Value* alias) const {
            AbstractResult effect;
            applyRecurse(effect, state, i, promEnv);

            auto observeStaticEnvs = [&]() {
                for (auto it = state.ignoreStore.begin();
                     it != state.ignoreStore.end();) {
                    if (Env::isStaticEnv(it->second)) {
                        it = state.ignoreStore.erase(it);
                        effect.update();
                    } else {
                        it++;
                    }
                }
            };

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
                for (auto& e : withPotentialParents(resolveEnv(i->env()))) {
                    Variable var({ld->varName, e});
                    if (!Env::isStaticEnv(e) &&
                        !state.partiallyObserved.count(var)) {
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
                Variable var({st->varName, resolveEnv(st->env())});
                // Two consecutive stores between observations => the first one
                // can be removed, since the second one overrides.
                if (!state.ignoreStore.count(var)) {
                    state.ignoreStore.insert(var);
                    effect.update();
                }
            } else if (i->exits() || i->effects.contains(Effect::ExecuteCode)) {
                auto leakedEnvs = leaked.leakedAt(i);
                for (auto& l : leakedEnvs.envs)
                    observeFullEnv(l);
            } else if (i->readsEnv()) {
                observeFullEnv(resolveEnv(i->env()));
            }

            if (i->exits() || i->readsEnv()) {
                observeStaticEnvs();
            }

            return effect;
        }

        AbstractResult handleRecurse(ObservedStores& state, Instruction* i,
                                     Promise* prom, Value* env) const override {
            EnvLeakAnalysis subLeak(closure, prom, leaked.leakedAt(i), env,
                                    log);
            ObservedStoreAnalysis analysis(closure, prom, env, subLeak, log);
            analysis();
            return state.merge(analysis.result());
        }

      public:
        bool isObserved(StVar* st) const {
            auto state = at<PositioningStyle::BeforeInstruction>(st);
            Variable var({st->varName, resolveEnv(st->env())});
            if (state.ignoreStore.count(var))
                return false;
            if (state.completelyObserved.count(st->env()))
                return true;
            return Env::isStaticEnv(st->env()) ||
                   state.partiallyObserved.count(var);
        }
    };

    EnvLeakAnalysis leak;
    ObservedStoreAnalysis observed;

  public:
    DeadStoreAnalysis(ClosureVersion* cls, LogStream& log)
        : leak(cls, cls, nullptr, log), observed(cls, cls, nullptr, leak, log) {
    }

    bool isDead(StVar* st) const {
        return !observed.isObserved(st);
    };
};
} // namespace pir
} // namespace rir

#endif
