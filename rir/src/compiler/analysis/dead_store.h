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

        AbstractResult applyCommon(State& state, Instruction* i) const {
            AbstractResult effect;
            if (auto force = Force::Cast(i)) {
                if (auto mk = MkArg::Cast(force->input()->followCasts())) {
                    if (!mk->isEager()) {
                        BB* bb = mk->prom()->entry;
                        if (bb->size() > 0 &&
                            LdFunctionEnv::Cast(*bb->begin())) {
                            // The promise could get forced here and
                            // use variables in the parent environment
                            effect.max(
                                recurse(state, i, mk->prom(), mk->promEnv()));
                        }
                    }
                }
            }
            return effect;
        }

        virtual AbstractResult recurse(State& state, Instruction* i,
                                       Promise* prom, Value* env) const = 0;
    };

    // 1. keep track of LdFunctionEnv aliases.
    class AliasSet {
      public:
        std::unordered_map<Value*, Value*> aliases;

        AbstractResult mergeExit(const AliasSet& other) { return merge(other); }
        AbstractResult merge(const AliasSet& other) {
            AbstractResult res;
            for (const auto& x : other.aliases) {
                if (!aliases.count(x.first)) {
                    aliases[x.first] = x.second;
                    res.update();
                }
            }
            return res;
        }
        void print(std::ostream& out, bool tty) const {
            // TODO
        }
    };

    class AliasAnalysis : public StaticAnalysis<AliasSet>,
                          private SubAnalysis<AliasSet> {
        Value* resolveEnv(const AliasSet& state, Value* env) const {
            if (LdFunctionEnv::Cast(env)) {
                env = state.aliases.at(env);
            }
            return env;
        }

        AbstractResult apply(AliasSet& state, Instruction* i) const override {
            return applyCommon(state, i);
        }

        AbstractResult recurse(AliasSet& state, Instruction* i, Promise* prom,
                               Value* env) const override {
            AbstractResult res;

            auto ld = LdFunctionEnv::Cast(*prom->entry->begin());
            assert(ld != NULL);
            if (!state.aliases.count(ld)) {
                state.aliases[ld] = resolveEnv(state, env);
                res.update();
            }

            AliasAnalysis analysis(closure, prom, log);
            analysis();
            res.max(state.merge(analysis.result()));

            return res;
        }

      public:
        AliasAnalysis(ClosureVersion* cls, Code* code, LogStream& log)
            : StaticAnalysis("aliasEnv", cls, code, log),
              SubAnalysis<AliasSet>() {}

        Value* resolveEnv(Value* env) const {
            return resolveEnv(result(), env);
        }
    };

    // 2. perfom an escape analysis on the environment.
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

    class EnvLeakAnalysis : public StaticAnalysis<EnvSet>,
                            private SubAnalysis<EnvSet> {
        const AliasAnalysis& aliases;

      public:
        EnvLeakAnalysis(ClosureVersion* cls, Code* code,
                        const AliasAnalysis& aliases, LogStream& log)
            : EnvLeakAnalysis(cls, code, EnvSet(), aliases, log) {}

        EnvLeakAnalysis(ClosureVersion* cls, Code* code,
                        const EnvSet& initialState,
                        const AliasAnalysis& aliases, LogStream& log)
            : StaticAnalysis("envLeak", cls, code, initialState, NULL, log),
              SubAnalysis(), aliases(aliases) {}

        EnvSet leakedAt(Instruction* i) const {
            return at<StaticAnalysis::PositioningStyle::BeforeInstruction>(i);
        }

      protected:
        AbstractResult apply(EnvSet& state, Instruction* i) const override {
            AbstractResult effect;
            if (i->leaksEnv()) {
                Value* env = aliases.resolveEnv(i->env());
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
            effect.max(applyCommon(state, i));
            return effect;
        }

        AbstractResult recurse(EnvSet& state, Instruction* i, Promise* prom,
                               Value* env) const override {
            EnvLeakAnalysis analysis(closure, prom, aliases, log);
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

    class ObservedStoreAnalysis : public BackwardStaticAnalysis<ObservedStores>,
                                  private SubAnalysis<ObservedStores> {
        const AliasAnalysis& aliases;
        const EnvLeakAnalysis& leaked;

      public:
        ObservedStoreAnalysis(ClosureVersion* cls, Code* code, const CFG& cfg,
                              const AliasAnalysis& aliases,
                              const EnvLeakAnalysis& leaked, LogStream& log)
            : BackwardStaticAnalysis("observedEnv", cls, code, cfg, log),
              SubAnalysis(), aliases(aliases), leaked(leaked) {}

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
            return apply(state, i, NULL);
        }

        AbstractResult apply(ObservedStores& state, Instruction* i,
                             Value* alias) const {
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
                for (auto& e :
                     withPotentialParents(aliases.resolveEnv(i->env()))) {
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
                observeFullEnv(aliases.resolveEnv(i->env()));
            } else if (i->exits() || i->effects.contains(Effect::ExecuteCode)) {
                auto leakedEnvs = leaked.leakedAt(i);
                for (auto& l : leakedEnvs.envs)
                    observeFullEnv(l);
            }

            effect.max(applyCommon(state, i));
            return effect;
        }

        AbstractResult recurse(ObservedStores& state, Instruction* i,
                               Promise* prom, Value* env) const override {
            CFG cfg(prom);
            EnvLeakAnalysis subLeak(closure, prom, leaked.leakedAt(i), aliases,
                                    log);
            ObservedStoreAnalysis analysis(closure, prom, cfg, aliases, subLeak,
                                           log);
            analysis();
            return state.merge(analysis.result());
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

    AliasAnalysis aliases;
    EnvLeakAnalysis leak;
    ObservedStoreAnalysis observed;

    static bool isLocal(Value* env) { return MkEnv::Cast(env); }

  public:
    DeadStoreAnalysis(ClosureVersion* cls, const CFG& cfg, LogStream& log)
        : aliases(cls, cls, log), leak(cls, cls, aliases, log),
          observed(cls, cls, cfg, aliases, leak, log) {}

    bool isDead(StVar* st) const {
        if (!isLocal(st->env()))
            return false;
        return !observed.isObserved(st);
    };
};
} // namespace pir
} // namespace rir

#endif
