#ifndef PIR_ENV_ESCAPE_ANALYSIS_H
#define PIR_ENV_ESCAPE_ANALYSIS_H

#include "../analysis/generic_static_analysis.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

class DeadStoreAnalysis {
    // perfom an escape analysis on the environment.
    class EnvSet {
      public:
        typedef std::unordered_set<Value*> Envs;
        std::unordered_set<MkArg*> visited;
        Envs leaked;
        Envs leakedByDeopt;
        AbstractResult mergeExit(const EnvSet& other) { return merge(other); }
        AbstractResult merge(const EnvSet& other) {
            AbstractResult res;
            for (const auto& environment : other.leaked) {
                if (!leaked.count(environment)) {
                    leaked.insert(environment);
                    res.update();
                }
            }
            for (const auto& a : other.visited) {
                if (!visited.count(a)) {
                    visited.insert(a);
                    res.update();
                }
            }
            for (const auto& environment : other.leakedByDeopt) {
                if (!leaked.count(environment) &&
                    !leakedByDeopt.count(environment)) {
                    leakedByDeopt.insert(environment);
                    res.update();
                }
            }
            return res;
        }
        void print(std::ostream& out, bool tty) const {
            out << "==============\nLeaked:\n";
            for (auto environment : leaked) {
                out << "\t";
                environment->printRef(std::cout);
                out << "\n";
            }
            std::cout << "Leaked only by deopt branches:\n";
            for (auto environment : leakedByDeopt) {
                out << "\t";
                environment->printRef(std::cout);
                out << "\n";
            }
            out << "==============\n";
        }
    };

    class EnvLeakAnalysis : public StaticAnalysis<EnvSet> {
        Value* promEnv = nullptr;

      public:
        EnvLeakAnalysis(ClosureVersion* cls, Code* code, Value* promEnv,
                        LogStream& log)
            : EnvLeakAnalysis(cls, code, EnvSet(), promEnv, log) {}

        EnvLeakAnalysis(ClosureVersion* cls, Code* code,
                        const EnvSet& initialState, Value* promEnv,
                        LogStream& log)
            : StaticAnalysis("envLeak", cls, code, initialState, NULL, log),
              promEnv(promEnv) {}

        EnvSet leakedWhile(Instruction* i) const {
            return at<StaticAnalysis::PositioningStyle::AfterInstruction>(i);
        }

      protected:
        AbstractResult apply(EnvSet& state, Instruction* i) const override {
            AbstractResult effect;
            auto markEnv = [&](Value* env) {
                if (LdFunctionEnv::Cast(env)) {
                    assert(promEnv);
                    env = promEnv;
                }
                if (auto m = MaterializeEnv::Cast(env))
                    env = m->env();
                if (i->bb()->isDeopt()) {
                    if (!state.leakedByDeopt.count(env)) {
                        state.leakedByDeopt.insert(env);
                        effect.update();
                    }
                } else {
                    if (auto mk = MkEnv::Cast(env)) {
                        // stubs cannot leak, or we deopt
                        if (mk->stub)
                            return;
                    }
                    if (!state.leaked.count(env)) {
                        state.leaked.insert(env);
                        effect.update();
                    }
                }
            };
            if (i->leaksEnv()) {
                markEnv(i->env());
            }
            if (auto fs = i->frameState()) {
                do {
                    markEnv(fs->env());
                    fs = fs->next();
                } while (fs);
            }
            return effect;
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
        std::unordered_map<Value*, std::unordered_set<Instruction*>>
            observedByDeopt;
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
                    if (observedByDeopt.count(f))
                        observedByDeopt.erase(f);
                    res.update();
                }
            }
            for (const auto& instPerEnv : other.observedByDeopt) {
                if (!completelyObserved.count(instPerEnv.first)) {
                    if (!observedByDeopt.count(instPerEnv.first)) {
                        observedByDeopt.emplace(instPerEnv.first,
                                                instPerEnv.second);
                        res.update();
                    } else {
                        auto& instructions =
                            observedByDeopt.at(instPerEnv.first);
                        for (auto instruction : instPerEnv.second) {
                            if (instructions.insert(instruction).second)
                                res.update();
                        }
                    }
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

        bool removeStoreIgnoralOf(Value* env) {
            for (auto it = ignoreStore.begin(); it != ignoreStore.end();) {
                if (it->second != env) {
                    it++;
                } else {
                    it = ignoreStore.erase(it);
                    return true;
                }
            }
            return false;
        }

        void print(std::ostream& out, bool tty) const {
            // TODO
        }
    };

    class ObservedStoreAnalysis
        : public StaticAnalysis<ObservedStores, DummyState, false> {
        Value* promEnv = nullptr;
        const EnvLeakAnalysis& leaked;

      public:
        ObservedStoreAnalysis(ClosureVersion* cls, Code* code, Value* promEnv,
                              const EnvLeakAnalysis& leaked, LogStream& log)
            : StaticAnalysis("observedEnv", cls, code, log), promEnv(promEnv),
              leaked(leaked) {}

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
                        if (state.observedByDeopt.count(e))
                            state.observedByDeopt.erase(e);
                        state.completelyObserved.insert(e);
                        effect.update();
                    }
                    if (state.removeStoreIgnoralOf(e))
                        effect.update();
                }
            };

            auto observeLeakedEnv = [&](Value* env, Instruction* instruction) {
                for (auto& e : withPotentialParents(env)) {
                    if (!MkEnv::Cast(e))
                        return observeFullEnv(e);
                    if (!state.completelyObserved.count(e)) {
                        if (!state.observedByDeopt.count(e)) {
                            std::unordered_set<Instruction*> set;
                            set.insert(instruction);
                            state.observedByDeopt.emplace(e, set);
                            effect.update();
                        } else {
                            if (state.observedByDeopt.at(e)
                                    .insert(instruction)
                                    .second)
                                effect.update();
                        }
                    }
                    if (state.removeStoreIgnoralOf(e))
                        effect.update();
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
                    auto j = state.ignoreStore.find(var);
                    if (j != state.ignoreStore.end()) {
                        state.ignoreStore.erase(j);
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
            } else {
                if (i->exits() || i->readsEnv() ||
                    i->effects.contains(Effect::ExecuteCode)) {
                    auto leakedEnvs = leaked.leakedWhile(i);
                    for (auto& l : leakedEnvs.leaked) {
                        if (i->bb()->isDeopt())
                            observeLeakedEnv(l, i);
                        else
                            observeFullEnv(l);
                    }
                    observeStaticEnvs();
                }
                if (Deopt::Cast(i)) {
                    auto leakedEnvs = leaked.leakedWhile(i);
                    for (auto& l : leakedEnvs.leakedByDeopt)
                        observeLeakedEnv(l, i);
                }

                if (i->hasEnv())
                    for (auto& e : withPotentialParents(i->env()))
                        state.removeStoreIgnoralOf(e);
            }

            return effect;
        }

      public:
        bool isObserved(StVar* st) const {
            auto state = at<PositioningStyle::BeforeInstruction>(st);
            auto e = resolveEnv(st->env());
            Variable var({st->varName, e});
            if (state.ignoreStore.count(var))
                return false;
            if (state.completelyObserved.count(e) ||
                state.observedByDeopt.count(e))
                return true;
            return Env::isStaticEnv(e) || state.partiallyObserved.count(var);
        }

        bool isObservedOnlyByDeopt(StVar* st) const {
            auto state = at<PositioningStyle::BeforeInstruction>(st);
            auto e = resolveEnv(st->env());
            Variable var({st->varName, e});
            assert(!(state.completelyObserved.count(e) &&
                     state.observedByDeopt.count(e)));
            return !Env::isStaticEnv(e) &&
                   !state.partiallyObserved.count(var) &&
                   state.observedByDeopt.count(e);
        }

        std::unordered_set<Instruction*>
        observedByDeoptInstructions(StVar* st) const {
            auto state = at<PositioningStyle::BeforeInstruction>(st);
            auto e = resolveEnv(st->env());
            assert(state.observedByDeopt.count(e));
            return state.observedByDeopt.at(e);
        }
    };

    EnvLeakAnalysis leak;
    ObservedStoreAnalysis observed;

  public:
    DeadStoreAnalysis(ClosureVersion* cls, Code* code, LogStream& log)
        : leak(cls, code, nullptr, log),
          observed(cls, code, nullptr, leak, log) {}

    bool isDead(StVar* st) const {
        return !observed.isObserved(st);
    };

    bool onlyObservedByDeopt(StVar* st) const {
        return observed.isObservedOnlyByDeopt(st);
    };

    std::unordered_set<Instruction*> deoptInstructionsFor(StVar* st) const {
        return observed.observedByDeoptInstructions(st);
    };
};
} // namespace pir
} // namespace rir

#endif
