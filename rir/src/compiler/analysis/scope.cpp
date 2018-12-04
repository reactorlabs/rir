#include "scope.h"
#include "../pir/pir_impl.h"
#include "query.h"

namespace {
using namespace rir::pir;

struct ScopeAnalysisState {
    AbstractREnvironmentHierarchy envs;
    std::unordered_map<Instruction*, AbstractPirValue> returnValues;
    AbstractPirValue result;
    std::set<Instruction*> observedStores;
    std::set<Value*> allStoresObserved;

    bool mayUseReflection = false;

    AbstractResult merge(const ScopeAnalysisState& other) {
        AbstractResult res;

        for (const auto& f : other.returnValues)
            res.max(returnValues[f.first].merge(f.second));
        if (!mayUseReflection && other.mayUseReflection) {
            mayUseReflection = true;
            res.lostPrecision();
        }
        allStoresObserved.insert(other.allStoresObserved.begin(),
                                 other.allStoresObserved.end());
        for (auto s : other.observedStores) {
            if (!observedStores.count(s)) {
                observedStores.insert(s);
                res.update();
            }
        }
        for (auto s : other.allStoresObserved) {
            if (!allStoresObserved.count(s)) {
                allStoresObserved.insert(s);
                res.update();
            }
        }
        res.max(envs.merge(other.envs));
        return res;
    }

    AbstractResult mergeCall(Code* cur, const ScopeAnalysisState& other) {
        AbstractResult res;

        if (!mayUseReflection && other.mayUseReflection) {
            mayUseReflection = true;
            res.lostPrecision();
        }
        for (auto s : other.observedStores) {
            if (!observedStores.count(s)) {
                observedStores.insert(s);
                res.update();
            }
        }
        for (auto s : other.allStoresObserved) {
            if (!allStoresObserved.count(s)) {
                allStoresObserved.insert(s);
                res.update();
            }
        }
        res.max(envs.merge(other.envs));
        return res;
    }

    void print(std::ostream& out, bool tty) {
        envs.print(out, tty);
        if (returnValues.size() > 0) {
            out << "== Infered call/force result:\n";
            for (auto& t : returnValues) {
                out << "* ";
                t.first->printRef(out);
                out << " : ";
                t.second.print(out, tty);
                out << "\n";
            }
        }
        out << "== Result: ";
        result.print(out, tty);
        out << "\n";
        if (mayUseReflection)
            out << "* Reflection possible\n";
    }
};

class TheScopeAnalysis : public StaticAnalysis<ScopeAnalysisState> {
  public:
    const std::vector<SEXP> argNames;
    const std::vector<Value*> args;

    static constexpr size_t MAX_DEPTH = 2;
    size_t depth;
    Value* staticClosureEnv = Env::notClosed();
    Value* promiseEnv = nullptr;

    TheScopeAnalysis(Closure* cls, const std::vector<SEXP>& argNames,
                     LogStream& log)
        : StaticAnalysis("Scope", cls, cls, log), argNames(argNames), depth(0) {
    }
    TheScopeAnalysis(Closure* cls, const std::vector<SEXP>& argNames,
                     const std::vector<Value*>& args, Value* staticClosureEnv,
                     const ScopeAnalysisState& initialState, size_t depth,
                     LogStream& log)
        : StaticAnalysis("Scope", cls, cls, initialState, log),
          argNames(argNames), args(args), depth(depth),
          staticClosureEnv(staticClosureEnv) {}
    TheScopeAnalysis(Closure* cls, Promise* prom, Value* promEnv,
                     const ScopeAnalysisState& initialState, size_t depth,
                     LogStream& log)
        : StaticAnalysis("Scope", cls, prom, initialState, log), depth(depth),
          promiseEnv(promEnv) {}

    AbstractResult apply(ScopeAnalysisState& state,
                         Instruction* i) const override;

    typedef std::function<void(AbstractLoad)> LoadMaybe;
    RIR_INLINE void tryLoad(const ScopeAnalysisState& envs, Value* i,
                            LoadMaybe) const;
    typedef std::function<void(const AbstractPirValue&)> AbstractValMaybe;
    typedef std::function<void()> Maybe;
    void lookup(const ScopeAnalysisState& envs, Value* i, AbstractValMaybe,
                Maybe notFound = []() {}) const;
};

void TheScopeAnalysis::tryLoad(const ScopeAnalysisState& s, Value* i,
                               LoadMaybe aLoad) const {
    if (auto ld = LdVar::Cast(i)) {
        aLoad(s.envs.get(ld->env(), ld->varName));
    } else if (auto sld = LdVarSuper::Cast(i)) {
        aLoad(s.envs.superGet(sld->env(), sld->varName));
    } else if (auto ldf = LdFun::Cast(i)) {
        aLoad(s.envs.get(ldf->env(), ldf->varName));
    } else if (auto sts = StVarSuper::Cast(i)) {
        aLoad(s.envs.superGet(sts->env(), sts->varName));
    }
}

// As we go, the analysis records stores and it records the results of
// interprocedurally followed calls and forces.
// The lookup helper checks the current state of the analysis, to see
// if for a particular value we have already a result available from the
// above.
void TheScopeAnalysis::lookup(const ScopeAnalysisState& state, Value* v,
                              AbstractValMaybe found, Maybe notFound) const {
    if (auto lda = LdArg::Cast(v)) {
        if (args.size() > lda->id) {
            auto argi = Instruction::Cast(args[lda->id]);
            if (argi) {
                auto arg = AbstractPirValue(argi, argi);
                return lookup(state, argi, found, [&]() { found(arg); });
            }
        }
    }

    auto instr = Instruction::Cast(v);
    if (state.returnValues.count(instr)) {
        auto& res = state.returnValues.at(instr);
        if (res.isSingleValue()) {
            return lookup(state, res.singleValue().val, found,
                          [&]() { found(res); });
        }
        found(res);
    }

    bool done = false;
    tryLoad(state, v, [&](AbstractLoad l) {
        done = true;
        auto& res = l.result;
        if (res.isSingleValue()) {
            return lookup(state, res.singleValue().val, found,
                          [&]() { found(res); });
        }
        found(res);
    });

    if (!done)
        notFound();
}

AbstractResult TheScopeAnalysis::apply(ScopeAnalysisState& state,
                                       Instruction* i) const {
    bool handled = false;
    AbstractResult effect = AbstractResult::None;

    if (auto ret = Return::Cast(i)) {
        auto res = ret->arg<0>().val();
        lookup(state, res,
               [&](const AbstractPirValue& analysisRes) {
                   state.result.merge(analysisRes);
               },
               [&]() { state.result.merge(ValOrig(res, i)); });
        effect.update();
    } else if (auto mk = MkEnv::Cast(i)) {
        Value* lexicalEnv = mk->lexicalEnv();
        // If we know the caller, we can fill in the parent env
        if (lexicalEnv == Env::notClosed() &&
            staticClosureEnv != Env::notClosed()) {
            lexicalEnv = staticClosureEnv;
        }
        state.envs[mk].parentEnv(lexicalEnv);
        mk->eachLocalVar(
            [&](SEXP name, Value* val) { state.envs[mk].set(name, val, mk); });
        handled = true;
        effect.update();
    } else if (auto le = LdFunctionEnv::Cast(i)) {
        // LdFunctionEnv happen inside promises and refer back to the caller
        // environment, ie. the instruction that created the promise.
        assert(promiseEnv);
        assert(!state.envs.aliases.count(le) ||
               state.envs.aliases.at(le) == promiseEnv);
        state.envs.aliases[le] = promiseEnv;
    } else if (auto ldfun = LdFun::Cast(i)) {
        // Avoid sideeffect if we know that we load a function without forcing
        if (state.envs.get(ldfun->env(), ldfun->varName)
                .result.type.isA(PirType::closure())) {
            effect.update();
            handled = true;
        }
    } else if (auto s = StVar::Cast(i)) {
        state.envs[s->env()].set(s->varName, s->val(), s);
        handled = true;
        effect.update();
    } else if (auto ss = StVarSuper::Cast(i)) {
        auto superEnv = state.envs[ss->env()].parentEnv();
        if (superEnv != AbstractREnvironment::UnknownParent) {
            auto binding = state.envs.superGet(ss->env(), ss->varName);
            if (!binding.result.isUnknown()) {
                // Make sure the super env stores are not prematurely removed
                binding.result.eachSource([&](ValOrig& src) {
                    state.observedStores.insert(src.origin);
                });
                state.envs[superEnv].set(ss->varName, ss->val(), ss);
                handled = true;
                effect.update();
            }
        }
    } else if (Force::Cast(i)) {
        // First try to figure out what we force. If it's a non lazy thing, we
        // do not need to bother.
        auto force = Force::Cast(i);
        auto arg = force->arg<0>().val();
        if (!arg->type.maybeLazy()) {
            effect.max(state.returnValues[i].merge(ValOrig(arg, i)));
            handled = true;
        } else {
            lookup(state, arg->followCastsAndForce(),
                   [&](const AbstractPirValue& analysisRes) {
                       if (!analysisRes.type.maybeLazy()) {
                           effect.max(state.returnValues[i].merge(analysisRes));
                           handled = true;
                       } else if (analysisRes.isSingleValue()) {
                           arg = analysisRes.singleValue().val;
                       }
                   });
        }

        if (!handled && depth < MAX_DEPTH && force->strict) {
            // We are certain that we do force something here. Let's peek
            // through the argument and see if we find a promise. If so, we
            // will analyze it.
            if (auto mkarg = MkArg::Cast(arg->followCastsAndForce())) {
                TheScopeAnalysis prom(closure, mkarg->prom(), mkarg->env(),
                                      state, depth + 1, log);
                prom();
                state.mergeCall(code, prom.result());
                state.returnValues[i].merge(prom.result().result);
                handled = true;
                effect.update();
            }
        }
        if (!handled) {
            state.returnValues[i].merge(AbstractPirValue::tainted());
            effect.taint();
        }
    } else if (CallInstruction::CastCall(i) && depth < MAX_DEPTH) {
        auto calli = CallInstruction::CastCall(i);
        if (auto call = Call::Cast(i)) {
            auto target = call->cls()->followCastsAndForce();
            lookup(state, target, [&](const AbstractPirValue& analysisRes) {
                if (analysisRes.isSingleValue()) {
                    target =
                        analysisRes.singleValue().val->followCastsAndForce();
                }
            });
            assert(target);
            if (auto cls = MkFunCls::Cast(target)) {
                if (cls->fun->argNames.size() == calli->nCallArgs()) {
                    if (cls->fun != closure) {
                        std::vector<Value*> args;
                        calli->eachCallArg(
                            [&](Value* v) { args.push_back(v); });
                        TheScopeAnalysis nextFun(cls->fun, cls->fun->argNames,
                                                 args, cls->lexicalEnv(), state,
                                                 depth + 1, log);
                        nextFun();
                        state.mergeCall(code, nextFun.result());
                        state.returnValues[i].merge(nextFun.result().result);
                        handled = true;
                        effect.update();
                    }
                }
            }
        } else if (auto call = StaticCall::Cast(i)) {
            auto target = call->cls();
            if (target && target->argNames.size() == calli->nCallArgs()) {
                if (target != closure) {
                    std::vector<Value*> args;
                    calli->eachCallArg([&](Value* v) { args.push_back(v); });
                    TheScopeAnalysis nextFun(target, target->argNames, args,
                                             target->closureEnv(), state,
                                             depth + 1, log);
                    nextFun();
                    state.mergeCall(code, nextFun.result());
                    state.returnValues[i].merge(nextFun.result().result);
                    handled = true;
                    effect.update();
                }
            }
        } else {
            // TODO: support for NamedCall
            assert((CallBuiltin::Cast(i) || CallSafeBuiltin::Cast(i) ||
                    NamedCall::Cast(i)) &&
                   "New call instruction not handled?");
            if (!CallSafeBuiltin::Cast(i))
                state.mayUseReflection = true;
        }
        if (!handled) {
            state.returnValues[i].merge(AbstractPirValue::tainted());
            effect.taint();
        }
    }

    if (!handled) {
        if (i->hasEnv()) {
            bool envIsNeeded = i->hasEnv();
            if (envIsNeeded && i->envOnlyForObj()) {
                envIsNeeded = i->anyArg([&](Value* v) {
                    if (v == i->env() || !v->type.maybeObj())
                        return false;

                    bool maybeObj = false;
                    lookup(state, v,
                           [&](const AbstractPirValue& analysisRes) {
                               if (analysisRes.type.maybeObj())
                                   maybeObj = true;
                           },
                           [&]() { maybeObj = true; });
                    return maybeObj;
                });
            }
            if (envIsNeeded && i->leaksEnv()) {
                state.envs[i->env()].leaked = true;
                for (auto env : state.envs.potentialParents(i->env()))
                    state.allStoresObserved.insert(env);
                effect.update();
            }
            if (envIsNeeded && i->changesEnv()) {
                state.envs[i->env()].taint();
                effect.taint();
            }
        }
        if (i->mayUseReflection()) {
            state.mayUseReflection = true;
            effect.update();
        }
    }

    tryLoad(state, i, [&](AbstractLoad load) {
        if (load.result.isUnknown()) {
            for (auto env : state.envs.potentialParents(i->env())) {
                if (!state.allStoresObserved.count(env)) {
                    effect.lostPrecision();
                    state.allStoresObserved.insert(env);
                }
            }
        } else {
            load.result.eachSource([&](ValOrig& src) {
                if (!state.observedStores.count(src.origin)) {
                    state.observedStores.insert(src.origin);
                    effect.update();
                }
            });
        }
    });
    return effect;
}
}

namespace rir {
namespace pir {

ScopeAnalysis::ScopeAnalysis(Closure* function, LogStream& log) {
    TheScopeAnalysis analysis(function, function->argNames, log);
    analysis();

    // Collect all abstract values of all loads
    analysis.foreach<TheScopeAnalysis::PositioningStyle::BeforeInstruction>(
        [&](const ScopeAnalysisState& state, Instruction* i) {
            analysis.tryLoad(
                state, i, [&](AbstractLoad load) { loads.emplace(i, load); });
        });
    for (auto s : analysis.result().observedStores)
        observedStores.insert(s);
    for (auto s : analysis.result().allStoresObserved)
        allStoresObserved.insert(s);
    returnValues = std::move(analysis.result().returnValues);
    mayUseReflection = analysis.result().mayUseReflection;
}
}
}
