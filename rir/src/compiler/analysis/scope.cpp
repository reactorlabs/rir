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

        std::unordered_set<Closure*> ks;
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

        std::unordered_set<Closure*> ks;
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
    typedef StaticAnalysis<ScopeAnalysisState> Super;

    static const std::vector<SEXP> noArgs;
    const std::vector<SEXP>& args = noArgs;

    static constexpr size_t maxDepth = 2;
    size_t depth;
    Value* staticClosureEnv = Env::notClosed();
    Value* promiseEnv = nullptr;

    TheScopeAnalysis(Closure* cls, const std::vector<SEXP>& args,
                     LogStream& log, DebugLevel debug = DebugLevel::None)
        : Super("Scope", cls, cls, log, debug), args(args), depth(0) {}
    TheScopeAnalysis(Closure* cls, const std::vector<SEXP>& args,
                     Value* staticClosureEnv,
                     const ScopeAnalysisState& initialState, size_t depth,
                     LogStream& log, DebugLevel debug)
        : Super("Scope", cls, cls, initialState, log, debug), args(args),
          depth(depth), staticClosureEnv(staticClosureEnv) {}
    TheScopeAnalysis(Closure* cls, Promise* prom, Value* promEnv,
                     const ScopeAnalysisState& initialState, size_t depth,
                     LogStream& log, DebugLevel debug)
        : Super("Scope", cls, prom, initialState, log, debug), depth(depth),
          promiseEnv(promEnv) {}

    AbstractResult apply(ScopeAnalysisState& state,
                         Instruction* i) const override;

    typedef std::function<void(AbstractLoad)> LoadMaybe;
    RIR_INLINE void tryLoad(const ScopeAnalysisState& envs, Value* i,
                            LoadMaybe) const;
};
const std::vector<SEXP> TheScopeAnalysis::noArgs;

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

AbstractResult TheScopeAnalysis::apply(ScopeAnalysisState& state,
                                       Instruction* i) const {
    bool handled = false;
    AbstractResult effect = AbstractResult::None;

    // As we go, the analysis records stores and it records the results of
    // interprocedurally followed calls and forces.
    // The drillDown helper checks the current state of the analysis, to see
    // if for a particular value we have already a result available from the
    // above.
    std::function<ValOrig(Value*, Instruction*)> drillDown =
        [=, &drillDown](Value* v, Instruction* orig) {
            ValOrig real(v, orig);
            bool followed = false;
            tryLoad(state, v, [&](AbstractLoad ld) {
                if (ld.result.isSingleValue()) {
                    real = ld.result.singleValue();
                    followed = true;
                }
            });
            auto* i = Instruction::Cast(v);
            if (i && state.returnValues.count(i)) {
                if (state.returnValues.at(i).isSingleValue()) {
                    real = state.returnValues.at(i).singleValue();
                    followed = true;
                }
            }
            if (followed)
                return drillDown(real.val, real.origin);
            else
                return real;
        };

    if (auto ret = Return::Cast(i)) {
        auto res = drillDown(ret->arg<0>().val(), i);
        state.result.merge(res);
        effect.max(AbstractResult::Updated);
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
        effect.max(AbstractResult::Updated);
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
            effect.max(AbstractResult::Updated);
            handled = true;
        }
    } else if (auto s = StVar::Cast(i)) {
        state.envs[s->env()].set(s->varName, s->val(), s);
        handled = true;
        effect.max(AbstractResult::Updated);
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
                effect.max(AbstractResult::Updated);
            }
        }
    } else if (Force::Cast(i)) {
        // First try to figure out what we force. If it's a non lazy thing, we
        // do not need to bother.
        auto force = Force::Cast(i);
        auto arg = drillDown(force->arg<0>().val(), i);
        if (!arg.val->type.maybeLazy()) {
            state.returnValues[i].merge(arg);
            handled = true;
            effect.max(AbstractResult::Updated);
        } else if (depth < maxDepth) {
            if (force->strict) {
                // We are certain that we do force something here. Let's peek
                // through the argument and see if we find a promise. If so, we
                // will analyze it.
                if (auto mkarg = MkArg::Cast(arg.val->followCastsAndForce())) {
                    TheScopeAnalysis prom(closure, mkarg->prom(), mkarg->env(),
                                          state, depth + 1, log, debug);
                    prom();
                    state.mergeCall(code, prom.result());
                    state.returnValues[i].merge(prom.result().result);
                    handled = true;
                    effect.max(AbstractResult::Updated);
                }
            }
        }
        if (!handled) {
            state.returnValues[i].merge(AbstractPirValue::tainted());
            effect.max(AbstractResult::Tainted);
        }
    } else if (CallInstruction::CastCall(i) && depth < maxDepth) {
        auto calli = CallInstruction::CastCall(i);
        if (auto call = Call::Cast(i)) {
            auto trg = drillDown(call->cls()->followCastsAndForce(), i).val;
            assert(trg);
            auto cls = state.envs.findClosure(call->callerEnv(), trg);
            if (cls != AbstractREnvironment::UnknownClosure) {
                if (cls->fun->argNames.size() == calli->nCallArgs()) {
                    if (cls->fun != closure) {
                        TheScopeAnalysis nextFun(cls->fun, cls->fun->argNames,
                                                 cls->lexicalEnv(), state,
                                                 depth + 1, log, debug);
                        nextFun();
                        state.mergeCall(code, nextFun.result());
                        state.returnValues[i].merge(nextFun.result().result);
                        handled = true;
                        effect.max(AbstractResult::Updated);
                    }
                }
            }
        } else if (auto call = StaticCall::Cast(i)) {
            auto trg = call->cls();
            if (trg && trg->argNames.size() == calli->nCallArgs()) {
                if (trg != closure) {
                    TheScopeAnalysis nextFun(trg, trg->argNames,
                                             trg->closureEnv(), state,
                                             depth + 1, log, debug);
                    nextFun();
                    state.mergeCall(code, nextFun.result());
                    state.returnValues[i].merge(nextFun.result().result);
                    handled = true;
                    effect.max(AbstractResult::Updated);
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
            effect.max(AbstractResult::Tainted);
            state.returnValues[i].merge(AbstractPirValue::tainted());
        }
    }

    // Keep track of closures
    auto mkfun = MkFunCls::Cast(i);
    if (!mkfun) {
        MkFunCls::Cast(i->followCastsAndForce());
    }
    if (mkfun) {
        state.envs[mkfun->lexicalEnv()].mkClosures[i] = mkfun;
        handled = true;
    }

    if (!handled) {
        if (i->hasEnv()) {
            bool needed = true;
            if (i->envOnlyForObj()) {
                needed = false;
                i->eachArg([&](Value* v) {
                    if (!needed && v->type.maybeObj() &&
                        drillDown(v, i).val->type.maybeObj()) {
                        needed = true;
                    }
                });
            }
            if (needed) {
                if (i->leaksEnv()) {
                    state.envs[i->env()].leaked = true;
                    for (auto env : state.envs.potentialParents(i->env()))
                        state.allStoresObserved.insert(env);
                    effect.max(AbstractResult::Updated);
                }
                if (i->changesEnv()) {
                    state.envs[i->env()].taint();
                    effect.max(AbstractResult::Tainted);
                }
            }
        }
        if (i->mayUseReflection()) {
            state.mayUseReflection = true;
            effect.max(AbstractResult::Updated);
        }
    }

    tryLoad(state, i, [&](AbstractLoad load) {
        if (load.result.isUnknown()) {
            for (auto env : state.envs.potentialParents(i->env()))
                state.allStoresObserved.insert(env);
        } else {
            load.result.eachSource(
                [&](ValOrig& src) { state.observedStores.insert(src.origin); });
        }
    });
    return effect;
}
}

namespace rir {
namespace pir {

ScopeAnalysis::ScopeAnalysis(Closure* function, LogStream& log) {
    TheScopeAnalysis analysis(function, function->argNames, log,
                              TheScopeAnalysis::DebugLevel::Taint);
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
