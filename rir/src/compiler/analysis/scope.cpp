#include "scope.h"
#include "../pir/pir_impl.h"
#include "query.h"

namespace rir {
namespace pir {

ScopeAnalysis::ScopeAnalysis(Closure* cls, Promise* prom, Value* promEnv,
                             const ScopeAnalysisState& initialState,
                             size_t depth, LogStream& log)
    : StaticAnalysis("Scope", cls, prom, initialState, log), depth(depth),
      staticClosureEnv(promEnv) {}

void ScopeAnalysis::lookup(const ScopeAnalysisState& state, Value* v,
                           const LoadMaybe& action,
                           const Maybe& notFound) const {
    auto instr = Instruction::Cast(v);
    if (!instr)
        return notFound();

    // If the this is an call instruction or force we might have some result
    // value from the inter-procedural analysis.
    if (state.returnValues.count(instr)) {
        auto& res = state.returnValues.at(instr);
        if (res.isSingleValue()) {
            lookup(state, res.singleValue().val, action,
                   [&]() { action(AbstractLoad(res)); });
            return;
        }
        action(AbstractLoad(res));
        return;
    }

    // IMPORTANT: Dead store elimination relies on the fact that we handle all
    // possible loads here

    // If this is a ldvar, we perform a get on the abstract environment and
    // if possible recurse on the result
    if (auto ld = LdVar::Cast(instr)) {
        action(load(state, ld->varName, ld->env()));
        return;
    }

    // If this is a ldfun, we perform a getFun on the abstract environment and
    // if possible recurse on the result. The loadFun is an abstract version of
    // ldFun and considers the special case of skipping non-closure bindings.
    // Thus it is a lot less reliable than normal load.
    if (auto ldf = LdFun::Cast(instr)) {
        action(loadFun(state, ldf->varName, ldf->env()));
        return;
    }

    // If this is a ldvarsuper, we perform a superget on the abstract
    // environment and if possible recurse on the result
    if (auto sld = LdVarSuper::Cast(instr)) {
        action(superLoad(state, sld->varName, sld->env()));
        return;
    }

    notFound();
}

AbstractResult ScopeAnalysis::apply(ScopeAnalysisState& state,
                                    Instruction* i) const {
    bool handled = false;
    AbstractResult effect = AbstractResult::None;

    if (auto ret = Return::Cast(i)) {
        // We keep track of the result of function returns.
        auto res = ret->arg<0>().val();
        lookup(state, res,
               [&](const AbstractPirValue& analysisRes) {
                   state.returnValue.merge(analysisRes);
               },
               [&]() { state.returnValue.merge(ValOrig(res, i)); });
        effect.update();
    } else if (Deopt::Cast(i)) {
        // who knows what the deopt target will return...
        state.returnValue.taint();
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
        assert(staticClosureEnv != Env::notClosed());
        assert(!state.envs.aliases.count(le) ||
               state.envs.aliases.at(le) == staticClosureEnv);
        state.envs.aliases[le] = staticClosureEnv;
    } else if (auto ldfun = LdFun::Cast(i)) {
        // Loadfun has collateral forcing if we touch intermediate envs.
        // But if we statically find the closure to load, then there is no issue
        // and we don't do anything.
        if (!loadFun(state, ldfun->varName, ldfun->env()).result.isUnknown())
            handled = true;
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
                ScopeAnalysis prom(closure, mkarg->prom(), mkarg->env(), state,
                                   depth + 1, log);
                prom();
                state.mergeCall(code, prom.result());
                state.returnValues[i].merge(prom.result().returnValue);
                handled = true;
                effect.update();
                effect.keepSnapshot = true;
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
                        ScopeAnalysis nextFun(cls->fun, args, cls->lexicalEnv(),
                                              state, depth + 1, log);
                        nextFun();
                        state.mergeCall(code, nextFun.result());
                        state.returnValues[i].merge(
                            nextFun.result().returnValue);
                        handled = true;
                        effect.update();
                        effect.keepSnapshot = true;
                    }
                }
            }
        } else if (auto call = StaticCall::Cast(i)) {
            auto target = call->cls();
            if (target && target->argNames.size() == calli->nCallArgs()) {
                if (target != closure) {
                    std::vector<Value*> args;
                    calli->eachCallArg([&](Value* v) { args.push_back(v); });
                    ScopeAnalysis nextFun(target, args, target->closureEnv(),
                                          state, depth + 1, log);
                    nextFun();
                    state.mergeCall(code, nextFun.result());
                    state.returnValues[i].merge(nextFun.result().returnValue);
                    handled = true;
                    effect.update();
                    effect.keepSnapshot = true;
                }
            }
        } else {
            // TODO: support for NamedCall
            assert((CallBuiltin::Cast(i) || CallSafeBuiltin::Cast(i) ||
                    NamedCall::Cast(i)) &&
                   "New call instruction not handled?");
            if (!CallSafeBuiltin::Cast(i)) {
                state.mayUseReflection = true;
                effect.lostPrecision();
            }
        }
        if (!handled) {
            state.returnValues[i].merge(AbstractPirValue::tainted());
            effect.taint();
        }
    }

    if (!handled) {
        if (i->hasEnv()) {
            bool envIsNeeded = i->hasEnv();
            // Already exclude the case where an operation needs an env only for
            // object arguments, but we know that none of the args are objects.
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

            // If an environemnt is leaked, then deadStore elimination does not
            // work anymore, since we cannot statically observer all the loads
            if (envIsNeeded && i->leaksEnv()) {
                state.envs[i->env()].leaked = true;
                for (auto env : state.envs.potentialParents(i->env()))
                    state.allStoresObserved.insert(env);
                effect.update();
            }

            // If an instruction does arbitrary changes to the environment, we
            // need to consider it tainted.
            if (envIsNeeded && i->changesEnv()) {
                state.envs[i->env()].taint();
                effect.taint();
            }
        }

        if (i->mayUseReflection()) {
            state.mayUseReflection = true;
            effect.lostPrecision();
        }
    }

    if (i->hasEnv()) {
        // For dead store elimination remember what loads do. This lambda will
        // be called in case of a load. If we know where exactly we load from,
        // then we mark the stores as observed. If we do not know where we load
        // from, then we need to mark all stores in the affected environment(s)
        // as potentially observed.
        lookup(state, i, [&](AbstractLoad load) {
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
    }
    return effect;
}

}
}
