#include "scope.h"
#include "../pir/pir_impl.h"
#include "query.h"

namespace rir {
namespace pir {

ScopeAnalysis::ScopeAnalysis(ClosureVersion* cls, Promise* prom, Value* promEnv,
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
               [&]() { state.returnValue.merge(ValOrig(res, i, depth)); });
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
        mk->eachLocalVar([&](SEXP name, Value* val) {
            state.envs[mk].set(name, val, mk, depth);
        });
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
        auto ld = loadFun(state, ldfun->varName, ldfun->env());
        if (!ld.result.isUnknown()) {
            // We statically know the closure
            handled = true;
        } else if (ld.env != AbstractREnvironment::UnknownParent) {
            // If our analysis give us an environment approximation for the
            // ldfun, then we can at least contain the tainted environments.
            state.envs[ld.env].leaked = true;
            for (auto env : state.envs.potentialParents(ld.env))
                state.allStoresObserved.insert(env);
            state.envs[ld.env].taint();
            effect.taint();
            handled = true;
        }
    } else if (auto s = StVar::Cast(i)) {
        state.envs[s->env()].set(s->varName, s->val(), s, depth);
        handled = true;
        effect.update();
    } else if (auto ss = StVarSuper::Cast(i)) {
        auto superEnv = state.envs[ss->env()].parentEnv();
        if (superEnv != AbstractREnvironment::UnknownParent) {
            auto binding = state.envs.superGet(ss->env(), ss->varName);
            if (!binding.result.isUnknown()) {
                // Make sure the super env stores are not prematurely removed
                binding.result.eachSource([&](const ValOrig& src) {
                    state.observedStores.insert(src.origin);
                });
                state.envs[superEnv].set(ss->varName, ss->val(), ss, depth);
                handled = true;
                effect.update();
            }
        }
    } else if (auto missing = Missing::Cast(i)) {
        auto res = load(state, missing->varName, missing->env());
        if (!res.result.isUnknown()) {
            res.result.eachSource(
                [&](auto orig) { state.observedStores.insert(orig.origin); });
            handled = true;
        }
    } else if (Force::Cast(i)) {
        // First try to figure out what we force. If it's a non lazy thing, we
        // do not need to bother.
        auto force = Force::Cast(i);
        auto arg = force->arg<0>().val();
        if (!arg->type.maybeLazy()) {
            if (!arg->type.maybePromiseWrapped())
                effect.max(state.returnValues[i].merge(ValOrig(arg, i, depth)));
            handled = true;
        }

        if (!handled) {
            lookup(state, arg->followCastsAndForce(),
                   [&](const AbstractPirValue& analysisRes) {
                       if (!analysisRes.type.maybeLazy()) {
                           if (!analysisRes.type.maybePromiseWrapped())
                               effect.max(
                                   state.returnValues[i].merge(analysisRes));
                           handled = true;
                       } else if (analysisRes.isSingleValue()) {
                           arg = analysisRes.singleValue().val;
                       }
                   });
        }

        if (!handled && depth < MAX_DEPTH && force->strict) {
            if (auto ld = LdArg::Cast(arg)) {
                if (ld->id < args.size())
                    arg = args[ld->id];
            }

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
    } else if (CallInstruction::CastCall(i)) {
        auto calli = CallInstruction::CastCall(i);

        auto interProceduralAnalysis = [&](ClosureVersion* version,
                                           Value* lexicalEnv) {
            if (depth == 0 && version == closure) {
                // At depth 0 we are sure that no contextual information is
                // considered when computing the analysis. Thus whatever is the
                // result of this functions analysis, a recursive call to itself
                // cannot excert more behaviors.
                handled = true;
                effect.needRecursion = true;
                state.returnValues[i].merge(AbstractPirValue::tainted());
                return;
            }

            if (depth == MAX_DEPTH)
                return;

            if (version->size() > MAX_SIZE)
                return;

            std::vector<Value*> args;
            calli->eachCallArg([&](Value* v) { args.push_back(v); });
            ScopeAnalysis nextFun(version, args, lexicalEnv, state, depth + 1,
                                  log);
            nextFun();
            state.mergeCall(code, nextFun.result());
            state.returnValues[i].merge(nextFun.result().returnValue);
            effect.keepSnapshot = true;
            handled = true;
            effect.update();
        };

        if (auto call = Call::Cast(i)) {
            auto target = call->cls()->followCastsAndForce();
            lookup(state, target, [&](const AbstractPirValue& result) {
                if (result.isSingleValue())
                    target = result.singleValue().val->followCastsAndForce();
            });
            assert(target);
            if (auto mk = MkFunCls::Cast(target))
                if (mk->cls->nargs() == calli->nCallArgs())
                    if (auto trg = call->tryDispatch(mk->cls))
                        interProceduralAnalysis(trg, mk->lexicalEnv());
        } else if (auto call = StaticCall::Cast(i)) {
            auto target = call->cls();
            if (target && target->nargs() == calli->nCallArgs())
                if (auto trg = call->tryDispatch())
                    interProceduralAnalysis(trg, target->closureEnv());
        } else {
            // TODO: support for NamedCall
            assert((CallBuiltin::Cast(i) || CallSafeBuiltin::Cast(i) ||
                    NamedCall::Cast(i)) &&
                   "New call instruction not handled?");
            if (!CallSafeBuiltin::Cast(i)) {
                state.mayUseReflection = true;
                effect.lostPrecision();
            } else {
                handled = true;
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

            if (auto mk = MkEnv::Cast(i->env())) {
                if (mk->stub)
                    envIsNeeded = false;
            }

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

            if (envIsNeeded && i->readsEnv()) {
                for (auto env : state.envs.potentialParents(i->env()))
                    state.allStoresObserved.insert(env);
                effect.update();
            }

            if (envIsNeeded && i->leaksEnv()) {
                state.envs[i->env()].leaked = true;
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
                load.result.eachSource([&](const ValOrig& src) {
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
