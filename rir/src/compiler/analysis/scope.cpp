#include "scope.h"
#include "../pir/pir_impl.h"
#include "../util/safe_builtins_list.h"
#include "query.h"

namespace rir {
namespace pir {

ScopeAnalysis::ScopeAnalysis(ClosureVersion* cls, Promise* prom, Value* promEnv,
                             const ScopeAnalysisState& initialState,
                             ScopeAnalysisResults* globalState, size_t depth,
                             LogStream& log)
    : StaticAnalysis("Scope", cls, prom, initialState, globalState, log),
      depth(depth), staticClosureEnv(promEnv) {}

void ScopeAnalysis::lookup(Value* v, const LoadMaybe& action,
                           const Maybe& notFound) const {
    auto instr = Instruction::Cast(v);
    if (!instr)
        return notFound();

    // If the this is an call instruction or force we might have some result
    // value from the inter-procedural analysis.
    // Since the "returnValues" and the "cache" are indexed by SSA variables,
    // they are always valid, even if "state" does not correspond with the
    // position of "v".
    if (globalState->returnValues.count(instr)) {
        auto& res = globalState->returnValues.at(instr);
        if (res.isSingleValue()) {
            lookup(res.singleValue().val, action,
                   [&]() { action(AbstractLoad(res)); });
            return;
        }
        action(AbstractLoad(res));
        return;
    }

    if (globalState->results.count(instr)) {
        auto& res = globalState->results.at(instr);
        if (res.result.isSingleValue()) {
            lookup(res.result.singleValue().val, action,
                   [&]() { action(res); });
            return;
        }
        action(res);
        return;
    }

    notFound();
}

void ScopeAnalysis::lookupAt(const ScopeAnalysisState& state,
                             Instruction* instr, const LoadMaybe& action,
                             const Maybe& notFound) const {

    lookup(instr, action, [&]() {
        // IMPORTANT: Dead store elimination relies on the fact that we handle
        // all
        // possible loads here

        // If this is a ldvar, we perform a get on the abstract environment and
        // if possible recurse on the result
        if (auto ld = LdVar::Cast(instr)) {
            action(load(state, ld->varName, ld->env()));
            return;
        }

        // If this is a ldfun, we perform a getFun on the abstract environment
        // and
        // if possible recurse on the result. The loadFun is an abstract version
        // of
        // ldFun and considers the special case of skipping non-closure
        // bindings.
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
    });
}

AbstractResult ScopeAnalysis::doCompute(ScopeAnalysisState& state,
                                        Instruction* i,
                                        bool updateGlobalState) {
    bool handled = false;
    AbstractResult effect = AbstractResult::None;

    auto storeResult = [&](const AbstractLoad& aload) {
        if (!updateGlobalState)
            return;
        auto& r = globalState->results;
        auto entry = r.find(i);
        if (entry == r.end()) {
            if (r.size() > MAX_RESULTS)
                return;
            r.emplace(i, aload);
            globalState->_changed = true;
        } else if (entry->second != aload) {
            entry->second = aload;
            globalState->_changed = true;
        }
    };

    auto updateReturnValue = [&](const AbstractPirValue& val) {
        if (!updateGlobalState)
            return;
        auto& r = globalState->returnValues;
        auto entry = r.find(i);
        if (entry == r.end()) {
            r.emplace(i, val);
            globalState->_changed = true;
        } else if (entry->second.merge(val).kind != AbstractResult::None) {
            globalState->_changed = true;
        }
    };

    if (auto ret = Return::Cast(i)) {
        // We keep track of the result of function returns.
        auto res = ret->arg<0>().val();
        lookup(
            res->followCasts(),
            [&](const AbstractPirValue& analysisRes) {
                state.returnValue.merge(analysisRes);
            },
            [&]() { state.returnValue.merge(ValOrig(res, i, depth)); });
        effect.update();
    } else if (Deopt::Cast(i)) {
        // who knows what the deopt target will return...
        state.returnValue.taint();
        effect.update();
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
        storeResult(ld);
        if (!ld.result.isUnknown()) {
            // We statically know the closure
            handled = true;
        } else if (ld.env != AbstractREnvironment::UnknownParent) {
            // If our analysis give us an environment approximation for the
            // ldfun, then we can at least contain the tainted environments.
            state.envs[ld.env].leaked = true;
            state.envs[ld.env].taint();
            effect.taint();
            handled = true;
        }
    } else if (auto ld = LdVar::Cast(i)) {
        auto res = load(state, ld->varName, ld->env());
        storeResult(res);
    } else if (auto ld = LdVarSuper::Cast(i)) {
        auto res = superLoad(state, ld->varName, ld->env());
        storeResult(res);
    } else if (auto s = StVar::Cast(i)) {
        state.envs[s->env()].set(s->varName, s->val(), s, depth);
        handled = true;
        effect.update();
    } else if (auto ss = StVarSuper::Cast(i)) {
        auto superEnv = state.envs[ss->env()].parentEnv();
        if (superEnv != AbstractREnvironment::UnknownParent) {
            auto binding = state.envs.superGet(ss->env(), ss->varName);
            if (!binding.result.isUnknown()) {
                state.envs[superEnv].set(ss->varName, ss->val(), ss, depth);
                handled = true;
                effect.update();
            }
        }
    } else if (auto missing = Missing::Cast(i)) {
        auto res = load(state, missing->varName, missing->env());
        if (!res.result.isUnknown()) {
            handled = true;
        }
    } else if (Force::Cast(i)) {
        // First try to figure out what we force. If it's a non lazy thing, we
        // do not need to bother.
        auto force = Force::Cast(i);
        auto arg = force->arg<0>().val();
        if (!arg->type.maybeLazy()) {
            if (!arg->type.maybePromiseWrapped())
                updateReturnValue(AbstractPirValue(arg, i, depth));
            handled = true;
        }

        if (!handled) {
            lookup(arg->followCastsAndForce(),
                   [&](const AbstractPirValue& analysisRes) {
                       if (!analysisRes.type.maybeLazy()) {
                           if (!analysisRes.type.maybePromiseWrapped())
                               updateReturnValue(analysisRes);
                           else
                               updateReturnValue(AbstractPirValue::tainted());
                           handled = true;
                       } else if (analysisRes.isSingleValue()) {
                           arg = analysisRes.singleValue().val;
                       }
                   });
        }

        if (!handled && LdArg::Cast(arg) &&
            closure->assumptions().includes(Assumption::NoReflectiveArgument)) {
            // Forcing an argument can only affect local envs by reflection.
            // Otherwise only leaked envs can be affected
            effect.max(state.envs.taintLeaked());
            updateReturnValue(AbstractPirValue::tainted());
            handled = true;
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
                auto stateCopy = state;
                stateCopy.mayUseReflection = false;
                ScopeAnalysis prom(closure, mkarg->prom(), mkarg->env(),
                                   stateCopy, globalState, depth + 1, log);
                prom();

                auto res = prom.result();
                if (!res.mayUseReflection)
                    mkarg->noReflection = true;

                state.mergeCall(code, res);
                updateReturnValue(res.returnValue);
                handled = true;
                effect.update();
                effect.keepSnapshot = true;
            }
        }
        if (!handled) {
            updateReturnValue(AbstractPirValue::tainted());
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
                updateReturnValue(AbstractPirValue::tainted());
                return;
            }

            if (depth == MAX_DEPTH)
                return;

            if (version->size() > MAX_SIZE)
                return;

            std::vector<Value*> args;
            calli->eachCallArg([&](Value* v) { args.push_back(v); });
            ScopeAnalysis nextFun(version, args, lexicalEnv, state, globalState,
                                  depth + 1, log);
            nextFun();
            state.mergeCall(code, nextFun.result());
            updateReturnValue(nextFun.result().returnValue);
            effect.keepSnapshot = true;
            handled = true;
            effect.update();
        };

        if (auto call = Call::Cast(i)) {
            auto target = call->cls()->followCastsAndForce();
            lookup(target, [&](const AbstractPirValue& result) {
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
            auto safe = false;
            if (auto builtin = CallBuiltin::Cast(i)) {
                if (SafeBuiltinsList::nonObject(builtin->blt)) {
                    safe = true;
                    builtin->eachCallArg([&](Value* arg) {
                        lookup(
                            arg->followCasts(),
                            [&](const AbstractPirValue& analysisRes) {
                                if (analysisRes.type.maybeObj())
                                    safe = false;
                            },
                            [&]() {
                                if (arg->type.maybeObj())
                                    safe = false;
                            });
                    });
                }
            }
            if (CallSafeBuiltin::Cast(i) || safe) {
                handled = true;
            } else {
                if (!CallBuiltin::Cast(i) ||
                    !SafeBuiltinsList::forInline(
                        CallBuiltin::Cast(i)->builtinId)) {
                    state.mayUseReflection = true;
                    effect.lostPrecision();
                }
            }
        }
        if (!handled) {
            updateReturnValue(AbstractPirValue::tainted());
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
                    lookup(
                        v,
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
                effect.update();
            }

            // If an instruction does arbitrary changes to the environment, we
            // need to consider it tainted.
            if (envIsNeeded && i->changesEnv()) {
                state.envs.taintLeaked();
                state.envs[i->env()].taint();
                effect.taint();
            }
        }

        if (i->effects.contains(Effect::Reflection)) {
            state.mayUseReflection = true;
            effect.lostPrecision();
        }
    }

    return effect;
}

void ScopeAnalysis::tryMaterializeEnv(const ScopeAnalysisState& state,
                                      Value* env,
                                      const MaybeMaterialized& action) {
    auto envState = state.envs.at(env);
    std::unordered_map<SEXP, AbstractPirValue> theEnv;
    for (auto& e : envState.entries) {
        if (e.second.isUnknown())
            return;
        // If any of the stores are StArg, then we cannot do this trick. The
        // reason is in the following case:
        //   e = MKEnv         x=missingArg
        //       StVar (StArg) x, ...
        // in this case starg must be preserved, since it does not override the
        // missing flag on the environment binding
        auto maybeStarg = e.second.checkEachSource([&](const ValOrig& src) {
            if (!src.origin)
                return false;
            auto st = StVar::Cast(src.origin);
            return st && st->isStArg;
        });
        if (maybeStarg)
            return;
        theEnv[e.first] = e.second;
    }

    action(theEnv);
}

} // namespace pir
} // namespace rir
