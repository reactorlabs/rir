#include "scope.h"
#include "../pir/pir_impl.h"
#include "../util/safe_builtins_list.h"
#include "query.h"
#include <memory>

namespace rir {
namespace pir {

void ScopeAnalysis::lookup(Value* v, const LoadMaybe& action,
                           const Maybe& notFound) const {
    auto instr = Instruction::Cast(v);
    if (!instr)
        return notFound();

    // Inter-procedural args handling
    if (auto ld = LdArg::Cast(instr))
        if (ld->pos < args.size())
            return lookup(args[ld->pos], action, notFound);

    // If the this is a call instruction or force we might have some result
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
        if (!res.isUnknown()) {
            action(AbstractLoad(res));
            return;
        }
    }

    if (globalState->results.count(instr)) {
        auto& res = globalState->results.at(instr);
        if (res.result.isSingleValue()) {
            lookup(res.result.singleValue().val, action,
                   [&]() { action(res); });
            return;
        }
        if (!res.result.isUnknown()) {
            action(res);
            return;
        }
    }

    notFound();
}

void ScopeAnalysis::lookupAt(const ScopeAnalysisState& state,
                             Instruction* instr, const LoadMaybe& action,
                             const Maybe& notFound) const {

    lookup(instr, action, [&]() {
        // IMPORTANT: Dead store elimination relies on the fact that we handle
        // all possible loads here

        // If this is a ldvar, we perform a get on the abstract environment and
        // if possible recurse on the result
        if (auto ld = LdVar::Cast(instr)) {
            action(load(state, ld->varName, ld->env()));
            return;
        }

        if (auto ld = LdDots::Cast(instr)) {
            action(load(state, R_DotsSymbol, ld->env()));
            return;
        }

        // If this is a ldfun, we perform a getFun on the abstract environment
        // and if possible recurse on the result. The loadFun is an abstract
        // version of ldFun and considers the special case of skipping
        // non-closure bindings. Thus it is a lot less reliable than normal
        // load.
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

    if (i->effects.includes(Effect::TriggerDeopt)) {
        auto fs = i->frameState();
        while (fs) {
            if (!MkEnv::Cast(fs->env()) || !MkEnv::Cast(fs->env())->stub)
                state.envs.leak(fs->env());
            fs = fs->next();
        }
    }

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
        // Deoptimization can only happen if the code has an Assume
        if (canDeopt) {
            // who knows what the deopt target will return...
            state.returnValue.taint();
            state.mayUseReflection = true;
            effect.taint();
        }
        handled = true;
    } else if (auto mk = MkEnv::Cast(i)) {
        Value* lexicalEnv = mk->lexicalEnv();
        // If we know the caller, we can fill in the parent env
        if (lexicalEnv == Env::notClosed() &&
            staticClosureEnv != Env::notClosed()) {
            lexicalEnv = staticClosureEnv;
        }
        auto& env = state.envs.at(mk);
        env.parentEnv(lexicalEnv);
        mk->eachLocalVar([&](SEXP name, Value* val, bool m) {
            env.set(name, val, mk, depth);
        });
        handled = true;
        effect.update();
    } else if (auto me = MaterializeEnv::Cast(i)) {
        state.envs.aliases[me] = me->arg(0).val();
    } else if (auto le = LdFunctionEnv::Cast(i)) {
        if (staticClosureEnv != Env::notClosed()) {
            // LdFunctionEnv happen inside promises and refer back to the caller
            // environment, ie. the instruction that created the promise.
            assert(!state.envs.aliases.count(le) ||
                   state.envs.aliases.at(le) == staticClosureEnv);
            state.envs.aliases[le] = staticClosureEnv;
        } else {
            state.envs.at(i).leak();
            effect.taint();
        }
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
            auto& env = state.envs.at(ld.env);
            state.envs.leak(ld.env);
            env.taint();
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
        state.envs.at(s->env()).set(s->varName, s->val(), s, depth);
        handled = true;
        effect.update();
    } else if (auto ss = StVarSuper::Cast(i)) {
        auto superEnv = state.envs.at(ss->env()).parentEnv();
        if (superEnv != AbstractREnvironment::UnknownParent) {
            auto binding = state.envs.superGet(ss->env(), ss->varName);
            if (!binding.result.isUnknown()) {
                state.envs.at(superEnv).set(ss->varName, ss->val(), ss, depth);
                handled = true;
                effect.update();
            }
        }
    } else if (auto missing = Missing::Cast(i)) {
        auto res = load(state, missing->varName, missing->env());
        if (!res.result.isUnknown()) {
            handled = true;
        }
    } else if (auto up = UpdatePromise::Cast(i)) {
        effect.max(state.forcedPromise[up->mkarg()].merge(
            AbstractPirValue(up->arg(1).val(), up, depth)));
        handled = true;
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
            auto doLookup = [&](const AbstractPirValue& analysisRes) {
                if (!analysisRes.type.maybeLazy()) {
                    if (!analysisRes.type.maybePromiseWrapped())
                        updateReturnValue(analysisRes);
                    else
                        updateReturnValue(AbstractPirValue::tainted());
                    handled = true;
                } else if (analysisRes.isSingleValue()) {
                    arg = analysisRes.singleValue().val;
                }
            };
            // Often we have `x = LdVar(...); y = Force(x)`. Since LdVar does
            // not change the environment, the state at Force is usable to query
            // the result of the LdVar.
            auto arg0 = Instruction::Cast(arg->followCastsAndForce());
            bool adjacent = false;
            if (arg0 && i->bb() == arg0->bb())
                for (const auto& j : *i->bb()) {
                    if (j == arg0)
                        adjacent = true;
                    else if (j == i)
                        break;
                    else if (adjacent && j->changesEnv()) {
                        adjacent = false;
                        break;
                    }
                }

            if (adjacent)
                lookupAt(state, arg0, doLookup);
            else
                lookup(arg->followCastsAndForce(), doLookup);
        }

        // Forcing an argument can only affect local envs by reflection.
        // Hence, only leaked envs can be affected
        auto env = MkEnv::Cast(force->env());
        if (!handled) {
            if (auto mkarg = MkArg::Cast(arg->followCastsAndForce())) {
                auto upd = state.forcedPromise.find(mkarg);
                if (upd == state.forcedPromise.end()) {
                    if (depth < MAX_DEPTH && !fixedPointReached() &&
                        force->strict && !state.envs.allTainted() &&
                        mkarg->prom()->numInstrs() < MAX_PROM_SIZE) {

                        // We are certain that we do force something
                        // here. Let's peek through the argument and see
                        // if we find a promise. If so, we will analyze
                        // it.

                        ScopeAnalysis* prom = nullptr;
                        if (!subAnalysis.count(i)) {
                            if (subAnalysis.size() < MAX_SUB_ANALYSIS) {
                                prom =
                                    subAnalysis
                                        .emplace(
                                            i,
                                            std::make_unique<ScopeAnalysis>(
                                                closure, mkarg->prom(),
                                                mkarg->env(), state,
                                                globalState, depth + 1, logger))
                                        .first->second.get();
                                prom->setInitialState(
                                    [&](ScopeAnalysisState& init) {
                                        init.mayUseReflection = false;
                                    });
                            }
                        } else {
                            prom = subAnalysis.at(i).get();
                            prom->setInitialState(
                                [&](ScopeAnalysisState& init) {
                                    init = state;
                                    init.mayUseReflection = false;
                                });
                        }
                        if (prom) {
                            (*prom)();

                            auto res = prom->result();

                            state.mergeCall(code, res);
                            updateReturnValue(res.returnValue);
                            effect.max(state.forcedPromise[mkarg].merge(
                                res.returnValue));
                            handled = true;
                            effect.update();
                            effect.keepSnapshot = true;
                        }
                    }
                } else if (!upd->second.isUnknown()) {
                    updateReturnValue(upd->second);
                    handled = true;
                }
                if (!handled) {
                    state.envs.at(mkarg->env()).taint();
                    effect.max(state.envs.taintLeaked());
                    updateReturnValue(AbstractPirValue::tainted());
                    handled = true;
                }
            } else if (auto a = LdArg::Cast(arg->followCastsAndForce())) {
                if (closure->context().isNonRefl(a->pos)) {
                    effect.max(state.envs.taintLeaked());
                    updateReturnValue(AbstractPirValue::tainted());
                    handled = true;
                }
            } else if (env && env->stub) {
                // Forcing using a stub should deopt if local vars are modified.
                effect.max(state.envs.taintLeaked());
                updateReturnValue(AbstractPirValue::tainted());
                handled = true;
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

            if (fixedPointReached() || depth == MAX_DEPTH)
                return;

            if (version->numNonDeoptInstrs() > MAX_SIZE)
                return;

            if (state.envs.allTainted())
                return;

            std::vector<Value*> args;
            calli->eachCallArg([&](Value* v) { args.push_back(v); });
            while (args.size() < version->effectiveNArgs())
                args.push_back(MissingArg::instance());

            // While analyzing the callee we have to assume the caller's
            // envrionment leaked. Because the callee can always access it
            // reflectively. If when the callee returns the env was not tainted,
            // it can be un-leaked again.
            ScopeAnalysis* nextFun;
            bool myEnvWasLeaked = state.envs.at(i->env()).leaked();
            if (!subAnalysis.count(i)) {
                if (subAnalysis.size() >= MAX_SUB_ANALYSIS)
                    return;
                auto subState = state;
                subState.envs.at(i->env()).leak();
                nextFun =
                    subAnalysis
                        .emplace(i, std::make_unique<ScopeAnalysis>(
                                        version, args, lexicalEnv, subState,
                                        globalState, depth + 1, logger))
                        .first->second.get();
            } else {
                nextFun = subAnalysis.at(i).get();
                nextFun->setInitialState([&state, i](ScopeAnalysisState& init) {
                    init = state;
                    init.envs.at(i->env()).leak();
                });
            }

            (*nextFun)();
            auto& result = const_cast<ScopeAnalysisState&>(nextFun->result());
            auto& myenv = result.envs.at(i->env());
            if (!myEnvWasLeaked && !myenv.tainted)
                myenv.unleak();
            state.mergeCall(code, result);
            updateReturnValue(nextFun->result().returnValue);
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
            bool anyDots = false;
            calli->eachCallArg(
                [&](Value* v) { anyDots = anyDots || ExpandDots::Cast(v); });
            if (auto mk = MkCls::Cast(target))
                if (!anyDots)
                    if (mk->tryGetCls()) {
                        if (auto trg = call->tryDispatch(mk->tryGetCls()))
                            interProceduralAnalysis(trg, mk->lexicalEnv());
                    }
        } else if (auto call = StaticCall::Cast(i)) {
            auto target = call->cls();
            bool anyDots = false;
            calli->eachCallArg(
                [&](Value* v) { anyDots = anyDots || ExpandDots::Cast(v); });
            if (target && !anyDots)
                if (auto trg = call->tryDispatch())
                    interProceduralAnalysis(trg, target->closureEnv());
        } else {
            // TODO: support for NamedCall
            assert((CallBuiltin::Cast(i) || CallSafeBuiltin::Cast(i) ||
                    NamedCall::Cast(i)) &&
                   "New call instruction not handled?");
            auto safe = false;
            if (auto builtin = CallBuiltin::Cast(i)) {
                if (SafeBuiltinsList::nonObject(builtin->builtinSexp)) {
                    safe = true;
                    builtin->eachCallArg([&](Value* arg) {
                        lookup(
                            arg->followCasts(),
                            [&](const AbstractPirValue& analysisRes) {
                                if (analysisRes.type.maybeObj() ||
                                    analysisRes.type.maybe(RType::expandedDots))
                                    safe = false;
                            },
                            [&]() {
                                if (arg->type.maybeObj() ||
                                    arg->type.maybe(RType::expandedDots))
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

    if (!CallSafeBuiltin::Cast(i) && !ChkFunction::Cast(i) &&
        !CastType::Cast(i) && !Force::Cast(i)) {
        i->eachArg([&](Value* a) {
            // The env arg has special treatment below since it can only be
            // leaked by instructions marked leaksEnv.
            if (i->hasEnv() && i->env() == a)
                return;
            a = a->followCasts();
            Value* leak = nullptr;
            if (auto mk = MkArg::Cast(a)) {
                leak = mk->env();
            } else if (auto mk = MkCls::Cast(a)) {
                leak = mk->lexicalEnv();
            } else if (!Env::isAnyEnv(i) && Env::isAnyEnv(a)) {
                leak = a;
            }
            if (leak && leak != Env::elided()) {
                if (auto mk = MkEnv::Cast(i)) {
                    state.envs.addDependency(mk, leak);
                } else if (auto st = StVar::Cast(i)) {
                    state.envs.addDependency(st->env(), leak);
                } else if (auto st = StVarSuper::Cast(i)) {
                    auto ld = state.envs.superGet(st->env(), st->varName);
                    state.envs.addDependency(ld.env, leak);
                } else {
                    state.envs.leak(leak);
                }
                effect.update();
            }
        });
    }

    if (!handled) {
        if (i->hasEnv()) {
            bool envIsNeeded = i->hasEnv();
            bool stubenv = MkEnv::Cast(i->env()) && MkEnv::Cast(i->env())->stub;

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

            if (envIsNeeded && !stubenv && i->leaksEnv()) {
                state.envs.leak(i->env());
                effect.update();
            }

            // If an instruction does arbitrary changes to the environment, we
            // need to consider it tainted.
            if (envIsNeeded && i->changesEnv()) {
                state.envs.taintLeaked();
                if (!stubenv)
                    state.envs.at(i->env()).taint();
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
    const auto& envState = state.envs.at(env);
    std::unordered_map<SEXP, std::pair<AbstractPirValue, bool>> theEnv;
    for (const auto& entry : envState.entries) {
        auto& name = entry.first;
        auto& val = entry.second;
        if (val.isUnknown())
            return;
        bool isInitiallyMissing = false;
        bool maybeInitiallyMissing = false;
        bool maybeSkippesStarg = false;
        val.eachSource([&](const ValOrig& src) {
            if (!src.origin) {
            } else if (auto mk = MkEnv::Cast(src.origin)) {
                mk->eachLocalVar([&](SEXP n, Value* v, bool miss) {
                    if (name == n) {
                        if (miss)
                            maybeInitiallyMissing = isInitiallyMissing = true;
                        else if (!closure->context().includes(
                                     Assumption::NoExplicitlyMissingArgs))
                            maybeInitiallyMissing = true;
                    }
                });
            } else if (auto st = StVar::Cast(src.origin)) {
                if (st->isStArg)
                    maybeSkippesStarg = true;
            } else {
                maybeSkippesStarg = true;
            }
        });
        // Ambiguous, we don't know if missing is set or not
        if (maybeInitiallyMissing && maybeSkippesStarg)
            return;
        theEnv[name] = {val, isInitiallyMissing};
    }

    action(theEnv);
}

} // namespace pir
} // namespace rir
