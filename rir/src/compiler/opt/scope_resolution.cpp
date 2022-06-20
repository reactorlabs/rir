#include "../analysis/query.h"
#include "../analysis/scope.h"
#include "../pir/pir_impl.h"
#include "../util/phi_placement.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "compiler/analysis/context_stack.h"
#include "compiler/compiler.h"
#include "compiler/util/bb_transform.h"
#include "pass_definitions.h"
#include "utils/Set.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;

// Checks (using the local state of the caller) if forcing a promise can have
// reflective effects
static bool noReflection(ClosureVersion* cls, Code* code, Value* callEnv,
                         const ScopeAnalysis& analysis,
                         const ScopeAnalysisState& state) {
    // Note that the entry block is empty and jumps to the next block; this is
    // to ensure that it has no predecessors.
    auto entry = code->entry;
    assert(entry->isEmpty() && entry->isJmp() &&
           !((const BB*)entry)->next()->isEmpty());
    auto funEnv = LdFunctionEnv::Cast(*entry->next()->begin());

    return Visitor::check(code->entry, [&](Instruction* i) {
        if (CallSafeBuiltin::Cast(i))
            return true;
        if (auto b = CallBuiltin::Cast(i))
            return SafeBuiltinsList::forInline(b->builtinId);

        auto maybeLazy = [&](AbstractPirValue& res) -> bool {
            if (res.isUnknown())
                return true;
            bool maybe = false;
            res.eachSource([&](ValOrig vo) {
                auto v = vo.val->followDownCastsAndForce();
                if (v->type.maybeLazy()) {
                    if (auto a = LdArg::Cast(v))
                        if (cls->context().isNonRefl(a->pos) ||
                            cls->context().isEager(a->pos))
                            return;
                    maybe = true;
                }
            });
            return maybe;
        };

        auto maybeLazyRes = [&](Instruction* i) {
            if (auto ld = LdVar::Cast(i)) {
                auto e = ld->env() == funEnv ? callEnv : ld->env();
                auto res = analysis.load(state, ld->varName, e);
                return maybeLazy(res.result);
            }

            if (auto ld = LdVarSuper::Cast(i)) {
                auto e = ld->env() == funEnv ? callEnv : ld->env();
                auto res = analysis.superLoad(state, ld->varName, e);
                return maybeLazy(res.result);
            }

            if (auto ld = LdFun::Cast(i)) {
                auto e = ld->env() == funEnv ? callEnv : ld->env();
                auto res = analysis.loadFun(state, ld->varName, e);
                return maybeLazy(res.result);
            }

            assert(false);
            return true;
        };

        if (LdFun::Cast(i))
            return !maybeLazyRes(i);

        if (auto force = Force::Cast(i)) {
            auto arg = force->arg<0>().val()->followCastsAndForce();
            if (LdFun::Cast(arg) || LdVar::Cast(arg) || LdVarSuper::Cast(arg))
                return !maybeLazyRes(Instruction::Cast(arg));
        }

        return !i->effects.includes(Effect::Reflection);
    });
}

} // namespace

namespace rir {
namespace pir {

bool ScopeResolution::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                            AbstractLog& log, size_t) const {

    DominanceGraph dom(code);
    DominanceFrontier dfront(code, dom);
    ContextStack contexts(cls, code, log);

    bool anyChange = false;
    ScopeAnalysis analysis(cls, code, log);
    analysis();
    auto& finalState = analysis.result();
    if (finalState.noReflection() && code == cls)
        cls->properties.set(ClosureVersion::Property::NoReflection);

    std::unordered_map<Value*, Value*> replacedValue;
    struct CreatedPhiCache {
        bool hasUnbound;
        std::unordered_map<BB*, Phi*> phis;
        std::unordered_map<BB*, BB*> dominatingPhi;
    };
    std::vector<std::pair<AbstractPirValue, CreatedPhiCache>> createdPhis;
    auto getReplacedValue = [&](Value* val) {
        while (replacedValue.count(val))
            val = replacedValue.at(val);
        return val;
    };
    auto getSingleLocalValue = [&](const AbstractPirValue& result) -> Value* {
        if (!result.isSingleValue())
            return nullptr;
        if (result.singleValue().recursionLevel != 0)
            return nullptr;
        auto val = getReplacedValue(result.singleValue().val);
        assert(val->validIn(code));
        return val;
    };

    auto tryInsertPhis = [&](Value* env, AbstractPirValue res, BB* bb,
                             BB::Instrs::iterator& iter,
                             bool allowUnbound) -> Value* {
        if (res.isUnknown())
            return nullptr;

        auto iterPos = *iter;
        bool onlyLocalVals = true;
        res.eachSource([&](const ValOrig& src) {
            if (src.recursionLevel > 0)
                onlyLocalVals = false;
        });
        if (!onlyLocalVals)
            return nullptr;

        std::unordered_map<BB*, Value*> inputs;
        bool fail = false;
        bool hasUnbound = false;
        res.eachSource([&](ValOrig v) {
            if (fail)
                return;
            if (!v.origin)
                assert(v.val == UnboundValue::instance());
            if (v.val == UnboundValue::instance()) {
                if (allowUnbound) {
                    auto initialBB = Instruction::Cast(env)
                                         ? Instruction::Cast(env)->bb()
                                         : code->entry;
                    inputs[initialBB] = UnboundValue::instance();
                    hasUnbound = true;
                } else {
                    fail = true;
                }
            } else {
                if (inputs.count(v.origin->bb()))
                    fail = true;
                else
                    inputs[v.origin->bb()] = v.val;
            }
        });
        if (fail)
            return nullptr;

        auto pl = PhiPlacement(code, inputs, dom, dfront);
        BB* targetPhiPosition = nullptr;
        if (pl.placement.count(bb))
            targetPhiPosition = bb;
        else if (pl.dominatingPhi.count(bb))
            targetPhiPosition = pl.dominatingPhi.at(bb);
        else
            return nullptr;

        auto findExistingPhi =
            [&](BB* pos,
                const rir::SmallSet<PhiPlacement::PhiInput>& inps) -> Phi* {
            for (auto i : *pos) {
                if (auto candidate = Phi::Cast(i)) {
                    if (candidate->nargs() == inps.size()) {
                        rir::SmallSet<PhiPlacement::PhiInput> candidateInp;
                        // This only works for value inputs, not for phi
                        // inputs. I am not sure how it could be extended...
                        candidate->eachArg([&](BB* inbb, Value* inval) {
                            candidateInp.insert({inbb, nullptr, inval});
                        });
                        if (candidateInp == inps)
                            return candidate;
                    }
                }
            }
            return nullptr;
        };

        std::unordered_map<BB*, Phi*> thePhis;
        for (auto& phi : pl.placement) {
            for (auto& p : phi.second) {
                if (p.aValue)
                    p.aValue = getReplacedValue(p.aValue);
            }

            if (auto p = findExistingPhi(phi.first, phi.second))
                thePhis[phi.first] = p;
            else
                thePhis[phi.first] = new Phi;
        }

        for (auto& computed : pl.placement) {
            auto& pos = computed.first;
            auto& phi = thePhis.at(pos);

            // Existing phi
            if (phi->nargs() > 0)
                continue;

            assert(computed.second.size() > 1);
            for (auto& p : computed.second) {
                if (p.aValue)
                    phi->addInput(p.inputBlock, p.aValue);
                else
                    phi->addInput(p.inputBlock, thePhis.at(p.otherPhi));
            }

            pos->insert(pos->begin(), phi);
            // If the insert changed the current bb, we need to keep the
            // iterator updated
            if (pos == bb)
                iter = bb->atPosition(iterPos);

            if (!phi->type.isA(res.type))
                phi->type = res.type;
        }

        for (auto& phi : thePhis)
            phi.second->updateTypeAndEffects();

        auto target = thePhis.at(targetPhiPosition);
        createdPhis.push_back(
            {res, CreatedPhiCache(
                      {hasUnbound, thePhis, std::move(pl.dominatingPhi)})});
        return target;
    };

    Visitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;

        auto before = analysis.before(*bb->begin());
        auto after = before;
        bool changed = false;

        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (changed) {
                before = analysis.before(i);
                anyChange = true;
                changed = false;
            } else {
                before = analysis.before(i, &after);
            }
            after = analysis.after(i, &before);

            // Force and callees can only see our env only through
            // reflection
            if (i->hasEnv() &&
                (CallInstruction::CastCall(i) || Force::Cast(i))) {
                if (after.noReflection()) {
                    i->elideEnv();
                    i->effects.reset(Effect::Reflection);
                    changed = true;
                }
                if (after.envNotEscaped(i->env()) &&
                    i->effects.includes(Effect::LeaksEnv)) {
                    changed = true;
                    i->effects.reset(Effect::LeaksEnv);
                }
            }

            if (auto mk = MkArg::Cast(i)) {
                if (!mk->noReflection)
                    if (noReflection(cls, mk->prom(),
                                     i->hasEnv() ? i->env() : Env::notClosed(),
                                     analysis, before)) {
                        mk->noReflection = true;
                        changed = true;
                    }
            }

            // If no reflective argument is passed to us, then forcing an
            // argument cannot see our environment
            if (auto force = Force::Cast(i)) {
                auto arg = force->arg<0>().val()->followCastsAndForce();
                analysis.lookup(arg, [&](const AbstractPirValue& res) {
                    res.ifSingleValue([&](Value* val) { arg = val; });
                });
                if (auto a = LdArg::Cast(arg)) {
                    if (force->hasEnv() && cls->context().isNonRefl(a->pos)) {
                        force->elideEnv();
                        force->effects.reset(Effect::Reflection);
                        changed = true;
                    }

                    if (after.noReflection()) {
                        force->type.fromContext(cls->context(), a->pos,
                                                cls->nargs(), true);
                    }
                }
            }

            // StVarSuper where the parent environment is known and
            // local, can be replaced by simple StVar, if the variable
            // exists in the super env. Or if the super env is the global
            // env, since super assign never goes beyond that one.
            if (auto sts = StVarSuper::Cast(i)) {
                auto aLoad =
                    analysis.superLoad(before, sts->varName, sts->env());
                if (aLoad.env != AbstractREnvironment::UnknownParent) {
                    auto env = Env::Cast(aLoad.env);
                    if ((env && env->rho == R_GlobalEnv) ||
                        (!aLoad.result.isUnknown() &&
                         aLoad.env->validIn(code))) {
                        auto r = new StVar(sts->varName, sts->val(), aLoad.env);
                        bb->replace(ip, r);
                        sts->replaceUsesWith(r);
                        replacedValue[sts] = r;
                        changed = true;
                    }
                }
                ip = next;
                continue;
            }

            // Constant fold "missing" if we can.
            if (auto missing = Missing::Cast(i)) {
                auto res =
                    analysis.load(before, missing->varName, missing->env());
                bool notMissing = false;
                if (res.result.isSingleValue()) {
                    auto v =
                        res.result.singleValue().val->followCastsAndForce();
                    if (!v->type.maybePromiseWrapped() &&
                        !v->type.maybeMissing() &&
                        /* Warning: Forcing a (non-missing) promise can
                            still return missing... */
                        !MkArg::Cast(v)) {
                        notMissing = true;
                    }
                    // If we find the (eager) root promise, we know if it is
                    // missing or not! Note this doesn't go throught forces.
                    if (auto mk = MkArg::Cast(
                            res.result.singleValue().val->followCasts())) {
                        if (mk->isEager() &&
                            mk->eagerArg() != MissingArg::instance())
                            notMissing = true;
                    }
                }
                if (!res.result.type.maybeMissing() &&
                    !res.result.type.maybePromiseWrapped()) {
                    notMissing = true;
                }

                if (notMissing) {
                    // Missing still returns TRUE, if the argument was
                    // initially missing, but then overwritten by a default
                    // argument.
                    if (auto env = MkEnv::Cast(missing->env())) {
                        bool initiallyMissing = false;
                        env->eachLocalVar([&](SEXP name, Value* val, bool m) {
                            if (name == missing->varName)
                                initiallyMissing = m;
                        });
                        if (!initiallyMissing) {
                            missing->replaceUsesWith(False::instance());
                            replacedValue[missing] = False::instance();
                            next = bb->remove(ip);
                            changed = true;
                        }
                    }
                } else {
                    res.result.ifSingleValue([&](Value* v) {
                        if (v == MissingArg::instance()) {
                            missing->replaceUsesWith(True::instance());
                            replacedValue[missing] = True::instance();
                            next = bb->remove(ip);
                            changed = true;
                        }
                    });
                }
            }

            if (bb->isDeopt()) {
                if (auto fs = FrameState::Cast(i)) {
                    if (auto mk = MkEnv::Cast(fs->env())) {
                        bool candidate = mk->bb() != bb;
                        // Environments which start off with a lot of
                        // uninitialized variables are not profitable to
                        // elide, because all these variables need to be
                        // boxed.
                        // TODO: implement unboxed uninitialized values
                        size_t unbound = 0;
                        if (candidate)
                            mk->eachLocalVar([&](SEXP, Value* v, bool) {
                                if (v == UnboundValue::instance())
                                    unbound++;
                            });
                        if (unbound > 3)
                            candidate = false;
                        std::unordered_set<Tag> allowed(
                            {Tag::FrameState, Tag::StVar, Tag::IsEnvStub});
                        if (!mk->stub)
                            allowed.insert(Tag::LdVar);
                        if (candidate)
                            if (!mk->usesAreOnly(code->entry, allowed))
                                candidate = false;

                        if (candidate) {
                            analysis.tryMaterializeEnv(
                                before, mk,
                                [&](const std::unordered_map<
                                    SEXP, std::pair<AbstractPirValue, bool>>&
                                        env) {
                                    std::vector<SEXP> names;
                                    std::vector<Value*> values;
                                    std::vector<bool> missing;
                                    for (auto& e : env) {
                                        names.push_back(e.first);
                                        auto v = e.second.first;
                                        auto miss = e.second.second;
                                        if (v.isUnknown())
                                            return;
                                        if (auto val = getSingleLocalValue(v)) {
                                            values.push_back(val);
                                        } else {
                                            Value* phi = nullptr;
                                            for (auto& c : createdPhis) {
                                                if (c.first == v) {
                                                    auto& cache = c.second;
                                                    if (cache.phis.count(bb))
                                                        phi = cache.phis.at(bb);
                                                    else if (cache.dominatingPhi
                                                                 .count(bb))
                                                        phi = cache.phis.at(
                                                            cache.dominatingPhi
                                                                .at(bb));
                                                    break;
                                                }
                                            }
                                            if (!phi) {
                                                phi = tryInsertPhis(mk, v, bb,
                                                                    ip, true);
                                            }
                                            if (!phi)
                                                return;
                                            values.push_back(phi);
                                        }
                                        missing.push_back(miss);
                                    }
                                    auto deoptEnv =
                                        new MkEnv(mk->lexicalEnv(), names,
                                                  values.data(), missing);
                                    ip = bb->insert(ip, deoptEnv);
                                    ip++;
                                    next = ip + 1;
                                    mk->replaceDominatedUses(deoptEnv, dom);
                                    if (mk->context) {
                                        auto diff =
                                            contexts.before(deoptEnv)
                                                .numContexts() -
                                            contexts.before(mk).numContexts();
                                        deoptEnv->context = mk->context + diff;
                                    } else {
                                        deoptEnv->context = 0;
                                    }
                                    changed = true;
                                });
                        }
                    }
                }
            }

            analysis.lookupAt(after, i, [&](const AbstractLoad& aLoad) {
                auto& res = aLoad.result;

                bool isActualLoad =
                    LdVar::Cast(i) || LdFun::Cast(i) || LdVarSuper::Cast(i);

                // In case the scope analysis is sure that this is
                // actually the same as some other PIR value. So let's just
                // replace it.
                if (res.isSingleValue()) {
                    if (auto val = getSingleLocalValue(res)) {
                        if (val->type.isA(i->type)) {
                            if (isActualLoad && val->type.maybeMissing()) {
                                // LdVar checks for missingness, so we need
                                // to preserve this.
                                auto chk = new ChkMissing(val);
                                ip = bb->insert(ip, chk);
                                ip++;
                                val = chk;
                            }
                            replacedValue[i] = val;
                            i->replaceUsesWith(val);
                            assert(!val->type.maybePromiseWrapped() ||
                                   i->type.maybePromiseWrapped());
                            next = bb->remove(ip);
                            changed = true;
                            return;
                        }
                    }
                }

                // Narrow down type according to what the analysis reports
                if (i->type.isRType()) {
                    auto inferedType = res.type;
                    if (!i->type.isA(inferedType) && !inferedType.isVoid()) {
                        i->type = inferedType;
                        changed = true;
                    }
                }

                // The generic case where we have a bunch of potential
                // values we will insert a phi to group all of them. In
                // general this is only possible if they all come from the
                // current function (and not through inter procedural
                // analysis from other functions).
                //
                // Also, we shold only do this for actual loads and not
                // in general. Otherwise there is a danger that we insert
                // the same phi twice (e.g. if a force returns the result
                // of a load, we will resolve the load and the force) which
                // ends up being rather painful.
                if (!res.isUnknown() && isActualLoad) {
                    Value* resPhi = nullptr;
                    bool failed = false;

                    for (auto& c : createdPhis) {
                        if (c.first == res) {
                            auto& cache = c.second;
                            if (cache.hasUnbound) {
                                failed = true;
                                break;
                            }
                            if (cache.phis.count(bb))
                                resPhi = cache.phis.at(bb);
                            else if (cache.dominatingPhi.count(bb))
                                resPhi =
                                    cache.phis.at(cache.dominatingPhi.at(bb));
                            break;
                        }
                    }
                    if (!resPhi && !failed)
                        resPhi = tryInsertPhis(i->env(), res, bb, ip, false);

                    if (resPhi) {
                        Value* val = resPhi;
                        if (val->type.maybeMissing()) {
                            // LdVar checks for missingness, so we need
                            // to preserve this.
                            auto chk = new ChkMissing(val);
                            ip = bb->insert(ip, chk);
                            ip++;
                            val = chk;
                        }
                        i->replaceUsesWith(val);
                        replacedValue[i] = val;
                        assert(!val->type.maybePromiseWrapped() ||
                               i->type.maybePromiseWrapped());
                        next = bb->remove(ip);
                        changed = true;
                        return;
                    }
                }

                // LdVarSuper where the parent environment is known and
                // local, can be replaced by a simple LdVar
                if (auto lds = LdVarSuper::Cast(i)) {
                    if (!LdFunctionEnv::Cast(lds->env())) {
                        auto e = Env::parentEnv(lds->env());
                        if (e) {
                            auto r = new LdVar(lds->varName, e);
                            bb->replace(ip, r);
                            lds->replaceUsesWith(r);
                            assert(!r->type.maybePromiseWrapped() ||
                                   i->type.maybePromiseWrapped());
                            replacedValue[lds] = r;
                            changed = true;
                        }
                    }
                    return;
                }

                // Ldfun needs some special treatment sometimes:
                // Since non closure bindings are skipped at runtime, we can
                // only resolve ldfun if we are certain which one is the
                // first binding that holds a closure. Often this is only
                // possible after inlining a promise. But inlining a promise
                // requires a force instruction. But ldfun does force
                // implicitly. To get out of this vicious circle, we add the
                // first binding we find with a normal load (as opposed to
                // loadFun) from the abstract state as a "guess" This will
                // enable other passes (especially the promise inliner pass)
                // to work on the guess and maybe the next time we end up
                // here, we can actually prove that the guess was right.
                if (auto ldfun = LdFun::Cast(i)) {
                    auto guess = ldfun->guessedBinding();
                    // If we already have a guess, let's see if now know
                    // that it is a closure.
                    if (guess) {
                        // TODO: if !guess->maybe(closure) we know that the
                        // guess is wrong and could try the next binding.
                        if (!guess->type.isA(PirType::function())) {
                            if (auto i = Instruction::Cast(guess)) {
                                analysis.lookupAt(
                                    before, i,
                                    [&](const AbstractPirValue& res) {
                                        if (auto val = getSingleLocalValue(res))
                                            guess = val;
                                    });
                            }
                        }
                        if (guess->type.isA(PirType::function()) &&
                            guess->validIn(code)) {
                            guess = getReplacedValue(guess);
                            ldfun->replaceUsesWith(guess);
                            replacedValue[ldfun] = guess;
                            next = bb->remove(ip);
                            changed = true;
                            return;
                        }
                    } else {
                        auto res =
                            analysis.load(before, ldfun->varName, ldfun->env())
                                .result;
                        if (auto firstBinding = getSingleLocalValue(res)) {
                            ip = bb->insert(
                                ip, new Force(firstBinding, ldfun->env(),
                                              Tombstone::framestate()));
                            ldfun->guessedBinding(*ip);
                            next = ip + 2;
                            return;
                        }
                    }
                }

                // If nothing else, narrow down the environment (in case we
                // found something more concrete).
                if (i->hasEnv() &&
                    aLoad.env != AbstractREnvironment::UnknownParent) {
                    if (!MaterializeEnv::Cast(i->env()) &&
                        i->env() != aLoad.env) {
                        changed = true;
                        i->env(aLoad.env);
                    }

                    // Assume bindings in base namespace stay unchanged
                    if (!bb->isDeopt()) {
                        if (auto env = Env::Cast(aLoad.env)) {
                            if (env->rho == R_BaseEnv ||
                                env->rho == R_BaseNamespace) {
                                SEXP name = nullptr;
                                if (auto ld = LdVar::Cast(i))
                                    name = ld->varName;
                                if (auto ldfun = LdFun::Cast(i))
                                    name = ldfun->varName;
                                if (name &&
                                    SafeBuiltinsList::assumeStableInBaseEnv(
                                        name)) {
                                    auto value = SYMVALUE(name);
                                    assert(Rf_findVar(name, env->rho) == value);
                                    if (TYPEOF(value) == PROMSXP)
                                        value = PRVALUE(value);
                                    if (value != R_UnboundValue)
                                        if (LdVar::Cast(i) ||
                                            TYPEOF(value) == BUILTINSXP ||
                                            TYPEOF(value) == SPECIALSXP ||
                                            TYPEOF(value) == CLOSXP) {
                                            i->replaceUsesWith(
                                                cmp.module->c(value));
                                            next = bb->remove(ip);
                                            changed = true;
                                            return;
                                        }
                                }
                            }
                        }
                    }
                }
                if (changed)
                    anyChange = true;
            });

            // TODO move this to a pass where it fits...
            if (auto b = CallBuiltin::Cast(i)) {
                bool noObjects = true;
                bool unsafe = false;
                i->eachArg([&](Value* v) {
                    if (v != i->env()) {
                        if (v->cFollowCastsAndForce()->type.maybeObj())
                            noObjects = false;
                        if (v->type.isA(RType::expandedDots))
                            unsafe = true;
                    }
                });

                if (!unsafe && noObjects &&
                    SafeBuiltinsList::nonObject(b->builtinId)) {
                    std::vector<Value*> args;
                    i->eachArg([&](Value* v) {
                        if (v != i->env()) {
                            auto mk = MkArg::Cast(v);
                            if (mk && mk->isEager())
                                args.push_back(mk->eagerArg());
                            else
                                args.push_back(v);
                        }
                    });
                    auto safe = BuiltinCallFactory::New(
                        b->env(), b->builtinSexp, args, b->srcIdx);
                    assert(!b->type.maybePromiseWrapped() ||
                           safe->type.maybePromiseWrapped());
                    b->replaceUsesWith(safe);
                    bb->replace(ip, safe);
                    replacedValue[b] = safe;
                    changed = true;
                }
            }

            ip = next;
        }
    });
    // Scope resolution can sometimes generate dead phis, so we remove them
    // here, before they cause errors in later compiler passes. (Sometimes, the
    // verifier will even catch these errors, but then segfault when trying to
    // print the offending instruction.)
    //
    // This happens because we use the standard phi placement algorithm but in
    // a different setting. The algorithm assumes it is examining the whole
    // program and inserting phis for all writes. In our setting, we want to
    // connect LdVars with StVars, replacing the LdVar with a phi if multiple
    // stores reach that load.
    //
    // However, if there is no LdVar to replace, that means the phi is dead;
    // none of the successor instructions needs that value. The problem is when
    // dead phis are malformed.
    BBTransform::removeDeadInstrs(code, 1);

    return anyChange;
}

} // namespace pir
} // namespace rir
