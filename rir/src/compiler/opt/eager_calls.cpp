#include "../analysis/available_checkpoints.h"
#include "../analysis/query.h"
#include "../pir/pir_impl.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/cfg.h"
#include "compiler/compiler.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

bool EagerCalls::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                       LogStream& log, size_t) const {
    AvailableCheckpoints checkpoint(cls, code, log);

    struct Speculation {
        SEXP builtin;
        Checkpoint* cp;
        FeedbackOrigin origin;
        Speculation() {}
        Speculation(SEXP builtin, Checkpoint* cp, const FeedbackOrigin& origin)
            : builtin(builtin), cp(cp), origin(origin) {
            assert(origin.pc());
        }
    };

    auto replaceLdFunBuiltinWithDeopt = [&](BB* bb, BB::Instrs::iterator ip,
                                            const Speculation& speculation,
                                            LdFun* ldfun) {
        assert(LdFun::Cast(*ip));
        assert(speculation.cp);

        // skip ldfun
        ++ip;

        auto expected = cmp.module->c(speculation.builtin);
        Instruction* given = ldfun;

        given->replaceUsesWith(expected);
        // We use ldvar instead of ldfun for the guard. The reason is that ldfun
        // can force promises, which is a pain for our optimizer to deal with.
        // Note that ldvar is conservative. If we find a non-function binding
        // with the same name, we will deopt unneccessarily. In the case of `c`
        // this is guaranteed to cause problems, since many variables are called
        // "c". Therefore we keep the ldfun in this case, unless we already know
        // that the function "c" comes from the global env.
        // TODO: Implement this with a dependency on the binding cell instead of
        // an eager check.
        auto funEnv = Env::Cast(ldfun->env());
        if (ldfun->varName != symbol::c ||
            (funEnv && funEnv->rho == R_GlobalEnv)) {
            given = new LdVar(ldfun->varName, ldfun->env());
            ip = bb->insert(ip, given);
            ++ip;
        }

        auto test = new Identical(given, expected, PirType::any());
        ip = bb->insert(ip, test);
        ++ip;

        auto assume = new Assume(
            test, speculation.cp,
            DeoptReason(speculation.origin, DeoptReason::CallTarget));
        ip = bb->insert(ip, assume);
        ++ip;

        return ip;
    };

    bool anyChange = false;
    auto replaceCallWithCallBuiltin = [&](BB* bb, BB::Instrs::iterator ip,
                                          Call* call, SEXP builtin,
                                          bool dependsOnAssume) {
        std::vector<Value*> args;
        call->eachCallArg([&](Value* a) {
            if (auto mk = MkArg::Cast(a)) {
                if (mk->isEager()) {
                    args.push_back(mk->eagerArg());
                } else {
                    auto asArg = new CastType(mk, CastType::Upcast, RType::prom,
                                              PirType::valOrLazy());
                    auto forced =
                        new Force(asArg, call->env(), Tombstone::framestate());
                    if (dependsOnAssume)
                        forced->effects.set(Effect::DependsOnAssume);
                    ip = bb->insert(ip, forced);
                    ip = bb->insert(ip, asArg);
                    args.push_back(forced);
                    ip += 2;
                }
            } else if (a->type.maybePromiseWrapped()) {
                auto forced =
                    new Force(a, call->env(), Tombstone::framestate());
                ip = bb->insert(ip, forced);
                if (dependsOnAssume)
                    forced->effects.set(Effect::DependsOnAssume);
                args.push_back(*ip);
                ++ip;
            } else {
                args.push_back(a);
            }
        });
        anyChange = true;
        auto bt =
            BuiltinCallFactory::New(call->env(), builtin, args, call->srcIdx);
        if (dependsOnAssume)
            bt->effects.set(Effect::DependsOnAssume);
        call->replaceUsesWith(bt);
        bb->replace(ip, bt);
        return ip;
    };

    // Search for calls that likely point to a builtin.
    std::unordered_map<LdFun*, Speculation> needsGuard;

    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            switch (i->tag) {
#define V(instr)                                                               \
    case Tag::instr:                                                           \
        if (!i->arg(0).val()->type.maybeObj()) {                               \
            i->eachArg([&](InstrArg& arg) {                                    \
                if (arg.val()->type.maybePromiseWrapped()) {                   \
                    ip = bb->insert(ip, new Force(arg.val(), i->env(),         \
                                                  Tombstone::framestate()));   \
                    arg.val() = *ip;                                           \
                    ip++;                                                      \
                }                                                              \
            });                                                                \
        }                                                                      \
        break;
                VECTOR_RW_INSTRUCTIONS(V);
#undef V
            default: {}
            }

            if (auto call = Call::Cast(*ip)) {
                bool dots = false;
                call->eachCallArg([&](Value* v) {
                    if (v->type.maybe(RType::expandedDots))
                        dots = true;
                });
                if (!dots) {
                    if (auto ldcn = Const::Cast(call->cls())) {
                        if (TYPEOF(ldcn->c()) == BUILTINSXP) {
                            ip = replaceCallWithCallBuiltin(
                                bb, ip, call, ldcn->c(),
                                call->effects.includes(
                                    Effect::DependsOnAssume));
                        }
                    } else if (auto ldfun = LdFun::Cast(call->cls())) {
                        if (ldfun->hint() && ldfun->hintHasStableEnv) {
                            auto kind = TYPEOF(ldfun->hint());
                            // We also speculate on calls to CLOSXPs, these will
                            // be picked up by MatchArgs opt pass and turned
                            // into a static call. TODO, for inner functions we
                            // need another kind of guard.
                            if (kind == BUILTINSXP || kind == CLOSXP) {
                                // We can only speculate if we have a checkpoint
                                // at the ldfun position, since we want to deopt
                                // before forcing arguments.
                                if (auto cp = checkpoint.at(ldfun)) {
                                    if (kind == BUILTINSXP) {
                                        ip = replaceCallWithCallBuiltin(
                                            bb, ip, call, ldfun->hint(), true);
                                    }
                                    needsGuard[ldfun] = {ldfun->hint(), cp,
                                                         ldfun->hintOrigin()};
                                }
                            }
                        } else {
                            // Only speculate if we don't have a static guess
                            if (!ldfun->guessedBinding()) {
                                auto env =
                                    Env::Cast(cls->owner()->closureEnv());
                                if (env != Env::notClosed() && env->rho) {
                                    auto name = ldfun->varName;
                                    auto builtin = Rf_findVar(name, env->rho);
                                    if (TYPEOF(builtin) == PROMSXP)
                                        builtin = PRVALUE(builtin);

                                    auto kind = TYPEOF(builtin);
                                    if (kind == BUILTINSXP || kind == CLOSXP) {
                                        auto rho = env->rho;
                                        bool inBase = false;
                                        if (rho == R_BaseEnv ||
                                            rho == R_BaseNamespace) {
                                            inBase =
                                                SYMVALUE(name) == builtin &&
                                                SafeBuiltinsList::
                                                    assumeStableInBaseEnv(name);
                                        }

                                        auto cp = checkpoint.at(ldfun);

                                        if (inBase || cp) {
                                            if (kind == BUILTINSXP) {
                                                ip = replaceCallWithCallBuiltin(
                                                    bb, ip, call, builtin,
                                                    !inBase);
                                            }
                                            if (!inBase &&
                                                ldfun->typeFeedback()
                                                    .feedbackOrigin.pc())
                                                needsGuard[ldfun] = {
                                                    builtin, cp,
                                                    ldfun->typeFeedback()
                                                        .feedbackOrigin};
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            ip++;
        }
    });

    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;

            // Insert the actual guards needed for the above transformation
            if (auto ldfun = LdFun::Cast(*ip)) {
                auto r = needsGuard.find(ldfun);
                if (r != needsGuard.end()) {
                    ip = replaceLdFunBuiltinWithDeopt(bb, ip, r->second, ldfun);
                    needsGuard.erase(r);
                    continue;
                }
            }

            // Remove promises from non-obj vector operations
            switch ((*ip)->tag) {
#define V(T)                                                                   \
    case Tag::T: {                                                             \
        auto i = T::Cast(*ip);                                                 \
        if (!i->effects.contains(Effect::Reflection)) {                        \
            (*ip)->eachArg([&](InstrArg& a) {                                  \
                if (!a.val()->type.maybePromiseWrapped()) {                    \
                    if (auto mk = MkArg::Cast(a.val())) {                      \
                        if (mk->isEager())                                     \
                            a.val() = mk->eagerArg();                          \
                    }                                                          \
                }                                                              \
            });                                                                \
        }                                                                      \
    } break;
                VECTOR_RW_INSTRUCTIONS(V)
#undef V
            default: {}
            };

            // Look for static calls, where we statically know that all (or
            // some) arguments are eager. In this case we will compile an
            // special eager version of the function and call this one instead.
            if (auto call = StaticCall::Cast(*ip)) {
                Closure* target = call->cls();
                ClosureVersion* version = call->tryDispatch();

                if (!version || call->nCallArgs() != target->nargs()) {
                    ip = next;
                    continue;
                }

                auto availableAssumptions = call->inferAvailableAssumptions();
                assert(version->context().numMissing() <=
                       availableAssumptions.numMissing());
                target->rirFunction()->clearDisabledAssumptions(
                    availableAssumptions);

                // We picked up more assumptions, let's compile a better
                // version. Maybe we should limit this at some point, to avoid
                // version explosion.
                if (availableAssumptions.isImproving(version)) {
                    auto newVersion = target->cloneWithAssumptions(
                        version, availableAssumptions,
                        [&](ClosureVersion* newCls) {
                            Visitor::run(newCls->entry, [&](Instruction* i) {
                                if (auto f = Force::Cast(i)) {
                                    if (auto a = LdArg::Cast(f->input())) {
                                        if (availableAssumptions.isNonRefl(
                                                a->pos)) {
                                            f->elideEnv();
                                            f->effects.reset(
                                                Effect::DependsOnAssume);
                                            f->effects.reset(
                                                Effect::Reflection);
                                        }
                                        if (availableAssumptions.isEager(
                                                a->pos)) {
                                            f->effects.reset();
                                        }
                                    }
                                }
                            });
                        });
                    call->hint = newVersion;
                    assert(call->tryDispatch() == newVersion);
                    ip = next;
                    continue;
                }

                bool allEager = version->properties.includes(
                    ClosureVersion::Property::IsEager);
                if (!allEager &&
                    version->properties.argumentForceOrder.empty()) {
                    ip = next;
                    continue;
                }

                auto preEval = [&](Value* a, unsigned i) -> MkArg* {
                    auto mk = MkArg::Cast(a);
                    if (!mk)
                        return nullptr;

                    auto safe =
                        Visitor::check(mk->prom()->entry, [&](Instruction* i) {
                            return !i->mayObserveContext() || Deopt::Cast(i);
                        });
                    if (!safe)
                        return nullptr;

                    if (allEager)
                        return mk;
                    unsigned count = 0;
                    for (auto a : version->properties.argumentForceOrder) {
                        // We know that a is forced before i, therefore we are
                        // not in left-to-right order
                        // TODO: support reordering of the evaluation
                        if (count != a)
                            return nullptr;
                        // We found the argument in the list of certainly forced
                        // promises
                        if (a == i)
                            return mk;
                        i++;
                    }
                    return nullptr;
                };

                bool noMissing = true;
                call->eachCallArg([&](Value* v) {
                    if (MissingArg::instance() == v)
                        noMissing = false;
                });
                if (!noMissing) {
                    ip = next;
                    continue;
                }

                // Remember which arguments are already expected to be eager by
                // the callee and ensure that we will evaluate them eagerly
                // below.
                unsigned i = 0;
                Context newAssumptions = availableAssumptions;
                SmallSet<unsigned> eager;
                bool improved = false;
                call->eachCallArg([&](InstrArg& arg) {
                    if (auto mk = preEval(arg.val(), i)) {
                        if (mk->isEager()) {
                            if (!mk->eagerArg()->type.maybeMissing()) {
                                improved = true;
                                arg.val() = mk->eagerArg();
                            }
                        } else {
                            improved = true;
                            auto asArg = new CastType(
                                mk, CastType::Upcast, RType::prom,
                                Query::returnType(mk->prom()).orLazy());
                            auto forced = new Force(asArg, Env::elided(),
                                                    Tombstone::framestate());
                            if (forced->type.maybeMissing()) {
                                auto upd = new MkArg(mk->prom(), forced,
                                                     Env::elided());
                                ip = bb->insert(ip, upd);
                                arg.val() = upd;
                            } else {
                                arg.val() = forced;
                            }
                            ip = bb->insert(ip, forced);
                            ip = bb->insert(ip, asArg);
                            ip += forced->type.maybeMissing() ? 3 : 2;
                        }
                        eager.insert(i);
                        if (!newAssumptions.isEager(i))
                            newAssumptions.setEager(i);
                    }
                    i++;
                });

                if (!improved) {
                    ip = next;
                    continue;
                }
                next = ip + 1;

                // This might fire back, since we don't know if we really have no
                // objects... We should have some profiling. It's still sound, since
                // static_call_ will check the assumptions
                for (size_t i = 0; i < call->nCallArgs(); ++i)
                    if (!newAssumptions.isNotObj(i) &&
                        newAssumptions.isEager(i))
                        newAssumptions.setNotObj(i);
                target->rirFunction()->clearDisabledAssumptions(newAssumptions);

                auto newVersion = target->cloneWithAssumptions(
                    version, newAssumptions, [&](ClosureVersion* newCls) {
                        anyChange = true;
                        Visitor::run(newCls->entry, [&](Instruction* i) {
                            if (auto ld = LdArg::Cast(i)) {
                                if (eager.count(ld->pos)) {
                                    ld->type = PirType::promiseWrappedVal()
                                                   .notObject()
                                                   .notMissing();
                                }
                            }
                        });
                    });
                call->hint = newVersion;
            }
            ip = next;
        }
    });

    // Third step: eagerly evaluate arguments if we know from above that we will
    // call a function that expects them to be eager.
    Visitor::run(code->entry, [&](BB* bb) {
        for (auto ip = bb->begin(); ip != bb->end(); ++ip) {
            if (auto call = StaticCall::Cast(*ip)) {
                auto version = call->tryDispatch();
                if (version && version->properties.includes(
                                   ClosureVersion::Property::NoReflection)) {
                    call->eachCallArg([&](InstrArg& arg) {
                        if (auto mk = MkArg::Cast(arg.val())) {
                            if (mk->isEager() &&
                                !mk->eagerArg()->type.maybeMissing()) {
                                anyChange = true;
                                arg.val() = mk->eagerArg();
                            }
                        }
                    });
                }
            }
        }
    });

    return anyChange;
}
} // namespace pir
} // namespace rir
