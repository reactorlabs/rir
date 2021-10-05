#include "R/r.h"
#include "compiler/analysis/available_checkpoints.h"
#include "compiler/analysis/cfg.h"
#include "compiler/analysis/context_stack.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/env_stub_info.h"
#include "compiler/util/safe_builtins_list.h"
#include "compiler/util/visitor.h"
#include "interpreter/builtins.h"
#include "pass_definitions.h"
#include "type_test.h"

#include <unordered_map>

namespace rir {
namespace pir {

bool ElideEnvSpec::apply(Compiler&, ClosureVersion* cls, Code* code,
                         LogStream& log, size_t iteration) const {

    constexpr bool debug = false;
    AvailableCheckpoints checkpoint(cls, code, log);
    ContextStack cs(cls, code, log);
    DominanceGraph dom(code);

    auto envOnlyForObj = [&](Instruction* i) {
        if (i->envOnlyForObj())
            return true;
        if (auto blt = CallBuiltin::Cast(i)) {
            bool dots = false;
            blt->eachCallArg([&](Value* v) {
                if (v->type.maybe(RType::expandedDots))
                    dots = true;
            });
            if (dots)
                return false;
            if (SafeBuiltinsList::nonObject(blt->builtinSexp))
                return true;
        }
        return false;
    };

    bool anyChange = false;
    // Speculatively elide environments on instructions that only require them
    // in case any of the arguments is an object
    VisitorNoDeoptBranch::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;
            if (i->hasEnv()) {
                // Speculatively elide environments on instructions in which
                // all operators are primitive values
                auto cp = checkpoint.at(i);
                if (cp && envOnlyForObj(i) && i->nonObjectArgs()) {
                    bool successful = true;
                    i->eachArg([&](Value* arg) {
                        if (arg == i->env() || !arg->type.maybeObj()) {
                            return;
                        }
                        auto argi = Instruction::Cast(arg);
                        assert(!arg->type.maybePromiseWrapped());
                        TypeFeedback seen;
                        if (argi)
                            seen = argi->typeFeedback();
                        if (auto j = Instruction::Cast(arg->followCasts()))
                            if (seen.type.isVoid() ||
                                (!j->typeFeedback().type.isVoid() &&
                                 !seen.type.isA(j->typeFeedback().type)))
                                seen = j->typeFeedback();
                        if (auto j =
                                Instruction::Cast(arg->followCastsAndForce()))
                            if (seen.type.isVoid() ||
                                (!j->typeFeedback().type.isVoid() &&
                                 !seen.type.isA(j->typeFeedback().type)))
                                seen = j->typeFeedback();

                        auto required = arg->type.notObject();
                        auto suggested = required;
                        // so far the only instruction where we can do more
                        // opts if we show that the vector has no attribs
                        if (auto e = Extract1_1D::Cast(i))
                            if (arg == e->vec())
                                suggested = required.noAttribsOrObject();

                        TypeTest::Create(
                            arg, seen, suggested, required,
                            [&](TypeTest::Info info) {
                                BBTransform::insertAssume(
                                    info.test, info.expectation, cp,
                                    info.feedbackOrigin, DeoptReason::Typecheck,
                                    bb, ip);

                                if (argi) {
                                    auto cast = new CastType(
                                        argi, CastType::Downcast,
                                        PirType::val(), info.result);
                                    cast->effects.set(Effect::DependsOnAssume);
                                    ip = bb->insert(ip, cast);
                                    ip++;
                                    argi->replaceDominatedUses(cast, dom);
                                }
                            },
                            [&]() { successful = false; });
                    });
                    if (successful) {
                        anyChange = true;
                        if (auto blt = CallBuiltin::Cast(i)) {
                            std::vector<Value*> args;
                            blt->eachCallArg(
                                [&](Value* v) { args.push_back(v); });
                            auto safe = new CallSafeBuiltin(blt->builtinSexp,
                                                            args, blt->srcIdx);
                            blt->replaceUsesWith(safe);
                            bb->replace(ip, safe);
                        } else {
                            i->elideEnv();
                        }
                        i->updateTypeAndEffects();
                    }
                    next = ip + 1;
                }
                // We do this in cleanup. Repeating it here increase the chances
                // to apply the following elide environment pass because it
                // reduces the number of forces attached to an environment
                else if (auto force = Force::Cast(i)) {
                    Value* arg = force->input();
                    // Missing args produce error.
                    if (!arg->type.maybePromiseWrapped() &&
                        !arg->type.maybeMissing()) {
                        force->replaceUsesWith(arg);
                        next = bb->remove(ip);
                        anyChange = true;
                    }
                }
            }
            ip = next;
        }
    });

    SmallSet<Value*> bannedEnvs;
    SmallSet<Value*> materializableStubs;

    VisitorNoDeoptBranch::run(code->entry, [&](Instruction* i) {
        i->eachArg([&](Value* val) {
            if (auto m = MkEnv::Cast(val)) {
                auto stub = EnvStubInfo::of(i->tag);
                if (m->stub && !materializableStubs.count(m)) {
                    if (!stub.allowedNotMaterializing)
                        materializableStubs.insert(m);
                }
                if (m->neverStub && !bannedEnvs.count(m)) {
                    bannedEnvs.insert(m);
                }
                if (!m->stub && !bannedEnvs.count(m)) {
                    auto bt = CallBuiltin::Cast(i);
                    if (!stub.allowed || iteration < stub.priority ||
                        !i->hasEnv() || i->env() != m ||
                        (bt && !supportsFastBuiltinCall(bt->builtinSexp,
                                                        bt->nCallArgs()))) {
                        bool ok = false;
                        if (auto mkarg = MkArg::Cast(i)) {
                            ok = Visitor::check(
                                mkarg->prom()->entry, [&](Instruction* j) {
                                    return EnvStubInfo::of(j->tag)
                                        .allowedNotMaterializing;
                                });
                        } else if (auto mk = MkEnv::Cast(i)) {
                            ok = mk->stub;
                        }
                        if (!ok) {
                            if (debug) {
                                std::cout << "Environment '";
                                m->print(std::cout);
                                std::cout << "' blocked by '";
                                i->print(std::cout);
                                std::cout << "'\n";
                            }
                            bannedEnvs.insert(m);
                            return;
                        }
                    }
                }
            }
        });
    });

    std::unordered_map<Instruction*,
                       std::unordered_map<Checkpoint*, SmallSet<MkEnv*>>>
        checks;
    std::unordered_map<MkEnv*, SmallSet<Instruction*>> needsMaterialization;
    std::unordered_map<MkEnv*, SmallSet<SEXP>> additionalEntries;
    Visitor::run(code->entry, [&](Instruction* i) {
        // We need to be careful with inlined contexts. Stubs, including the
        // ones that leak into a context, need to be checked after each
        // instruction that can cause them to be materialized, as well as after
        // PopContext (for non-local jumps from inlined code).
        if (!i->bb()->isDeopt() &&
            (i->effects.contains(Effect::ExecuteCode) || PopContext::Cast(i))) {
            cs.before(i).eachLeakedEnvRev([&](MkEnv* mk) {
                if (!mk->stub && !bannedEnvs.count(mk)) {
                    if (auto cp = checkpoint.next(i, mk, dom)) {
                        checks[i][cp].insert(mk);
                    } else {
                        if (debug) {
                            std::cout << "Environment (in context) '";
                            mk->print(std::cout);
                            std::cout << "' blocked by missing checkpoint at '";
                            i->print(std::cout);
                            std::cout << "'\n";
                        }
                        bannedEnvs.insert(mk);
                    }
                }
            });
        }
        if (i->hasEnv()) {
            if (auto st = StVar::Cast(i)) {
                if (!bannedEnvs.count(i->env())) {
                    if (auto mk = MkEnv::Cast(i->env())) {
                        if (!mk->stub && !mk->contains(st->varName)) {
                            additionalEntries[mk].insert(st->varName);
                        }
                    }
                }
            }
            if (FrameState::Cast(i) || StVar::Cast(i) || LdVar::Cast(i) ||
                StVarSuper::Cast(i) || PushContext::Cast(i))
                return;
            if (auto mk = MkEnv::Cast(i->env())) {
                if (!mk->stub && !bannedEnvs.count(mk)) {
                    if (i->bb()->isDeopt()) {
                        needsMaterialization[mk].insert(i);
                    } else {
                        // We can only stub an environment if we have a
                        // checkpoint available after every use.
                        if (auto cp = checkpoint.next(i, mk, dom)) {
                            checks[i][cp].insert(mk);
                        } else {
                            if (debug) {
                                std::cout << "Environment '";
                                mk->print(std::cout);
                                std::cout
                                    << "' blocked by missing checkpoint at '";
                                i->print(std::cout);
                                std::cout << "'\n";
                            }
                            bannedEnvs.insert(mk);
                        }
                    }
                }
            }
        }
    });

    std::unordered_map<BB*, SmallSet<MkEnv*>> materialized;
    VisitorNoDeoptBranch::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (checks.count(i)) {
                for (auto check : checks[i]) {
                    auto cp = check.first;
                    for (auto env : check.second) {
                        if (!bannedEnvs.count(env)) {
                            auto condition = new IsEnvStub(env);
                            BBTransform::insertAssume(
                                condition, true, cp,
                                env->typeFeedback().feedbackOrigin,
                                DeoptReason::EnvStubMaterialized);
                            anyChange = true;
                            assert(cp->bb()->trueBranch() != bb);
                        }
                    }
                }
            }

            if (auto env = MkEnv::Cast(i)) {
                if (!env->stub && !bannedEnvs.count(i) && !bb->isDeopt()) {
                    if (debug) {
                        std::cout << "stubbing '";
                        env->print(std::cout);
                        std::cout << "'\n";
                    }
                    env->stub = true;
                    materializableStubs.insert(env);
                    for (auto n : additionalEntries[env]) {
                        env->varName.push_back(n);
                        env->pushArg(UnboundValue::instance(), PirType::any());
                        anyChange = true;
                    }
                    // After eliding an env we must ensure to add a
                    // materialization before every usage in deopt branches
                    for (auto mkArg : needsMaterialization[env]) {
                        auto targetBB = mkArg->bb();
                        if (!materialized.count(targetBB) ||
                            !materialized[targetBB].includes(env)) {
                            anyChange = true;
                            Instruction* materialize = new MaterializeEnv(env);
                            env->replaceUsesIn(materialize, targetBB);
                            targetBB->insert(targetBB->begin(), materialize);
                            materialized[targetBB].insert(env);
                        }
                    }
                }
            }

            ip = next;
        }
    });

    // Those absolutely depend on *not* getting the materialized version
    constexpr static auto needStubbed = {Tag::LdVar, Tag::StVar};
    Visitor::run(code->entry, [&](Instruction* i) {
        if (std::find(needStubbed.begin(), needStubbed.end(), i->tag) !=
                needStubbed.end() &&
            i->hasEnv() && !IsEnvStub::Cast(i) &&
            !i->effects.contains(Effect::DependsOnAssume) &&
            MkEnv::Cast(i->env()) && MkEnv::Cast(i->env())->stub) {
            i->effects.set(Effect::DependsOnAssume);
            anyChange = true;
        }
        if (auto is = IsEnvStub::Cast(i)) {
            if (!materializableStubs.count(i->env())) {
                is->replaceUsesWith(True::instance());
                is->effects.reset();
                anyChange = true;
            }
        }
        if (auto mk = MkArg::Cast(i)) {
            if (materializableStubs.count(mk->env())) {
                if (auto e = mk->prom()->env()) {
                    e->stub = true;
                    anyChange = true;
                }
            }
        }
    });

    return anyChange;
}
} // namespace pir
} // namespace rir
