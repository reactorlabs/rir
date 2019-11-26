#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/type_test.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "compiler/util/safe_builtins_list.h"
#include "interpreter/builtins.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void ElideEnvSpec::apply(RirCompiler&, ClosureVersion* function,
                         LogStream& log) const {

    constexpr bool debug = false;
    AvailableCheckpoints checkpoint(function, function, log);
    DominanceGraph dom(function);

    auto envOnlyForObj = [&](Instruction* i) {
        if (i->envOnlyForObj())
            return true;
        if (auto blt = CallBuiltin::Cast(i))
            if (SafeBuiltinsList::nonObject(blt->blt))
                return true;
        return false;
    };

    // Speculatively elide environments on instructions that only require them
    // in case any of the arguments is an object
    VisitorNoDeoptBranch::run(function->entry, [&](BB* bb) {
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
                        Instruction::TypeFeedback seen;
                        if (argi)
                            seen = argi->typeFeedback;
                        if (auto j = Instruction::Cast(arg->followCasts()))
                            if (seen.type.isVoid() ||
                                (!j->typeFeedback.type.isVoid() &&
                                 !seen.type.isA(j->typeFeedback.type)))
                                seen = j->typeFeedback;
                        if (auto j =
                                Instruction::Cast(arg->followCastsAndForce()))
                            if (seen.type.isVoid() ||
                                (!j->typeFeedback.type.isVoid() &&
                                 !seen.type.isA(j->typeFeedback.type)))
                                seen = j->typeFeedback;
                        if (seen.type.isVoid())
                            seen.type = arg->type.notObject();

                        TypeTest::Create(
                            arg, seen,
                            [&](TypeTest::Info info) {
                                BBTransform::insertAssume(
                                    info.test, cp, bb, ip, info.expectation,
                                    info.srcCode, info.origin);

                                if (argi) {
                                    auto cast = new CastType(
                                        argi, CastType::Downcast,
                                        PirType::val(), info.result);
                                    cast->effects.set(Effect::DependsOnAssume);
                                    ip = bb->insert(ip, cast);
                                    ip++;
                                    argi->replaceDominatedUses(cast);
                                }
                            },
                            [&]() { successful = false; });
                    });
                    if (successful) {
                        if (auto blt = CallBuiltin::Cast(i)) {
                            std::vector<Value*> args;
                            blt->eachCallArg(
                                [&](Value* v) { args.push_back(v); });
                            auto safe = new CallSafeBuiltin(blt->blt, args,
                                                            blt->srcIdx);
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
                    }
                }
            }
            ip = next;
        }
    });

    SmallSet<Value*> bannedEnvs;
    SmallSet<Value*> materializableStubs;

    // If we only see these (and call instructions) then we stub an environment,
    // since it can only be accessed reflectively.
    static constexpr auto allowed = {
        Tag::Force,      Tag::PushContext, Tag::LdVar,
        Tag::StVar,      Tag::StVarSuper,  Tag::Call,
        Tag::FrameState, Tag::CallBuiltin, Tag::StaticCall};
    // Those do not materialize the stub in any case
    static constexpr auto dontMaterialize = {Tag::PushContext, Tag::LdVar,
                                             Tag::StVar,       Tag::StVarSuper,
                                             Tag::FrameState,  Tag::IsEnvStub};
    VisitorNoDeoptBranch::run(function->entry, [&](Instruction* i) {
        i->eachArg([&](Value* val) {
            if (auto m = MkEnv::Cast(val)) {
                if (m->stub && !materializableStubs.count(m)) {
                    if (std::find(dontMaterialize.begin(),
                                  dontMaterialize.end(),
                                  i->tag) == dontMaterialize.end())
                        materializableStubs.insert(m);
                }
                if (!m->stub && !bannedEnvs.count(m)) {
                    auto bt = CallBuiltin::Cast(i);
                    if (std::find(allowed.begin(), allowed.end(), i->tag) ==
                            allowed.end() ||
                        !i->hasEnv() || i->env() != m ||
                        (bt && !supportsFastBuiltinCall(bt->blt))) {
                        if (debug) {
                            std::cout << "Environment:";
                            m->print(std::cout);
                            std::cout << " blocked by ";
                            i->print(std::cout);
                            std::cout << "\n";
                        }
                        bannedEnvs.insert(m);
                        return;
                    }
                }
            }
        });
    });

    std::unordered_map<Instruction*, std::pair<Checkpoint*, MkEnv*>> checks;
    std::unordered_map<MkEnv*, SmallSet<Instruction*>> needsMaterialization;
    std::unordered_map<MkEnv*, SmallSet<SEXP>> additionalEntries;
    Visitor::run(function->entry, [&](Instruction* i) {
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
                if (mk->stub || bannedEnvs.count(mk))
                    return;
                if (i->bb()->isDeopt()) {
                    needsMaterialization[mk].insert(i);
                } else {
                    // We can only stub an environment if all uses have a
                    // checkpoint available after every use.
                    if (auto cp = checkpoint.next(i, mk, dom))
                        checks[i] = std::pair<Checkpoint*, MkEnv*>(cp, mk);
                    else {
                        if (debug) {
                            std::cout << "Environment:";
                            mk->print(std::cout);
                            std::cout << " blocked by missing checkpoint at ";
                            i->print(std::cout);
                            std::cout << "\n";
                        }
                        bannedEnvs.insert(mk);
                    }
                }
            }
        }
    });

    std::unordered_map<BB*, SmallSet<MkEnv*>> materialized;
    VisitorNoDeoptBranch::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (checks.count(i) && !bannedEnvs.count(i->env())) {
                auto env = checks[i].second;
                auto cp = checks[i].first;
                auto condition = new IsEnvStub(env);
                BBTransform::insertAssume(condition, cp, true);
                assert(cp->bb()->trueBranch() != bb);
            }

            if (auto env = MkEnv::Cast(i)) {
                if (!env->stub && !bannedEnvs.count(i) && !bb->isDeopt()) {
                    if (debug) {
                        std::cout << "stubbing ";
                        env->print(std::cout);
                        std::cout << "\n";
                    }
                    env->stub = true;
                    materializableStubs.insert(env);
                    for (auto n : additionalEntries[env]) {
                        env->varName.push_back(n);
                        env->pushArg(UnboundValue::instance(), PirType::any());
                    }
                    // After eliding an env we must ensure to add a
                    // materialization before every usage in deopt branches
                    for (auto mkArg : needsMaterialization[env]) {
                        auto targetBB = mkArg->bb();
                        if (!materialized.count(targetBB) ||
                            !materialized[targetBB].includes(env)) {
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
    Visitor::run(function->entry, [&](Instruction* i) {
        if (std::find(needStubbed.begin(), needStubbed.end(), i->tag) !=
                needStubbed.end() &&
            i->hasEnv() && !IsEnvStub::Cast(i) &&
            !i->effects.contains(Effect::DependsOnAssume) &&
            MkEnv::Cast(i->env()) && MkEnv::Cast(i->env())->stub) {
            i->effects.set(Effect::DependsOnAssume);
        }
        if (auto is = IsEnvStub::Cast(i)) {
            if (!materializableStubs.count(i->env())) {
                is->replaceUsesWith(True::instance());
                is->effects.reset();
            }
        }
    });
}
} // namespace pir
} // namespace rir
