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

    AvailableCheckpoints checkpoint(function, log);

    auto nonObjectArgs = [&](Instruction* i) {
        auto answer = true;
        i->eachArg([&](Value* arg) {
            if (!answer)
                return;

            if (i->env() == arg)
                return;
            if (!arg->followCastsAndForce()->type.maybeObj())
                return;
            if (arg->type.maybePromiseWrapped()) {
                answer = false;
                return;
            }

            auto fb = PirType::bottom();
            if (auto j = Instruction::Cast(arg))
                fb = j->typeFeedback.type;
            if (fb.isVoid()) {
                if (auto j = Instruction::Cast(arg->followCastsAndForce()))
                    fb = j->typeFeedback.type;
            }

            if (fb.isVoid() || fb.maybeObj())
                answer = false;
        });
        return answer;
    };

    SmallSet<Value*> bannedEnvs;

    // If we only see these (and call instructions) then we stub an environment,
    // since it can only be accessed reflectively.
    static std::unordered_set<Tag> allowed{Tag::Force,       Tag::FrameState,
                                           Tag::PushContext, Tag::LdVar,
                                           Tag::StVar,       Tag::StVarSuper};

    VisitorNoDeoptBranch::run(function->entry, [&](Instruction* i) {
        i->eachArg([&](Value* val) {
            if (auto m = MkEnv::Cast(val)) {
                if (!m->stub && !bannedEnvs.count(m)) {
                    // Prevent us from leaking stub envs
                    if (!i->hasEnv() || i->env() != m)
                        bannedEnvs.insert(m);
                    if (CallInstruction::CastCall(i)) {
                        // Call builtin materializes env right away, so no point in
                        // stubbing, unless we have a fastcase.
                        if (auto bt = CallBuiltin::Cast(i))
                            if (!supportsFastBuiltinCall(bt->blt))
                                bannedEnvs.insert(m);
                        return;
                    }
                    if (!allowed.count(i->tag)){
                        bannedEnvs.insert(m);
                    }
                }
            }
        });
    });

    std::unordered_map<Instruction*, std::pair<Checkpoint*, MkEnv*>> checks;
    std::unordered_map<MkEnv*, SmallSet<BB*>> moveToDeopt;
    Visitor::run(function->entry, [&](Instruction* i) {
        if (i->hasEnv()) {
            if (FrameState::Cast(i) || StVar::Cast(i) || LdVar::Cast(i) ||
                StVarSuper::Cast(i))
                return;
            if (auto mk = MkEnv::Cast(i->env())) {
                if (mk->stub || bannedEnvs.count(mk))
                    return;
                // We can only stub an environment if all uses have a checkpoint
                // available after every use or the reamining uses are in deopt branches and
                // then we can safely copy the mkEnv there.
                if (i->bb()->isDeopt()) {
                    moveToDeopt[mk].insert(i->bb());
                } else {
                    if (auto cp = checkpoint.next(i))
                        checks[i] = std::pair<Checkpoint*, MkEnv*>(cp, mk);
                    else {
                        bannedEnvs.insert(mk);
                    }
                }
            }
        }
    });

    auto envOnlyForObj = [&](Instruction* i) {
        if (i->envOnlyForObj())
            return true;
        // Subassign is not mark as envOnlyForObject because the environment is
        // also needed to track error messages.
        if (Subassign1_1D::Cast(i) || Subassign2_1D::Cast(i)) {
            return true;
        }
        if (auto blt = CallBuiltin::Cast(i))
            if (SafeBuiltinsList::nonObject(blt->blt))
                return true;
        return false;
    };

    VisitorNoDeoptBranch::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (checks.count(i)) {
                // Speculatively elide instructions that only require them
                // in case they access promises reflectively
                if (!bannedEnvs.count(i->env())) {
                    auto env = checks[i].second;
                    if (!moveToDeopt[env].count(checks[i].first->bb()->falseBranch())) {
                        if (!env->stub) {
                            for (auto targetBBs : moveToDeopt[env]) {
                                auto newEnvInstr = env->clone();
                                targetBBs->insert(targetBBs->begin(), newEnvInstr);
                                env->replaceUsesIn(newEnvInstr, targetBBs);
                            }
                            env->stub = true;
                        }    
                        auto cp = checks[i].first;
                        auto condition = new IsEnvStub(env);
                        BBTransform::insertAssume(condition, cp, true);
                        assert(cp->bb()->trueBranch() != bb);
                    }
                }
            } else if (i->hasEnv()) {
                // Speculatively elide environments on instructions in which
                // all operators are primitive values
                auto cp = checkpoint.at(i);
                if (cp && envOnlyForObj(i) && nonObjectArgs(i)) {
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
                        if (seen.type.isVoid()) {
                            if (auto j = Instruction::Cast(
                                    arg->followCastsAndForce()))
                                seen = j->typeFeedback;
                        }
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
            }

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
