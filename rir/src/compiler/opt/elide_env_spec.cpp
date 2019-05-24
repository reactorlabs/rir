#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "compiler/util/safe_builtins_list.h"
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
            if (i->env() == arg)
                return;
            if (arg->type.maybeObj() &&
                (arg->typeFeedback.isVoid() || arg->typeFeedback.maybeObj()))
                answer = false;
        });
        return answer;
    };

    std::unordered_set<Value*> bannedEnvs;

    // If we only see these (and call instructions) then we stub an environment,
    // since it can only be accessed reflectively.
    static std::unordered_set<Tag> allowed{Tag::Force, Tag::FrameState,
                                           Tag::PushContext, Tag::LdVar};

    Visitor::run(function->entry, [&](Instruction* i) {
        if (i->hasEnv()) {
            if (auto mk = MkEnv::Cast(i->env())) {
                if (mk->stub)
                    return;
                // StArg is not implemented for stub envs
                if (auto st = StVar::Cast(i))
                    if (!st->isStArg)
                        return;
                if (allowed.count(i->tag))
                    return;
                if (CallInstruction::CastCall(i)) {
                    if (auto bt = CallBuiltin::Cast(i)) {
                        if (SafeBuiltinsList::forInline(bt->builtinId)) {
                            return;
                        }
                        // reflective builtins will trigger deopt, so let's not
                        // stub those environemtns
                    } else {
                        return;
                    }
                }
                bannedEnvs.insert(mk);
            }
        }
    });

    std::unordered_map<Instruction*, std::pair<Checkpoint*, MkEnv*>> checks;
    Visitor::run(function->entry, [&](Instruction* i) {
        if (i->hasEnv()) {
            if (FrameState::Cast(i) || StVar::Cast(i) || LdVar::Cast(i))
                return;
            if (auto mk = MkEnv::Cast(i->env())) {
                if (bannedEnvs.count(mk))
                    return;
                // We can only stub an environment if all uses have a checkpoint
                // available after every use.
                if (auto cp = checkpoint.next(i)) {
                    checks[i] = std::pair<Checkpoint*, MkEnv*>(cp, mk);
                } else {
                    bannedEnvs.insert(mk);
                }
            }
        }
    });

    VisitorNoDeoptBranch::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (i->hasEnv()) {
                // Speculatively elide environments on instructions in which
                // all operators are primitive values
                if (checkpoint.at(i) && i->envOnlyForObj() &&
                    nonObjectArgs(i)) {
                    i->elideEnv();
                    i->eachArg([&](Value* arg) {
                        if (arg != i->env())
                            if (arg->type.maybeObj()) {
                                auto condition = new IsObject(arg);
                                ip = bb->insert(ip, condition);
                                ip++;
                                ip = bb->insert(
                                    ip,
                                    (new Assume(condition, checkpoint.at(i)))
                                        ->Not());
                                ip++;
                            }
                    });
                    next = ip + 1;
                    i->type.setNotObject();
                    i->effects.reset(Effect::Reflection);
                    i->type = i->type.forced();
                } else if (checks.count(i)) {
                    // Speculatively elide instructions that only require them
                    // in
                    // case they access promises reflectively
                    if (!bannedEnvs.count(i->env())) {
                        auto env = checks[i].second;
                        env->stub = true;
                        auto cp = checks[i].first;
                        auto condition = new IsEnvStub(env);
                        BBTransform::insertAssume(condition, cp, true);
                    }
                }
            }
            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
