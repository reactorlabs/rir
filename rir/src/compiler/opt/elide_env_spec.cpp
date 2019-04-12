#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
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
            if (arg->type.maybeObj() && arg->typeFeedback.maybeObj())
                answer = false;
        });
        return answer;
    };

    std::unordered_set<MkEnv*> stubbedEnvs;
    std::unordered_set<MkEnv*> bannedEnvs;

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (i->hasEnv()) {
                // Speculatively elide environments on instructions in which
                // both operators are primitive values
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
                    i->type = i->type.forced();
                }

                // Speculatively elide envs on forces that only require them in
                // case they access promises reflectively
                if (auto force = Force::Cast(i)) {
                    if (auto environment = MkEnv::Cast(force->env())) {
                        if (auto cp = checkpoint.next(i)) {
                            static std::unordered_set<Tag> forces{
                                Tag::Force, Tag::FrameState, Tag::PushContext,
                                // stvar might lead to deopts, but really in
                                // almost all cases deadStoreRemoval will
                                // remove it later, once we stubbe the env.
                                Tag::StVar};

                            if (cp->bb()->trueBranch() == bb) {
                                bannedEnvs.insert(environment);
                                ip = next;
                                continue;
                            }

                            if (!environment->stub &&
                                !bannedEnvs.count(environment) &&
                                (stubbedEnvs.count(environment) ||
                                 environment->usesAreOnly(function->entry,
                                                          forces))) {

                                stubbedEnvs.insert(environment);
                                environment = MkEnv::Cast(force->env());
                                auto condition = new IsEnvStub(environment);
                                BBTransform::insertAssume(condition, cp, true);
                            }
                        } else {
                            bannedEnvs.insert(environment);
                        }
                    }
                }
            }
            ip = next;
        }

        // Stub out all envs where we managed to guard all forces
        for (auto& e : stubbedEnvs)
            if (!bannedEnvs.count(e))
                e->stub = true;
    });
}
} // namespace pir
} // namespace rir
