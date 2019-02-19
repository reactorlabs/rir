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
                answer = true;
        });
        return answer;
    };

    std::function<Instruction*(Instruction*, BB*, bool)> nextInstr =
        [&](Instruction* from, BB* bb, bool stepFirst) {
            auto i = from;

            if (stepFirst) {
                auto ip = bb->atPosition(from);
                if (++ip == bb->end()) {
                    auto currentBB = bb->trueBranch();
                    while (currentBB->isEmpty())
                        currentBB = currentBB->trueBranch();
                    ip = currentBB->begin();
                }
                i = *ip;
            }
            if (i->branches()) {
                auto cp = Checkpoint::Cast(i);
                assert(cp);
                auto nextBB = bb->trueBranch();
                while (nextBB->isEmpty())
                    nextBB = nextBB->trueBranch();
                return nextInstr(*(nextBB->begin()), nextBB, false);
            }
            return i;
        };

    Visitor::run(function->entry, [&](BB* bb) {
        std::unordered_map<Value*, Value*> stubFor;
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
                                auto condition = (new TypeTest(arg))->object();
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
                }

                // Speculatively elide envs on forces that only require them in
                // case they access promises reflectively

                if (auto force = Force::Cast(i)) {
                    auto nextInstruction = nextInstr(force, bb, true);
                    if (checkpoint.at(nextInstruction)) {

                        auto environment = MkEnv::Cast(force->env());
                        static std::unordered_set<Tag> forces{Tag::Force,
                                                              Tag::FrameState};

                        if (!environment->stub &&
                            environment->usesAreOnly(function->entry, forces)) {
                            environment = MkEnv::Cast(force->env());

                            if (!stubFor.count(environment)) {
                                auto stub = (MkEnv*)environment->clone();
                                stub->stub = true;
                                stubFor.emplace(environment, stub);
                                environment->bb()->insert(
                                    environment->bb()->atPosition(environment),
                                    stub);

                                next = bb->atPosition(force) + 1;

                                // Clean framestate
                                auto innerIt = bb->falseBranch()->begin();
                                while (innerIt != bb->falseBranch()->end()) {
                                    if (auto fs = FrameState::Cast(*innerIt)) {
                                        fs->env(stub);
                                        break;
                                    }
                                    innerIt++;
                                }
                            }

                            auto stubEnvironment = stubFor.at(environment);
                            force->env(stubEnvironment);
                            ip++;
                            auto condition = (new TypeTest(stubEnvironment))
                                                 ->environmentStub();
                            auto position = bb->trueBranch()->begin();
                            BBTransform::insertAssume(
                                bb->trueBranch(), condition,
                                checkpoint.at(nextInstruction), position, true);
                        }
                    }
                }
            }
            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
