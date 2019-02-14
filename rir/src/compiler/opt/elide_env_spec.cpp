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
                         LogStream&) const {
    // Elide environments of binary operators in which both operators are
    // primitive values
    std::unordered_map<BB*, Checkpoint*> bckCheckpoints;
    std::unordered_map<BB*, Checkpoint*> fwdCheckpoints;

    auto nonObjectArgs = [&](Instruction* i) {
        auto answer = true;
        i->eachArg([&](Value* arg) {
            if (arg->type.maybeObj() && arg->typeFeedback.maybeObj())
                answer = true;
        });
        return answer;
    };

    auto usesAreOnlyForces = [&](Value* environment) {
        auto onlyForces = true;
        Visitor::run(function->entry, [&](Instruction* i) {
            i->eachArg([&](InstrArg& arg) {
                if (arg.val() == environment && !Force::Cast(i) &&
                    !FrameState::Cast(i)) {
                    onlyForces = false;
                }
            });
        });
        return onlyForces;
    };

    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;
        if (auto cp = Checkpoint::Cast(bb->last())) {
            bckCheckpoints.emplace(bb->trueBranch(), cp);
            fwdCheckpoints.emplace(bb, cp);
        }
    });

    Visitor::run(function->entry, [&](BB* bb) {
        std::unordered_map<Value*, Value*> stubFor;
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (i->hasEnv()) {
                // Speculatively elide environments on instructions that only
                // require them in case any argument is an object
                if (i->envOnlyForObj() && bckCheckpoints.count(bb) &&
                    nonObjectArgs(i)) {
                    auto cp = bckCheckpoints.at(bb);

                    i->elideEnv();
                    i->eachArg([&](Value* arg) {
                        if (arg != i->env())
                            if (arg->type.maybeObj()) {
                                auto condition = (new TypeTest(arg))->object();
                                BBTransform::insertAssume(bb, condition, cp, ip,
                                                          false);
                            }
                    });
                    next = ip + 1;
                    i->type.setNotObject();
                }

                // Speculatively elide environments on force instructions that
                // only require them in case the force access promises
                // reflectively
                if (Force::Cast(*ip) && fwdCheckpoints.count(bb)) {
                    auto force = Force::Cast(*ip);
                    auto environment = MkEnv::Cast(force->env());
                    auto cp = fwdCheckpoints.at(bb);
                    if (!environment->stub && usesAreOnlyForces(environment)) {
                        environment = MkEnv::Cast(force->env());

                        if (!stubFor.count(environment)) {
                            auto stub = (MkEnv*)environment->clone();
                            stub->stub = true;
                            stubFor.emplace(environment, stub);
                            environment->bb()->insert(
                                environment->bb()->atPosition(environment),
                                stub);

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
                        auto condition =
                            (new TypeTest(stubEnvironment))->environmentStub();
                        auto position = bb->trueBranch()->begin();
                        BBTransform::insertAssume(bb->trueBranch(), condition,
                                                  cp, position, true);
                    }
                }
            }

            // Effect between assume and checkpoint is not allowed
            if (i->changesEnv() || i->hasEffect())
                break;

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
