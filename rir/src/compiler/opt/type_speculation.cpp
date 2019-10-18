#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/type_test.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void TypeSpeculation::apply(RirCompiler&, ClosureVersion* function,
                            LogStream& log) const {

    AvailableCheckpoints checkpoint(function, log);

    std::unordered_map<Checkpoint*,
                       std::unordered_map<Instruction*, TypeTest::Info>>
        speculate;

    Visitor::run(function->entry, [&](Instruction* i) {
        if (i->typeFeedback.type.isVoid() || i->type.isA(i->typeFeedback.type))
            return;

        Instruction* speculateOn = nullptr;
        Checkpoint* guardPos = nullptr;
        Instruction::TypeFeedback feedback;

        if (auto force = Force::Cast(i)) {
            if (auto arg = Instruction::Cast(force->input()->followCasts())) {
                // Blacklist of where it is not worthwhile
                if (!LdConst::Cast(arg) &&
                    // leave this to the promise inliner
                    !MkArg::Cast(arg) && !Force::Cast(arg) &&
                    // leave this to scope resolution
                    !LdVar::Cast(arg) && !LdVarSuper::Cast(arg)) {
                    speculateOn = i;
                }
                if (auto ld = LdVar::Cast(arg))
                    if (!Env::isPirEnv(ld->env()))
                        speculateOn = i;
                if (auto ld = LdVarSuper::Cast(arg))
                    if (!Env::isPirEnv(ld->env()))
                        speculateOn = i;

                if (speculateOn) {
                    feedback = i->typeFeedback;

                    // If this force was observed to receive evaluated
                    // promises, better speculate on the input already.
                    switch (force->observed) {
                    case Force::ArgumentKind::value:
                        speculateOn = arg;
                        guardPos = checkpoint.at(i);
                        break;
                    case Force::ArgumentKind::evaluatedPromise:
                        speculateOn = arg;
                        guardPos = checkpoint.at(i);
                        feedback.type = feedback.type.orPromiseWrapped();
                        break;
                    case Force::ArgumentKind::promise:
                    case Force::ArgumentKind::unknown:
                        guardPos = checkpoint.next(i);
                        break;
                    }
                }
            }
        }

        if (!speculateOn || !guardPos)
            return;

        TypeTest::Create(speculateOn, feedback,
                         [&](TypeTest::Info info) {
                             speculate[guardPos][speculateOn] = info;
                             // Prevent redundant speculation
                             speculateOn->typeFeedback.type = PirType::bottom();
                         },
                         []() {});
    });

    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;
        auto cp = Checkpoint::Cast(bb->last());
        if (!cp)
            return;
        if (!speculate.count(cp))
            return;

        for (auto sp : speculate[cp]) {
            auto speculationTarget = sp.first;
            TypeTest::Info& info = sp.second;
            // The instruction requiring speculation could be before or after
            // the cp, so we need to find the place that is after both to insert
            // the typetest and the assume
            BB* targetBB;
            BB::Instrs::iterator insertPosition;
            targetBB = bb->trueBranch();
            if (speculationTarget->bb()->beforeInCfg(targetBB)) {
                insertPosition = targetBB->begin();
            } else {
                if (targetBB->beforeInCfg(speculationTarget->bb()))
                    targetBB = speculationTarget->bb();
                insertPosition = targetBB->atPosition(speculationTarget) + 1;
            }

            BBTransform::insertAssume(info.test, cp, targetBB, insertPosition,
                                      info.expectation, info.srcCode,
                                      info.origin);

            auto cast = new CastType(speculationTarget, CastType::Downcast,
                                     PirType::any(), info.result);
            cast->effects.set(Effect::DependsOnAssume);
            targetBB->insert(insertPosition, cast);
            speculationTarget->replaceDominatedUses(cast);
        }
    });
}
} // namespace pir
} // namespace rir
