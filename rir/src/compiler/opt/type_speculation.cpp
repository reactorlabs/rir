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

bool TypeSpeculation::apply(RirCompiler&, ClosureVersion* cls, Code* code,
                            LogStream& log) const {

    AvailableCheckpoints checkpoint(cls, code, log);

    std::unordered_map<
        BB*, std::unordered_map<Instruction*,
                                std::pair<Checkpoint*, TypeTest::Info>>>
        speculate;

    auto dom = DominanceGraph(code);
    Visitor::run(code->entry, [&](Instruction* i) {
        if (i->typeFeedback.used || i->typeFeedback.type.isVoid() ||
            i->type.isA(i->typeFeedback.type))
            return;

        Instruction* speculateOn = nullptr;
        Checkpoint* guardPos = nullptr;
        TypeFeedback feedback;
        BB* typecheckPos = nullptr;

        if (auto force = Force::Cast(i)) {
            if (auto arg = Instruction::Cast(force->input()->followCasts())) {
                // Blacklist of where it is not worthwhile
                if (!LdConst::Cast(arg) &&
                    // leave this to the promise inliner
                    !MkArg::Cast(arg) && !Force::Cast(arg)) {

                    bool localLoad =
                        LdVar::Cast(arg) && !Env::isStaticEnv(i->env());

                    feedback = i->typeFeedback;
                    // If this force was observed to receive evaluated
                    // promises, better speculate on the input already.
                    switch (force->observed) {
                    case Force::ArgumentKind::value:
                        speculateOn = arg;
                        guardPos = checkpoint.at(arg);
                        typecheckPos = arg->bb();
                        break;
                    case Force::ArgumentKind::evaluatedPromise:
                        if (!localLoad) {
                            speculateOn = arg;
                            guardPos = checkpoint.at(arg);
                            typecheckPos = arg->bb();
                            feedback.type = feedback.type.orPromiseWrapped();
                        }
                        break;
                    case Force::ArgumentKind::promise:
                        if (!localLoad) {
                            speculateOn = i;
                            guardPos = checkpoint.next(i, i, dom);
                            if (guardPos)
                                typecheckPos = guardPos->nextBB();
                        }
                        break;
                    case Force::ArgumentKind::unknown:
                        break;
                    }
                }
            }
        } else if ((!i->type.unboxable() && i->typeFeedback.type.unboxable()) ||
                   (i->type.maybeLazy() && !i->typeFeedback.type.maybeLazy()) ||
                   // Vector where Extract is unboxed if we speculate
                   (i->type.isA(PirType::num()) &&
                    !i->type.scalar().unboxable() &&
                    i->typeFeedback.type.scalar().unboxable())) {
            speculateOn = i;
            feedback = i->typeFeedback;
            guardPos = checkpoint.next(i, i, dom);
            if (guardPos)
                typecheckPos = guardPos->nextBB();
        }

        if (!speculateOn || !guardPos)
            return;

        TypeTest::Create(
            speculateOn, feedback, speculateOn->type.notObject(),
            PirType::any(),
            [&](TypeTest::Info info) {
                speculate[typecheckPos][speculateOn] = {guardPos, info};
                // Prevent redundant speculation
                speculateOn->typeFeedback.used = true;
                //                speculateOn->typeFeedback.type =
                //                PirType::bottom();
            },
            []() {});
    });

    bool anyChange = false;
    Visitor::run(code->entry, [&](BB* bb) {
        if (!speculate.count(bb))
            return;

        for (auto sp : speculate[bb]) {
            auto i = sp.first;

            auto ip = bb->begin();
            if (i->bb() == bb)
                ip = bb->atPosition(i) + 1;

            auto cp = sp.second.first;
            TypeTest::Info& info = sp.second.second;

            BBTransform::insertAssume(info.test, cp, bb, ip, info.expectation,
                                      info.srcCode, info.origin);

            auto cast = new CastType(i, CastType::Downcast, PirType::any(),
                                     info.result);
            cast->effects.set(Effect::DependsOnAssume);
            bb->insert(ip, cast);
            i->replaceDominatedUses(cast);
            anyChange = true;
        }
    });
    return anyChange;
}
} // namespace pir
} // namespace rir
