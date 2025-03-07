#include "../analysis/available_checkpoints.h"
#include "../analysis/dead.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "compiler/analysis/cfg.h"
#include "compiler/util/bb_transform.h"
#include "pass_definitions.h"
#include "type_test.h"

#include <unordered_map>

namespace rir {
namespace pir {

std::string streamToString(std::function<void(std::stringstream&)> f) {
    std::stringstream ss;
    f(ss);
    return ss.str();
}

bool TypeSpeculation::apply(Compiler&, ClosureVersion* cls, Code* code,
                            AbstractLog& log, size_t) const {

    std::cerr << "Type speculation on: " << cls->owner()->name() << "\n";

    AvailableCheckpoints checkpoint(cls, code, log);
    DeadInstructions maybeUsedUnboxed(code, 1, Effects::Any(),
                                      DeadInstructions::IgnoreBoxedUses);

    std::unordered_map<
        BB*, std::unordered_map<Instruction*,
                                std::pair<Checkpoint*, TypeTest::Info>>>
        speculate;

    auto dom = DominanceGraph(code);
    VisitorNoDeoptBranch::run(code->entry, [&](Instruction* i) {
        if (i->typeFeedback().type.isVoid() || i->typeFeedbackUsed) {
            return;
        }

        if (i->type.isA(i->typeFeedback().type)) {

            auto& tf = i->typeFeedback();
            if (!tf.defaultFeedback &&
                tf.feedbackOrigin.index().kind == rir::FeedbackKind::Type) {
                auto index = tf.feedbackOrigin.index();

                if (!cls->hasFeedbackStatsFor(tf.feedbackOrigin.function())) {
                    auto& feedbackStats =
                        cls->feedbackStatsFor(tf.feedbackOrigin.function());

                    rir::pir::SlotNotUsedStaticTypeReason snu;

                    snu.staticType = streamToString(
                        [&](std::stringstream& ss) { i->type.print(ss); });

                    snu.feedbackType =
                        streamToString([&](std::stringstream& ss) {
                            i->typeFeedback().type.print(ss);
                        });

                    // std::cerr << "---- instruction  ";
                    // i->print(std::cerr, false);
                    // std::cerr << "\n is ";

                    snu.fromContext = false;
                    snu.equalTypes = (i->type == i->typeFeedback().type);

                    if (auto ldi = LdArg::Cast(i->followCastsAndForce())) {
                        snu.fromContext = true;
                        snu.ctx = cls->context();
                        snu.fromInstruction =
                            streamToString([&](std::stringstream& ss) {
                                ldi->print(ss, false);
                            });

                        // std::cerr << "\n and got type from LdArg \n ";
                        // ldi->print(std::cerr, false);
                        // std::cerr << "\n ";
                        // std::cerr << cls->context();
                        // std::cerr << "\n\n ";
                    }

                    feedbackStats.slotsReadNotUsedStaticTypeReason[index] = snu;

                    // code->printCode(std::cerr, true, false);

                    std::cerr << "\n--------------------- \n ";
                }
            }

            return;
        }

        Instruction* speculateOn = nullptr;
        Checkpoint* guardPos = nullptr;
        TypeFeedback feedback;
        BB* typecheckPos = nullptr;

        if (auto force = Force::Cast(i)) {
            if (auto arg = Instruction::Cast(force->input()->followCasts())) {
                // Blacklist of where it is not worthwhile
                if ( // leave this to the promise inliner
                    !MkArg::Cast(arg) && !Force::Cast(arg)) {

                    bool localLoad =
                        LdVar::Cast(arg) && !Env::isStaticEnv(i->env());

                    feedback = i->typeFeedback();
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
        } else if ((!i->type.unboxable() &&
                    i->typeFeedback().type.unboxable()) ||
                   (i->type.maybeLazy() &&
                    !i->typeFeedback().type.maybeLazy()) ||
                   // Vector where Extract is unboxed if we speculate
                   (i->type.isA(PirType::num()) &&
                    !i->type.simpleScalar().unboxable() &&
                    i->typeFeedback().type.simpleScalar().unboxable() &&
                    maybeUsedUnboxed.isAlive(i))) {
            speculateOn = i;
            feedback = i->typeFeedback();
            guardPos = checkpoint.next(i, i, dom);
            if (guardPos)
                typecheckPos = guardPos->nextBB();
        }

        if (!speculateOn || !guardPos || !typecheckPos ||
            typecheckPos->isDeopt() ||
            (speculate.count(typecheckPos) &&
             speculate[typecheckPos].count(speculateOn)))
            return;

        // leave this for scope analysis
        if (auto ld = LdVar::Cast(speculateOn))
            if (auto mk = MkEnv::Cast(ld->env()))
                if (mk->contains(ld->varName))
                    return;

        TypeTest::Create(
            speculateOn, feedback, speculateOn->type.notObject(),
            PirType::any(),
            [&](TypeTest::Info info) {
                speculate[typecheckPos][speculateOn] = {guardPos, info};
                // Prevent redundant speculation
                assert(i->hasTypeFeedback());
                i->typeFeedbackUsed = true;
            },
            []() {});
    });

    bool anyChange = false;
    VisitorNoDeoptBranch::run(code->entry, [&](BB* bb) {
        if (!speculate.count(bb))
            return;

        for (auto sp : speculate[bb]) {
            auto i = sp.first;

            auto ip = bb->begin();
            if (i->bb() == bb)
                ip = bb->atPosition(i) + 1;

            auto cp = sp.second.first;
            TypeTest::Info& info = sp.second.second;

            BBTransform::insertAssume(info.test, info.expectation, cp,
                                      info.feedbackOrigin,
                                      DeoptReason::Typecheck, bb, ip);

            auto assume = Assume::Cast(*(ip - 1));
            info.updateAssume(*assume);

            // std::cerr <<  " ************************* FROM TYPE SPECULATION"
            // << "\n"; assume->print(std::cerr, true); std::cerr << "\n";

            auto cast = new CastType(i, CastType::Downcast, PirType::any(),
                                     info.result);
            cast->effects.set(Effect::DependsOnAssume);
            bb->insert(ip, cast);
            i->replaceDominatedUses(cast, dom);
            anyChange = true;
        }
    });
    return anyChange;
}
} // namespace pir
} // namespace rir
