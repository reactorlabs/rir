#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

bool CleanupCheckpoints::apply(Compiler&, ClosureVersion* cls, Code* code,
                               AbstractLog&, size_t) const {
    bool anyChange = false;
    std::unordered_set<Checkpoint*> used;
    Visitor::run(code->entry, [&](Instruction* i) {
        if (auto a = Assume::Cast(i)) {
            used.insert(a->checkpoint());
        }
    });

    std::unordered_set<BB*> toDelete;
    Visitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;
        if (auto cp = Checkpoint::Cast(bb->last())) {
            if (!used.count(cp)) {
                toDelete.insert(bb->deoptBranch());
                assert(bb->deoptBranch()->isExit() &&
                       "deopt blocks should be just one BB");
                bb->remove(bb->end() - 1);
                bb->convertBranchToJmp(true);
            }
        }
    });
    if (!toDelete.empty())
        anyChange = true;
    // Deopt blocks are exit blocks. They have no other predecessors and
    // are not phi inputs. We can delete without further checks.
    for (auto bb : toDelete) {
        delete bb;
    }

    // Feedback paper patch - Fix assume instruction origin!!
    Visitor::run(code->entry, [&](Instruction* i) {
        if (auto assume = Assume::Cast(i)) {

            auto fo = assume->reason.origin;

            if (!assume->defaultFeedback && !fo.index().isUndefined() &&
                fo.index().kind == FeedbackKind::Type) {

                // Type test
                auto assumeArg = assume->arg<0>().val();
                auto typeTest = IsType::Cast(assumeArg);
                if (!typeTest) {
                    // assume->print(std::cerr, true);
                    assert(assumeArg == pir::False::instance() ||
                           assumeArg == pir::True::instance());

                    return; // skip the constant-folded false
                }

                // Instruction we speculated on
                pir::Instruction* speculatedOn =
                    Instruction::Cast(typeTest->arg<0>().val());
                assert(speculatedOn);

                // fix feedback origin in Assume instruction
                if (speculatedOn->hasTypeFeedback()) {
                    auto& tfSpecOn = speculatedOn->typeFeedback(false);
                    assume->reason.origin.function_ =
                        tfSpecOn.feedbackOrigin.function_;
                    assume->reason.origin.index_ =
                        tfSpecOn.feedbackOrigin.index_;
                }
            }
        }
    });

    return anyChange;
}
} // namespace pir
} // namespace rir
