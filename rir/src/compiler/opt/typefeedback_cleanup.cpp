#include "R/r.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool TypefeedbackCleanup::apply(Compiler&, ClosureVersion* cls, Code* code,
                                LogStream& log, size_t) const {

    auto version = cls->isContinuation();
    if (!version)
        return false;
    auto ctx = version->continuationContext;
    auto deoptCtx = ctx->asDeoptContext();

    bool anyChange = false;

    SEXP changedVar = nullptr;
    TypeFeedback changedVarType;

    std::unordered_set<Instruction*> affected;
    if (deoptCtx)
        Visitor::run(version->entry, [&](Instruction* i) {
            if (!i->hasTypeFeedback())
                return;
            if (auto pc = i->typeFeedback().feedbackOrigin.pc()) {
                if (pc == deoptCtx->reason().pc()) {
                    if (deoptCtx->reason().reason == DeoptReason::Typecheck) {
                        i->updateTypeFeedback().type =
                            PirType(deoptCtx->deoptTrigger());
                    } else if (deoptCtx->reason().reason ==
                               DeoptReason::DeadBranchReached) {
                        if (deoptCtx->deoptTrigger() == R_TrueValue)
                            i->updateTypeFeedback().value = True::instance();
                        else if (deoptCtx->deoptTrigger() == R_FalseValue)
                            i->updateTypeFeedback().value = False::instance();
                    }
                    if (auto f = Force::Cast(i)) {
                        if (auto ld = LdVar::Cast(f->input())) {
                            changedVar = ld->varName;
                            changedVarType = f->typeFeedback();
                        }
                    }
                    affected.insert(i);
                }
            }
        });

    bool changed = true;
    while (changed) {
        changed = false;
        Visitor::run(version->entry, [&](Instruction* i) {
            if (affected.count(i))
                return;
            if (auto f = Force::Cast(i)) {
                if (auto ld = LdVar::Cast(f->input())) {
                    if (ld->varName == changedVar) {
                        affected.insert(i);
                        i->updateTypeFeedback() = changedVarType;
                        changed = true;
                        return;
                    }
                }
            }
            bool needUpdate = false;
            bool allInputsHaveFeedback = true;
            i->eachArg([&](Value* v) {
                if (!needUpdate) {
                    if (auto vi = Instruction::Cast(v)) {
                        if (!vi->hasTypeFeedback()) {
                            allInputsHaveFeedback = false;
                        }
                        if (affected.count(vi)) {
                            needUpdate = true;
                        }
                    }
                } else {
                    allInputsHaveFeedback = false;
                }
            });
            if ((needUpdate && i->hasTypeFeedback()) ||
                (allInputsHaveFeedback && !i->hasTypeFeedback())) {
                affected.insert(i);
                auto inferred = i->inferType([&](Value* v) {
                    if (auto vi = Instruction::Cast(v)) {
                        auto tf = vi->typeFeedback().type;
                        if (!tf.isVoid())
                            return tf;
                    }
                    return v->type;
                });
                i->updateTypeFeedback().type = inferred;
                i->updateTypeFeedback().value = nullptr;
                changed = true;
            }
        });
        if (changed)
            anyChange = true;
    }

    return anyChange;
}

} // namespace pir
} // namespace rir
