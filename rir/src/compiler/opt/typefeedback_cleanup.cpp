#include "R/r.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool TypefeedbackCleanup::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
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
            if (i->typeFeedback().feedbackOrigin.pc() ==
                deoptCtx->reason().pc()) {
                if (deoptCtx->reason().reason == DeoptReason::Typecheck) {
                    i->updateTypeFeedback().type = deoptCtx->typeCheckTrigger();
                } else if (deoptCtx->reason().reason ==
                           DeoptReason::DeadBranchReached) {
                    if (deoptCtx->deadBranchTrigger() == R_TrueValue)
                        i->updateTypeFeedback().value = True::instance();
                    else if (deoptCtx->deadBranchTrigger() == R_FalseValue)
                        i->updateTypeFeedback().value = False::instance();
                } else if (deoptCtx->reason().reason ==
                               DeoptReason::CallTarget ||
                           deoptCtx->reason().reason == DeoptReason::DeadCall) {
                    // TODO
                }
                if (auto ld = LdVar::Cast(i->followCastsAndForce())) {
                    changedVar = ld->varName;
                    changedVarType = i->typeFeedback();
                }
                affected.insert(i);
            }
            // If typefeedback contradicts actual type of deoptless continuation
            // state then it is clearly stale
            if (LdArg::Cast(i))
                if (!i->typeFeedback().type.isVoid() &&
                    (i->typeFeedback().type & i->type).isVoid()) {
                    i->typeFeedback_->type = PirType::voyd();
                    affected.insert(i);
                }
        });

    std::unordered_set<SEXP> otherAffectedVars;
    bool changed = true;
    while (changed) {
        changed = false;
        Visitor::run(version->entry, [&](Instruction* i) {
            if (affected.count(i))
                return;
            bool needUpdate = false;
            if (auto ld = LdVar::Cast(i->followCastsAndForce())) {
                if (ld->varName == changedVar) {
                    affected.insert(i);
                    i->updateTypeFeedback() = changedVarType;
                    if (Force::Cast(i))
                        i->updateTypeFeedback().type =
                            i->typeFeedback().type.forced();
                    changed = true;
                    return;
                } else if (otherAffectedVars.count(ld->varName)) {
                    needUpdate = true;
                }
            }
            bool allInputsHaveFeedback = true;
            i->eachArg([&](Value* v) {
                if (auto vi = Instruction::Cast(v)) {
                    if (!vi->hasTypeFeedback() ||
                        vi->typeFeedback().type.isVoid())
                        allInputsHaveFeedback = false;
                    if (affected.count(vi))
                        needUpdate = true;
                } else {
                    allInputsHaveFeedback = false;
                }
            });
            // If types of a local variable changed then assume that all loads
            // and stores from/to this variable are tainted.
            if (needUpdate) {
                SEXP varName = nullptr;
                if (StVar::Cast(i))
                    varName = StVar::Cast(i)->varName;
                if (LdVar::Cast(i))
                    varName = LdVar::Cast(i)->varName;
                if (varName)
                    changed =
                        otherAffectedVars.insert(varName).second || changed;
            }
            if ((needUpdate && i->hasTypeFeedback()) ||
                (allInputsHaveFeedback && i->hasTypeFeedback() &&
                 i->typeFeedback().type.isVoid())) {
                affected.insert(i);
                std::unordered_set<Value*> vals;
                auto inferred = i->inferType([&](Value* v) {
                    if (auto vi = Instruction::Cast(v)) {
                        auto tf = vi->typeFeedback().type;
                        vals.insert(vi->typeFeedback().value);
                        if (!tf.isVoid())
                            return tf;
                    } else {
                        vals.insert(v);
                    }
                    return v->type;
                });
                if (needUpdate || !inferred.isVoid()) {
                    i->updateTypeFeedback().type = inferred;
                    if (vals.size() == 1)
                        i->updateTypeFeedback().value = *vals.begin();
                    else
                        i->updateTypeFeedback().value = nullptr;
                    changed = true;
                }
            }
        });
        if (changed)
            anyChange = true;
    }

    return anyChange;
}

} // namespace pir
} // namespace rir
