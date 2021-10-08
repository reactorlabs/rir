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

    bool anyChange = false;

    SEXP changedVar = nullptr;
    TypeFeedback changedVarType;

    std::unordered_set<Instruction*> affected;
    Visitor::run(version->entry, [&](Instruction* i) {
        if (!i->hasTypeFeedback())
            return;
        if (auto pc = i->typeFeedback().feedbackOrigin.pc()) {
            auto& ctx = version->deoptContext;

            if (pc == ctx.reason().pc()) {
                if (ctx.reason().reason == DeoptReason::Typecheck) {
                    i->updateTypeFeedback().type = PirType(ctx.deoptTrigger());
                } else if (ctx.reason().reason ==
                           DeoptReason::DeadBranchReached) {
                    if (ctx.deoptTrigger() == R_TrueValue)
                        i->updateTypeFeedback().value = True::instance();
                    else if (ctx.deoptTrigger() == R_FalseValue)
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
            i->eachArg([&](Value* v) {
                if (auto vi = Instruction::Cast(v)) {
                    if (affected.count(vi)) {
                        affected.insert(i);
                        if (i->hasTypeFeedback()) {
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
                        }
                        changed = true;
                    }
                }
            });
        });
        if (changed)
            anyChange = true;
    }

    return anyChange;
}

} // namespace pir
} // namespace rir
