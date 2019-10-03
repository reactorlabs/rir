#include "dead.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"

namespace rir {
namespace pir {

DeadInstructions::DeadInstructions(Code* code, DeadInstructionsMode mode) {
    Visitor::run(code->entry, [&](Instruction* i) {
        i->eachArg([&](Value* v) {
            if (auto j = Instruction::Cast(v)) {
                switch (mode) {
                case IgnoreTypeTests:
                    if ((i->tag == Tag::CastType || i->tag == Tag::IsType ||
                         i->tag == Tag::IsObject) &&
                        v == i->arg(0).val())
                        return;
                    break;
                case IgnoreUpdatePromise:
                    if (i->tag == Tag::UpdatePromise && v == i->arg(0).val())
                        return;
                    break;
                case CountAll:
                    break;
                }
                used_.insert(j);
            }
        });
    });
}

bool DeadInstructions::used(Instruction* i) {
    if (i->branchOrExit())
        return true;
    return (i->type != PirType::voyd()) && used_.count(i);
}

bool DeadInstructions::unused(Instruction* i) { return !used(i); }

bool DeadInstructions::unused(Value* v) {
    if (auto i = Instruction::Cast(v))
        return unused(i);
    return false;
}

bool DeadInstructions::used(Value* v) {
    if (auto i = Instruction::Cast(v))
        return used(i);
    return true;
}

} // namespace pir
} // namespace rir
