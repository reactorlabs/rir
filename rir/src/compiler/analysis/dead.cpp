#include "dead.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include <algorithm>

namespace rir {
namespace pir {

DeadInstructions::DeadInstructions(Code* code, uint8_t maxBurstSize,
                                   Effects ignoreEffects,
                                   DeadInstructionsMode mode) {
    UsesTree dataDependencies(code);
    std::unordered_map<Instruction*, SmallSet<BB*>> usedOnlyInDeopt;
    bool changed = true;
    Visitor::run(code->entry, [&](Instruction* i) {
        // unused ldfun must be a left over from a guard where ldfun was
        // converted into ldvar.
        if (dataDependencies.at(i).empty() &&
            (LdFun::Cast(i) ||
             ((i->getObservableEffects() / ignoreEffects).empty() &&
              !i->branchOrExit())))
            unused_.insert(i);
    });
    auto i = 1;
    while (changed && i <= maxBurstSize) {
        changed = false;
        for (auto instructionUses : dataDependencies) {
            auto candidate = instructionUses.first;
            auto addToDead = true;
            if (unused_.count(candidate) ||
                (!(candidate->getObservableEffects() / ignoreEffects).empty() ||
                 candidate->branchOrExit()))
                continue;
            auto uses = instructionUses.second;
            for (auto use : uses) {
                switch (mode) {
                case IgnoreTypeTests:
                    if (std::find(TypecheckInstrsList.begin(),
                                  TypecheckInstrsList.end(),
                                  use->tag) == TypecheckInstrsList.end() &&
                        isAlive(use))
                        addToDead = false;
                    break;
                case IgnoreUpdatePromise: {
                    auto up = UpdatePromise::Cast(use);
                    if (!(up && up->arg(0).val() == candidate) && isAlive(use))
                        addToDead = false;
                    break;
                }
                case CountAll:
                    if (isAlive(use))
                        addToDead = false;
                    break;
                }
            }
            if (addToDead) {
                changed = true;
                unused_.insert(candidate);
            }
        }
        i++;
    }
}

bool DeadInstructions::isAlive(Instruction* i) { return !isDead(i); }

bool DeadInstructions::isDead(Instruction* i) { return unused_.count(i); }

bool DeadInstructions::isDead(Value* v) {
    if (auto i = Instruction::Cast(v))
        return isDead(i);
    return false;
}

bool DeadInstructions::isAlive(Value* v) {
    if (auto i = Instruction::Cast(v))
        return isAlive(i);
    return true;
}

} // namespace pir
} // namespace rir
