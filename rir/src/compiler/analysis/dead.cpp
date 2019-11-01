#include "dead.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include <algorithm>

namespace rir {
namespace pir {

DeadInstructions::DeadInstructions(Code* code, DeadInstructionsMode mode) {
    UsesTree dataDependencies(code);
    std::unordered_map<Instruction*, SmallSet<BB*>> usedOnlyInDeopt;
    bool changed = true;
    Visitor::run(code->entry, [&](Instruction* i) {
        if (dataDependencies.at(i).empty())
            unused_.insert(i);
    });
    while (changed) {
        changed = false;
        for (auto instructionUses : dataDependencies) {
            auto candidate = instructionUses.first;
            auto addToDead = true;
            if (unused_.count(candidate))
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
                case IgnoreUpdatePromise:
                    if (!UpdatePromise::Cast(use) && isAlive(use))
                        addToDead = false;
                    break;
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
    }
    for (auto instruction : unused_) {
        instruction->print(std::cout);
        std::cout << "\n";
    }
    std::cout << "Termine\n";
}

bool DeadInstructions::isAlive(Instruction* i) { return !isDead(i); }

bool DeadInstructions::isDead(Instruction* i) {
    if (i->branchOrExit())
        return false;
    return (i->type != PirType::voyd()) && unused_.count(i);
}

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
