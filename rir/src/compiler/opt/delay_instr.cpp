#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void DelayInstr::apply(RirCompiler&, ClosureVersion* function,
                       LogStream&) const {

    std::unordered_map<Instruction*, SmallSet<Instruction*>> dataDependencies;

    auto isTarget = [](Instruction* j) {
        return LdFun::Cast(j) || MkArg::Cast(j) || DotsList::Cast(j) ||
               FrameState::Cast(j) || CastType::Cast(j);
    };

    Visitor::run(function->entry, [&](Instruction* instruction) {
        instruction->eachArg([&](Value* v) {
            if (auto usage = Instruction::Cast(v)) {
                if (isTarget(usage) && !usage->bb()->isDeopt())
                    dataDependencies[usage].insert(instruction);
            }
        });
    });

    std::unordered_map<Instruction*, SmallSet<BB*>> usedOnlyInDeopt;
    bool changed = true;
    while (changed) {
        changed = false;
        for (auto instructionUses : dataDependencies) {
            auto candidate = instructionUses.first;
            if (usedOnlyInDeopt.count(candidate))
                continue;
            auto uses = instructionUses.second;
            auto addToDeopt = true;
            if (uses.empty()) {
                addToDeopt = candidate->bb()->isDeopt();
            } else {
                for (auto use : uses) {
                    if (!use->bb()->isDeopt() && !usedOnlyInDeopt.count(use))
                        addToDeopt = false;
                }
            }
            if (addToDeopt) {
                auto& deoptUses = usedOnlyInDeopt[candidate];
                for (auto use : uses) {
                    if (use->bb()->isDeopt()) {
                        deoptUses.insert(use->bb());
                    } else {
                        for (auto deoptUse : usedOnlyInDeopt[use])
                            deoptUses.insert(deoptUse);
                    }
                }
                changed = true;
            }
        }
    }

    std::unordered_map<Instruction*, SmallSet<std::pair<BB*, Instruction*>>>
        replacements;

    VisitorNoDeoptBranch::run(function->entry, [&](BB* bb) {
        auto ip = bb->rbegin();
        while (ip != bb->rend()) {
            auto instruction = *ip;
            auto next = ip + 1;
            if (usedOnlyInDeopt.count(instruction)) {
                for (auto targetBB : usedOnlyInDeopt[instruction]) {
                    auto newInstr = instruction->clone();
                    auto tagetPosition = targetBB->begin();
                    tagetPosition = targetBB->insert(tagetPosition, newInstr);
                    newInstr->eachArg([&](InstrArg& arg) {
                        if (auto instruction = Instruction::Cast(arg.val())) {
                            if (replacements.count(instruction)) {
                                for (auto replacementAtBB :
                                     replacements[instruction]) {
                                    if (replacementAtBB.first == targetBB) {
                                        arg.val() = replacementAtBB.second;
                                        targetBB->swapWithNext(tagetPosition);
                                        tagetPosition++;
                                    }
                                }
                            }
                        }
                    });
                    instruction->replaceUsesIn(newInstr, targetBB);
                    replacements[instruction].insert({targetBB, newInstr});
                }
                usedOnlyInDeopt.erase(instruction);
            }
            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
