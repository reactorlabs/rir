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
               FrameState::Cast(j) || CastType::Cast(j) || MkEnv::Cast(j);
    };

    Visitor::run(function->entry, [&](Instruction* instruction) {
        instruction->eachArg([&](Value* v) {
            if (auto usage = Instruction::Cast(v)) {
                if (isTarget(usage) && !usage->bb()->isDeopt())
                    dataDependencies[usage].insert(instruction);
            }
        });
    });

    auto passObservableEffects = [&](Effects e) -> Effects {
        e.reset(Effect::MutatesArgument);
        return e;
    };

    std::unordered_map<Instruction*, SmallSet<BB*>> usedOnlyInDeopt;
    std::unordered_map<Instruction*, SmallSet<Instruction*>>
        floatingEffectlessUses;
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
                    if (use->type.isVoid() &&
                        passObservableEffects(use->getObservableEffects())
                            .empty() &&
                        !use->branchOrExit()) {
                        floatingEffectlessUses[candidate].insert(use);
                    } else if (!use->bb()->isDeopt() &&
                               !usedOnlyInDeopt.count(use)) {
                        addToDeopt = false;
                    }
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

    auto replaceArgs =
        [&](InstrArg& arg,
            std::unordered_map<Instruction*,
                               SmallSet<std::pair<BB*, Instruction*>>>&
                replacements,
            BB* targetBB) {
            if (auto instruction = Instruction::Cast(arg.val())) {
                if (replacements.count(instruction)) {
                    for (auto replacementAtBB : replacements[instruction]) {
                        if (replacementAtBB.first == targetBB)
                            arg.val() = replacementAtBB.second;
                    }
                }
            }
        };
    std::unordered_map<Instruction*, SmallSet<std::pair<BB*, Instruction*>>>
        replacements;
    std::unordered_map<BB*, std::vector<rir::pir::Instruction*>::iterator>
        insertPositions;
    std::unordered_set<BB*> visited;
    VisitorNoDeoptBranch::run(function->entry, [&](BB* bb) {
        visited.insert(bb);
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto instruction = *ip;
            auto next = ip + 1;
            if (usedOnlyInDeopt.count(instruction)) {
                for (auto targetBB : usedOnlyInDeopt[instruction]) {
                    auto newInstr = instruction->clone();
                    newInstr->eachArg([&](InstrArg& arg) {
                        replaceArgs(arg, replacements, targetBB);
                    });
                    if (!insertPositions.count(targetBB))
                        insertPositions[targetBB] = targetBB->begin();
                    std::vector<rir::pir::Instruction*>::iterator&
                        insertPosition = insertPositions[targetBB];
                    insertPosition =
                        targetBB->insert(insertPosition, newInstr) + 1;
                    instruction->replaceUsesIn(newInstr, targetBB);
                    replacements[instruction].insert({targetBB, newInstr});
                    for (auto floatingDependency :
                         floatingEffectlessUses[instruction]) {
                        auto valid = true;
                        floatingDependency->eachArg([&](InstrArg& arg) {
                            if (auto instruction =
                                    Instruction::Cast(arg.val())) {
                                if (!visited.count(instruction->bb())) {
                                    valid = false;
                                    return;
                                }
                            }
                        });
                        if (valid) {
                            auto newInstr = floatingDependency->clone();
                            newInstr->eachArg([&](InstrArg& arg) {
                                replaceArgs(arg, replacements, targetBB);
                            });
                            insertPosition =
                                targetBB->insert(insertPosition, newInstr) + 1;
                        }
                    }
                }
            }
            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
