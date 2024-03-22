#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"
#include "compiler/util/safe_builtins_list.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

bool DelayInstr::apply(Compiler&, ClosureVersion* cls, Code* code, AbstractLog&,
                       size_t) const {
    bool anyChange = false;

    auto isTarget = [](Instruction* j) {
        if (LdFun::Cast(j) || DotsList::Cast(j) || MkArg::Cast(j) ||
            FrameState::Cast(j) || CastType::Cast(j))
            return true;
        int builtinId = -1;
        if (auto call = CallBuiltin::Cast(j))
            builtinId = call->builtinId;
        if (auto call = CallSafeBuiltin::Cast(j))
            builtinId = call->builtinId;
        if (builtinId != -1) {
            if (j->hasObservableEffects())
                return false;
            if (j->mergedInputType().maybeObj(true, "delayinstr"))
                return SafeBuiltinsList::nonObjectIdempotent(builtinId);
            return SafeBuiltinsList::idempotent(builtinId);
        }
        return false;
    };

    const UsesTree dataDependencies(code);

    std::unordered_map<Instruction*, SmallSet<BB*>> usedOnlyInDeopt;
    bool changed = true;

    while (changed) {
        changed = false;
        for (const auto& instructionUses : dataDependencies) {
            auto candidate = instructionUses.first;
            if (!candidate->bb()->isDeopt() && isTarget(candidate)) {
                if (usedOnlyInDeopt.count(candidate))
                    continue;
                auto uses = instructionUses.second;
                auto addToDeopt = true;
                for (auto use : uses) {
                    if (Phi::Cast(use) || (!use->bb()->isDeopt() &&
                                           !usedOnlyInDeopt.count(use))) {
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

    VisitorNoDeoptBranch::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto instruction = *ip;
            auto next = ip + 1;
            if (usedOnlyInDeopt.count(instruction)) {
                for (auto targetBB : usedOnlyInDeopt[instruction]) {
                    anyChange = true;
                    auto insertPosition = targetBB->begin();
                    // ensure we don't insert any instruction before its
                    // arguments
                    auto seek = [&](Instruction* instr) {
                        instr->eachArg([&](Value* v) {
                            if (auto dep = Instruction::Cast(v)) {
                                if (dep->bb() == targetBB) {
                                    auto depPos = targetBB->atPosition(dep);
                                    if (depPos >= insertPosition)
                                        insertPosition = depPos + 1;
                                }
                            }
                        });
                    };
                    auto newInstr = instruction->clone();
                    newInstr->eachArg([&](InstrArg& arg) {
                        replaceArgs(arg, replacements, targetBB);
                    });
                    seek(newInstr);
                    insertPosition =
                        targetBB->insert(insertPosition, newInstr) + 1;
                    instruction->replaceUsesIn(newInstr, targetBB);
                    replacements[instruction].insert({targetBB, newInstr});
                }
                next = bb->remove(ip);
            }
            ip = next;
        }
    });
    return anyChange;
}
} // namespace pir
} // namespace rir
