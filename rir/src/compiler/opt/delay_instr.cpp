#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

/*
 * The main idea of this optimization is to try to move some operations, that
 * are usually only used by deopt branches, and maybe expensive such as
 * mkArg, or capture environments such as framestate, to all those branches
 * and thus cleanup the fastpath.
 */

void DelayInstr::apply(RirCompiler&, ClosureVersion* function,
                       LogStream&) const {

    std::unordered_map<Instruction*, SmallSet<Instruction*>> deoptUses;
    std::unordered_map<Instruction*, SmallSet<Instruction*>> fastpathUses;
    std::unordered_map<Instruction*, SmallSet<BB*>> usedOnlyInDeopt;

    auto isTarget = [](Instruction* j) {
        return LdFun::Cast(j) || MkArg::Cast(j) || DotsList::Cast(j) ||
               FrameState::Cast(j) || CastType::Cast(j);
    };

    CFG cfg(function);

    Visitor::run(function->entry, [&](Instruction* i) {
        i->eachArg([&](Value* v) {
            if (auto j = Instruction::Cast(v)) {
                if (isTarget(j)) {
                    if (i->bb()->isDeopt()) {
                        usedOnlyInDeopt[j].insert(i->bb());
                        deoptUses[j].insert(i);
                    } else {
                        fastpathUses[j].insert(i);
                    }
                }
            }
        });
    });

    for (auto entry : cfg.exits()) {
        VisitorNoDeoptBranch::runBackward(entry, cfg, [&](Instruction* i) {
            if (i->bb()->isDeopt())
                return;
            i->eachArg([&](Value* v) {
                if (auto j = Instruction::Cast(v)) {
                    if (isTarget(j)) {
                        auto& u = usedOnlyInDeopt[j];
                        if (!fastpathUses.count(i)) {
                            for (auto instruction : deoptUses[i]) {
                                u.insert(instruction->bb());
                            }
                        } else {
                            // We could maybe go recursively here to move more
                            // instructions together in one pass
                            u.insert((BB*)-1);
                        }
                    }
                }
            });
        });
    }

    for (auto entry : cfg.exits()) {
        Visitor::runBackward(entry, cfg, [&](Instruction* i) {
            i->eachArg([&](Value* v) {
                if (auto j = Instruction::Cast(v)) {
                    if (isTarget(j)) {
                        auto& u = usedOnlyInDeopt[j];
                        if (i->bb()->isDeopt()) {
                            u.insert(i->bb());
                        } else {
                            if (!usedOnlyInDeopt[i].empty()) {
                                for (auto targetBB : usedOnlyInDeopt[i])
                                    u.insert(targetBB);
                            } else {
                                u.insert((BB*)-1);
                            }
                        }
                    }
                }
            });
        });
    }

    std::unordered_map<Instruction*, SmallSet<std::pair<BB*, Instruction*>>>
        replacements;
    for (auto entry : cfg.exits()) {
        VisitorNoDeoptBranch::runBackward(entry, cfg, [&](BB* bb) {
            if (bb->isDeopt())
                return;
            auto ip = bb->rbegin();
            while (ip != bb->rend()) {
                auto i = *ip;
                auto next = ip + 1;

                if (isTarget(i)) {
                    if (!usedOnlyInDeopt[i].empty() &&
                        !usedOnlyInDeopt[i].count((BB*)-1)) {
                        for (auto targetBB : usedOnlyInDeopt[i]) {
                            auto newInstr = i->clone();
                            auto tagetPosition = targetBB->begin();
                            tagetPosition =
                                targetBB->insert(tagetPosition, newInstr);
                            newInstr->eachArg([&](InstrArg& arg) {
                                if (auto instruction =
                                        Instruction::Cast(arg.val())) {
                                    if (replacements.count(instruction)) {
                                        for (auto replacementAtBB :
                                             replacements[instruction]) {
                                            if (replacementAtBB.first ==
                                                targetBB) {
                                                arg.val() =
                                                    replacementAtBB.second;
                                                targetBB->swapWithNext(
                                                    tagetPosition);
                                                tagetPosition++;
                                            }
                                        }
                                    }
                                }
                            });
                            i->replaceUsesIn(newInstr, targetBB);
                            replacements[i].insert({targetBB, newInstr});
                        }
                        bb->remove(bb->atPosition(i));
                        ip++;
                    }
                }
                ip = next;
            }
        });
    }
}
} // namespace pir
} // namespace rir
