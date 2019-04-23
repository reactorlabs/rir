#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../translations/rir_compiler.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_set>

namespace rir {
namespace pir {

void HoistInstruction::apply(RirCompiler& cmp, ClosureVersion* function,
                             LogStream&) const {
    DominanceGraph dom(function);

    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;

        // Currently mostly loop invariant code motion
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;

            // Those are hoisted even if they have effects, but needs expensive
            // effect reordering checks
            static std::unordered_set<Tag> whitelist = {
                Tag::Force, Tag::ChkMissing, Tag::ChkClosure,
            };
            // Those are never hoisted
            static std::unordered_set<Tag> blacklist = {
                // updates context
                Tag::MkEnv,
                // would need patching of input branches
                Tag::Phi,
                // promises are modified by force, moving them is tricky
                Tag::MkArg,
            };

            if (!i->effects.empty() || i->branchOrExit() ||
                blacklist.count(i->tag))
                if (!whitelist.count(i->tag)) {
                    ip++;
                    continue;
                }

            auto next = ip + 1;

            BB* trg = nullptr;
            bool success = true;

            i->eachArg([&](Value* a) {
                auto arg = Instruction::Cast(a);
                // cppcheck-suppress knownConditionTrueFalse
                if (!success || !arg)
                    return;

                // Try to find a hoisting candidate that is dominated by all
                // arguments to i
                // cppcheck-suppress knownConditionTrueFalse
                if (!trg)
                    trg = arg->bb();
                if (trg != arg->bb()) {
                    if (dom.dominates(arg->bb(), trg)) {
                        // nothing to do
                    } else if (dom.dominates(trg, arg->bb())) {
                        trg = arg->bb();
                    } else {
                        success = false;
                    }
                }

                // !trg->isBranch, is a cheap heuristic to avoid moving
                // something from the loop body to the loop header, which is
                // usually not great.
                if (!success || trg == bb || trg->isBranch() ||
                    !dom.dominates(trg, bb)) {
                    success = false;
                    return;
                }

                std::function<bool(BB*)> doesNotReorderEffects = [&](BB* x) {
                    // empty or loop
                    if (!x || x == trg)
                        return true;
                    // done, check cur bb for effects
                    if (x == bb) {
                        for (auto& j : *bb) {
                            if (i == j)
                                return true;
                            if (j->hasObservableEffects())
                                return false;
                        }
                        assert(false);
                    }
                    if (!x->isEmpty()) {
                        // We can only hoist effects over branches if both
                        // branch targets will trigger the effect
                        if (x->last()->branches()) {
                            if (!dom.dominates(x->trueBranch(), bb) ||
                                !dom.dominates(x->falseBranch(), bb))
                                return false;
                        }
                    }

                    for (auto& j : *bb) {
                        if (j->hasObservableEffects())
                            return false;
                    }

                    return doesNotReorderEffects(x->next0) &&
                           doesNotReorderEffects(x->next1);
                };

                success =
                    !i->hasObservableEffects() || doesNotReorderEffects(trg);
            });

            if (trg && success && trg != bb)
                next = bb->moveToLast(ip, trg);

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
