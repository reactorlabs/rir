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

    VisitorNoDeoptBranch::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;

        /*
         * Currently mostly loop invariant code motion
         *
         * The general idea is the following.
         *
         * Given an instruction i and its source BB b
         * 1. identify a target block t, such that the end of that block is
         *    * dominated by all inputs
         *    * strictly dominates b
         * 2. hoist the instruction to t if any of those applies
         *    * i has no effects and is free to execute
         *    * i has no effects and hoisting it does not incur
         *      unnecessary computation. This is checked by verifying that
         *      none of the intermediate blocks between t and b branch off, to
         *      an unrelated trace.
         *    * i has an effect, but there is no conflicting effect between t
         *      and b, thus reordering is not observable.
         *
         * TODO: compute loop boundaries for placement instead of taking the
         *       whatever block happens to be dominated by all inputs.
         */
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

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
                // these stay in deopt branches
                Tag::FrameState,
            };

            bool hasEffects = !i->effects.empty();
            if (hasEffects || i->branchOrExit() || blacklist.count(i->tag))
                if (!whitelist.count(i->tag)) {
                    ip = next;
                    continue;
                }

            bool onlyDependsOnAssume =
                !hasEffects && i->effects.contains(Effect::DependsOnAssume);

            BB* target = nullptr;
            {
                bool success = true;
                i->eachArg([&](Value* a) {
                    if (!success)
                        return;

                    auto arg = Instruction::Cast(a);
                    if (!arg)
                        return;

                    // Try to find a hoisting candidate that is dominated by all
                    // arguments to i
                    if (!target)
                        target = arg->bb();
                    if (target != arg->bb()) {
                        if (dom.dominates(arg->bb(), target)) {
                            // nothing to do
                        } else if (dom.dominates(target, arg->bb())) {
                            target = arg->bb();
                        } else {
                            success = false;
                        }
                    }

                    if (target == bb || !dom.dominates(target, bb))
                        success = false;
                });
                if (!success || !target)
                    target = nullptr;
                else
                    // both branches dominate bb, then we should move target
                    // forward until they join again
                    while (target->next0 && target->next0 != bb &&
                           dom.dominates(target->next0, bb) &&
                           (!target->next1 || dom.dominates(target->next1, bb)))
                        target = target->next0;
            }

            if (!target) {
                ip = next;
                continue;
            }

            auto allowReorder = [&](BB* x) {
                std::function<bool(BB*)> compute = [&](BB* x) {
                    if (!x || x == target)
                        return true;

                    if (!x->isEmpty()) {
                        // We can only hoist effects over branches if both
                        // branch targets will trigger the effect
                        if (x->last()->branches()) {
                            if (!dom.dominates(x->trueBranch(), bb) ||
                                !dom.dominates(x->falseBranch(), bb))
                                return false;
                        }
                    }

                    for (auto& j : *bb)
                        if (x == bb && i == j) {
                            return true;
                        } else if (onlyDependsOnAssume) {
                            if (Assume::Cast(j))
                                return false;
                        } else if (i->isTypecheck()) {
                            if (Force::Cast(j) &&
                                i->arg(0).val()->type.maybePromiseWrapped() &&
                                j->effects.includes(Effect::Force) &&
                                j->arg(0).val()->followCastsAndForce() ==
                                    i->arg(0).val()->followCastsAndForce())
                                return false;
                        } else if (hasEffects && j->hasStrongEffects()) {
                            return false;
                        }

                    return compute(x->next0) && compute(x->next1);
                };
                return compute(x->next0) && compute(x->next1);
            };

            auto noUnneccessaryComputation = [&](BB* x, unsigned exceptions) {
                std::function<bool(BB*)> compute = [&](BB* x) {
                    if (!x || x == bb || x == target)
                        return true;
                    if (!x->isEmpty()) {
                        // If we hoist over a branch, but one of the
                        // branches does not need the value, then this will
                        // waste computation
                        if (x->last()->branches()) {
                            if (!dom.dominates(x->trueBranch(), bb) ||
                                !dom.dominates(x->falseBranch(), bb)) {
                                if (exceptions == 0)
                                    return false;
                                exceptions--;
                            }
                        }
                    }
                    return compute(x->next0) && compute(x->next1);
                };
                return compute(x->next0) && compute(x->next1);
            };

            bool success = true;
            if (hasEffects || i->isTypecheck())
                success = allowReorder(target);
            else if (i->cost() > 0)
                success = noUnneccessaryComputation(target, 1);

            if (success)
                next = bb->moveToLast(ip, target);

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
