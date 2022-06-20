#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/cfg.h"
#include "compiler/analysis/context_stack.h"
#include "pass_definitions.h"

#include <unordered_set>

namespace rir {
namespace pir {

bool HoistInstruction::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                             AbstractLog& log, size_t) const {
    bool anyChange = false;
    DominanceGraph dom(code);
    ContextStack cs(cls, code, log);

    VisitorNoDeoptBranch::run(code->entry, [&](BB* bb) {
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
                Tag::Force,
                Tag::ChkMissing,
                Tag::ChkFunction,
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
                else if (target->isBranch())
                    // both branches dominate bb, then we should move target
                    // forward until they join again
                    while (dom.strictlyDominates(*target->successors().begin(),
                                                 bb) &&
                           (!target->isBranch() ||
                            dom.strictlyDominates(target->falseBranch(), bb)))
                        target = *target->successors().begin();
            }

            if (!target) {
                ip = next;
                continue;
            }

            // Check that we are not hoisting into inlined code. If we move an
            // instruction from after a PopContext in between a PushContext and
            // a PopContext, we basically move it into the inlined callee.
            // However, the callee can jump to the PopContext and thus skip the
            // hoisted instruction.
            // TODO: This still allows hoisting over the push-pop pair or out of
            // the callee into the caller, but it depends on where the instruction
            // arguments are, ie, sometimes it could be possible to hoist but we
            // don't. In the example we don't move the Inc to the end of BB1
            // BB1
            //   %1 = LdConst 0
            //   %2 = PushContext
            //   # we don't hoist to here but could be more precise
            //   # and find the push maybe?
            //   Branch -> BB2 (if true) | BB3 (if false)
            // BB2
            //   %3 = PopContext %2
            //   %4 = Inc %1
            {
                auto b = target;
                while (b->isEmpty())
                    b = *b->predecessors().begin();

                if (cs.after(b->last()).numContexts() >
                    cs.before(i).numContexts()) {
                    ip = next;
                    continue;
                }
            }

            auto allowReorder = [&](BB* x) {
                std::function<bool(BB*)> compute = [&](BB* x) {
                    if (!x || x == target)
                        return true;

                    if (!x->isEmpty() && x != bb) {
                        // We can only hoist effects over branches if both
                        // branch targets will trigger the effect
                        if (x->last()->branches()) {
                            if (!dom.strictlyDominates(x->trueBranch(), bb) ||
                                !dom.strictlyDominates(x->falseBranch(), bb))
                                return false;
                        }
                    }

                    for (auto& j : *bb)
                        if (x == bb && i == j) {
                            return true;
                        } else if (onlyDependsOnAssume) {
                            if (Assume::Cast(j))
                                return false;
                            if (Branch::Cast(j))
                                for (auto b : bb->successors())
                                    if (b->isDeopt())
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

                    return x->successors().all(compute);
                };
                return x->successors().all(compute);
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
                            if (!dom.strictlyDominates(x->trueBranch(), bb) ||
                                !dom.strictlyDominates(x->falseBranch(), bb)) {
                                if (exceptions == 0)
                                    return false;
                                exceptions--;
                            }
                        }
                    }
                    return x->successors().all(compute);
                };
                return x->successors().all(compute);
            };

            bool success = true;
            if (hasEffects || i->isTypecheck())
                success = allowReorder(target);
            else if (i->cost() > 0)
                success = noUnneccessaryComputation(target, 1);

            if (success) {
                next = bb->moveToLast(ip, target);
                anyChange = true;
            }

            ip = next;
        }
    });

    // Search for branches where both legs force the same promise, ie.
    //
    //        branch -> A, B
    //     A:
    //        force(x)
    //        ...
    //     B:
    //        force(x)
    //        ...
    //
    // If we find such a case then we lift the force above the branch. Here we
    // exploit the fact that our graph is in edge-split form. We know that
    // branches are the only predecessor of their next blocks. Thus we can only
    // hoist from immediate successors. Otherwise we would need a cfg.

    const static int SEARCH = 5;
    VisitorNoDeoptBranch::run(code->entry, [&](BB* bb) {
        if (bb->isBranch()) {
            auto bb1 = bb->trueBranch();
            auto bb2 = bb->falseBranch();

            if (bb1->isEmpty() || bb2->isEmpty())
                return;

            auto it1 = bb1->begin();
            auto it2 = bb2->begin();

            while (it1 != bb1->end() && it1 - bb1->begin() < SEARCH) {
                if (auto f1 = Force::Cast(*it1)) {
                    while (it2 != bb2->end() && it2 - bb2->begin() < SEARCH) {
                        if (auto f2 = Force::Cast(*it2)) {
                            if (f1->input() == f2->input()) {
                                auto it = bb->end() - 1;
                                auto f = new Force(f1->input(),
                                                   f1->hasEnv() && f2->hasEnv()
                                                       ? f1->env()
                                                       : Env::elided(),
                                                   Tombstone::framestate());
                                bb->insert(it, f);
                                anyChange = true;
                                f1->replaceUsesWith(f);
                                f2->replaceUsesWith(f);
                                it1 = bb1->remove(it1);
                                it2 = bb2->remove(it2);
                            }
                        }
                        if (it2 == bb2->end())
                            break;
                        if ((*it2)->hasObservableEffects())
                            break;
                        it2++;
                    }
                }
                if (it1 == bb1->end())
                    break;
                if ((*it1)->hasObservableEffects())
                    break;
                it1++;
            }
        }
    });
    return anyChange;
}

} // namespace pir
} // namespace rir
