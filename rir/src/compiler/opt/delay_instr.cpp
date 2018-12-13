#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void DelayInstr::apply(RirCompiler&, Closure* function, LogStream&) const {
    std::vector<MkEnv*> envs;

    Visitor::run(function->entry, [&](BB* bb) {
        Checkpoint* checkpoint =
            bb->isEmpty() ? nullptr : Checkpoint::Cast(bb->last());
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

            if (!i->hasEnv() && !i->hasEffect() && !Phi::Cast(i) &&
                !i->branchOrExit()) {
                Instruction* usage = i->hasSingleUse();
                if (usage && usage->bb() != bb) {
                    auto phi = Phi::Cast(usage);
                    if (phi) {
                        // If the usage is a phi, we make sure to keep this
                        // value only in the input branch, where it is
                        // actually needed.
                        for (size_t j = 0; j < phi->nargs(); ++j) {
                            if (phi->arg(j).val() == i) {
                                if (phi->input[j] != bb) {
                                    next = bb->moveToEnd(ip, phi->input[j]);
                                }
                                break;
                            }
                        }
                    } else {
                        next = bb->moveToBegin(ip, usage->bb());
                    }
                } else if (usage && usage != *next) {
                    auto swap = ip;
                    while (swap + 1 != bb->end() && *(swap + 1) != usage) {
                        bb->swapWithNext(swap);
                        ++swap;
                    }
                }
            }

            /*
             * The Idea with the following line is to convert code produced by
             * speculative call target resolution. We want to convert the
             * following code:
             *
             *          f = ldfun("f")
             *          c = checkpoint()  -> BB1 | BB2
             *      BB2:
             *          deopt(stack=[f])
             *      BB1:
             *          assume (f==someFunction, c)
             *
             * into this:
             *
             *          c = checkpoint()  -> BB1 | BB2
             *      BB2:
             *          f1 = ldfun("f")
             *          deopt(stack=[f1])
             *      BB1:
             *          f2 = ldvar("f")
             *          assume (f2==someFunction, c)
             *
             * The later is better because f2 (in the main branch) does not have
             * sideeffects and the sideeffecting ldfun is moved to the deopt
             * branch.
             */
            if (checkpoint) {
                if (auto ldfun = LdFun::Cast(i)) {
                    auto usage = ldfun->hasSingleUse();
                    if (usage && Deopt::Cast(usage) &&
                        usage->bb() == checkpoint->deoptBranch()) {
                        next = bb->moveToBegin(ip, usage->bb());
                    }
                }
            }

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
