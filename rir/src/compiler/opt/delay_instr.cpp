#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void DelayInstr::apply(RirCompiler&, ClosureVersion* function,
                       LogStream&) const {
    Visitor::run(function->entry, [&](BB* bb) {
        Checkpoint* checkpoint =
            bb->isEmpty() ? nullptr : Checkpoint::Cast(bb->last());
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

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
                    if (usage && FrameState::Cast(usage) &&
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
