#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void DelayInstr::apply(RirCompiler&, ClosureVersion* function,
                       LogStream&) const {

    std::unordered_map<Instruction*, BB*> usedOnlyInDeopt;

    auto isTarget = [](Instruction* j) {
        return LdFun::Cast(j) || MkArg::Cast(j) || DotsList::Cast(j) ||
               FrameState::Cast(j) || CastType::Cast(j);
    };

    Visitor::run(function->entry, [&](Instruction* i) {
        i->eachArg([&](Value* v) {
            if (auto j = Instruction::Cast(v)) {
                if (isTarget(j)) {
                    auto& u = usedOnlyInDeopt[j];
                    if (i->bb()->isDeopt() || usedOnlyInDeopt[i]) {
                        if (!u)
                            u = i->bb();
                        else
                            u = (BB*)-1;
                    } else {
                        u = (BB*)-1;
                    }
                }
            }
        });
    });

    Visitor::run(function->entry, [&](BB* bb) {
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
            if (isTarget(i)) {
                if (usedOnlyInDeopt.count(i)) {
                    auto u = usedOnlyInDeopt.at(i);
                    if (u && u != (BB*)-1 && u != i->bb()) {
                        next = bb->moveToBegin(ip, u);
                    }
                }
            }

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
