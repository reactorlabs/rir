#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void DelayInstr::apply(RirCompiler&, ClosureVersion* function,
                       LogStream&) const {

    std::unordered_map<Instruction*, SmallSet<BB*>> usedOnlyInDeopt;

    auto isTarget = [](Instruction* j) {
        return LdFun::Cast(j) || MkArg::Cast(j) || DotsList::Cast(j) ||
               FrameState::Cast(j) || CastType::Cast(j);
    };

    CFG cfg(function);
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
                                    usedOnlyInDeopt[j].insert(targetBB);
                            } else {
                                u.insert((BB*)-1);
                            }
                        }
                    }
                }
            });
        });
    }

    for (auto entry : cfg.exits()) {
        Visitor::runBackward(entry, cfg, [&](BB* bb) {
        if (bb->isDeopt())
            return;

        auto ip = bb->rbegin();
        while (ip != bb->rend()) {
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
                if (!usedOnlyInDeopt[i].empty() && !usedOnlyInDeopt[i].count((BB*)-1)) {
                    std::cout << "entre";
                    i->print(std::cout);
                    std::cout << "\n";
                    for (auto targetBB : usedOnlyInDeopt[i]){
                        auto newInstr = i->clone();
                        targetBB->insert(targetBB->begin(), newInstr);
                        i->replaceUsesIn(newInstr, targetBB);
                    }
                    bb->remove(bb->atPosition(i));
                }
            }
            ip = next;
        }
    });}
}
} // namespace pir
} // namespace rir
