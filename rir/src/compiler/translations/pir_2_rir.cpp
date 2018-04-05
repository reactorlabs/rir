#include "pir_2_rir.h"
#include "../../ir/CodeStream.h"
#include "../../utils/FunctionWriter.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "interpreter/runtime.h"
#include "ir/CodeVerifier.h"

namespace rir {
namespace pir {

rir::Function* Pir2Rir::operator()(Function* fun) {

    // For each Phi, insert copies
    BreadthFirstVisitor::run(fun->entry, [&](BB* bb) {
        std::vector<Instruction*> phiCopies;
        for (auto instr : *bb) {
            Phi* phi = Phi::Cast(instr);
            if (phi) {
                for (size_t i = 0; i < phi->nargs(); ++i) {
                    BB* pred = phi->input[i];
                    // pred is either jump (insert copy at end) or branch
                    // (insert copy before the branch instr)
                    auto it = pred->isJmp() ? pred->end() : pred->end() - 1;
                    Instruction* iav = Instruction::Cast(phi->arg(i).val());
                    auto copy = pred->insert(it, new PirCopy(iav));
                    phi->arg(i).val() = *copy;
                }
                auto phiCopy = new PirCopy(phi);
                phiCopies.push_back(phiCopy);
                phi->replaceUsesWith(phiCopy);
            }
        }
        // find last phi in bb, insert all copies after it
        auto lastPhi = bb->end();
        for (auto it = bb->begin(); it != bb->end(); ++it)
            if (Phi::Cast(*it))
                lastPhi = it;
        ++lastPhi;
        bb->insert(lastPhi, phiCopies);
    });

    // std::cout << "--- phi copies inserted ---\n";
    // fun->print(std::cout);

    typedef size_t LocalSlotIdx;
    LocalSlotIdx maxLocalIdx = 0;
    std::unordered_map<Value*, LocalSlotIdx> alloc;
    BreadthFirstVisitor::run(fun->entry, [&](Instruction* instr) {
        Phi* phi = Phi::Cast(instr);
        if (phi) {
            alloc[phi] = maxLocalIdx;
            phi->eachArg([&](Value* arg) {
                assert(alloc.count(arg) == 0);
                alloc[arg] = maxLocalIdx;
            });
            ++maxLocalIdx;
        }
    });
    BreadthFirstVisitor::run(fun->entry, [&](Instruction* instr) {
        if (instr->type != PirType::voyd() && alloc.count(instr) == 0)
            alloc[instr] = maxLocalIdx++;
    });

    // std::cout << "--- locals allocation ---\n";
    // for (auto x : alloc) {
    //     std::cout << "  ";
    //     x.first->printRef(std::cout);
    //     std::cout << " -> " << x.second << "\n";
    // }
    // std::cout << "total slots needed: " << maxLocalIdx << "\n";

    return nullptr;
}

} // namespace pir
} // namespace rir
