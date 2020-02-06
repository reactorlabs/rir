#include "../analysis/dead_store.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void DeadStoreRemoval::apply(RirCompiler&, ClosureVersion* function,
                             LogStream& log) const {
    bool noStores = Visitor::check(
        function->entry, [&](Instruction* i) { return !StVar::Cast(i); });
    if (noStores)
        return;

    {
        DeadStoreAnalysis analysis(function, log);

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                auto next = ip + 1;
                if (auto st = StVar::Cast(*ip)) {
                    if (analysis.isDead(st)) 
                        next = bb->remove(ip);
                }
                ip = next;
            }
        });

        VisitorNoDeoptBranch::runBackward(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                auto next = ip + 1;
                if (auto st = StVar::Cast(*ip)) {
                    if (analysis.onlyObservedByDeopt(st)) {
                        std::unordered_set<BB*> copied;
                        for (auto instruction : analysis.deoptInstructionsFor(st)) {
                            if (!copied.count(instruction->bb())) {
                                instruction->bb()->insert(
                                    instruction->bb()->begin(), st->clone());
                                copied.insert(instruction->bb());
                            }
                        }
                        next = bb->remove(ip);
                        continue;
                    }
                }
                ip = next;
            }
        });
    }
}

} // namespace pir
} // namespace rir
