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
                    if (analysis.isDead(st)) {
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
