#include "../analysis/dead_store.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

bool DeadStoreRemoval::apply(RirCompiler&, ClosureVersion* cls, Code* code,
                             LogStream& log) const {
    bool noStores = Visitor::check(
        code->entry, [&](Instruction* i) { return !StVar::Cast(i); });
    if (noStores)
        return false;

    bool anyChange = false;
    {
        DeadStoreAnalysis analysis(cls, code, log);

        Visitor::run(code->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                auto next = ip + 1;
                if (auto st = StVar::Cast(*ip)) {
                    if (analysis.isDead(st)) {
                        next = bb->remove(ip);
                        anyChange = true;
                    }
                }
                ip = next;
            }
        });

        VisitorNoDeoptBranch::runBackward(code->entry, [&](BB* bb) {
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
                        anyChange = true;
                        next = bb->remove(ip);
                        continue;
                    }
                }
                ip = next;
            }
        });
    }
    return anyChange;
}

} // namespace pir
} // namespace rir
