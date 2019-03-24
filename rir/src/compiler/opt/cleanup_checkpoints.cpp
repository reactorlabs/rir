#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void CleanupCheckpoints::apply(RirCompiler&, ClosureVersion* function,
                               LogStream&) const {
    auto apply = [](Code* code) {
        std::unordered_set<BB*> toDelete;
        Visitor::run(code->entry, [&](BB* bb) {
            if (bb->isEmpty())
                return;
            if (auto cp = Checkpoint::Cast(bb->last())) {
                if (cp->unused()) {
                    bb->remove(bb->end() - 1);
                    toDelete.insert(bb->next1);
                    assert(bb->next1->isExit() &&
                           "deopt blocks should be just one BB");
                    bb->next1 = nullptr;
                }
            }
        });
        // Deopt blocks are exit blocks. They have no other predecessors and
        // are not phi inputs. We can delete without further checks.
        for (auto bb : toDelete)
            delete bb;
    };
    apply(function);
    function->eachPromise([&](Promise* p) { apply(p); });
}
} // namespace pir
} // namespace rir
