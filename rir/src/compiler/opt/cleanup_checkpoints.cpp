#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void CleanupCheckpoints::apply(RirCompiler&, Closure* function,
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
        BBTransform::removeBBs(code, toDelete);
    };
    apply(function);
    for (auto& p : function->promises)
        if (p)
            apply(p);
}
} // namespace pir
} // namespace rir
