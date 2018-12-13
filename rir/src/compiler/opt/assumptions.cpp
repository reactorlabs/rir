#include "../pir/pir_impl.h"
#include "../translations/pir_translator.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

void OptimizeAssumptions::apply(RirCompiler&, Closure* function,
                                LogStream&) const {
    CFG cfg(function);
    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto instr = *ip;

            if (auto assume = Assume::Cast(instr)) {
                auto cp = assume->checkpoint();
                auto bb = cp->bb();
                // We are trying to group multiple assumes into the same
                // checkpoint by finding for each assume the topmost compatible
                // checkpoint.
                // TODO: we could also try to move up the assume itself, since
                // if we move both at the same time, we could even jump over
                // effectful instructions.
                while (true) {
                    if (!cfg.hasSinglePred(bb))
                        break;

                    bool hasEffect = false;
                    for (auto i : *bb)
                        if (i->hasEffect())
                            hasEffect = true;
                    if (hasEffect)
                        break;
                    bb = cfg.immediatePredecessors(bb).front();

                    if (!bb->isEmpty())
                        if (auto prevCp = Checkpoint::Cast(bb->last()))
                            cp = prevCp;
                }
                assume->checkpoint(cp);
            }

            ip = next;
        }
    });
}

} // namespace pir
} // namespace rir
