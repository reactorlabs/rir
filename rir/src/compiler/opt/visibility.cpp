#include "../analysis/visibility.h"
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

void OptimizeVisibility::apply(RirCompiler&, ClosureVersion* function,
                               LogStream& log) const {
    VisibilityAnalysis visible(function, log);

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto instr = *ip;

            if (auto vis = Visible::Cast(instr)) {
                if (!visible.observed(vis)) {
                    next = bb->remove(ip);
                }
            } else if (auto vis = Invisible::Cast(instr)) {
                if (!visible.observed(vis)) {
                    next = bb->remove(ip);
                }
            } else if (instr->effects.contains(Effect::Visibility)) {
                if (!visible.observed(instr) &&
                    !instr->effects.contains(Effect::Reflection)) {
                    instr->effects.reset(Effect::Visibility);
                }
            }

            ip = next;
        }
    });
}

} // namespace pir
} // namespace rir
