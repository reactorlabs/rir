#include "../analysis/visibility.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"

#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool OptimizeVisibility::apply(Compiler&, ClosureVersion* cls, Code* code,
                               LogStream& log) const {
    VisibilityAnalysis visible(cls, code, log);

    bool anyChange = false;
    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto instr = *ip;

            if (auto vis = Visible::Cast(instr)) {
                if (!visible.observed(vis)) {
                    anyChange = true;
                    next = bb->remove(ip);
                }
            } else if (auto vis = Invisible::Cast(instr)) {
                if (!visible.observed(vis)) {
                    anyChange = true;
                    next = bb->remove(ip);
                }
            } else if (instr->effects.contains(Effect::Visibility)) {
                if (!visible.observed(instr)) {
                    anyChange = true;
                    instr->effects.reset(Effect::Visibility);
                }
            }

            ip = next;
        }
    });

    return anyChange;
}

} // namespace pir
} // namespace rir
