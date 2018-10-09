#include "../pir/pir_impl.h"
#include "../transform/replace.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void CleanupFrameState::apply(Closure* function) const {
    auto apply = [](Code* code) {
        Visitor::run(code->entry, [&](BB* bb) {
            auto it = bb->begin();
            while (it != bb->end()) {
                auto next = it + 1;
                if (auto sp = FrameState::Cast(*it)) {
                    if (sp->unused())
                        next = bb->remove(it);
                }
                it = next;
            }
        });
    };
    apply(function);
    for (auto& p : function->promises)
        if (p)
            apply(p);
}
} // namespace pir
} // namespace rir
