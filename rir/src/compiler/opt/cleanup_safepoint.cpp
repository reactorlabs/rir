#include "cleanup_safepoint.h"
#include "../pir/pir_impl.h"
#include "../transform/replace.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

#include <unordered_set>

namespace rir {
namespace pir {

void CleanupSafepoint::apply(Closure* function) const {
    std::unordered_map<Deopt*, Safepoint*> scheduled;

    auto apply = [](Code* code) {
        Visitor::run(code->entry, [&](BB* bb) {
            for (auto it = bb->begin(); it != bb->end(); ++it) {
                if (auto sp = Safepoint::Cast(*it)) {
                    if (sp->unused())
                        it = bb->remove(it);
                }
                if (it == bb->end())
                    break;
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
