#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

bool CleanupFramestate::apply(Compiler&, ClosureVersion* function, Code* code,
                              LogStream&) const {
    bool anyChange = false;
    Visitor::run(code->entry, [&](Instruction* i) {
        if (!Deopt::Cast(i) && i->frameState()) {
            anyChange = true;
            i->clearFrameState();
        }
    });
    return anyChange;
}
} // namespace pir
} // namespace rir
