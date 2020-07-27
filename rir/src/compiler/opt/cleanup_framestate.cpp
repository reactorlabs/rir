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
            if (auto call = CallInstruction::CastCall(i)) {
                if (call->hasFrameState()) {
                    anyChange = true;
                    call->clearFrameState();
                }
            }
        });
    return anyChange;
}
} // namespace pir
} // namespace rir
