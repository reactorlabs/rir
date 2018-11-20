#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void CleanupFramestate::apply(RirCompiler&, Closure* function,
                              LogStream&) const {
    auto apply = [](Code* code) {
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto call = CallInstruction::CastCall(i)) {
                call->clearFrameState();
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
