#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

bool CleanupFramestate::apply(RirCompiler&, ClosureVersion* function,
                              LogStream&) const {
    bool anyChange = false;
    auto apply = [&](Code* code) {
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto call = CallInstruction::CastCall(i)) {
                if (call->hasFrameState()) {
                    anyChange = true;
                    call->clearFrameState();
                }
            }
        });
    };
    apply(function);
    function->eachPromise([&](Promise* p) { apply(p); });
    return anyChange;
}
} // namespace pir
} // namespace rir
