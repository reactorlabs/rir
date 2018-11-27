#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void insertCheckpoints::apply(RirCompiler&, Closure* function) const {
    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;
        auto ip = bb->begin();
        auto ipPlus = ip + 1;
        while (ipPlus != bb->end()) {
            Instruction* next = *(ipPlus);

            if (FrameState::Cast(*ip)) {
                if (next->maySpecialize() && next->env() != Env::elided()) {
                    BBTransform::addCheckpoint(function, bb, ip);
                    break;
                }
            }
            ip = ipPlus++;
        }
    });
}
} // namespace pir
} // namespace rir
