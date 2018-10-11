#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void AdaptForSpec::apply(Closure* function) const {

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            Instruction* next = *(ip + 1);

            if (FrameState::Cast(i)) {
                if ((ip + 1) != bb->end() && next->maySpecialize() &&
                    next->env() != Env::elided()) {
                    BBTransform::addCheckpoint(function, bb, ip);
                    break;
                }
            }
            ip++;
        }

        /*
         * Now lets try to move the instructions previous to the framestate
         * to the newly created bb because that will probably make
         * the code easier to read
         */

        /*ip = bb->end() - 2;
        while (ip != bb->begin()) {
            ip--;
            Instruction* i = *ip;
            if (!(i->hasEffect() | i->mayAccessEnv()))
                bb->moveToBegin(ip, split);
            else
                break;
        }
        */
    });
}
} // namespace pir
} // namespace rir
