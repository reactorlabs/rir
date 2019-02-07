#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void ElideEnvSpec::apply(RirCompiler&, ClosureVersion* function,
                         LogStream&) const {
    // Elide environments of binary operators in which both operators are
    // primitive values

    Visitor::runPostChange(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (i->isSpeculable()) {
                /* The following if is here just because we do not have
                 * framestates in promises. Then if we inline the promise
                 * and then try to speculate it will fail. A good story
                 * for where to jump back in case a speculation fail on an
                 * inlined promise is still needed I guess
                 */
                if (i->frameState() != Tombstone::framestate()) {
                    if (i->envOnlyForObj() && i->hasEnv()) {
                        bool objectExpected = false;

                        i->eachArg([&](Value* arg) {
                            if (arg != i->env())
                                if (arg->type.maybeObj() &&
                                    arg->typeFeedback.maybeObj())
                                    objectExpected = true;
                        });

                        if (!objectExpected) {
                            i->elideEnv();
                            auto cp =
                                BBTransform::addDeopt(bb, ip, i->frameState());
                            auto split = bb->trueBranch();
                            i->eachArg([&](Value* arg) {
                                if (arg != i->env() && arg != i->frameState())
                                    if (arg->type.maybeObj()) {
                                        Instruction* condition =
                                            (new TypeTest(arg))->object();
                                        auto ip2 = split->begin();
                                        BBTransform::insertAssume(
                                            split, condition, cp, ip2, false);
                                    }
                            });
                            i->clearFrameState();
                            next = bb->end();
                            i->type.setNotObject();
                        }
                    }
                }
            }
            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
