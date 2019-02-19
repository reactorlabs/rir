#include "../analysis/available_checkpoints.h"
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
                         LogStream& log) const {

    AvailableCheckpoints checkpoint(function, log);

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (i->envOnlyForObj() && i->hasEnv()) {
                bool objectExpected = false;

                i->eachArg([&](Value* arg) {
                    if (arg != i->env())
                        if (arg->type.maybeObj() &&
                            arg->typeFeedback.maybeObj())
                            objectExpected = true;
                });

                if (!objectExpected) {
                    if (auto cp = checkpoint.at(i)) {
                        i->elideEnv();
                        i->eachArg([&](Value* arg) {
                            if (arg != i->env())
                                if (arg->type.maybeObj()) {
                                    auto condition = new IsObject(arg);
                                    ip = bb->insert(ip, condition);
                                    ip++;
                                    ip = bb->insert(
                                        ip, (new Assume(condition, cp))->Not());
                                    ip++;
                                }
                        });
                        next = ip + 1;
                        i->type.setNotObject();
                    }
                }
            }

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
