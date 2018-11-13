#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void ElideEnvSpec::apply(RirCompiler&, Closure* function) const {

    // Elide environments of binary operators in which both operators are
    // primitive values
    std::unordered_map<BB*, Checkpoint*> checkpoints;

    auto insertExpectAndAdvance = [&](BB* src, Value* operand,
                                      BB::Instrs::iterator position) {
        Instruction* condition = new IsObject(operand);

        position = src->insert(position, condition);
        position++;
        assert(checkpoints[src]);
        position =
            src->insert(position, new assumeNot(condition, checkpoints[src]));
        position++;
        return position;
    };

    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;

        if (auto cp = Checkpoint::Cast(bb->last()))
            checkpoints.emplace(bb->trueBranch(), cp);

        if (!checkpoints.count(bb))
            return;

        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;

            if (i->maySpecialize() && i->env() != Env::elided()) {
                ProfiledValues* profile = &function->runtimeFeedback;
                Value* opLeft = i->arg(0).val();
                Value* opRight = i->arg(1).val();
                bool maybeObjL = opLeft->type.maybeObj();
                bool maybeObjR = opRight->type.maybeObj();
                bool assumeNonObjL =
                    profile->hasTypesFor(opLeft) &&
                    !profile->types.at(opLeft).observedObject();
                bool assumeNonObjR =
                    profile->hasTypesFor(opRight) &&
                    !profile->types.at(opRight).observedObject();

                if ((!maybeObjL && !maybeObjR) ||
                    (assumeNonObjL && assumeNonObjR)) {
                    i->elideEnv();
                    if (maybeObjL)
                        ip = insertExpectAndAdvance(bb, opLeft, ip);
                    if (maybeObjR)
                        ip = insertExpectAndAdvance(bb, opRight, ip);
                    i->type = i->type.notObject();
                }
            }
            ip++;
        }
    });
}
} // namespace pir
} // namespace rir
