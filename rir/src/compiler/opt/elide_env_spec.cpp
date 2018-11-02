#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void ElideEnvSpec::apply(Closure* function) const {

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
        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;

            if (i->maySpecialize() && i->env() != Env::elided()) {
                ProfiledValues* profile = &function->runtimeFeedback;
                Value* opLeft = i->arg(0).val();
                Value* opRight = i->arg(1).val();
                bool maybeObjL = !LdConst::Cast(opLeft) ||
                                 isObject(LdConst::Cast(opLeft)->c);
                bool maybeObjR = !LdConst::Cast(opRight) ||
                                 isObject(LdConst::Cast(opRight)->c);
                bool assumeNonObjL =
                    profile->hasTypesFor(opLeft) &&
                    !profile->types.at(opLeft).observedObject();
                bool assumeNonObjR =
                    profile->hasTypesFor(opRight) &&
                    !profile->types.at(opRight).observedObject();

                if (assumeNonObjL && assumeNonObjR) {
                    i->elideEnv();
                    if (maybeObjL)
                        ip = insertExpectAndAdvance(bb, opLeft, ip);
                    if (maybeObjR)
                        ip = insertExpectAndAdvance(bb, opRight, ip);
                }
            } else {
                if (Checkpoint* cp = Checkpoint::Cast(i))
                    checkpoints.emplace(bb->trueBranch(), cp);
            }
            ip++;
        }
    });
}
} // namespace pir
} // namespace rir
