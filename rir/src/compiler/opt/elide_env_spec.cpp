#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void ElideEnvSpec::apply(RirCompiler&, Closure* function, LogStream&) const {
    // Elide environments of binary operators in which both operators are
    // primitive values
    std::unordered_map<BB*, Checkpoint*> checkpoints;

    auto insertExpect = [&](BB* src, Value* condition, Value* operand,
                            Checkpoint* cp, BB::Instrs::iterator position) {
        assert(checkpoints[src]);
        position = src->insert(position, (new Assume(condition, cp))->Not());
        position++;
        return position;
    };
    auto insertExpectNotObj = [&](BB* src, Value* operand, Checkpoint* cp,
                                  BB::Instrs::iterator position) {
        auto condition = new IsObject(operand);
        position = src->insert(position, condition);
        position++;
        return insertExpect(src, condition, operand, cp, position);
    };

    Visitor::run(function->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;
        if (auto cp = Checkpoint::Cast(bb->last()))
            checkpoints.emplace(bb->trueBranch(), cp);
    });

    Visitor::run(function->entry, [&](BB* bb) {
        if (!checkpoints.count(bb))
            return;

        auto cp = checkpoints.at(bb);

        auto ip = bb->begin();
        while (ip != bb->end()) {
            Instruction* i = *ip;
            auto next = ip + 1;

            if (i->envOnlyForObj() && i->hasEnv()) {
                Value* opLeft = i->arg(0).val();
                Value* opRight = i->arg(1).val();
                bool noObjL = !opLeft->type.maybeObj();
                bool noObjR = !opRight->type.maybeObj();
                bool seenObjL = opLeft->typeFeedback.maybeObj();
                bool seenObjR = opRight->typeFeedback.maybeObj();

                if ((noObjL || !seenObjL) && (noObjR || !seenObjR)) {
                    i->elideEnv();
                    if (!noObjL)
                        ip = insertExpectNotObj(bb, opLeft, cp, ip);
                    if (!noObjR)
                        ip = insertExpectNotObj(bb, opRight, cp, ip);
                    next = ip + 1;
                    i->type.setNotObject();
                }
            }

            // Effect between assume and checkpoint is not allowed
            if (i->changesEnv() || i->hasEffect())
                break;

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
