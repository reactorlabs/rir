#include "value.h"
#include "instruction.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Value::callArgTypeToContext(Context& assumptions, unsigned i) const {
    // this is for function arguments
    auto arg = this;
    assert(!arg->type.maybePromiseWrapped());

    if (arg == MissingArg::instance()) {
        assumptions.remove(Assumption::NoExplicitlyMissingArgs);
        return;
    }

    if (auto mk = MkArg::Cast(arg)) {
        if (mk->isEager() || mk->noReflection)
            assumptions.setNonRefl(i);

        if (mk->isEager()) {
            assumptions.setEager(i);
            arg = mk->eagerArg();
        } else {
            return;
        }
    } else {
        assumptions.setNonRefl(i);
    }

    assert(arg != UnboundValue::instance());

    auto value = arg->cFollowCastsAndForce();
    if (value != MissingArg::instance()) {
        if (!value->type.maybeLazy())
            assumptions.setEager(i);
        if (!value->type.maybeObj()) {
            assumptions.setNotObj(i);
            if (!value->type.maybeHasAttrs() && value->type.isScalar()) {
                if (value->type.isRType(RType::real))
                    assumptions.setSimpleReal(i);
                if (value->type.isRType(RType::integer))
                    assumptions.setSimpleInt(i);
            }
        }
    }
}
}
}
