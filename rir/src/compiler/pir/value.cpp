#include "value.h"
#include "instruction.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Value::typeToContext(Context& assumptions, unsigned i) const {
    auto arg = this;
    if (auto mk = MkArg::Cast(arg)) {
        if (mk->isEager()) {
            if (mk->eagerArg() == MissingArg::instance())
                assumptions.remove(Assumption::NoExplicitlyMissingArgs);
            else
                assumptions.setEager(i);
        }
        if (!mk->noReflection)
            assumptions.setNonRefl(i);
    } else {
        auto value = arg->cFollowCastsAndForce();
        if (value == MissingArg::instance()) {
            assumptions.remove(Assumption::NoExplicitlyMissingArgs);
        } else {
            assumptions.setNonRefl(i);
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
}
