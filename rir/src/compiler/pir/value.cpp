#include "value.h"
#include "instruction.h"
#include "pir_impl.h"
#include <unordered_map>

namespace rir {
namespace pir {

void Value::callArgTypeToContext(Context& assumptions, unsigned i) const {
    // this is for function arguments
    auto arg = this;
    assert(!arg->type.maybePromiseWrapped());

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
        assumptions.setEager(i);
    }

    if (arg == MissingArg::instance()) {
        assumptions.remove(Assumption::NoExplicitlyMissingArgs);
        return;
    }

    assert(arg != UnboundValue::instance());

    auto check = [&](const Value* arg) {
        if (!arg->type.maybeLazy())
            assumptions.setEager(i);
        if (!arg->type.maybeObj()) {
            assumptions.setNotObj(i);
            if (arg->type.isSimpleScalar()) {
                if (arg->type.isRType(RType::real))
                    assumptions.setSimpleReal(i);
                if (arg->type.isRType(RType::integer))
                    assumptions.setSimpleInt(i);
            }
        }
    };
    check(arg);
    arg = arg->cFollowCasts();
    if (!MkArg::Cast(arg))
        check(arg);
    arg = arg->cFollowCastsAndForce();
    if (!MkArg::Cast(arg))
        check(arg);
}
}
}
