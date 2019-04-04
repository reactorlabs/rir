#include "ConvertAssumptions.h"
#include "../pir/pir_impl.h"

namespace rir {
namespace pir {

void readArgTypeFromAssumptions(const Assumptions& assumptions, PirType& type,
                                int i) {
    if (assumptions.isEager(i))
        type = PirType::promiseWrappedVal().notMissing();
    if (assumptions.isNotObj(i))
        type.setNotObject();
    if (assumptions.isSimpleReal(i)) {
        assert(assumptions.isEager(i) && assumptions.isNotObj(i));
        type.setScalar(RType::real);
    }
    if (assumptions.isSimpleInt(i)) {
        assert(assumptions.isEager(i) && assumptions.isNotObj(i) &&
               !assumptions.isSimpleReal(i));
        type.setScalar(RType::integer);
    }
}

void writeArgTypeToAssumptions(Assumptions& assumptions, Value* arg, int i) {
    auto mk = MkArg::Cast(arg);
    if (mk && mk->isEager()) {
        assumptions.setEager(i);
        if (mk->eagerArg() == MissingArg::instance())
            assumptions.remove(Assumption::NoExplicitlyMissingArgs);
    }
    Value* value = arg->followCastsAndForce();
    if (!MkArg::Cast(value)) {
        if (!value->type.maybeObj())
            assumptions.setNotObj(i);
        if (assumptions.isEager(i) && assumptions.isNotObj(i) &&
            value->type.isScalar()) {
            assert(value->type.isRType());
            if (value->type == RType::real)
                assumptions.setSimpleReal(i);
            if (value->type == RType::integer)
                assumptions.setSimpleInt(i);
        }
    }
}

} // namespace pir
} // namespace rir
