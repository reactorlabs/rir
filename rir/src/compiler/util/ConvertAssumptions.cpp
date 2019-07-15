#include "ConvertAssumptions.h"
#include "../pir/pir_impl.h"

namespace rir {
namespace pir {

void readArgTypeFromAssumptions(const Assumptions& assumptions, PirType& type,
                                int i) {
    if (assumptions.isEager(i))
        type = type.notLazy().notMissing();
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
    if (auto mk = MkArg::Cast(arg)) {
        if (mk->isEager()) {
            if (mk->eagerArg() == MissingArg::instance())
                assumptions.remove(Assumption::NoExplicitlyMissingArgs);
            else
                assumptions.setEager(i);
        }
        if (!mk->noReflection)
            assumptions.remove(Assumption::NoReflectiveArgument);
    } else {
        Value* value = arg->followCastsAndForce();
        if (value == MissingArg::instance()) {
            assumptions.remove(Assumption::NoExplicitlyMissingArgs);
        } else {
            if (!value->type.maybeObj())
                assumptions.setNotObj(i);
            if (assumptions.isEager(i) && assumptions.isNotObj(i) &&
                value->type.isScalar()) {
                assert(value->type.isRType());
                if (value->type.isRType(RType::real))
                    assumptions.setSimpleReal(i);
                if (value->type.isRType(RType::integer))
                    assumptions.setSimpleInt(i);
            }
        }
    }
}

} // namespace pir
} // namespace rir
