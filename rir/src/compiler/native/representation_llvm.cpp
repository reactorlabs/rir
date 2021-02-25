#include "representation_llvm.h"
#include "compiler/pir/pir_impl.h"

namespace rir {
namespace pir {

Representation Representation::Of(PirType t) {
    // Combined types like integer|real cannot be unbox, since we do not know
    // how to re-box again.
    if (!t.maybeMissing() && !t.maybePromiseWrapped()) {
        if (t.isA(PirType(RType::logical).scalar().notObject()))
            return Representation::Integer;
        if (t.isA(PirType(RType::integer).scalar().notObject()))
            return Representation::Integer;
        if (t.isA(PirType(RType::real).scalar().notObject()))
            return Representation::Real;
    }
    return Representation::Sexp;
}

Representation Representation::Of(Value* v) { return Of(v->type); }

} // namespace pir
} // namespace rir
