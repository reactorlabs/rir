#include "representation_llvm.h"
#include "compiler/pir/pir_impl.h"

namespace rir {
namespace pir {

Rep Rep::Of(PirType t) {
    // Combined types like integer|real cannot be unbox, since we do not know
    // how to re-box again.
    if (!t.maybeMissing() && !t.maybePromiseWrapped()) {
        if (t.isA(NativeType::deoptReason)) {
            return Rep::DeoptReason;
        }
        if (t.isA(PirType(RType::logical).simpleScalar().notObject())) {
            assert(t.unboxable());
            return Rep::i32;
        }
        if (t.isA(PirType(RType::integer).simpleScalar().notObject())) {
            assert(t.unboxable());
            return Rep::i32;
        }
        if (t.isA(PirType(RType::real).simpleScalar().notObject())) {
            assert(t.unboxable());
            return Rep::f64;
        }
    }
    assert(!t.unboxable());
    return Rep::SEXP;
}

Rep Rep::Of(Value* v) { return Of(v->type); }

} // namespace pir
} // namespace rir
