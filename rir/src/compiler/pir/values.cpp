#include "values.h"

#include "tag.h"

#include "R/Printing.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

DeoptReasonWrapper::DeoptReasonWrapper(const DeoptReason& r)
    : ValueImpl(NativeType::deoptReason), reason(r) {}

void DeoptReasonWrapper::printRef(std::ostream& out) const { out << reason; }

DeoptReasonWrapper* DeoptReasonWrapper::unknown() {
    static DeoptReasonWrapper instance(DeoptReason::unknown());
    return &instance;
}

Const::Const(BC::PoolIdx idx, PirType type) : ValueImpl(type), idx(idx) {}

SEXP Const::c() const { return Pool::get(idx); }

void Const::printRef(std::ostream& out) const {
    if (c() == R_UnboundValue) {
        out << "unboundValue";
        return;
    }
    out << Print::dumpSexp(Pool::get(idx), 40);
}

} // namespace pir
} // namespace rir
