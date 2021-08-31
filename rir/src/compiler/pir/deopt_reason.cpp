#include "deopt_reason.h"

#include "tag.h"

namespace rir {
namespace pir {

DeoptReasonWrapper::DeoptReasonWrapper(const DeoptReason& r)
    : Value(NativeType::deoptReason, Tag::DeoptReason), reason(r) {}

void DeoptReasonWrapper::printRef(std::ostream& out) const { out << reason; }

DeoptReasonWrapper* DeoptReasonWrapper::unknown() {
    static DeoptReasonWrapper instance(DeoptReason::unknown());
    return &instance;
}

} // namespace pir
} // namespace rir
