#include "closure_signature.h"
#include "../transform/bb.h"
#include "../util/visitor.h"
#include "closure.h"
#include "pir_impl.h"

#include <iostream>

namespace rir {
namespace pir {

ClosureSignature ClosureSignature::operator|(const ClosureSignature& o) const {
    return ClosureSignature((EnumSet<ClosureSignatureFlag>)*this |
                                (EnumSet<ClosureSignatureFlag>)o,
                            type | o.type);
}

std::ostream& operator<<(std::ostream& out, const ClosureSignatureFlag& p) {
    switch (p) {
#define V(Aug)                                                                 \
    case ClosureSignatureFlag::Aug:                                            \
        out << #Aug;                                                           \
        break;
        LIST_OF_CLOSURE_SIGNATURE_FLAGS(V);
#undef V
    default:
        assert(false);
        break;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out, const ClosureSignature& sig) {
    for (auto p = sig.begin(); p != sig.end(); ++p) {
        out << *p;
        if (!sig.type.isAny() || (p + 1) != sig.end())
            out << ", ";
    }
    if (!sig.type.isAny())
        out << sig.type;
    return out;
}

} // namespace pir
} // namespace rir
