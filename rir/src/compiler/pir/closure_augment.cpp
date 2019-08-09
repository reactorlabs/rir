#include "closure_augment.h"
#include "../transform/bb.h"
#include "../util/visitor.h"
#include "closure.h"
#include "pir_impl.h"

#include <iostream>

namespace rir {
namespace pir {

ClosureAugments ClosureAugments::operator|(const ClosureAugments& o) const {
    return ClosureAugments((EnumSet<ClosureAugment>)*this |
                               (EnumSet<ClosureAugment>)o,
                           signature | o.signature);
}

std::ostream& operator<<(std::ostream& out, const ClosureAugment& p) {
    switch (p) {
#define V(Aug)                                                                 \
    case ClosureAugment::Aug:                                                  \
        out << #Aug;                                                           \
        break;
        LIST_OF_CLOSURE_AUGMENTS(V);
#undef V
    default:
        assert(false);
        break;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out, const ClosureAugments& augs) {
    for (auto p = augs.begin(); p != augs.end(); ++p) {
        out << *p;
        if (!augs.signature.isAny() || (p + 1) != augs.end())
            out << ", ";
    }
    if (!augs.signature.isAny())
        out << "Signature: " << augs.signature;
    return out;
}

} // namespace pir
} // namespace rir
