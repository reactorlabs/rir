#pragma once

#include "pir.h"
#include "type_signature.h"
#include <functional>
#include <sstream>
#include <unordered_map>

namespace rir {
namespace pir {

// Remember to update ClosureSignatureFlag::LAST
#define LIST_OF_CLOSURE_SIGNATURE_FLAGS(V)                                     \
    V(NoSuperAssign)                                                           \
    V(NoReflection)

enum class ClosureSignatureFlag {
#define V(Prop) Prop,
    LIST_OF_CLOSURE_SIGNATURE_FLAGS(V)
#undef V

        FIRST = NoSuperAssign,
    LAST = NoReflection
};

struct ClosureSignature : public EnumSet<ClosureSignatureFlag> {
    ClosureSignature(const EnumSet<ClosureSignatureFlag>& x,
                     const TypeSignature& sig)
        : EnumSet<ClosureSignatureFlag>(x), type(sig){};
    ClosureSignature()
        : EnumSet<ClosureSignatureFlag>(), type(TypeSignature::any()){};

    TypeSignature type;

    ClosureSignature operator|(const ClosureSignature&) const;
    friend std::ostream& operator<<(std::ostream& out, const ClosureSignature&);
};

std::ostream& operator<<(std::ostream& out, const ClosureSignatureFlag&);

} // namespace pir
} // namespace rir
