#pragma once

#include "pir.h"
#include "signature.h"
#include <functional>
#include <sstream>
#include <unordered_map>

namespace rir {
namespace pir {

// Remember to update ClosureAugment::LAST
#define LIST_OF_CLOSURE_AUGMENTS(V)                                            \
    V(Strict)                                                                  \
    V(NoReflection)

enum class ClosureAugment {
#define V(Prop) Prop,
    LIST_OF_CLOSURE_AUGMENTS(V)
#undef V

        FIRST = Strict,
    LAST = NoReflection
};

struct ClosureAugments : public EnumSet<ClosureAugment> {
    ClosureAugments(const EnumSet<ClosureAugment>& x, const PirSignature& sig)
        : EnumSet<ClosureAugment>(x), signature(sig){};
    ClosureAugments()
        : EnumSet<ClosureAugment>(), signature(PirSignature::any()){};

    PirSignature signature;

    ClosureAugments operator|(const ClosureAugments&) const;
    friend std::ostream& operator<<(std::ostream& out, const ClosureAugments&);
};

std::ostream& operator<<(std::ostream& out, const ClosureAugment&);

} // namespace pir
} // namespace rir
