#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <iostream>

namespace rir {
namespace pir {

enum class Assumption {
    EagerArgs,
    CorrectNumberOfArguments,
    MaxNumberOfArguments,
    CorrectOrderOfArguments,

    FIRST = EagerArgs,
    LAST = CorrectOrderOfArguments
};

struct Assumptions : public EnumSet<Assumption> {
    Assumptions() : EnumSet<Assumption>(){};
    Assumptions(const EnumSet<Assumption>& other)
        : EnumSet<Assumption>(other) {}
    Assumptions(const Assumption& other) : EnumSet<Assumption>(other) {}
};

std::ostream& operator<<(std::ostream& out, Assumption a);

} // namespace pir
} // namespace rir

#endif
