#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <iostream>

namespace rir {
namespace pir {

enum class Assumptions {
    CorrectNumberOfArguments,
    MaxNumberOfArguments,
    CorrectOrderOfArguments,

    FIRST = CorrectNumberOfArguments,
    LAST = CorrectOrderOfArguments
};

struct AssumptionsSet : public EnumSet<Assumptions> {
    AssumptionsSet() : EnumSet<Assumptions>(){};
    AssumptionsSet(const EnumSet<Assumptions>& other)
        : EnumSet<Assumptions>(other) {}
    AssumptionsSet(const Assumptions& other) : EnumSet<Assumptions>(other) {}
};

std::ostream& operator<<(std::ostream& out, Assumptions a);

} // namespace pir
} // namespace rir

#endif
