#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

namespace rir {
namespace pir {

enum class Assumptions {
    CorrectNumberOfArguments,
    CorrectOrderOfArguments,

    FIRST = CorrectNumberOfArguments,
    LAST = CorrectOrderOfArguments
};

struct AssumptionsSet : public EnumSet<Assumptions> {
    AssumptionsSet() : EnumSet<Assumptions>(){};
    AssumptionsSet(const AssumptionsSet& other) : EnumSet<Assumptions>(other) {}
    AssumptionsSet(const Assumptions& other) : EnumSet<Assumptions>(other) {}
};

} // namespace pir
} // namespace rir

#endif
