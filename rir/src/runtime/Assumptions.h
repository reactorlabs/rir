#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <iostream>

namespace rir {

typedef uint32_t Immediate;

enum class Assumption {
    EagerArgs,
    NonObjectArgs,
    NoMissingArguments,
    NotTooManyArguments,
    CorrectOrderOfArguments,

    FIRST = EagerArgs,
    LAST = CorrectOrderOfArguments
};

struct Assumptions : public EnumSet<Assumption, Immediate> {
    Assumptions() : EnumSet<Assumption, Immediate>(){};
    Assumptions(const EnumSet<Assumption, Immediate>& other)
        : EnumSet<Assumption, Immediate>(other) {}
    Assumptions(const Assumption& other)
        : EnumSet<Assumption, Immediate>(other) {}
};

std::ostream& operator<<(std::ostream& out, Assumption a);

} // namespace rir

#endif
