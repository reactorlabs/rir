#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <array>
#include <iostream>

namespace rir {

typedef uint32_t Immediate;

enum class Assumption {
    EagerArgs_,

    Arg1IsEager_,
    Arg2IsEager_,
    Arg3IsEager_,

    NonObjectArgs_,

    Arg1IsNonObj_,
    Arg2IsNonObj_,
    Arg3IsNonObj_,

    NoMissingArguments,
    NotTooManyArguments,
    CorrectOrderOfArguments,

    FIRST = EagerArgs_,
    LAST = CorrectOrderOfArguments
};

struct Assumptions : public EnumSet<Assumption, Immediate> {
    Assumptions() : EnumSet<Assumption, Immediate>(){};
    Assumptions(const EnumSet<Assumption, Immediate>& other)
        : EnumSet<Assumption, Immediate>(other) {}
    Assumptions(const Assumption& other)
        : EnumSet<Assumption, Immediate>(other) {}

    bool isEager(size_t i) const;
    void setEager(size_t i, bool);
    bool notObj(size_t i) const;
    void setNotObj(size_t i, bool);
};

std::ostream& operator<<(std::ostream& out, Assumption a);

} // namespace rir

#endif
