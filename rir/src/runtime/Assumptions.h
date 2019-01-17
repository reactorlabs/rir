#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <array>
#include <iostream>

namespace rir {

typedef uint32_t Immediate;

enum class Assumption {
    EagerArgs,

    Arg1IsEager,
    Arg2IsEager,
    Arg3IsEager,

    NonObjectArgs,

    Arg1IsNonObj,
    Arg2IsNonObj,
    Arg3IsNonObj,

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

    static constexpr std::array<Assumption, 3> ObjAssumptions = {
        Assumption::Arg1IsNonObj, Assumption::Arg2IsNonObj,
        Assumption::Arg3IsNonObj};
    static constexpr std::array<Assumption, 3> EagerAssumptions = {
        Assumption::Arg1IsEager, Assumption::Arg2IsEager,
        Assumption::Arg3IsEager};
};

std::ostream& operator<<(std::ostream& out, Assumption a);

} // namespace rir

#endif
