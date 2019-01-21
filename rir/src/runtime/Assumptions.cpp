#include "Assumptions.h"

namespace rir {

std::ostream& operator<<(std::ostream& out, Assumption a) {
    switch (a) {
    case Assumption::Arg1IsEager_:
        out << "EA1";
        break;
    case Assumption::Arg2IsEager_:
        out << "EA2";
        break;
    case Assumption::Arg3IsEager_:
        out << "EA3";
        break;
    case Assumption::EagerArgs_:
        out << "EA";
        break;
    case Assumption::CorrectOrderOfArguments:
        out << "COA";
        break;
    case Assumption::NoMissingArguments:
        out << "NMA";
        break;
    case Assumption::NonObjectArgs_:
        out << "NO";
        break;
    case Assumption::Arg1IsNonObj_:
        out << "NO1";
        break;
    case Assumption::Arg2IsNonObj_:
        out << "NO2";
        break;
    case Assumption::Arg3IsNonObj_:
        out << "NO3";
        break;
    case Assumption::NotTooManyArguments:
        out << "NTA";
        break;
    }
    return out;
};

static constexpr std::array<Assumption, 3> ObjAssumptions = {
    Assumption::Arg1IsNonObj_, Assumption::Arg2IsNonObj_,
    Assumption::Arg3IsNonObj_};
static constexpr std::array<Assumption, 3> EagerAssumptions = {
    Assumption::Arg1IsEager_, Assumption::Arg2IsEager_,
    Assumption::Arg3IsEager_};

bool Assumptions::isEager(size_t i) const {
    if (includes(Assumption::EagerArgs_))
        return true;

    if (i < EagerAssumptions.size())
        if (includes(EagerAssumptions[i]))
            return true;

    return false;
}

void Assumptions::setEager(size_t i, bool eager) {
    if (eager && i < EagerAssumptions.size())
        set(EagerAssumptions[i]);
    else if (!eager)
        reset(Assumption::EagerArgs_);
}

bool Assumptions::notObj(size_t i) const {
    if (includes(Assumption::NonObjectArgs_))
        return true;

    if (i < ObjAssumptions.size())
        if (includes(ObjAssumptions[i]))
            return true;

    return false;
}

void Assumptions::setNotObj(size_t i, bool notObj) {
    if (notObj && i < ObjAssumptions.size())
        set(ObjAssumptions[i]);
    else if (!notObj)
        reset(Assumption::NonObjectArgs_);
}

} // namespace rir
