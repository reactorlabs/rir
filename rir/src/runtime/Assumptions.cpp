#include "Assumptions.h"

namespace rir {

std::ostream& operator<<(std::ostream& out, Assumption a) {
    switch (a) {
    case Assumption::Arg1IsEager:
        out << "EA1";
        break;
    case Assumption::Arg2IsEager:
        out << "EA2";
        break;
    case Assumption::Arg3IsEager:
        out << "EA3";
        break;
    case Assumption::EagerArgs:
        out << "EA";
        break;
    case Assumption::CorrectOrderOfArguments:
        out << "COA";
        break;
    case Assumption::NoMissingArguments:
        out << "NMA";
        break;
    case Assumption::NonObjectArgs:
        out << "NO";
        break;
    case Assumption::Arg1IsNonObj:
        out << "NO1";
        break;
    case Assumption::Arg2IsNonObj:
        out << "NO2";
        break;
    case Assumption::Arg3IsNonObj:
        out << "NO3";
        break;
    case Assumption::NotTooManyArguments:
        out << "NTA";
        break;
    }
    return out;
};

constexpr std::array<Assumption, 3> Assumptions::ObjAssumptions;
constexpr std::array<Assumption, 3> Assumptions::EagerAssumptions;

} // namespace rir
