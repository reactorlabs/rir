#include "Assumptions.h"

namespace rir {

std::ostream& operator<<(std::ostream& out, Assumption a) {
    switch (a) {
    case Assumption::NoExplicitlyMissingArgs:
        out << "!ExpMi";
        break;
    case Assumption::Arg1IsEager_:
        out << "Eager1";
        break;
    case Assumption::Arg2IsEager_:
        out << "Eager2";
        break;
    case Assumption::Arg3IsEager_:
        out << "Eager3";
        break;
    case Assumption::Arg4IsEager_:
        out << "Eager4";
        break;
    case Assumption::Arg0IsEager_:
        out << "Eager0";
        break;
    case Assumption::CorrectOrderOfArguments:
        out << "CorrOrd";
        break;
    case Assumption::Arg1IsNonObj_:
        out << "!Obj1";
        break;
    case Assumption::Arg2IsNonObj_:
        out << "!Obj2";
        break;
    case Assumption::Arg3IsNonObj_:
        out << "!Obj3";
        break;
    case Assumption::Arg0IsNonObj_:
        out << "!Obj0";
        break;
    case Assumption::NotTooManyArguments:
        out << "!TMany";
        break;
    case Assumption::NotTooFewArguments:
        out << "!TFew";
        break;
    }
    return out;
};

std::ostream& operator<<(std::ostream& out, const Assumptions& a) {
    for (auto i = a.flags.begin(); i != a.flags.end(); ++i) {
        out << *i;
        if (i + 1 != a.flags.end())
            out << ",";
    }
    if (a.missing > 0)
        out << " miss: " << (int)a.missing;
    return out;
}

constexpr std::array<Assumption, 5> Assumptions::ObjAssumptions;
constexpr std::array<Assumption, 4> Assumptions::EagerAssumptions;

} // namespace rir
