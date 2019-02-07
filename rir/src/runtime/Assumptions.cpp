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
    case Assumption::EagerArgs_:
        out << "Eagers";
        break;
    case Assumption::CorrectOrderOfArguments:
        out << "CorrOrd";
        break;
    case Assumption::NonObjectArgs_:
        out << "!Objs";
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

static constexpr std::array<Assumption, 3> ObjAssumptions = {
    {Assumption::Arg1IsNonObj_, Assumption::Arg2IsNonObj_,
     Assumption::Arg3IsNonObj_}};
static constexpr std::array<Assumption, 3> EagerAssumptions = {
    {Assumption::Arg1IsEager_, Assumption::Arg2IsEager_,
     Assumption::Arg3IsEager_}};

bool Assumptions::isEager(size_t i) const {
    if (flags.includes(Assumption::EagerArgs_))
        return true;

    if (i < EagerAssumptions.size())
        if (flags.includes(EagerAssumptions[i]))
            return true;

    return false;
}

void Assumptions::setEager(size_t i, bool eager) {
    if (eager && i < EagerAssumptions.size())
        flags.set(EagerAssumptions[i]);
    else if (!eager)
        flags.reset(Assumption::EagerArgs_);
}

bool Assumptions::notObj(size_t i) const {
    if (flags.includes(Assumption::NonObjectArgs_))
        return true;

    if (i < ObjAssumptions.size())
        if (flags.includes(ObjAssumptions[i]))
            return true;

    return false;
}

void Assumptions::setNotObj(size_t i, bool notObj) {
    if (notObj && i < ObjAssumptions.size())
        flags.set(ObjAssumptions[i]);
    else if (!notObj)
        flags.reset(Assumption::NonObjectArgs_);
}

} // namespace rir
