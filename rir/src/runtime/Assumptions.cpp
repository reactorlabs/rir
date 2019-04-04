#include "Assumptions.h"

namespace rir {

std::ostream& operator<<(std::ostream& out, Assumption a) {
    switch (a) {
    case Assumption::NoExplicitlyMissingArgs:
        out << "!ExpMi";
        break;
    case Assumption::CorrectOrderOfArguments:
        out << "CorrOrd";
        break;
    case Assumption::NotTooManyArguments:
        out << "!TMany";
        break;
    case Assumption::NotTooFewArguments:
        out << "!TFew";
        break;
#define TYPE_ASSUMPTIONS(Type, Msg)                                            \
    case Assumption::Arg0Is##Type##_:                                          \
        out << Msg << "0";                                                     \
        break;                                                                 \
    case Assumption::Arg1Is##Type##_:                                          \
        out << Msg << "1";                                                     \
        break;                                                                 \
    case Assumption::Arg2Is##Type##_:                                          \
        out << Msg << "2";                                                     \
        break;
        TYPE_ASSUMPTIONS(Eager, "Eager");
        TYPE_ASSUMPTIONS(NotObj, "!Obj");
        TYPE_ASSUMPTIONS(SimpleInt, "SimpleInt");
        TYPE_ASSUMPTIONS(SimpleReal, "SimpleReal");
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

constexpr std::array<Assumption, Assumptions::NUM_ARGS>
    Assumptions::EagerAssumptions;
constexpr std::array<Assumption, Assumptions::NUM_ARGS>
    Assumptions::NotObjAssumptions;
constexpr std::array<Assumption, Assumptions::NUM_ARGS>
    Assumptions::SimpleIntAssumptions;
constexpr std::array<Assumption, Assumptions::NUM_ARGS>
    Assumptions::SimpleRealAssumptions;

} // namespace rir
