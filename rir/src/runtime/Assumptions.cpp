#include "Assumptions.h"

namespace rir {

std::ostream& operator<<(std::ostream& out, Assumption a) {
    switch (a) {
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
        out << "NOA";
        break;
    case Assumption::NotTooManyArguments:
        out << "NTA";
        break;
    }
    return out;
};

} // namespace rir
