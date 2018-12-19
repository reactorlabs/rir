#include "assumptions.h"

namespace rir {
namespace pir {

std::ostream& operator<<(std::ostream& out, Assumption a) {
    switch (a) {
    case Assumption::CorrectOrderOfArguments:
        out << "CorrectOrderOfArguments";
        break;
    case Assumption::MaxNumberOfArguments:
        out << "MaxNumberOfArguments";
        break;
    case Assumption::CorrectNumberOfArguments:
        out << "CorrectNumberOfArguments";
        break;
    }
    return out;
};

} // namespace pir
} // namespace rir
