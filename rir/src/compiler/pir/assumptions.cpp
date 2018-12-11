#include "assumptions.h"

namespace rir {
namespace pir {

std::ostream& operator<<(std::ostream& out, Assumptions a) {
    switch (a) {
    case Assumptions::CorrectOrderOfArguments:
        out << "CorrectOrderOfArguments";
        break;
    case Assumptions::MaxNumberOfArguments:
        out << "MaxNumberOfArguments";
        break;
    case Assumptions::CorrectNumberOfArguments:
        out << "CorrectNumberOfArguments";
        break;
    }
    return out;
};

} // namespace pir
} // namespace rir
