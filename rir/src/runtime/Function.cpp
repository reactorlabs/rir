#include "Function.h"

namespace rir {
void Function::disassemble(std::ostream& out) {
    body()->disassemble(out);
}

void Function::printInvocation(std::ostream& out) {
    out << "Invocation count: " << invocationCount << "\n";
}

} // namespace rir
