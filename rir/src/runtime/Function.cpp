#include "Function.h"

namespace rir {
void Function::disassemble(std::ostream& out) {
    body()->disassemble(out);
    for (auto c : *this) {
        if (c != body()) {
            out << "\n[Prom (index " << c->index << ")]\n";
            c->disassemble(out);
        }
    }
}

void Function::printInvocation(std::ostream& out) {
    out << "Invocation count: " << invocationCount << "\n";
}

} // namespace rir
