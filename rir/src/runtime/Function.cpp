#include "Function.h"

namespace rir {
void Function::disassemble(std::ostream& out) {
    body()->disassemble(out);
}
} // namespace rir
