#include "Deoptimization.h"
#include "runtime/Code.h"

namespace rir {
void DeoptMetadata::print(std::ostream& out) const {
    for (auto f : frames)
        out << f.code << "+" << f.pc - f.code->code() << " ";
}
} // namespace rir
