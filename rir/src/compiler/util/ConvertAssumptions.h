#pragma once

#include "../pir/pir.h"

namespace rir {
namespace pir {

void readArgTypeFromAssumptions(const Assumptions& assumptions, PirType& type,
                                int i);
void writeArgTypeToAssumptions(Assumptions& assumptions, Value* arg, int i);

} // namespace pir
} // namespace rir
