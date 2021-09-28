#pragma once

#include "common.h"
#include "ir/BC_inc.h"
#include "runtime/LazyEnvironment.h"

namespace rir {
namespace pir {

struct DeoptContext {
    Opcode* pc;
    LazyEnvironment* env;
    std::vector<PirType> stack;

    DeoptContext(Opcode* pc, LazyEnvironment* env,
                 const std::vector<PirType>& stack)
        : pc(pc), env(env), stack(stack) {}

    bool operator==(const DeoptContext& other) const {
        return pc == other.pc && env == other.env && stack == other.stack;
    }

    friend struct std::hash<DeoptContext>;
};

} // namespace pir
} // namespace rir
