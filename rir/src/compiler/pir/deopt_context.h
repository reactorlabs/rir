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
    DeoptReason reason;
    SEXP deoptTrigger;

    DeoptContext(Opcode* pc, LazyEnvironment* env,
                 const std::vector<PirType>& stack, const DeoptReason& reason,
                 SEXP deoptTrigger)
        : pc(pc), env(env), stack(stack), reason(reason),
          deoptTrigger(deoptTrigger) {}

    bool operator==(const DeoptContext& other) const {
        return pc == other.pc && env == other.env && stack == other.stack &&
               reason == other.reason && deoptTrigger == other.deoptTrigger;
    }

    friend struct std::hash<DeoptContext>;
};

} // namespace pir
} // namespace rir
