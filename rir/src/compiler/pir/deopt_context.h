#pragma once

#include "common.h"
#include "ir/BC_inc.h"
#include "runtime/LazyEnvironment.h"

namespace rir {
namespace pir {

struct DeoptContext {
    Opcode* pc;
    std::vector<std::tuple<SEXP, PirType, bool>> env;
    std::vector<PirType> stack;
    DeoptReason reason;
    SEXP deoptTrigger;

    DeoptContext(Opcode* pc, LazyEnvironment* env,
                 const std::vector<PirType>& stack, const DeoptReason& reason,
                 SEXP deoptTrigger);

    // TODO: a bit of a hack since the trigger is not used for equality (but
    // the compiler only uses it as a heuristic to update type feedback)
    bool operator==(const DeoptContext& other) const {
        return pc == other.pc && env == other.env && stack == other.stack &&
               reason == other.reason;
    }

    friend struct std::hash<DeoptContext>;
};

} // namespace pir
} // namespace rir
