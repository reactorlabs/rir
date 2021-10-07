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

    DeoptContext() : pc(nullptr), reason(DeoptReason::unknown()) {}
    DeoptContext(Opcode* pc, LazyEnvironment* env,
                 const std::vector<PirType>& stack, const DeoptReason& reason,
                 SEXP deoptTrigger);

    // TODO: a bit of a hack since the trigger is not used for equality (but
    // the compiler only uses it as a heuristic to update type feedback)
    bool operator==(const DeoptContext& other) const {
        return pc == other.pc && env == other.env && stack == other.stack &&
               reason == other.reason;
    }

    // a smaller b  ==>  b can be called when a is the current context
    bool smaller(const DeoptContext& other) const {
        if (pc != other.pc || env.size() != other.env.size() ||
            stack.size() != other.stack.size() || !(reason == other.reason))
            return false;
        for (size_t i = 0; i < env.size(); ++i) {
            auto& here = env.at(i);
            auto& there = other.env.at(i);
            if (std::get<SEXP>(here) != std::get<SEXP>(there) ||
                std::get<bool>(here) != std::get<bool>(there))
                return false;
            if (!std::get<PirType>(here).isA(std::get<PirType>(there)))
                return false;
        }
        for (size_t i = 0; i < stack.size(); ++i) {
            auto& here = stack.at(i);
            auto& there = other.stack.at(i);
            if (!here.isA(there))
                return false;
        }
        return true;
    }

    bool operator<(const DeoptContext& other) const {
        if (*this == other)
            return false;
        if (smaller(other))
            return true;
        if (other.smaller(*this))
            return false;

        // Linearize to complete order
        char here[sizeof(*this)];
        char there[sizeof(*this)];
        memcpy(here, this, sizeof(*this));
        memcpy(there, &other, sizeof(*this));
        return strncmp(here, there, sizeof(*this)) < 0;
    }

    friend struct std::hash<DeoptContext>;
};

} // namespace pir
} // namespace rir
