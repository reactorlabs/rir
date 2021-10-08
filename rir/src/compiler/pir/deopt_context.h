#pragma once

#include "common.h"
#include "ir/BC_inc.h"
#include "runtime/LazyEnvironment.h"

#include <vector>

namespace rir {
namespace pir {

struct DeoptContext {
    Opcode* pc = nullptr;

    static constexpr size_t MAX_ENV = 10;
    static constexpr size_t MAX_STACK = 5;

  private:
    unsigned short stackSize_;
    std::array<PirType, MAX_STACK> stack_;

  public:
    size_t stackSize() const { return stackSize_; }
    std::array<PirType, MAX_STACK>::const_iterator stackBegin() const {
        return stack_.begin();
    }
    std::array<PirType, MAX_STACK>::const_iterator stackEnd() const {
        return stackBegin() + stackSize();
    }
    const std::vector<PirType> stack() const {
        std::vector<PirType> s;
        s.insert(s.begin(), stack_.begin(), stackEnd());
        return s;
    }

  private:
    unsigned short envSize_;
    std::array<std::tuple<SEXP, PirType, bool>, MAX_ENV> env_;

  public:
    size_t envSize() const { return envSize_; }
    std::array<std::tuple<SEXP, PirType, bool>, MAX_ENV>::const_iterator
    envBegin() const {
        return env_.begin();
    }
    std::array<std::tuple<SEXP, PirType, bool>, MAX_ENV>::const_iterator
    envEnd() const {
        return envBegin() + envSize();
    }

  private:
    DeoptReason reason_;
    SEXP deoptTrigger_ = nullptr;

  public:
    const DeoptReason& reason() const { return reason_; }
    SEXP deoptTrigger() const { return deoptTrigger_; }

    DeoptContext()
        : stackSize_(0), envSize_(0), reason_(DeoptReason::unknown()) {}
    DeoptContext(Opcode* pc, LazyEnvironment* env,
                 const std::vector<PirType>& stack, const DeoptReason& reason,
                 SEXP deoptTrigger);

    // TODO: a bit of a hack since the trigger is not used for equality (but
    // the compiler only uses it as a heuristic to update type feedback)
    bool operator==(const DeoptContext& other) const {
        if (pc != other.pc || envSize() != other.envSize() ||
            stackSize() != other.stackSize() || !(reason() == other.reason()))
            return false;
        auto e1 = envBegin();
        auto e2 = other.envBegin();
        while (e1 != envEnd()) {
            if (*e1 != *e2)
                return false;
            e1++;
            e2++;
        }
        auto s1 = stackBegin();
        auto s2 = other.stackBegin();
        while (s1 != stackEnd()) {
            if (*s1 != *s2)
                return false;
            s1++;
            s2++;
        }
        return true;
    }

    // a smaller b  ==>  b can be called when a is the current context
    bool smaller(const DeoptContext& other) const {
        if (pc != other.pc || envSize() != other.envSize() ||
            stackSize() != other.stackSize() || !(reason() == other.reason()))
            return false;

        {
            auto here = envBegin();
            auto there = other.envBegin();
            while (here != envEnd()) {
                if (std::get<SEXP>(*here) != std::get<SEXP>(*there) ||
                    std::get<bool>(*here) != std::get<bool>(*there))
                    return false;
                if (!std::get<PirType>(*here).isA(std::get<PirType>(*there)))
                    return false;
                here++;
                there++;
            }
        }
        {
            auto here = stackBegin();
            auto there = other.stackBegin();
            while (here != stackEnd()) {
                if (!here->isA(*there))
                    return false;
                here++;
                there++;
            }
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
