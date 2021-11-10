#pragma once

#include "common.h"
#include "ir/BC_inc.h"
#include "runtime/LazyEnvironment.h"

#include <vector>

namespace rir {
namespace pir {

struct DeoptContext;

struct ContinuationContext {
    static constexpr size_t MAX_ENV = 10;
    static constexpr size_t MAX_STACK = 10;

  protected:
    Opcode* pc_ = nullptr;

    unsigned short stackSize_;
    std::array<PirType, MAX_STACK> stack_;

  public:
    Opcode* pc() const { return pc_; }

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
        assert(s.size() == stackSize());
        return s;
    }

  protected:
    unsigned short envSize_ = 0;
    std::array<std::tuple<SEXP, PirType, bool>, MAX_ENV> env_;
    bool leakedEnv_ = true;

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

  public:
    bool leakedEnv() const { return leakedEnv_; }

    ContinuationContext() : stackSize_(0), envSize_(0) {}
    ContinuationContext(Opcode* pc, SEXP env, bool leaked, R_bcstack_t* base,
                        size_t stackSize);

    bool operator==(const ContinuationContext& other) const {
        if (pc() != other.pc() || envSize() != other.envSize() ||
            stackSize() != other.stackSize() || leakedEnv_ != other.leakedEnv_)
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

    virtual const DeoptContext* asDeoptContext() const { return nullptr; }

    friend struct std::hash<ContinuationContext>;
};

} // namespace pir
} // namespace rir
