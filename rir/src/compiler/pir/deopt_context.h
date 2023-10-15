#pragma once

#include "bc/BC_inc.h"
#include "common.h"
#include "continuation_context.h"
#include "runtime/LazyEnvironment.h"

#include <vector>

namespace rir {
namespace pir {

struct DeoptContext : public ContinuationContext {
  private:
    DeoptReason reason_;
    PirType typeCheckTrigger_;
    SEXP deadBranchTrigger_ = nullptr;
    SEXP callTargetTrigger_ = nullptr;

  public:
    const DeoptReason& reason() const { return reason_; }
    const PirType& typeCheckTrigger() const {
        assert(reason_.reason == DeoptReason::Typecheck);
        return typeCheckTrigger_;
    }
    SEXP deadBranchTrigger() const {
        assert(reason_.reason == DeoptReason::DeadBranchReached);
        return deadBranchTrigger_;
    }
    SEXP callTargetTrigger() const {
        assert(reason_.reason == DeoptReason::CallTarget);
        return callTargetTrigger_;
    }

    DeoptContext();
    DeoptContext(Opcode* pc, size_t envSize, SEXP actualEnv,
                 LazyEnvironment* env, bool leaked, R_bcstack_t* base,
                 size_t stackSize, const DeoptReason& reason,
                 SEXP deoptTrigger);

    const DeoptContext* asDeoptContext() const override final { return this; }

    bool operator==(const DeoptContext& other) const {
        return ContinuationContext::operator==(other) && reason_ == reason_ &&
               deadBranchTrigger_ == other.deadBranchTrigger_ &&
               typeCheckTrigger_ == other.typeCheckTrigger_ &&
               callTargetTrigger_ == other.callTargetTrigger_;
    }

    // a smaller b  ==>  b can be called when a is the current context
    bool smaller(const DeoptContext& other) const {
        if (pc() != other.pc() || envSize() != other.envSize() ||
            stackSize() != other.stackSize() ||
            leakedEnv_ != other.leakedEnv_ ||
            reason_.reason != other.reason_.reason)
            return false;

        if (reason_.reason == DeoptReason::Typecheck)
            if (!typeCheckTrigger().isA(other.typeCheckTrigger()))
                return false;
        if (reason_.reason == DeoptReason::DeadBranchReached)
            if (!deadBranchTrigger() && other.deadBranchTrigger())
                return false;
        if (reason_.reason == DeoptReason::CallTarget)
            if (callTargetTrigger() != other.callTargetTrigger())
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
        if (pc() < other.pc() || stackSize_ < other.stackSize_ ||
            envSize_ < other.envSize_ || leakedEnv_ < other.leakedEnv_)
            return true;
        if (pc() > other.pc() || stackSize_ > other.stackSize_ ||
            envSize_ > other.envSize_ || leakedEnv_ > other.leakedEnv_)
            return false;

        if (reason_.reason < other.reason_.reason)
            return true;
        if (reason_.reason > other.reason_.reason)
            return false;

        if (reason_.reason == DeoptReason::Typecheck) {
            auto a = typeCheckTrigger().hash();
            auto b = other.typeCheckTrigger().hash();
            if (a < b)
                return true;
            if (b < a)
                return false;
        }
        if (reason_.reason == DeoptReason::DeadBranchReached) {
            auto a = deadBranchTrigger();
            auto b = other.deadBranchTrigger();
            if (a < b)
                return true;
            if (b < a)
                return false;
        }
        if (reason_.reason == DeoptReason::CallTarget) {
            auto a = callTargetTrigger();
            auto b = other.callTargetTrigger();
            if (a < b)
                return true;
            if (b < a)
                return false;
        }

        {
            auto here = envBegin();
            auto there = other.envBegin();
            while (here != envEnd()) {
                auto an = std::get<SEXP>(*here);
                auto bn = std::get<SEXP>(*there);
                auto am = std::get<bool>(*here);
                auto bm = std::get<bool>(*there);
                auto a = std::get<PirType>(*here).hash();
                auto b = std::get<PirType>(*there).hash();
                if (an < bn || am < bm || a < b)
                    return true;
                if (bn < an || bm < am || b < a)
                    return false;
                here++;
                there++;
            }
        }
        {
            auto here = stackBegin();
            auto there = other.stackBegin();
            while (here != stackEnd()) {
                auto a = here->hash();
                auto b = there->hash();
                if (a < b)
                    return true;
                if (b < a)
                    return false;
                here++;
                there++;
            }
        }

        return false;
    }
};

} // namespace pir
} // namespace rir
