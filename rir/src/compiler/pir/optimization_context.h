#ifndef PIR_OPTIMIZATION_CONTEXT_H
#define PIR_OPTIMIZATION_CONTEXT_H

#include "assumptions.h"
#include "pir.h"
#include "runtime/Function.h"

namespace rir {
namespace pir {

struct OptimizationContext {
    OptimizationContext(Env* environment, const Assumptions& assumptions)
        : environment(environment), assumptions(assumptions) {}

    Env* environment;
    Assumptions assumptions;

    bool operator<(const OptimizationContext& other) const {
        if (environment == other.environment)
            return assumptions.to_ulong() < other.assumptions.to_ulong();
        return environment < other.environment;
    }

    bool operator==(const OptimizationContext& other) const {
        return assumptions.to_ulong() == other.assumptions.to_ulong() &&
               environment == other.environment;
    }
};

} // namespace pir
} // namespace rir

namespace std {
template <>
struct hash<rir::pir::OptimizationContext> {
    std::size_t operator()(const rir::pir::OptimizationContext& v) const {
        using std::hash;
        return hash<unsigned long long>()(v.assumptions.to_ulong()) ^
               hash<rir::pir::Env*>()(v.environment);
    }
};
} // namespace std

#endif
