#ifndef PIR_OPTIMIZATION_CONTEXT_H
#define PIR_OPTIMIZATION_CONTEXT_H

#include "pir.h"
#include "runtime/Assumptions.h"
#include "runtime/Function.h"

namespace rir {
namespace pir {

struct OptimizationContext {
    OptimizationContext(const Assumptions& assumptions)
        : assumptions(assumptions) {}

    Assumptions assumptions;

    bool operator<(const OptimizationContext& other) const {
        if (assumptions.count() != other.assumptions.count())
            return assumptions.count() < other.assumptions.count();
        return assumptions.to_i() < other.assumptions.to_i();
    }

    bool operator==(const OptimizationContext& other) const {
        return assumptions == other.assumptions;
    }
};

} // namespace pir
} // namespace rir

namespace std {
template <>
struct hash<rir::pir::OptimizationContext> {
    std::size_t operator()(const rir::pir::OptimizationContext& v) const {
        using std::hash;
        return hash<unsigned long long>()(v.assumptions.to_i());
    }
};
} // namespace std

#endif
