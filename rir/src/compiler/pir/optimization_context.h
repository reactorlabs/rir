#ifndef PIR_OPTIMIZATION_CONTEXT_H
#define PIR_OPTIMIZATION_CONTEXT_H

#include "pir.h"
#include "runtime/Assumptions.h"
#include "runtime/Function.h"

namespace rir {
namespace pir {

struct OptimizationContext {
    explicit OptimizationContext(const Assumptions& assumptions)
        : assumptions(assumptions) {}

    Assumptions assumptions;

    bool operator<(const OptimizationContext& other) const {
        return assumptions < other.assumptions;
    }

    bool operator==(const OptimizationContext& other) const {
        return assumptions == other.assumptions;
    }

    bool subtype(const OptimizationContext& other) const {
        return assumptions.subtype(other.assumptions);
    }
};

} // namespace pir
} // namespace rir

namespace std {
template <>
struct hash<rir::pir::OptimizationContext> {
    std::size_t operator()(const rir::pir::OptimizationContext& v) const {
        using std::hash;
        return hash<rir::Assumptions>()(v.assumptions);
    }
};
} // namespace std

#endif
