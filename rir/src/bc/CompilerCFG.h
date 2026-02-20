#ifndef RIR_COMPILER_CFG_H
#define RIR_COMPILER_CFG_H

#include "R/r.h"
#include <unordered_set>

namespace rir {

// ============================================================================
// Parameter tracking for record_type_once_ optimization
// ============================================================================

class CompilerCFGBuilder {
  private:
    // Parameters extracted from formals
    std::unordered_set<SEXP> parameters_;

    // Parameters excluded from optimization (assigned or shadowed by inner
    // functions)
    std::unordered_set<SEXP> excluded_parameters_;

    // Parameters that are referenced within at least one loop body
    // std::unordered_set<SEXP> parameters_used_in_loops_;

    void scanBody(SEXP e);

  public:
    // Extract parameters from formals and scan body for exclusions
    void configure(SEXP formals, SEXP body);

    // Returns true if any supported parameters exist
    bool shouldRecordOnceInFunction() const { return !parameters_.empty(); }

    // Mark a parameter as excluded from optimization
    void markParameterExcluded(SEXP var);

    // Check if a variable is a non-excluded parameter
    bool isSupportedParameter(SEXP var) const;

    // Check if any supported parameters exist
    bool hasSupportedParameters() const;
};

} // namespace rir

#endif
