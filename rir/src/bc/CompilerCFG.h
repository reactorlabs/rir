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

    void scanForExclusions(SEXP e);

    bool hasLoop_ = false;

  public:
    // Extract parameters from formals and scan body for exclusions
    void configure(SEXP formals, SEXP body);

    // Returns true if the function body contains any loops
    bool shouldRecordOnceInFunction() const { return hasLoop_; }

    // Mark a parameter as excluded from optimization
    void markParameterExcluded(SEXP var);

    // Check if a variable is a non-excluded parameter (eligible for
    // record_type_once_)
    bool isSupportedParameter(SEXP var) const;

    // Check if any supported parameters exist (i.e., record_type_once_ was
    // emitted)
    bool hasSupportedParameters() const;
};

} // namespace rir

#endif
