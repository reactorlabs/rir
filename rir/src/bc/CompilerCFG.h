#ifndef RIR_COMPILER_CFG_H
#define RIR_COMPILER_CFG_H

#include "R/r.h"
#include <unordered_set>

namespace rir {

// Parameter eligibility for record_type_once_promise_ optimization.
//
// A use of a function parameter inside a promise can be recorded once per
// invocation of the enclosing function (rather than on every promise force) if:
//   - the parameter is never assigned in the function body
//   - the parameter is not shadowed by an inner function's formal
//   - the parameter is referenced inside some loop (while/for/repeat) — this is
//     the case where once-per-invocation recording actually saves work
class CompilerCFGBuilder {
  private:
    std::unordered_set<SEXP> parameters_;
    std::unordered_set<SEXP> excludedParameters_;
    std::unordered_set<SEXP> parametersUsedInLoops_;
    std::unordered_set<SEXP> supportedParameters_;

    void scanBody(SEXP e, bool inLoop = false);
    void markParameterExcluded(SEXP var);

  public:
    void configure(SEXP formals, SEXP body);
    bool isSupportedParameter(SEXP var) const;
};

} // namespace rir

#endif
