#include "CompilerCFG.h"
#include "R/RList.h"
#include "R/Symbols.h"

namespace rir {

void CompilerCFGBuilder::scanParameters(SEXP formals, SEXP body) {
    // Extract parameters from formals
    for (RListIter arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (arg.tag() != R_NilValue && TYPEOF(arg.tag()) == SYMSXP)
            parameters_.insert(arg.tag());
    }

    // Scan body AST for exclusions
    scanForExclusions(body);
}

void CompilerCFGBuilder::scanForExclusions(SEXP e) {
    if (!e || TYPEOF(e) != LANGSXP)
        return;

    SEXP fun = CAR(e);

    // Check for assignments
    if (fun == symbol::Assign || fun == symbol::Assign2 ||
        fun == symbol::SuperAssign) {
        SEXP lhs = CADR(e);
        if (TYPEOF(lhs) == SYMSXP && parameters_.count(lhs))
            markParameterExcluded(lhs);
    }

    // Check for nested functions
    if (fun == symbol::Function) {
        // Check if any nested function parameter shadows an outer parameter
        SEXP innerFormals = CADR(e);
        if (innerFormals != R_NilValue) {
            for (RListIter arg = RList(innerFormals).begin();
                 arg != RList::end(); ++arg) {
                SEXP innerParam = arg.tag();
                if (innerParam != R_NilValue && TYPEOF(innerParam) == SYMSXP) {
                    if (parameters_.count(innerParam))
                        markParameterExcluded(innerParam);
                }
            }
        }
        // Recursively scan the body of nested function for deeper nesting
        if (CDR(e) != R_NilValue && CDDR(e) != R_NilValue)
            scanForExclusions(CADDR(e));
        return;
    }

    // Recursively scan all sub-expressions
    for (auto arg : RList(CDR(e)))
        scanForExclusions(arg);
}

void CompilerCFGBuilder::markParameterExcluded(SEXP var) {
    if (parameters_.count(var))
        excluded_parameters_.insert(var);
}

bool CompilerCFGBuilder::isSupportedParameter(SEXP var) const {
    return var && parameters_.count(var) && !excluded_parameters_.count(var);
}

bool CompilerCFGBuilder::hasSupportedParameters() const {
    for (SEXP p : parameters_) {
        if (!excluded_parameters_.count(p))
            return true;
    }
    return false;
}

} // namespace rir
