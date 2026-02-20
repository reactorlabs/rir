#include "CompilerCFG.h"
// #include "AstUtils.h"
#include "R/RList.h"
#include "R/Symbols.h"

namespace rir {

// static constexpr int kMaxLinesForRecordOnce = 20;

void CompilerCFGBuilder::configure(SEXP formals, SEXP body) {
    // if (countNodes(body, kMaxLinesForRecordOnce) >= kMaxLinesForRecordOnce)
    //     return;

    // Extract parameters from formals
    for (RListIter arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (arg.tag() != R_NilValue && TYPEOF(arg.tag()) == SYMSXP)
            parameters_.insert(arg.tag());
    }

    // Scan body for exclusions
    scanBody(body);
}

void CompilerCFGBuilder::scanBody(SEXP e) {
    if (!e || e == R_NilValue)
        return;

    if (TYPEOF(e) == SYMSXP) {
        // if (inLoop && parameters_.count(e))
        //     parameters_used_in_loops_.insert(e);
        return;
    }

    if (TYPEOF(e) != LANGSXP)
        return;

    SEXP fun = CAR(e);

    // Assignments: exclude the parameter on the LHS, scan RHS
    if (fun == symbol::Assign || fun == symbol::Assign2 ||
        fun == symbol::SuperAssign) {
        SEXP lhs = CADR(e);
        if (TYPEOF(lhs) == SYMSXP && parameters_.count(lhs))
            markParameterExcluded(lhs);
        scanBody(CADDR(e));
        return;
    }

    // Loop constructs
    // if (fun == symbol::For) {
    //     scanBody(CADDR(e), inLoop);
    //     scanBody(CADDDR(e), true);
    //     return;
    // }
    // if (fun == symbol::While) {
    //     scanBody(CADR(e), true);
    //     scanBody(CADDR(e), true);
    //     return;
    // }
    // if (fun == symbol::Repeat) {
    //     scanBody(CADR(e), true);
    //     return;
    // }

    // Nested functions: exclude shadowed parameters, don't scan body
    if (fun == symbol::Function) {
        SEXP innerFormals = CADR(e);
        if (innerFormals != R_NilValue) {
            for (RListIter arg = RList(innerFormals).begin();
                 arg != RList::end(); ++arg) {
                SEXP innerParam = arg.tag();
                if (innerParam != R_NilValue && TYPEOF(innerParam) == SYMSXP &&
                    parameters_.count(innerParam))
                    markParameterExcluded(innerParam);
            }
        }
        return;
    }

    // Recursively scan all arguments
    for (SEXP arg = CDR(e); arg != R_NilValue; arg = CDR(arg))
        scanBody(CAR(arg));
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
