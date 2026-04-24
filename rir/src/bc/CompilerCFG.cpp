#include "CompilerCFG.h"
#include "R/RList.h"
#include "R/Symbols.h"

namespace rir {

void CompilerCFGBuilder::configure(SEXP formals, SEXP body) {
    for (RListIter arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (arg.tag() != R_NilValue && TYPEOF(arg.tag()) == SYMSXP)
            parameters_.insert(arg.tag());
    }

    scanBody(body);

    for (SEXP p : parametersUsedInLoops_)
        if (!excludedParameters_.count(p))
            supportedParameters_.insert(p);
}

void CompilerCFGBuilder::scanBody(SEXP e, bool inLoop) {
    if (!e || e == R_NilValue)
        return;

    if (TYPEOF(e) == SYMSXP) {
        if (inLoop && parameters_.count(e))
            parametersUsedInLoops_.insert(e);
        return;
    }

    if (TYPEOF(e) != LANGSXP)
        return;

    SEXP fun = CAR(e);

    if (fun == symbol::Assign || fun == symbol::Assign2 ||
        fun == symbol::SuperAssign) {
        SEXP lhs = CADR(e);
        if (TYPEOF(lhs) == SYMSXP && parameters_.count(lhs))
            markParameterExcluded(lhs);
        scanBody(CADDR(e), inLoop);
        return;
    }

    if (fun == symbol::For) {
        scanBody(CADDR(e), inLoop);
        scanBody(CADDDR(e), true);
        return;
    }
    if (fun == symbol::While) {
        scanBody(CADR(e), true);
        scanBody(CADDR(e), true);
        return;
    }
    if (fun == symbol::Repeat) {
        scanBody(CADR(e), true);
        return;
    }

    // Inner function: any formal that shadows our parameter excludes it.
    // Don't recurse into the body — uses inside the inner closure refer to
    // the inner scope, not ours.
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

    for (SEXP arg = CDR(e); arg != R_NilValue; arg = CDR(arg))
        scanBody(CAR(arg), inLoop);
}

void CompilerCFGBuilder::markParameterExcluded(SEXP var) {
    if (parameters_.count(var))
        excludedParameters_.insert(var);
}

bool CompilerCFGBuilder::isSupportedParameter(SEXP var) const {
    return supportedParameters_.count(var);
}

} // namespace rir
