#include "CompilerCFG.h"
#include "AstUtils.h"
#include "R/RList.h"
#include "R/Symbols.h"

namespace rir {

static constexpr int kMaxLinesForRecordOnce = 20;

void CompilerCFGBuilder::configure(SEXP formals, SEXP body) {
    if (countNodes(body, kMaxLinesForRecordOnce) >= kMaxLinesForRecordOnce)
        return;

    // Extract parameters from formals
    for (RListIter arg = RList(formals).begin(); arg != RList::end(); ++arg) {
        if (arg.tag() != R_NilValue && TYPEOF(arg.tag()) == SYMSXP)
            parameters_.insert(arg.tag());
    }

    // Scan body: detect loop usage and exclusions
    scanBody(body, false);
}

void CompilerCFGBuilder::scanBody(SEXP e, bool inLoop) {
    if (!e || e == R_NilValue)
        return;

    // A bare symbol reference: if inside a loop, mark the parameter
    if (TYPEOF(e) == SYMSXP) {
        if (inLoop && parameters_.count(e))
            parameters_used_in_loops_.insert(e);
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
        scanBody(CADDR(e), inLoop);
        return;
    }

    // Loop constructs: scan body with inLoop=true
    if (fun == symbol::For) {
        // for(var in seq) body — seq evaluated once, body on every iteration
        scanBody(CADDR(e), inLoop); // seq: not in loop
        scanBody(CADDDR(e), true);  // body: in loop
        return;
    }
    if (fun == symbol::While) {
        // while(cond) body — both cond and body re-evaluated each iteration
        scanBody(CADR(e), true);  // cond: in loop
        scanBody(CADDR(e), true); // body: in loop
        return;
    }
    if (fun == symbol::Repeat) {
        // repeat body
        scanBody(CADR(e), true); // body: in loop
        return;
    }

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
        scanBody(CAR(arg), inLoop);
}

void CompilerCFGBuilder::markParameterExcluded(SEXP var) {
    if (parameters_.count(var))
        excluded_parameters_.insert(var);
}

bool CompilerCFGBuilder::isSupportedParameter(SEXP var) const {
    return var && parameters_used_in_loops_.count(var) &&
           !excluded_parameters_.count(var);
}

bool CompilerCFGBuilder::hasSupportedParameters() const {
    for (SEXP p : parameters_used_in_loops_) {
        if (!excluded_parameters_.count(p))
            return true;
    }
    return false;
}

} // namespace rir
