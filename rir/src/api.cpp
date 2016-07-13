/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include <cassert>

#include "api.h"

#include "ir/Compiler.h"
#include "interpreter/interp_context.h"
#include "interpreter/interp.h"
#include "ir/BC.h"

#include "utils/FunctionHandle.h"

using namespace rir;


// actual rir api --------------------------------------------------------------

/** Returns TRUE if given SEXP is a valid rir compiled function. FALSE otherwise.
 */
REXPORT SEXP rir_isValidFunction(SEXP what) {
    return isValidFunctionSEXP(what) == nullptr ? R_FalseValue : R_TrueValue;
}

/** Prints the information in given Function SEXP
 */
REXPORT SEXP rir_disassemble(SEXP store) {
    if (TYPEOF(store) != INTSXP)
        Rf_error("Invalid type (expected INTSXP), got %u", TYPEOF(store));

    assert((unsigned)Rf_length(store) > sizeof(::Function) and
           "Corrupted int vector send");

    rir::FunctionHandle fun(store);
    Rprintf("Container length %u.\n", Rf_length(store));

    Function* f = fun.function;
    printFunction(f);
    return R_NilValue;
}

REXPORT SEXP rir_compile(SEXP what) {
    // TODO make this nicer
    if (TYPEOF(what) == CLOSXP) {
        if (TYPEOF(BODY(what)) != BCODESXP) {
            SEXP result = allocSExp(CLOSXP);
            PROTECT(result);
            auto res = Compiler::compileClosure(BODY(what), CLOENV(what), FORMALS(what));
            SET_FORMALS(result, res.formals);
            SET_CLOENV(result, CLOENV(what));
            SET_BODY(result, rir_createWrapperAst(res.bc));
            Rf_copyMostAttrib(what, result);
            UNPROTECT(1);
            return result;
        }
    } else if (TYPEOF(what) != BCODESXP) {
        auto res = Compiler::compileExpression(what);
        return rir_createWrapperAst(res.bc);
    }
    // can ony be bytecode now
    Rf_error("Cannot compile R bytecode - use ASTs instead");
}

/** Evaluates the given expression.
 */
REXPORT SEXP rir_eval(SEXP what, SEXP env) {
    ::Function * f = isValidFunctionObject(what);
    if (f == nullptr)
        f = isValidCodeWrapperSEXP(what);
    if (f == nullptr)
        Rf_error("Not rir compiled code");
    return evalRirCode(functionCode(f), globalContext(), env, 0);
}

// debugging & internal purposes API only --------------------------------------

/** Returns the constant pool object for inspection from R.
 */
REXPORT SEXP rir_cp() {
    return globalContext()->cp.list;
}

/** Returns the ast (source) pool object for inspection from R.
 */
REXPORT SEXP rir_src() {
    return globalContext()->src.list;
}

REXPORT SEXP rir_body(SEXP cls) {
    ::Function * f = isValidFunctionSEXP(cls);
    if (f == nullptr)
        Rf_error("Not a valid rir compiled function");
    return functionSEXP(f);
}

REXPORT SEXP rir_executeWrapper(SEXP bytecode, SEXP env) {
    ::Function * f = reinterpret_cast<::Function *>(INTEGER(bytecode));
    return evalRirCode(functionCode(f), globalContext(), env, 0);
}

REXPORT SEXP rir_executePromiseWrapper(SEXP function, SEXP offset, SEXP env) {
    assert(TYPEOF(function) == INTSXP && "Invalid rir function");
    assert(TYPEOF(offset) == INTSXP && Rf_length(offset) == 1 && "Invalid offset");
    unsigned ofs = (unsigned)INTEGER(offset)[0];
    ::Code * c = codeAt((Function*)INTEGER(function), ofs);
    return evalRirCode(c, globalContext(), env, 0);
}

// startup ---------------------------------------------------------------------

/** Initializes the rir contexts, registers the gc and so on...

  VECSXP length == # of pointers?
         tl = 0

 */
bool startup() {

    initializeRuntime(rir_compile);

    return true;
}

bool startup_ok = startup();
