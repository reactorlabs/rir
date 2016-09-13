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

#include "optimizer/Printer.h"
#include "code/analysis.h"
#include "optimizer/cp.h"

using namespace rir;

extern "C" void resetCompileExpressionOverride();
extern "C" void resetCmpFunOverride();
extern "C" void setCompileExpressionOverride(int, SEXP (*fun)(SEXP, SEXP));
extern "C" void setCmpFunOverride(int, SEXP (*fun)(SEXP));
typedef SEXP (*callback_eval)(SEXP, SEXP);
extern "C" void setEvalHook(callback_eval);
extern "C" void resetEvalHook();
static int rirJitEnabled = 0;

REXPORT SEXP rir_da(SEXP what) {
    ::Function * f = TYPEOF(what) == CLOSXP ? isValidClosureSEXP(what) : isValidFunctionSEXP(what);

    if (f == nullptr)
        Rf_error("Not a rir compiled code");

    rir::FunctionHandle fun(functionSEXP(f));
    CodeEditor ce(fun);
    Printer p;
    p.run(ce);

    ConstantPropagation cp;
    cp.analyze(ce);
    cp.print();


    return R_NilValue;
}



// actual rir api --------------------------------------------------------------

/** Returns TRUE if given SEXP is a valid rir compiled function. FALSE otherwise.
 */
REXPORT SEXP rir_isValidFunction(SEXP what) {
    return isValidClosureSEXP(what) == nullptr ? R_FalseValue : R_TrueValue;
}

/** Prints the information in given Function SEXP
 */
REXPORT SEXP rir_disassemble(SEXP what) {

    ::Function * f = TYPEOF(what) == CLOSXP ? isValidClosureSEXP(what) : isValidFunctionSEXP(what);

    if (f == nullptr)
        Rf_error("Not a rir compiled code");

    rir::FunctionHandle fun(functionSEXP(f));
    Rprintf("Container length %u.\n", Rf_length(what));

    printFunction(f);
    return R_NilValue;
}

REXPORT SEXP rir_compile(SEXP what) {

    // TODO make this nicer
    if (TYPEOF(what) == CLOSXP) {
        SEXP body = BODY(what);
        if (TYPEOF(body) == BCODESXP) {
            body = VECTOR_ELT(CDR(body), 0);
        }

        if (TYPEOF(body) == INTSXP)
            Rf_error("closure already compiled");

        SEXP result = allocSExp(CLOSXP);
        PROTECT(result);
        auto res = Compiler::compileClosure(body, FORMALS(what));
        SET_FORMALS(result, res.formals);
        SET_CLOENV(result, CLOENV(what));
        SET_BODY(result, rir_createWrapperAst(res.bc));
        Rf_copyMostAttrib(what, result);
        UNPROTECT(1);
        return result;
    } else {
        if (TYPEOF(what) == BCODESXP) {
            what = VECTOR_ELT(CDR(what), 0);
        }
        auto res = Compiler::compileExpression(what);
        return rir_createWrapperAst(res.bc);
    }
}

REXPORT SEXP rir_jitDisable(SEXP expression) {
    if (rirJitEnabled == 2)
        Rf_error("enabled sticky, cannot disable");

    rirJitEnabled = 0;
    resetCompileExpressionOverride();
    resetCmpFunOverride();
    resetEvalHook();
    return R_NilValue;
}

static SEXP compileClosure_(SEXP cls) {
    return rir_compile(cls);
}

static SEXP compileExpression_(SEXP exp, SEXP env) {
    return rir_compile(exp);
}

REXPORT SEXP rir_jitEnable(SEXP expression) {
    rirJitEnabled = 1;

    if (TYPEOF(expression) == INTSXP && INTEGER(expression)[0] == 1)
        rirJitEnabled = 2;

    setCompileExpressionOverride(INTSXP, &compileExpression_);
    setCmpFunOverride(INTSXP, &compileClosure_);
    setEvalHook(&rirEval);
    return R_NilValue;
}


/** Evaluates the given expression.
 */
REXPORT SEXP rir_eval(SEXP what, SEXP env) {
    ::Function * f = isValidFunctionObject(what);
    if (f == nullptr)
        f = isValidClosureSEXP(what);
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
    ::Function * f = isValidClosureSEXP(cls);
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
