/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include <cassert>

// r print statement
#include "R_ext/Print.h"

#include "api.h"

#include "ir/Compiler.h"
#include "interpreter/interp_context.h"
#include "interpreter/interp.h"
#include "ir/BC.h"

#include "utils/FunctionHandle.h"

using namespace rir;

typedef bool (*callback_isValidFunction)(SEXP);
typedef SEXP (*callback_rirEval_f)(SEXP, SEXP);
typedef SEXP (*callback_rirExpr)(SEXP);

extern "C" void initializeCallbacks(callback_isValidFunction,
                                    callback_isValidFunction,
                                    callback_rirEval_f,
                                    callback_rirExpr);

// =======================================================================
// == RIR API
//


/** Compiles the given ast.
 */
REXPORT SEXP rir_compileAst(SEXP ast, SEXP env) {
    auto res = Compiler::compileExpression(ast);
    return res.bc;
}

REXPORT SEXP rir_compileClosure(SEXP f) {
    assert(TYPEOF(f) == CLOSXP and "Can only do closures");
    SEXP body = BODY(f);

    if (TYPEOF(body) == BCODESXP) {
        body = VECTOR_ELT(CDR(body), 0);
        //warning("Skipping jit of Bytecode");
        //return f;
    }

    assert(TYPEOF(body) != INTSXP and TYPEOF(body) != BCODESXP and
           "Can only do asts");
    SEXP result = allocSExp(CLOSXP);
    PROTECT(result);
    auto res = Compiler::compileClosure(body, CLOENV(f), FORMALS(f));
    SET_FORMALS(result, res.formals);
    SET_CLOENV(result, CLOENV(f));
    SET_BODY(result, res.bc);
    Rf_copyMostAttrib(f, result);
    UNPROTECT(1);
    return result;
}

extern "C" void resetCompileExpressionOverride();
extern "C" void resetCmpFunOverride();
extern "C" void setCompileExpressionOverride(int, SEXP (*fun)(SEXP, SEXP));
extern "C" void setCmpFunOverride(int, SEXP (*fun)(SEXP));

REXPORT SEXP rir_jitDisable(SEXP expression) {
    resetCompileExpressionOverride();
    resetCmpFunOverride();
    return R_NilValue;
}

REXPORT SEXP rir_jitEnable(SEXP expression) {
    setCompileExpressionOverride(INTSXP, &rir_compileAst);
    setCmpFunOverride(INTSXP, &rir_compileClosure);
    return R_NilValue;
}


REXPORT SEXP rir_compileClosureInPlace(SEXP f) {
    assert(TYPEOF(f) == CLOSXP and "Can only do closures");
    SEXP body = BODY(f);
    assert(TYPEOF(body) != INTSXP and TYPEOF(body) != BCODESXP and
           "Can only do asts");
    auto res = Compiler::compileClosure(body, CLOENV(f), FORMALS(f));
    SET_BODY(f, res.bc);
    SET_FORMALS(f, res.formals);
    return f;
}

REXPORT SEXP rir_exec(SEXP bytecode, SEXP env) {
    assert(isValidFunction(bytecode));
    ::Function* f = reinterpret_cast<::Function*>(INTEGER(bytecode));
    return rirEval_c(functionCode(f), globalContext(), env, 0);
}

/** Helper function that prints the code object.
 */
extern "C" void printCode(::Code* c) {
    Rprintf("Code object (offset %x (hex))\n", c->header);
    Rprintf("  Magic:     %x (hex)\n", c->magic);
    Rprintf("  Source:    %u (index to src pool)\n", c->src);
    Rprintf("  Stack (o): %u\n", c->stackLength);
    Rprintf("  Stack (i): %u\n", c->iStackLength);
    Rprintf("  Num insns: %u\n", c->srcLength);
    Rprintf("  Code size: %u [b]\n", c->codeSize);
    if (c->magic != CODE_MAGIC)
        Rf_error("Wrong magic number -- corrupted IR bytecode");

    rir::CodeHandle(c).print();
}

extern "C" void printFunction(::Function* f) {
    Rprintf("Function object:\n");
    Rprintf("  Magic:           %x (hex)\n", f->magic);
    Rprintf("  Size:            %u\n", f->size);
    Rprintf("  Origin:          %s\n", f->origin ? "optimized" : "unoptimized");
    Rprintf("  Code objects:    %u\n", f->codeLength);
    Rprintf("  Fun code offset: %x (hex)\n", f->foffset);

    if (f->magic != FUNCTION_MAGIC)
        Rf_error("Wrong magic number -- not rir bytecode");

    // print respective code objects
    for (::Code *c = ::begin(f), *e = ::end(f); c != e; c = ::next(c))
        printCode(c);
}

/** Prints the information in given Function SEXP
 */
REXPORT SEXP rir_print(SEXP store) {
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


// =======================================================================
// == Callbacks
//

void rjit_globalGcCallback(void (*forward_node)(SEXP)) {
    Precious::gcCallback(forward_node);
    rir_interp_gc_callback(forward_node);
}

/** Initializes the rir contexts, registers the gc and so on...
 */
bool startup() {
    
    // TODO give a compiler proper
    interp_initialize(rir_compileAst);

    registerGcCallback(&rjit_globalGcCallback);

    initializeCallbacks(isValidFunction, isValidPromise, rirEval_f, rirExpr);

    return true;
}

bool startup_ok = startup();
