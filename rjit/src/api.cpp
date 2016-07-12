/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include <cassert>

// r print statement
#include <R_ext/Print.h>

#include "api.h"

#include "ir/Compiler.h"
#include "interpreter/interp_context.h"
#include "interpreter/interp.h"
#include "ir/BC.h"

#include "utils/FunctionHandle.h"

using namespace rir;

namespace {
    SEXP envSymbol;
    SEXP callSymbol;
    SEXP execName;
    SEXP promExecName;
}


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

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise returns nullptr.
 */
REXPORT ::Function * c_isValidFunction(SEXP closure) {
    if (TYPEOF(closure) != CLOSXP)
        return nullptr;
    SEXP body = BODY(closure);
    if (TYPEOF(body) != LANGSXP)
        return nullptr;
    // now we know it is uncompiled function, check that it contains what we expect
    SEXP x = CAR(body);
    if (x != callSymbol)
        return nullptr;
    body = CDR(body);
    if (body == R_NilValue)
        return nullptr;
    x = CAR(body);
    if (x != execName)
        return nullptr;
    body = CDR(body);
    if (body == R_NilValue)
        return nullptr;
    x = CAR(body);
    if (TYPEOF(x) != INTSXP)
        return nullptr;
    // that's enough checking, return the function
    return reinterpret_cast<::Function*>(INTEGER(x));
}

REXPORT ::Code * c_isValidPromise(SEXP promise) {
    SEXP body = PRCODE(promise);
    if (TYPEOF(body) != LANGSXP)
        return nullptr;
    // now we know it is uncompiled function, check that it contains what we expect
    SEXP x = CAR(body);
    if (x != callSymbol)
        return nullptr;
    body = CDR(body);
    if (body == R_NilValue)
        return nullptr;
    x = CAR(body);
    if (x != promExecName)
        return nullptr;
    body = CDR(body);
    if (body == R_NilValue)
        return nullptr;
    SEXP code  = CAR(body);
    if (TYPEOF(code) != INTSXP)
        return nullptr;
    body = CDR(body);
    if (body == R_NilValue)
        return nullptr;
    x = CAR(body);
    if (TYPEOF(x) != INTSXP or Rf_length(x) != 1)
        return nullptr;
    unsigned offset = static_cast<unsigned>(INTEGER(x)[0]);
    return codeAt(reinterpret_cast<::Function*>(INTEGER(code)), offset);
}

REXPORT SEXP rir_isValidFunction(SEXP what) {
    return c_isValidFunction(what) == nullptr ? R_FalseValue : R_TrueValue;
}

REXPORT SEXP rir_createWrapperAst(SEXP rirBytecode) {
    SEXP envCall = Rf_lang1(envSymbol);
    PROTECT(envCall);
    SEXP result =  Rf_lang4(callSymbol, execName, rirBytecode, envCall);
    UNPROTECT(1);
    return result;
}

/** Compiles the given ast.
 */
REXPORT SEXP rir_compileAst(SEXP ast, SEXP env) {
    auto res = Compiler::compileExpression(ast);

    return rir_createWrapperAst(res.bc);
    // return res.bc;
}



REXPORT SEXP rir_createWrapperPromise(Code * code) {
    printf("Creating promise");
    SEXP envCall = lang1(envSymbol);
    PROTECT(envCall);
    SEXP offset = Rf_allocVector(INTSXP, 1);
    PROTECT(offset);
    INTEGER(offset)[0] = code->header;
    SEXP result =  Rf_lang5(callSymbol, promExecName, functionSEXP(function(code)), offset, envCall);
    UNPROTECT(2);
    return result;
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
    //SET_BODY(result, res.bc);
    SET_BODY(result, rir_createWrapperAst(res.bc));
    Rf_copyMostAttrib(f, result);
    UNPROTECT(1);
    return result;
}





REXPORT SEXP rir_executeWrapper(SEXP bytecode, SEXP env) {
    ::Function * f = reinterpret_cast<::Function *>(INTEGER(bytecode));
    printf("Evaluating function\n");
    return rirEval_c(functionCode(f), globalContext(), env, 0);
}

REXPORT SEXP rir_executePromiseWrapper(SEXP function, SEXP offset, SEXP env) {
    assert(TYPEOF(function) == INTSXP && "Invalid rir function");
    assert(TYPEOF(offset) == INTSXP && Rf_length(offset) == 1 && "Invalid offset");
    unsigned ofs = (unsigned)INTEGER(offset)[0];
    printf("Evaluating promise at offset %u\n", ofs);
    ::Code * c = codeAt((Function*)INTEGER(function), ofs);
    return rirEval_c(c, globalContext(), env, 0);
}

//extern "C" void resetCompileExpressionOverride();
//extern "C" void resetCmpFunOverride();
//extern "C" void setCompileExpressionOverride(int, SEXP (*fun)(SEXP, SEXP));
//extern "C" void setCmpFunOverride(int, SEXP (*fun)(SEXP));

REXPORT SEXP rir_jitDisable(SEXP expression) {
//    resetCompileExpressionOverride();
//    resetCmpFunOverride();
    return R_NilValue;
}

REXPORT SEXP rir_jitEnable(SEXP expression) {
//    setCompileExpressionOverride(INTSXP, &rir_compileAst);
//    setCmpFunOverride(INTSXP, &rir_compileClosure);
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

/** Initializes the rir contexts, registers the gc and so on...

  VECSXP length == # of pointers?
         tl = 0

 */
bool startup() {
    envSymbol = Rf_install("environment");
    callSymbol = Rf_install(".Call");
    execName = Rf_mkString("rir_executeWrapper");
    R_PreserveObject(execName);
    promExecName = Rf_mkString("rir_executePromiseWrapper");
    R_PreserveObject(promExecName);

    interp_initialize(rir_compileAst);

    return true;
}

bool startup_ok = startup();
