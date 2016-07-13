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

// helper functions - not really exported to R, but helpful for both C and C++ code


extern "C" ::Function * isValidCodeWrapperSEXP(SEXP wrapper) {
    if (TYPEOF(wrapper) != LANGSXP)
        return nullptr;
    // now we know it is uncompiled function, check that it contains what we expect
    SEXP x = CAR(wrapper);
    if (x != callSymbol)
        return nullptr;
    wrapper = CDR(wrapper);
    if (wrapper == R_NilValue)
        return nullptr;
    x = CAR(wrapper);
    if (x != execName)
        return nullptr;
    wrapper = CDR(wrapper);
    if (wrapper == R_NilValue)
        return nullptr;
    x = CAR(wrapper);
    if (TYPEOF(x) != INTSXP)
        return nullptr;
    // that's enough checking, return the function
    return reinterpret_cast<::Function*>(INTEGER(x));
}

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise returns nullptr.
 */
extern "C" ::Function * isValidFunctionSEXP(SEXP closure) {
    if (TYPEOF(closure) != CLOSXP)
        return nullptr;
    return isValidCodeWrapperSEXP(BODY(closure));
}

extern "C" ::Code * isValidPromiseSEXP(SEXP promise) {
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

extern "C" SEXP rir_createWrapperAst(SEXP rirBytecode) {
    SEXP envCall = Rf_lang1(envSymbol);
    PROTECT(envCall);
    SEXP result =  Rf_lang4(callSymbol, execName, rirBytecode, envCall);
    UNPROTECT(1);
    return result;
}

extern "C" SEXP rir_createWrapperPromise(Code * code) {
    SEXP envCall = lang1(envSymbol);
    PROTECT(envCall);
    SEXP offset = Rf_allocVector(INTSXP, 1);
    PROTECT(offset);
    INTEGER(offset)[0] = code->header;
    SEXP result =  Rf_lang5(callSymbol, promExecName, functionSEXP(function(code)), offset, envCall);
    UNPROTECT(2);
    return result;
}

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
    envSymbol = Rf_install("environment");
    callSymbol = Rf_install(".Call");
    execName = Rf_mkString("rir_executeWrapper");
    R_PreserveObject(execName);
    promExecName = Rf_mkString("rir_executePromiseWrapper");
    R_PreserveObject(promExecName);

    interp_initialize(rir_compile);

    return true;
}

bool startup_ok = startup();
