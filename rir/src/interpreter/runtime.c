#include "runtime.h"
#include "interp.h"

SEXP envSymbol;
SEXP callSymbol;
SEXP execName;
SEXP promExecName;
Context * globalContext_;

#if RIR_AS_PACKAGE == 1

// envir.c: 1320
// only needed by ddfindVar
int ddVal(SEXP symbol) {
    const char *buf;
    char *endp;
    int rval;

    buf = CHAR(PRINTNAME(symbol));
    if( !strncmp(buf,"..",2) && strlen(buf) > 2 ) {
    buf += 2;
    rval = (int) strtol(buf, &endp, 10);
    if( *endp != '\0')
        return 0;
    else
        return rval;
    }
    return 0;
}

// envir.c 1357, our version ignores i18n
SEXP Rf_ddfindVar(SEXP symbol, SEXP rho) {
    int i;
    SEXP vl;

    /* first look for ... symbol  */
    vl = findVar(R_DotsSymbol, rho);
    i = ddVal(symbol);
    if (vl != R_UnboundValue) {
        if (Rf_length(vl) >= i) {
            vl = nthcdr(vl, i - 1);
            return(CAR(vl));
        } else {
            error("the ... list does not contain %d elements", i);
        }
    } else {
        error("..%d used in an incorrect context, no ... to look in", i);
    }
    return R_NilValue;
}

// memory.c 2333
/** Creates a promise.

  This is not the fastest way as we always PROTECT expr and rho, but GNU-R's GC does not seem to publish its free nodes API.
 */
SEXP mkPROMISE(SEXP expr, SEXP rho) {
    PROTECT(expr);
    PROTECT(rho);
    SEXP s = Rf_allocSExp(PROMSXP);
    UNPROTECT(2);

    /* precaution to ensure code does not get modified via
       substitute() and the like */
    if (NAMED(expr) < 2) SET_NAMED(expr, 2);

    SET_PRCODE(s, expr);
    SET_PRENV(s, rho);
    SET_PRVALUE(s, R_UnboundValue);
    SET_PRSEEN(s, 0);

    return s;
}

#endif

Function * isValidCodeWrapperSEXP(SEXP wrapper) {
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
    return (Function*)INTEGER(x);
}

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise returns nullptr.
 */
Function * isValidFunctionSEXP(SEXP closure) {
    if (TYPEOF(closure) != CLOSXP)
        return nullptr;
    return isValidCodeWrapperSEXP(BODY(closure));
}

Code * isValidPromiseSEXP(SEXP promise) {
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
    if (TYPEOF(x) != INTSXP || Rf_length(x) != 1)
        return nullptr;
    unsigned offset = (unsigned)INTEGER(x)[0];
    return codeAt((Function*)INTEGER(code), offset);
}

// for now, we will have to rewrite this when it goes to GNU-R proper

extern void c_printCode(Code * c);

void printCode(Code* c) {
    Rprintf("Code object (offset %x (hex))\n", c->header);
    Rprintf("  Magic:     %x (hex)\n", c->magic);
    Rprintf("  Source:    %u (index to src pool)\n", c->src);
    Rprintf("  Stack (o): %u\n", c->stackLength);
    Rprintf("  Stack (i): %u\n", c->iStackLength);
    Rprintf("  Num insns: %u\n", c->srcLength);
    Rprintf("  Code size: %u [b]\n", c->codeSize);
    if (c->magic != CODE_MAGIC)
        Rf_error("Wrong magic number -- corrupted IR bytecode");
    c_printCode(c);
}

void printFunction(Function* f) {
    Rprintf("Function object:\n");
    Rprintf("  Magic:           %x (hex)\n", f->magic);
    Rprintf("  Size:            %u\n", f->size);
    Rprintf("  Origin:          %s\n", f->origin ? "optimized" : "unoptimized");
    Rprintf("  Code objects:    %u\n", f->codeLength);
    Rprintf("  Fun code offset: %x (hex)\n", f->foffset);

    if (f->magic != FUNCTION_MAGIC)
        Rf_error("Wrong magic number -- not rir bytecode");

    // print respective code objects
    for (Code *c = begin(f), *e = end(f); c != e; c = next(c))
        printCode(c);
}

SEXP rir_createWrapperAst(SEXP rirBytecode) {
#if RIR_AS_PACKAGE == 1
    SEXP envCall = Rf_lang1(envSymbol);
    PROTECT(envCall);
    SEXP result =  Rf_lang4(callSymbol, execName, rirBytecode, envCall);
    UNPROTECT(1);
    return result;
#else
    return rirBytecode;
#endif
}

SEXP rir_createWrapperPromise(Code * code) {
#if RIR_AS_PACKAGE == 1
    SEXP envCall = lang1(envSymbol);
    PROTECT(envCall);
    SEXP offset = Rf_allocVector(INTSXP, 1);
    PROTECT(offset);
    INTEGER(offset)[0] = code->header;
    SEXP result =  Rf_lang5(callSymbol, promExecName, functionSEXP(function(code)), offset, envCall);
    UNPROTECT(2);
    return result;
#else
    return (SEXP)code;
#endif
}

// TODO change gnu-r to expect ptr and not bool aand we can get rid of the wrapper
int isValidFunctionObject_int_wrapper(SEXP closure) {
    return isValidFunctionObject(closure) != nullptr;
}

int isValidCodeObject_int_wrapper(SEXP code) {
    return isValidCodeObject(code) != nullptr;

}




void initializeRuntime(CompilerCallback compiler) {
    envSymbol = Rf_install("environment");
    callSymbol = Rf_install(".Call");
    execName = Rf_mkString("rir_executeWrapper");
    R_PreserveObject(execName);
    promExecName = Rf_mkString("rir_executePromiseWrapper");
    R_PreserveObject(promExecName);
    // initialize the global context
    globalContext_ = context_create(compiler);
#if RIR_AS_PACKAGE == 0
    initializeCallbacks(
        isValidFunctionObject_int_wrapper,
        isValidCodeObject_int_wrapper,
        rirEval_f,
        rirExpr
    );
#endif
}

Context * globalContext() {
    return globalContext_;
}


