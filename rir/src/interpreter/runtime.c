#include "runtime.h"
#include "interp.h"

SEXP envSymbol;
SEXP callSymbol;
SEXP execName;
SEXP promExecName;
Context* globalContext_;

Function* isValidFunctionSEXP(SEXP wrapper) {
    return isValidFunctionObject(wrapper);
}

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise returns nullptr.
 */
Function* isValidClosureSEXP(SEXP closure) {
    if (TYPEOF(closure) != CLOSXP)
        return nullptr;
    return isValidFunctionObject(BODY(closure));
}

Code* isValidPromiseSEXP(SEXP promise) {
    return isValidCodeObject(PRCODE(promise));
}

// for now, we will have to rewrite this when it goes to GNU-R proper

extern void c_printCode(Code * c);

void printCode(Code* c) {
    Rprintf("Code object (%p offset %x (hex))\n", c, c->header);
    Rprintf("  Source:    %u (index to src pool)\n", c->src);
    Rprintf("  Magic:     %x (hex)\n", c->magic);
    Rprintf("  Stack (o): %u\n", c->stackLength);
    Rprintf("  Code size: %u [b]\n", c->codeSize);
    if (c->magic != CODE_MAGIC)
        Rf_error("Wrong magic number -- corrupted IR bytecode");

    Rprintf("\n  Skiplist:  %u \n", c->skiplistLength);
    unsigned* sl = skiplist(c);
    for (unsigned i = 0; i < c->skiplistLength; ++i) {
        if (*(sl + 1) != -1)
            Rprintf("    pc: %u -> src_idx: %u\n", *sl, *(sl + 1));
        sl += 2;
    }
    Rprintf("\n");
    c_printCode(c);
}

void printFunction(Function* f) {
    Rprintf("Function object:\n");
    Rprintf("  Magic:           %x (hex)\n", f->magic);
    Rprintf("  Size:            %u\n", f->size);
    Rprintf("  Origin:          %s\n", f->origin ? "optimized" : "unoptimized");
    Rprintf("  Code objects:    %u\n", f->codeLength);
    Rprintf("  Fun code offset: %x (hex)\n", f->foffset);
    Rprintf("  Invoked:         %u\n", f->invocationCount);

    if (f->magic != FUNCTION_MAGIC)
        Rf_error("Wrong magic number -- not rir bytecode");

    // print respective code objects
    for (Code *c = begin(f), *e = end(f); c != e; c = next(c))
        printCode(c);
}

// TODO change gnu-r to expect ptr and not bool and we can get rid of the wrapper
int isValidFunctionObject_int_wrapper(SEXP closure) {
    return isValidFunctionObject(closure) != nullptr;
}

int isValidCodeObject_int_wrapper(SEXP code) {
    return isValidCodeObject(code) != nullptr;
}

void initializeRuntime(CompilerCallback compiler, OptimizerCallback optimizer) {
    envSymbol = Rf_install("environment");
    callSymbol = Rf_install(".Call");
    execName = Rf_mkString("rir_executeWrapper");
    R_PreserveObject(execName);
    promExecName = Rf_mkString("rir_executePromiseWrapper");
    R_PreserveObject(promExecName);
    // initialize the global context
    globalContext_ = context_create(compiler, optimizer);
    registerExternalCode(rirEval_f, compiler, rirExpr);
}

Context * globalContext() {
    return globalContext_;
}


