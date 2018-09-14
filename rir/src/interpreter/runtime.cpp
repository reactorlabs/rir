#include "runtime.h"
#include "api.h"
#include "interp.h"

#include <sstream>

SEXP envSymbol;
SEXP callSymbol;
SEXP execName;
SEXP promExecName;
Context* globalContext_;
rir::Configurations* configurations;

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise
  returns nullptr.
 */
Function* isValidClosureSEXP(SEXP closure) {
    if (TYPEOF(closure) != CLOSXP) {
        return nullptr;
    }
    if (auto t = DispatchTable::check(BODY(closure))) {
        return t->first();
    }
    return nullptr;
}

// for now, we will have to rewrite this when it goes to GNU-R proper

void printFunction(Function* f) {
    Rprintf("Function object (%p):\n", f);
    Rprintf("  Magic:           %x (hex)\n", f->info.magic);
    Rprintf("  Size:            %u\n", f->size);
    Rprintf("  Origin:          %p %s\n", f->origin(), f->origin() ? "" : "(unoptimized)");
    Rprintf("  Next:            %p\n", f->next());
    Rprintf("  Code objects:    %u\n", f->codeLength);
    Rprintf("  Fun code addr:   %x (hex)\n", f->body());
    Rprintf("  Invoked:         %u\n", f->invocationCount);
    Rprintf("  Signature:       %p\n", f->signature);
    if (f->signature)
        f->signature->print();

    if (f->info.magic != FUNCTION_MAGIC)
        Rf_error("Wrong magic number -- not rir bytecode");

    // print respective code objects
    std::stringstream output;
    for (Code* c : *f)
        c->print(output);
    std::cout << output.str();
}

void initializeRuntime() {
    envSymbol = Rf_install("environment");
    callSymbol = Rf_install(".Call");
    execName = Rf_mkString("rir_executeWrapper");
    R_PreserveObject(execName);
    promExecName = Rf_mkString("rir_executePromiseWrapper");
    R_PreserveObject(promExecName);
    // initialize the global context
    globalContext_ = context_create();
    registerExternalCode(rirEval_f, rir_compile, rirExpr);
    configurations = new rir::Configurations();
}

Context* globalContext() { return globalContext_; }
rir::Configurations* pirConfigurations() { return configurations; }
