#include "runtime.h"
#include "api.h"
#include "interp.h"

#include <iomanip>

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

void printFunction(Function* f, std::ostream& out) {
    out << "Function object (" << static_cast<void*>(f) << "):\n";
    out << std::left << std::setw(20) << "  Magic:" << std::hex << f->info.magic
        << std::dec << " (hex)\n";
    out << std::left << std::setw(20) << "  Size:" << f->size << "\n";
    out << std::left << std::setw(20) << "  Origin:";
    if (f->origin()) {
        out << static_cast<void*>(f->origin()) << "\n";
    } else {
        out << "(nil) (unoptimized)\n";
    }
    out << std::left << std::setw(20) << "  Next:";
    if (f->next()) {
        out << f->next() << "\n";
    } else {
        out << "(nil)"
            << "\n";
    }
    out << std::left << std::setw(20) << "  Code objects:" << f->codeLength
        << "\n";
    out << std::left << std::setw(20)
        << "  Fun code addr:" << static_cast<void*>(f->body()) << "\n";
    out << std::left << std::setw(20) << "  Invoked:" << f->invocationCount
        << "\n";
    out << std::left << std::setw(20) << "  Signature:";
    if (f->signature) {
        out << static_cast<void*>(f->signature) << "\n";
        f->signature->print(out);
    } else {
        out << "(nil)"
            << "\n";
    }

    if (f->info.magic != FUNCTION_MAGIC)
        Rf_error("Wrong magic number -- not rir bytecode");

    // print respective code objects
    for (Code* c : *f)
        c->print(out);
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
