#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "R/r.h"
#include "R/Preserve.h"
#include "R/Protect.h"
#include "utils/FunctionHandle.h"

#include <unordered_map>
#include <iostream>
#include <functional>
#include <cassert>

typedef struct RCNTXT RCNTXT;
extern RCNTXT* R_GlobalContext;
extern "C" SEXP R_syscall(int n, RCNTXT *cptr);
extern "C" SEXP R_sysfunction(int n, RCNTXT *cptr);

namespace rir {

class Compiler {
    SEXP exp;
    SEXP formals;
    Preserve preserve;

  public:

    Compiler(SEXP exp) : exp(exp), formals(R_NilValue) {
        preserve(exp);
    }

    Compiler(SEXP exp, SEXP formals) : exp(exp), formals(formals) {
        preserve(exp);
        preserve(formals);
    }

    SEXP finalize();

    static SEXP compileExpression(SEXP ast) {
#if 0
        size_t count = 1;
        static std::unordered_map<SEXP, size_t> counts;
        if (counts.count(ast)) {
            counts.at(ast) = count = 1 + counts.at(ast);
        } else {
            counts[ast] = 1;
        }
        if (count % 200 == 0) {
            std::cout << "<<<<<<< Warning: expression compiled "
                      << count << "x:\n";
            Rf_PrintValue(ast);
            std::cout << "== Call:\n";
            Rf_PrintValue(R_syscall(0, R_GlobalContext));
            std::cout << "== Function:\n";
            Rf_PrintValue(R_sysfunction(0, R_GlobalContext));
            std::cout << ">>>>>>>\n";
        }
#endif

        // Rf_PrintValue(ast);
        Compiler c(ast);
        return c.finalize();
    }

    static SEXP compileClosure(SEXP ast, SEXP formals) {
        Protect p;
        SEXP closure = p(allocSExp(CLOSXP));

        Compiler c(ast, formals);
        SEXP res = p(c.finalize());

        // Set the compiled function's closure pointer.
        Function* func = sexp2function(res);
        func->closure = closure;

        // Allocate a new vtable.
        size_t vtableSize = sizeof(DispatchTable) + sizeof(DispatchTableEntry);
        SEXP vtableStore = p(Rf_allocVector(EXTERNALSXP, vtableSize));
        DispatchTable* vtable = sexp2dispatchTable(vtableStore);

        // Initialize the vtable. Initially the table has one entry, which is
        // the compiled function.
        SET_TRUELENGTH(vtableStore, 1);
        vtable->magic = DISPATCH_TABLE_MAGIC;
        vtable->length = 1;
        vtable->entry[0] = res;

        // Set the closure fields.
        // NOTE: The closure environment is set by the caller.
        SET_BODY(closure, vtableStore);
        SET_FORMALS(closure, formals);

        return closure;
    }
};

}

#endif
