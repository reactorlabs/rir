#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "R/r.h"
#include "R/Preserve.h"
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
    struct CompilerRes {
        SEXP bc;
        SEXP formals;
    };

    Compiler(SEXP exp) : exp(exp), formals(R_NilValue) { preserve(exp); }

    Compiler(SEXP exp, SEXP formals) : exp(exp), formals(formals) {
        preserve(exp);
        preserve(formals);
    }

    CompilerRes finalize();

#ifdef ENABLE_SLOWASSERT 
    static std::unordered_map<SEXP, unsigned> counter;
#endif

    static CompilerRes compileExpression(SEXP ast) {
#ifdef ENABLE_SLOWASSERT 
        unsigned count = 0;
        if (counter.count(ast))
            count = counter.at(ast);
        count++;
        counter[ast] = count;;

        if (count % 100 == 0) {
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

        static SEXP untrace, trace = NULL;
        if (!trace) {
            trace = Rf_install("trace");
            untrace = Rf_install("untrace");
        }

        // TODO wtf is up with trace?
        if (TYPEOF(ast) == LANGSXP && CAR(ast) != untrace &&
            CAR(ast) != trace) {
            static SEXP cache;
            if (!cache)
                cache = Rf_install("*.cachedBC.*");

            if (ATTRIB(ast) && ATTRIB(ast) != R_NilValue) {
                SEXP cached = Rf_getAttrib(ast, cache);
                if (cached != R_NilValue) {

#ifdef ENABLE_SLOWASSERT
                    Compiler c(ast);
                    auto res = c.finalize();

                    assert(Rf_length(res.bc) == Rf_length(cached));
#endif

                    return {cached, R_NilValue};
                }
            }

            Compiler c(ast);
            auto res = c.finalize();

            Rf_setAttrib(ast, cache, res.bc);

            return res;
        }

        Compiler c(ast);
        return c.finalize();
    }
    
    static CompilerRes compileClosure(SEXP ast, SEXP env, SEXP formals) {
        Compiler c(ast, formals);
        return c.finalize();
    }
};
}

#endif
