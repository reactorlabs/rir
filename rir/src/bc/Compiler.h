#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "R/Preserve.h"
#include "R/Protect.h"
#include "R/r.h"
#include "runtime/DispatchTable.h"
#include "runtime/TypeFeedback.h"
#include "utils/FunctionWriter.h"
#include "utils/Pool.h"

#include <cassert>
#include <functional>
#include <iostream>
#include <unordered_map>
#include <unordered_set>

namespace rir {

class Compiler {
  private:
    SEXP exp;
    SEXP formals;
    SEXP closureEnv;

    // Stable variables captured from any enclosing function. Empty for
    // top-level compilations; populated when this Compiler is invoked for
    // an inner-function literal during another function's compilation.
    std::unordered_set<SEXP> outerSafe;

    Preserve preserve;

    explicit Compiler(SEXP exp)
        : exp(exp), formals(R_NilValue), closureEnv(nullptr) {
        preserve(exp);
    }

    Compiler(SEXP exp, SEXP formals, SEXP env,
             std::unordered_set<SEXP> outerSafe = {})
        : exp(exp), formals(formals), closureEnv(env),
          outerSafe(std::move(outerSafe)) {
        preserve(exp);
        preserve(formals);
        preserve(env);
    }

    SEXP finalize();

  public:
    static bool profile;
    static bool unsoundOpts;
    static bool loopPeelingEnabled;
    static bool recordLessEnabled;

    static SEXP compileExpression(SEXP ast) {
        Compiler c(ast);
        return c.finalize();
    }

    // Compile a function which is not yet closed.
    // `outerSafe` is the set of stable captured variable names from enclosing
    // scopes; pass empty (default) for top-level compilations.
    static SEXP compileFunction(SEXP ast, SEXP formals,
                                std::unordered_set<SEXP> outerSafe = {}) {
        Protect p;

        Compiler c(ast, formals, nullptr, std::move(outerSafe));
        auto res = p(c.finalize());

        // Allocate a new vtable.
        auto dt = DispatchTable::create();

        // Initialize the vtable. Initially the table has one entry, which is
        // the compiled function.
        dt->baseline(Function::unpack(res));

        return dt->container();
    }

    static void compileClosure(SEXP inClosure) {
        assert(TYPEOF(inClosure) == CLOSXP);

        Protect p;

        SEXP body = BODY(inClosure);
        SEXP origBC = nullptr;
        if (TYPEOF(body) == BCODESXP) {
            origBC = p(body);
            body = VECTOR_ELT(CDR(body), 0);
        }

        Compiler c(body, FORMALS(inClosure), CLOENV(inClosure));
        auto res = p(c.finalize());

        // Allocate a new vtable.
        auto dt = DispatchTable::create();
        p(dt->container());

        // Initialize the vtable. Initially the table has one entry, which is
        // the compiled function.
        dt->baseline(Function::unpack(res));
        // Keep alive. TODO: why is this needed?
        if (origBC)
            dt->baseline()->body()->addExtraPoolEntry(origBC);

        // Set the closure fields.
        SET_BODY(inClosure, dt->container());
    }
};

} // namespace rir

#endif
