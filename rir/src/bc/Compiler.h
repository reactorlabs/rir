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

    // Variables from any enclosing function in our "realm" (controlled env
    // chain). Superset of outerImmutable. Enables RecordOnce.
    std::unordered_set<SEXP> outerControlled;
    // Strict subset: values that truly won't change during this function's
    // lifetime. Reserved for future cross-invocation optimizations.
    std::unordered_set<SEXP> outerImmutable;

    Preserve preserve;

    explicit Compiler(SEXP exp)
        : exp(exp), formals(R_NilValue), closureEnv(nullptr) {
        preserve(exp);
    }

    Compiler(SEXP exp, SEXP formals, SEXP env,
             std::unordered_set<SEXP> outerImmutable = {},
             std::unordered_set<SEXP> outerControlled = {})
        : exp(exp), formals(formals), closureEnv(env),
          outerControlled(std::move(outerControlled)),
          outerImmutable(std::move(outerImmutable)) {
        preserve(exp);
        preserve(formals);
        preserve(env);
    }

    SEXP finalize();

  public:
    static bool profile;
    static bool unsoundOpts;
    static bool loopPeelingEnabled;
    static bool recordLess_Leaf_Enabled;

    static bool isRecordlessLeafEnabled() {
        return profile && recordLess_Leaf_Enabled;
    }

    static SEXP compileExpression(SEXP ast) {
        Compiler c(ast);
        return c.finalize();
    }

    // Compile a function which is not yet closed.
    // `outerImmutable` / `outerControlled` are capture sets from the enclosing
    // compiler; pass empty (default) for top-level compilations.
    static SEXP compileFunction(SEXP ast, SEXP formals,
                                std::unordered_set<SEXP> outerImmutable = {},
                                std::unordered_set<SEXP> outerControlled = {}) {
        Protect p;

        Compiler c(ast, formals, nullptr, std::move(outerImmutable),
                   std::move(outerControlled));
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
