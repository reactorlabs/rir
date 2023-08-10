#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "R/Preserve.h"
#include "R/Protect.h"
#include "R/r.h"
#include "compilerClientServer/CompilerClient.h"
#include "compilerClientServer/CompilerServer.h"
#include "runtime/DispatchTable.h"
#include "utils/FunctionWriter.h"
#include "utils/Pool.h"

#include <cassert>
#include <functional>
#include <iostream>
#include <unordered_map>

namespace rir {

class Compiler {
  private:
    SEXP exp;
    SEXP formals;
    SEXP closureEnv;

    Preserve preserve;

    explicit Compiler(SEXP exp)
        : exp(exp), formals(R_NilValue), closureEnv(nullptr) {
        preserve(exp);
    }

    Compiler(SEXP exp, SEXP formals, SEXP env)
        : exp(exp), formals(formals), closureEnv(env) {
        preserve(exp);
        preserve(formals);
        preserve(env);
    }

    SEXP finalize();

  public:
    static bool profile;
    static bool unsoundOpts;
    static bool loopPeelingEnabled;

    static SEXP compileExpression(SEXP ast) {
        Compiler c(ast);
        return c.finalize();
    }

    // Compile a function which is not yet closed
    static SEXP compileFunction(SEXP ast, SEXP formals) {
        Protect p;

        Compiler c(ast, formals, nullptr);
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
        if (CompilerClient::isRunning() || CompilerServer::isRunning()) {
            // Store original body so we can send the AST to the server
            dt->setOriginalBody(BODY(inClosure));
        }

        // Set the closure fields.
        SET_BODY(inClosure, dt->container());
    }

    /// Input is a compiled closure whose body is a dispatch table, output is a
    /// closure with the same formals and environment, but with the body it had
    /// before compilation. Only works on the compiler client
    static SEXP decompiledClosure(SEXP closure) {
        assert((CompilerClient::isRunning() || CompilerServer::isRunning()) &&
               "we only store original closure bodies if the compiler client "
               "or server is running. See the above line in compileClosure "
               "where we call dt->setOriginalBody. Add extra to the if"
               "condition and then modify this assertion to extend support for "
               "other cases");
        assert(TYPEOF(closure) == CLOSXP);
        assert(DispatchTable::check(BODY(closure)));

        auto originalBody = DispatchTable::unpack(BODY(closure))->originalBody();
        assert(originalBody && "original body not set in dispatch table, how was it compiled?");

        SEXP newClosure = Rf_allocSExp(CLOSXP);
        SET_FORMALS(newClosure, FORMALS(closure));
        SET_BODY(newClosure, originalBody);
        SET_CLOENV(newClosure, CLOENV(closure));

        return newClosure;
    }
};

} // namespace rir

#endif
