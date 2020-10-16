#ifndef RIR_INTERPRETER_DATA_C_H
#define RIR_INTERPRETER_DATA_C_H

#include "../config.h"
#include <assert.h>
#include <stdint.h>

#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"

#include "R/Symbols.h"
#include "R/r.h"

#include "LazyEnvironment.h"
#include "instance.h"
#include "interp_incl.h"
#include "safe_force.h"

namespace rir {

struct CallContext {
    CallContext(const CallContext&) = delete;
    CallContext& operator=(CallContext&) = delete;

  private:
    CallContext(Code* c, SEXP callee, size_t nargs, size_t nargsOrig, SEXP ast,
                R_bcstack_t* stackArgs, Immediate* argOrderOrig,
                Immediate* names, SEXP callerEnv, const Context& givenContext,
                bool staticCall)
        : caller(c), suppliedArgs(nargs), nargsOrig(nargsOrig),
          passedArgs(nargs), stackArgs(stackArgs), argOrderOrig(argOrderOrig),
          names(names), callerEnv(callerEnv), ast(ast), callee(callee),
          givenContext(givenContext), staticCall(staticCall) {
        assert(callerEnv);
        assert(callee &&
               (TYPEOF(callee) == CLOSXP || TYPEOF(callee) == SPECIALSXP ||
                TYPEOF(callee) == BUILTINSXP));
        assert((staticCall && argOrderOrig && !names) ||
               (!staticCall && !argOrderOrig));
        SLOWASSERT(callerEnv == symbol::delayedEnv ||
                   TYPEOF(callerEnv) == ENVSXP || callerEnv == R_NilValue ||
                   LazyEnvironment::check(callerEnv));
        if (nargs == 0)
            this->argOrderOrig = nullptr;
    }

  public:
    // For regular calls (named and with dots, too)
    CallContext(Code* c, SEXP callee, size_t nargs, SEXP ast,
                R_bcstack_t* stackArgs, Immediate* names, SEXP callerEnv,
                const Context& givenContext)
        : CallContext(c, callee, nargs, nargs, ast, stackArgs, nullptr, names,
                      callerEnv, givenContext, false) {}

    // For static calls
    CallContext(Code* c, SEXP callee, size_t nargs, size_t nargsOrig, SEXP ast,
                R_bcstack_t* stackArgs, Immediate* argOrderOrig, SEXP callerEnv,
                const Context& givenContext)
        : CallContext(c, callee, nargs, nargsOrig, ast, stackArgs, argOrderOrig,
                      nullptr, callerEnv, givenContext, true) {}

    // For builtin calls
    CallContext(Code* c, SEXP callee, size_t nargs, SEXP ast,
                R_bcstack_t* stackArgs, SEXP callerEnv,
                const Context& givenContext)
        : CallContext(c, callee, nargs, nargs, ast, stackArgs, nullptr, nullptr,
                      callerEnv, givenContext, false) {}

    const Code* caller;
    const size_t suppliedArgs;
    const size_t nargsOrig;
    size_t passedArgs;
    const R_bcstack_t* stackArgs;
    const Immediate* argOrderOrig;
    const Immediate* names;
    SEXP callerEnv;
    const SEXP ast;
    const SEXP callee;
    Context givenContext;
    bool staticCall;
    SEXP arglist = nullptr;

    bool hasEagerCallee() const { return TYPEOF(callee) == BUILTINSXP; }
    bool hasNames() const { return names; }

    SEXP stackArg(unsigned i) const {
        assert(stackArgs && i < passedArgs);
        return ostack_at_cell(stackArgs + i);
    }

    SEXP formals() const {
        return TYPEOF(callee) == CLOSXP ? FORMALS(callee) : nullptr;
    }

    SEXP name(unsigned i, InterpreterInstance* ctx) const {
        assert(hasNames() && i < suppliedArgs);
        return cp_pool_at(ctx, names[i]);
    }

    void safeForceArgs() const {
        for (unsigned i = 0; i < passedArgs; i++) {
            SEXP arg = stackArg(i);
            if (TYPEOF(arg) == PROMSXP) {
                safeForcePromise(arg);
            }
        }
    }
};

} // namespace rir

#endif // RIR_INTERPRETER_C_H
