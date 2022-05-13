#ifndef RIR_INTERPRETER_DATA_C_H
#define RIR_INTERPRETER_DATA_C_H

#include "R/Symbols.h"
#include "R/r.h"
#include "config.h"
#include "instance.h"
#include "interp_incl.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/LazyEnvironment.h"
#include "safe_force.h"

#include <assert.h>
#include <stdint.h>

namespace rir {

struct CallContext {
    CallContext(const CallContext&) = delete;
    CallContext& operator=(CallContext&) = delete;

    CallContext(ArglistOrder::CallId callId, const Code* c, SEXP callee,
                size_t nargs, SEXP ast, R_bcstack_t* stackArgs,
                const Immediate* names, SEXP callerEnv, SEXP suppliedvars,
                const Context& givenContext)
        : callId(callId), caller(c), suppliedArgs(nargs), passedArgs(nargs),
          stackArgs(stackArgs), names(names), callerEnv(callerEnv),
          suppliedvars(suppliedvars), ast(ast), callee(callee),
          givenContext(givenContext) {
        assert(callerEnv);
        assert(callee &&
               (TYPEOF(callee) == CLOSXP || TYPEOF(callee) == SPECIALSXP ||
                TYPEOF(callee) == BUILTINSXP));
        SLOWASSERT(callerEnv == symbol::delayedEnv ||
                   TYPEOF(callerEnv) == ENVSXP || callerEnv == R_NilValue ||
                   LazyEnvironment::check(callerEnv));
    }

    // cppcheck-suppress uninitMemberVar
    CallContext(ArglistOrder::CallId callId, Code* c, SEXP callee, size_t nargs,
                Immediate ast, R_bcstack_t* stackArgs, Immediate* names,
                SEXP callerEnv, SEXP suppliedvars, const Context& givenContext)
        : CallContext(callId, c, callee, nargs, cp_pool_at(ast), stackArgs,
                      names, callerEnv, suppliedvars, givenContext) {}

    // cppcheck-suppress uninitMemberVar
    CallContext(ArglistOrder::CallId callId, Code* c, SEXP callee, size_t nargs,
                Immediate ast, R_bcstack_t* stackArgs, SEXP callerEnv,
                SEXP suppliedvars, const Context& givenContext)
        : CallContext(callId, c, callee, nargs, cp_pool_at(ast), stackArgs,
                      nullptr, callerEnv, suppliedvars, givenContext) {}

    const ArglistOrder::CallId callId;
    const Code* caller;
    const size_t suppliedArgs;
    size_t passedArgs;
    R_bcstack_t* stackArgs;
    const Immediate* names;
    SEXP callerEnv;
    SEXP suppliedvars;
    const SEXP ast;
    const SEXP callee;
    Context givenContext;
    SEXP arglist = nullptr;
    bool triggerOsr = false;

    bool hasEagerCallee() const { return TYPEOF(callee) == BUILTINSXP; }
    bool hasNames() const { return names; }

    SEXP stackArg(unsigned i) const {
        assert(stackArgs);
        assert(i < passedArgs);
        return ostack_at_cell(stackArgs + i);
    }

    void setStackArg(SEXP what, unsigned i) const {
        assert(stackArgs && i < passedArgs);
        ostack_set_cell(stackArgs + i, what);
    }

    SEXP name(unsigned i) const {
        assert(hasNames() && i < suppliedArgs);
        return cp_pool_at(names[i]);
    }

    void safeForceArgs() const {
        for (unsigned i = 0; i < passedArgs; i++) {
            SEXP arg = stackArg(i);
            if (TYPEOF(arg) == PROMSXP) {
                safeForcePromise(arg);
            }
        }
    }

    void depromiseArgs() const {

        // Both stackArgs and listArgs should point to the same promises
        // So forcing twice shouldn't execute the promise twice

        // depromise on stack
        for (unsigned i = 0; i < passedArgs; i++) {
            SEXP arg = stackArg(i);
            if (TYPEOF(arg) == PROMSXP) {
                auto v = forcePromise(arg);
                setStackArg(v, i);
            }
        }

        // depromise on listArgs (linked list of CONS)
        if (arglist) {
            for (SEXP c = arglist; c != R_NilValue; c = CDR(c)) {
                SEXP each = CAR(c);
                if (TYPEOF(each) == PROMSXP) {
                    auto v = forcePromise(each);
                    SETCAR(c, v);
                }
            }
        }
    }
};

} // namespace rir

#endif // RIR_INTERPRETER_C_H
