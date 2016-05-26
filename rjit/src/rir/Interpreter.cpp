#include "Interpreter.h"
#include "RIntlns.h"
#include "Function.h"
#include "Compiler.h"
#include "CodeStream.h"
#include "Runtime.h"
#include "Primitives.h"
#include "../RList.h"
#include "../Symbols.h"

#include <iostream>
#include <deque>
#include <array>

extern "C" Rboolean convertToLogicalNoNA(SEXP what, SEXP consts, int call);

namespace rjit {
namespace rir {

namespace {

BCClosure* jit(SEXP fun) {
    std::cout << "JIT Compiling a function to BC\n";
    Compiler c(BODY(fun), FORMALS(fun));
    BCClosure* cls = new BCClosure;
    cls->env = CLOENV(fun);
    cls->fun = c.finalize();
    // TODO: compile default args
    cls->formals = FORMALS(fun);
    cls->nargs = RList(cls->formals).length();
    return cls;
}

BCClosure* jit(SEXP ast, SEXP formals, SEXP env) {
    Compiler c(ast, formals);
    BCClosure* cls = new BCClosure;
    cls->env = env;
    cls->fun = c.finalize();
    // TODO: compile default args
    cls->formals = formals;
    cls->nargs = RList(formals).length();
    return cls;
}

#define INLINE __attribute__((always_inline)) inline

// ============================================================================
// Common container class for various stacks in the interpreter

template <typename T>
class Stack {
    std::vector<T> stack;
    size_t size_ = 0;
    size_t capacity = 2048;

    void grow() {
        capacity *= 2;
        stack.resize(capacity);
    }

  public:
    Stack() { stack.resize(capacity); }

    INLINE void push(T s) {
        if (size_ == capacity)
            grow();
        stack[size_++] = s;
    }

    INLINE bool empty() { return size_ == 0; }

    INLINE T pop() { return stack[--size_]; }
    INLINE void pop(size_t n) { size_ -= n; }

    INLINE T& peek(size_t offset) { return stack[size_ - 1 - offset]; }

    INLINE T set(size_t offset, T val) {
        return stack[size_ - 1 - offset] = val;
    }

    INLINE T& at(size_t pos) { return stack[pos]; }

    INLINE T& top() { return stack[size_ - 1]; }

    INLINE size_t size() { return size_; }
};

// =============================================================================
// === Interpreter Datastructures

// The stacks

static Stack<SEXP> stack;
Stack<int> stacki;
Stack<fun_idx_t> callArgs;

// =============================================================================

static INLINE SEXP callPrimitive(SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP),
                                 SEXP call, SEXP op, SEXP env,
                                 num_args_t numargs) {
    // args are expected to be on the stack
    SEXP arglist = R_NilValue;
    for (num_args_t i = 0; i < numargs; ++i) {
        SEXP t = stack.pop();
        assert(TYPEOF(t) != BCProm::type);
        arglist = CONS_NR(t, arglist);
    }

    stack.push(arglist);
    SEXP res = primfun(call, op, arglist, env);
    stack.pop();
    return res;
}

// =============================================================================
// == Interpreter loop
//

static SEXP rirEval(Function* fun, fun_idx_t c, SEXP env, num_args_t numArgs);

static INLINE void evalCallArgs(Function* fun, num_args_t nargs, SEXP env) {

    for (size_t i = 0; i < nargs; ++i) {
        fun_idx_t idx = callArgs.peek(nargs - i - 1);
        SEXP arg = rirEval(fun, idx, env, 0);
        stack.push(arg);
    }

    callArgs.pop(nargs);
}

static INLINE SEXP callSpecial(SEXP call, SEXP op, SEXP env) {
    // call, op, args, rho
    SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP) = 
                        R_FunTab[Rinternals::primoffset(op)].cfun;

    return primfun(call, op, CDR(call), env);
}

static INLINE SEXP callBuiltin(Function* caller, SEXP call, SEXP op,
                               num_args_t nargs, SEXP env) {

    evalCallArgs(caller, nargs, env);

    // call, op, args, rho
    SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP) = 
                        R_FunTab[Rinternals::primoffset(op)].cfun;

    return callPrimitive(primfun, call, op, env, nargs);
}

static INLINE SEXP callClosure(Function* caller, BCClosure* cls,
                               num_args_t nargs, SEXP env, SEXP newEnv) {
    assert(cls->nargs == VARIADIC_ARGS || cls->nargs == nargs);

    if (cls->eager == true) {
        evalCallArgs(caller, nargs, env);
        return rirEval(cls->fun, 0, newEnv, nargs);
    }

    for (int i = 0; i < nargs; ++i) {
        auto prom = new BCProm(caller, callArgs.peek(nargs - 1 - i), env);
        stack.push((SEXP)prom);
    }
    callArgs.pop(nargs);

    SEXP res = rirEval(cls->fun, 0, newEnv, nargs);

    stack.pop(nargs);
    return res;
}

static INLINE SEXP callClosure(Function* caller, BCClosure* cls,
                               num_args_t nargs, SEXP env) {

    if (cls->env) {
        SEXP newEnv = Rf_NewEnvironment(R_NilValue, R_NilValue, env);
        stack.push(newEnv);
        SEXP res = callClosure(caller, cls, nargs, env, newEnv);
        stack.pop();
        return res;
    }

    return callClosure(caller, cls, nargs, env, env);
}

static INLINE SEXP doCall(Function* caller, SEXP call, SEXP callee,
                          num_args_t nargs, SEXP env) {
    size_t bp = stack.size();
    size_t bpi = stacki.size();

    SEXP res = nullptr;
    switch (TYPEOF(callee)) {
    case SPECIALSXP:
        res = callSpecial(call, callee, env);
        break;
    case BUILTINSXP:
        res = callBuiltin(caller, call, callee, nargs, env);
        break;
    case BCClosure::type:
        res = callClosure(caller, (BCClosure*)callee, nargs, env);
        break;
    default:
        assert(false);
    }

    assert(bp == stack.size());
    assert(bpi == stacki.size());
    return res;
}

static INLINE SEXP forcePromise(BCProm* prom) {
    assert(!prom->val);
    SEXP res = rirEval(prom->fun, prom->idx, prom->env, 0);
    prom->val = res;
    return res;
}

static SEXP rirEval(Function* fun, fun_idx_t c, SEXP env, num_args_t numArgs) {
    assert(fun->code.size() > c);

    // Current Code Object (from the function) being evaluated
    Code* cur = fun->code[c];
    // Current pc
    register BC_t* pc = cur->bc;

    // base pointer
    size_t bp = stack.size();

    // some helpers
    auto getArg = [numArgs, bp](size_t idx) {
        return stack.at(bp - numArgs + idx);
    };
    auto loadConst = [&pc]() {
        pool_idx_t i = BC::readImmediate<pool_idx_t>(&pc);
        return Pool::instance().get(i);
    };

    // Main loop
    while (true) {
        BC_t bc = BC::readBC(&pc);

        switch (bc) {
        case BC_t::call_special: {
            assert(false);
            break;
        }

        case BC_t::call_builtin: {
            assert(false);
            break;
        }

        case BC_t::to_bool: {
            SEXP t = stack.pop();
            int cond = NA_LOGICAL;

            // TODO
            assert(Rf_length(t) <= 2);

            if (Rf_length(t) > 0) {
                /* inline common cases for efficiency */
                switch (TYPEOF(t)) {
                case BCProm::type:
                    assert(false);
                    break;
                case LGLSXP:
                    cond = LOGICAL(t)[0];
                    break;
                case INTSXP:
                    cond =
                        INTEGER(t)[0]; /* relies on NA_INTEGER == NA_LOGICAL */
                    break;
                default:
                    cond = asLogical(t);
                }
            }

            if (cond == NA_LOGICAL) {
                const char* msg =
                    Rf_length(t)
                        ? (isLogical(t)
                               ? ("missing value where TRUE/FALSE needed")
                               : ("argument is not interpretable as logical"))
                        : ("argument is of length zero");
                Rf_error(msg);
            }

            if (cond) {
                stack.push(R_TrueValue);
            } else {
                stack.push(R_FalseValue);
            }
            break;
        }

        case BC_t::jmp:
            pc = (BC_t*)((uintptr_t)pc + BC::readImmediate<jmp_t>(&pc));
            break;

        case BC_t::jmp_true: {
            jmp_t j = BC::readImmediate<jmp_t>(&pc);
            if (stack.pop() == R_TrueValue)
                pc = (BC_t*)((uintptr_t)pc + j);
            break;
        }

        case BC_t::jmp_false: {
            jmp_t j = BC::readImmediate<jmp_t>(&pc);
            if (stack.pop() == R_FalseValue)
                pc = (BC_t*)((uintptr_t)pc + j);
            break;
        }

        case BC_t::push: {
            SEXP c = loadConst();
            stack.push(c);
            break;
        }

        case BC_t::lti: {
            int b = stacki.pop();
            int a = stacki.pop();
            stack.push(a < b ? R_TrueValue : R_FalseValue);
            break;
        }

        case BC_t::eqi: {
            int b = stacki.pop();
            int a = stacki.pop();
            stack.push(a == b ? R_TrueValue : R_FalseValue);
            break;
        }

        case BC_t::getfun: {
            SEXP sym = loadConst();
            SEXP val = findVar(sym, env);
            R_Visible = TRUE;

            if (val == R_UnboundValue)
                assert(false and "Unbound var");
            else if (val == R_MissingArg)
                assert(false and "Missing argument");

            assert(TYPEOF(val) != PROMSXP);
            if (TYPEOF(val) == BCProm::type) {
                BCProm* prom = (BCProm*)val;

                if (prom->val) {
                    val = prom->val;
                } else {
                    val = forcePromise(prom);
                }
            }

            // WTF? is this just defensive programming or what?
            if (NAMED(val) == 0 && val != R_NilValue)
                SET_NAMED(val, 1);

            switch (TYPEOF(val)) {
            case BCClosure::type:
                break;
            case CLOSXP:
                val = (SEXP)jit(val);
                break;
            case SPECIALSXP:
            case BUILTINSXP: {
                BCClosure* prim = Primitives::compilePrimitive(val);
                if (prim)
                    val = (SEXP)prim;
                break;
            }

            default:
                // TODO!
                assert(false);
            }

            stack.push(val);
            break;
        }

        case BC_t::getvar: {
            SEXP sym = loadConst();
            SEXP val = findVar(sym, env);
            R_Visible = TRUE;

            if (val == R_UnboundValue)
                assert(false and "Unbound var");
            else if (val == R_MissingArg)
                assert(false and "Missing argument");

            assert(TYPEOF(val) != PROMSXP);
            if (TYPEOF(val) == BCProm::type) {
                BCProm* prom = (BCProm*)val;

                if (prom->val) {
                    val = prom->val;
                } else {
                    val = forcePromise(prom);
                }
            }

            // WTF? is this just defensive programming or what?
            if (NAMED(val) == 0 && val != R_NilValue)
                SET_NAMED(val, 1);

            stack.push(val);
            break;
        }

        case BC_t::push_arg: {
            callArgs.push(BC::readImmediate<fun_idx_t>(&pc));
            break;
        }

        case BC_t::mkprom: {
            fun_idx_t idx = BC::readImmediate<fun_idx_t>(&pc);
            auto prom = new BCProm(fun, idx, env);
            assert(fun->code.size() > idx);
            stack.push((SEXP)prom);
            break;
        }

        case BC_t::call_name: {
            SEXP n = loadConst();
            SEXP cls = stack.pop();
            num_args_t nargs = RVector(n).size();

            switch (TYPEOF(cls)) {
            // Primitives do not care about names
            case SPECIALSXP:
            case BUILTINSXP:
                break;
            case BCClosure::type: {
                RVector names(n);
                RList formals(((BCClosure*)cls)->formals);

                std::vector<int> matched(formals.length());
                std::vector<bool> used(formals.length());

                int finger = 0;
                int positional = 0;

                for (auto formal = formals.begin(); formal != RList::end();
                     ++formal) {
                    bool found = false;

                    // TODO dotdotdot
                    {
                        int current = 0;
                        for (auto supplied : names) {
                            if (!used[current] && formal.tag() == supplied) {
                                // TODO err
                                assert(!found);
                                found = true;
                                matched[finger] =
                                    callArgs.peek(nargs - current - 1);
                            }
                            current++;
                        }
                    }

                    if (!found) {
                        int current = 0;
                        for (auto supplied : names) {
                            if (!used[current] && supplied != R_NilValue) {
                                std::string given(CHAR(PRINTNAME(supplied)));
                                std::string f(CHAR(PRINTNAME(formal.tag())));
                                if (f.compare(0, given.length(), given) == 0) {
                                    assert(!found);
                                    found = true;
                                    matched[finger] =
                                        callArgs.peek(nargs - current - 1);
                                    break;
                                }
                            }
                            current++;
                        }
                    }

                    if (!found)
                        while (positional < nargs) {
                            if (names[positional++] == R_NilValue) {
                                found = true;
                                matched[finger] =
                                    callArgs.peek(nargs - positional);
                                break;
                            }
                        }

                    if (!found) {
                        assert(false);
                        // TODO need some way to load R_MissingArgs
                        // matched[finger] = -1;
                    }

                    finger++;
                }
                callArgs.pop(nargs);
                for (auto i : matched) {
                    callArgs.push(i);
                }
                nargs = matched.size();
                break;
            }
            default:
                assert(false);
                break;
            }
            SEXP res = doCall(fun, cur->getAst(pc), cls, nargs, env);
            stack.push(res);
            break;
        }

        case BC_t::call: {
            SEXP cls = stack.pop();
            num_args_t nargs = BC::readImmediate<num_args_t>(&pc);
            if (TYPEOF(cls) == BCClosure::type) {
                // TODO missing
            }
            SEXP res = doCall(fun, cur->getAst(pc), cls, nargs, env);
            stack.push(res);
            break;
        }

        case BC_t::load_arg: {
            num_args_t a = BC::readImmediate<num_args_t>(&pc);
            assert(a < numArgs);
            stack.push(getArg(a));
            break;
        }

        case BC_t::numargi: {
            stacki.push(numArgs);
            break;
        }

        case BC_t::get_ast: {
            SEXP t = stack.pop();
            assert(TYPEOF(t) == BCProm::type);
            BCProm* p = (BCProm*)t;
            stack.push(p->ast());
            break;
        }

        case BC_t::setvar: {
            SEXP val = stack.pop();
            SEXP sym = stack.pop();
            // TODO: complex assign
            assert(TYPEOF(sym) == SYMSXP);
            defineVar(sym, val, env);
            stack.push(val);
            break;
        }

        case BC_t::force_all: {
            for (size_t a = 0; a < numArgs; ++a) {
                SEXP arg = getArg(a);
                if (TYPEOF(arg) == BCProm::type) {
                    SEXP val = forcePromise((BCProm*)arg);
                    stack.push(val);
                } else {
                    stack.push(arg);
                }
            }
            break;
        }

        case BC_t::force: {
            SEXP tos = stack.pop();
            assert(TYPEOF(tos) == BCProm::type);
            BCProm* prom = (BCProm*)tos;
            SEXP val = forcePromise(prom);
            stack.push(val);
            break;
        }

        case BC_t::pop:
            stack.pop();
            break;

        case BC_t::ret: {
            goto done;
        }

        case BC_t::pushi: {
            stacki.push(BC::readImmediate<int>(&pc));
            break;
        }

        case BC_t::dup:
            stack.push(stack.top());
            break;

        case BC_t::dupi:
            stacki.push(stacki.top());
            break;

        case BC_t::load_argi: {
            int pos = stacki.pop();
            stack.push(getArg(pos));
            break;
        }

        case BC_t::inci: {
            stacki.top()++;
            break;
        }

        case BC_t::mkclosure: {
            SEXP body = stack.pop();
            SEXP arglist = stack.pop();

            BCClosure* bcls = jit(body, arglist, env);
            stack.push((SEXP)bcls);
            break;
        }

        case BC_t::add: {
            // TODO
            SEXP rhs = stack.pop();
            SEXP lhs = stack.pop();
            if (TYPEOF(lhs) == REALSXP && TYPEOF(lhs) == REALSXP &&
                Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
                SEXP res = Rf_allocVector(REALSXP, 1);
                SET_NAMED(res, 1);
                REAL(res)[0] = REAL(lhs)[0] + REAL(rhs)[0];
                stack.push(res);
            } else
                assert(false);
            break;
        }

        case BC_t::sub: {
            // TODO
            SEXP rhs = stack.pop();
            SEXP lhs = stack.pop();
            if (TYPEOF(lhs) == REALSXP && TYPEOF(lhs) == REALSXP &&
                Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
                SEXP res = Rf_allocVector(REALSXP, 1);
                SET_NAMED(res, 1);
                REAL(res)[0] = REAL(lhs)[0] - REAL(rhs)[0];
                stack.push(res);
            } else
                assert(false);
            break;
        }

        case BC_t::lt: {
            // TODO
            SEXP rhs = stack.pop();
            SEXP lhs = stack.pop();
            if (TYPEOF(lhs) == REALSXP && TYPEOF(lhs) == REALSXP &&
                Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
                stack.push(REAL(lhs)[0] < REAL(rhs)[0] ? R_TrueValue
                                                       : R_FalseValue);
            } else
                assert(false);
            break;
        }

        case BC_t::num_of:
        case BC_t::invalid:
            assert(false);
        }
    }

done:
    return stack.pop();
}
} // namespace

SEXP Interpreter::run(SEXP env) { return rirEval(fun, 0, env, 0); }

void Interpreter::gcCallback(void (*forward_node)(SEXP)) {
    for (size_t i = 0; i < stack.size(); ++i) {
        SEXP e = stack.at(i);
        // TODO: that's a bit of a hack, we should fix it
        if (TYPEOF(e) == BCProm::type)
            forward_node(((BCProm*)e)->val);
        else if (TYPEOF(e) != BCClosure::type)
            forward_node(stack.at(i));
    }
}

} // rir
} // rjit
