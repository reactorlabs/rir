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

extern "C" {
Rboolean convertToLogicalNoNA(SEXP what, SEXP consts, int call);
}

namespace rjit {
namespace rir {

namespace {

static CCODE getPrimfun(std::string symbol) {
    for (size_t i = 0;; ++i) {
        if (symbol.compare(R_FunTab[i].name) == 0)
            return R_FunTab[i].cfun;
    }
    assert(false);
    return nullptr;
}

static SEXP getPrimitive(const char* name) {
    SEXP value = SYMVALUE(Rf_install(name));
    assert(TYPEOF(value) == SPECIALSXP || TYPEOF(value) == BUILTINSXP);
    return value;
}

SEXP jit(SEXP fun) {
    std::cout << "JIT Compiling a function to BC\n";
    Compiler c(BODY(fun), FORMALS(fun));

    SEXP bc = mkBCCls(c.finalize(), FORMALS(fun), RList(FORMALS(fun)).length(),
                      BCClosure::CC::envLazy, CLOENV(fun));
    return bc;
}

SEXP jit(SEXP ast, SEXP formals, SEXP env) {
    Compiler c(ast, formals);
    SEXP bc = mkBCCls(c.finalize(), formals, RList(formals).length(),
                      BCClosure::CC::envLazy, env);
    return bc;
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
// ==== Interpreter Datastructures

// The stacks

static Stack<SEXP> stack;
Stack<int> stacki;

// =============================================================================
// ==== Helper : for not yet implemented primitives we call the gnur ones

static INLINE SEXP callPrimitive(SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP),
                                 SEXP call, SEXP op, SEXP env,
                                 num_args_t numargs) {
    // args are expected to be on the stack
    SEXP arglist = R_NilValue;
    for (num_args_t i = 0; i < numargs; ++i) {
        SEXP t = stack.pop();
        arglist = CONS_NR(t, arglist);
    }

    stack.push(arglist);
    SEXP res = primfun(call, op, arglist, env);
    stack.pop();
    return res;
}

static INLINE SEXP callPrimitive(SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP),
                                 SEXP call, SEXP op, SEXP env,
                                 std::vector<SEXP> args) {
    // args are expected to be on the stack
    SEXP arglist = R_NilValue;
    for (auto i = args.rbegin(); i != args.rend(); ++i) {
        arglist = CONS_NR(*i, arglist);
    }

    stack.push(arglist);
    SEXP res = primfun(call, op, arglist, env);
    stack.pop();
    return res;
}

// =============================================================================
// ==== Interpreter call wrappers
//

static SEXP rirEval(Function* fun, fun_idx_t c, SEXP env, num_args_t numArgs,
                    SEXP call);

static INLINE void evalCallArgs(Function* fun, int args[], num_args_t nargs,
                                SEXP env) {

    for (size_t i = 0; i < nargs; ++i) {
        fun_idx_t idx = args[i];
        if (idx == MISSING_ARG_IDX) {
            stack.push(R_MissingArg);
        } else {
            SEXP arg = rirEval(fun, idx, env, 0, fun->code[idx]->ast);
            stack.push(arg);
        }
    }
}

static INLINE SEXP callSpecial(SEXP call, SEXP op, SEXP env) {
    // call, op, args, rho
    SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP) = 
                        R_FunTab[Rinternals::primoffset(op)].cfun;

    return primfun(call, op, CDR(call), env);
}

static INLINE SEXP callBuiltin(Function* caller, SEXP call, SEXP op, int args[],
                               num_args_t nargs, SEXP env) {

    evalCallArgs(caller, args, nargs, env);

    // call, op, args, rho
    SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP) = 
                        R_FunTab[Rinternals::primoffset(op)].cfun;

    return callPrimitive(primfun, call, op, env, nargs);
}

static INLINE SEXP callClosure(Function* caller, BCClosure* cls, int args[],
                               num_args_t nargs, SEXP env, SEXP call) {

    assert(cls->nargs == VARIADIC_ARGS || cls->nargs == nargs);

    switch (cls->cc) {
    case BCClosure::CC::stackEager: {
        assert(!cls->env);
        evalCallArgs(caller, args, nargs, env);
        return rirEval(cls->fun, 0, env, nargs, call);
    }

    case BCClosure::CC::stackLazy: {
        for (size_t i = 0; i < nargs; ++i) {
            fun_idx_t idx = args[i];
            if (idx == MISSING_ARG_IDX) {
                stack.push(R_MissingArg);
            } else {
                SEXP prom = mkBCProm(caller, idx, env);
                stack.push(prom);
            }
        }
        SEXP res = rirEval(cls->fun, 0, env, nargs, call);
        stack.pop(nargs);
        return res;
    }

    case BCClosure::CC::envLazy: {
        SEXP argslist = R_NilValue;

        for (size_t i = 0; i < nargs; ++i) {
            fun_idx_t idx = args[i];
            if (idx == MISSING_ARG_IDX) {
                argslist = CONS_NR(R_MissingArg, argslist);
            } else {
                SEXP prom = mkBCProm(caller, idx, env);
                argslist = CONS_NR(prom, argslist);
            }
        }

        SEXP newEnv = Rf_NewEnvironment(cls->formals, argslist, cls->env);
        stack.push(newEnv);

        SEXP res = rirEval(cls->fun, 0, newEnv, nargs, call);
        stack.pop();

        return res;
    }
    }
    assert(false);
    return nullptr;
}

static INLINE SEXP doCall(Function* caller, SEXP call, SEXP callee, int args[],
                          num_args_t nargs, SEXP env) {
    size_t bp = stack.size();
    size_t bpi = stacki.size();

    SEXP res = nullptr;
    switch (Rinternals::typeof(callee)) {
    case SPECIALSXP:
        res = callSpecial(call, callee, env);
        break;
    case BUILTINSXP:
        res = callBuiltin(caller, call, callee, args, nargs, env);
        break;
    case BCCodeType:
        assert(isBCCls(callee));
        res = callClosure(caller, getBCCls(callee), args, nargs, env, call);
        break;
    default:
        assert(false);
    }

    assert(bp == stack.size());
    assert(bpi == stacki.size());
    return res;
}

static INLINE SEXP forcePromise(BCProm* prom, SEXP wrapper) {
    assert(!prom->val(wrapper));
    SEXP res = rirEval(prom->fun, prom->idx, prom->env, 0, prom->ast());
    prom->val(wrapper, res);
    return res;
}

// =============================================================================
// ==== Interpreter main loop
//

static SEXP rirEval(Function* fun, fun_idx_t c, SEXP env, num_args_t numArgs,
                    SEXP call) {
    assert(fun->code.size() > c);

    // Current Code Object (from the function) being evaluated
    Code* cur = fun->code[c];
    // Current pc
    register BC_t* pc = cur->bc;

    // base pointer
    size_t bp = stack.size();

    auto getCurrentCall = [cur, pc, call]() {
        SEXP c = cur->getAst(pc);
        if (c)
            return c;
        return call;
    };

    // some helpers
    auto getArg = [numArgs, bp](size_t idx) {
        return stack.at(bp - numArgs + idx);
    };
    auto loadConst = [&pc]() {
        pool_idx_t i = BC::readImmediate<pool_idx_t>(&pc);
        return Pool::instance().get(i);
    };

    // Main BC_t dispatch
    while (true) {
        switch (BC::readBC(&pc)) {
        case BC_t::to_bool: {
            SEXP t = stack.top();
            int cond = NA_LOGICAL;

            if (Rf_length(t) > 1) {
                warningcall(getCurrentCall(),
                            ("the condition has length > 1 and only the first "
                             "element will be used"));
            }
            stack.pop();

            if (Rf_length(t) > 0) {
                /* inline common cases for efficiency */
                switch (Rinternals::typeof(t)) {
                case BCCodeType:
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
                errorcall(getCurrentCall(), msg);
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

        case BC_t::check_special: {
            SEXP sym = loadConst();
            SEXP val = findVar(sym, env);
            assert(TYPEOF(val) == SPECIALSXP);
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

            assert(Rinternals::typeof(val) != PROMSXP);
            if (isBCProm(val)) {
                BCProm* prom = getBCProm(val);

                if (prom->val(val)) {
                    val = prom->val(val);
                } else {
                    val = forcePromise(prom, val);
                }
            }

            // WTF? is this just defensive programming or what?
            if (NAMED(val) == 0 && val != R_NilValue)
                SET_NAMED(val, 1);

            switch (Rinternals::typeof(val)) {
            case BCCodeType:
                break;
            case CLOSXP:
                val = (SEXP)jit(val);
                break;
            case SPECIALSXP:
            case BUILTINSXP: {
                SEXP prim = Primitives::compilePrimitive(val);
                if (prim)
                    val = prim;
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

            assert(Rinternals::typeof(val) != PROMSXP);
            if (isBCProm(val)) {
                BCProm* prom = getBCProm(val);

                if (prom->val(val)) {
                    val = prom->val(val);
                } else {
                    val = forcePromise(prom, val);
                }
            }

            // WTF? is this just defensive programming or what?
            if (NAMED(val) == 0 && val != R_NilValue)
                SET_NAMED(val, 1);

            stack.push(val);
            break;
        }

        case BC_t::mkprom: {
            fun_idx_t idx = BC::readImmediate<fun_idx_t>(&pc);
            SEXP prom = mkBCProm(fun, idx, env);
            assert(fun->code.size() > idx);
            stack.push((SEXP)prom);
            break;
        }

        case BC_t::call: {
            call_args_t callArgs = BC::readImmediate<call_args_t>(&pc);

            SEXP args_ = Pool::instance().get(callArgs.args);
            int* args = INTEGER(args_);
            int nargs = Rf_length(args_);

            SEXP cls = stack.pop();

            if (callArgs.names) {
                RVector names(Pool::instance().get(callArgs.names));
                switch (Rinternals::typeof(cls)) {
                // Primitives do not care about names
                case SPECIALSXP:
                case BUILTINSXP:
                    break;
                case BCCodeType: {
                    assert(isBCCls(cls));
                    BCClosure* bcls = getBCCls(cls);
                    RList formals(bcls->formals);

                    std::vector<int> matched(formals.length());
                    std::vector<bool> used(formals.length());

                    int finger = 0;
                    int positional = 0;

                    // TODO dotdotdot

                    // Match given arguments to formal arguments:
                    // Go through all the formal arguments
                    for (auto formal = formals.begin(); formal != RList::end();
                         ++formal, ++finger) {
                        bool found = false;

                        // Check if any of the supplied args has a matching tag
                        {
                            int current = 0;
                            for (auto supplied : names) {
                                if (used[current] || supplied == R_NilValue)
                                    continue;
                                if (formal.tag() != supplied)
                                    continue;

                                // TODO err: same name given twice!
                                assert(!found);

                                found = true;
                                matched[finger] = args[current];
                                used[current] = 1;

                                current++;
                            }
                        }

                        // Check if any of the supplied args has a partially
                        // matching tag
                        if (!found) {
                            int current = 0;
                            for (auto supplied : names) {
                                if (used[current] || supplied == R_NilValue)
                                    continue;

                                std::string given(CHAR(PRINTNAME(supplied)));
                                std::string f(CHAR(PRINTNAME(formal.tag())));
                                if (f.compare(0, given.length(), given) != 0)
                                    continue;

                                // TODO err: same name given twice!
                                assert(!found);

                                found = true;
                                matched[finger] = args[current];
                                used[current] = 1;

                                current++;
                            }
                        }

                        // No match -> find the next untagged (ie. positional)
                        // argument
                        if (!found)
                            while (positional < nargs) {
                                if (names[positional++] == R_NilValue) {
                                    found = true;
                                    matched[finger] = args[positional - 1];
                                    used[positional - 1] = 1;
                                    break;
                                }
                            }

                        // No more positional args left?
                        if (!found) {
                            matched[finger] = MISSING_ARG_IDX;
                        }
                    }

                    // Replace call args on the stack by the reordered match
                    SEXP res = doCall(fun, cur->getAst(pc), cls, matched.data(),
                                      matched.size(), env);
                    stack.push(res);
                    break;
                }
                default:
                    assert(false);
                    break;
                }
            } else {
                if (isBCCls(cls)) {
                    num_args_t expected = getBCCls(cls)->nargs;
                    if (expected != VARIADIC_ARGS) {
                        if (nargs < expected) {
                            std::vector<int> allArgs(expected, MISSING_ARG_IDX);
                            memcpy(allArgs.data(), args, nargs * sizeof(SEXP));
                            SEXP res =
                                doCall(fun, cur->getAst(pc), cls,
                                       allArgs.data(), allArgs.size(), env);
                            stack.push(res);
                            break;
                        }
                    }
                }
                SEXP res = doCall(fun, cur->getAst(pc), cls, args, nargs, env);
                stack.push(res);
                break;
            }
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
            assert(isBCProm(t));
            BCProm* p = getBCProm(t);
            stack.push(p->ast());
            break;
        }

        case BC_t::setvar: {
            SEXP val = stack.pop();
            SEXP sym = stack.pop();
            // TODO: complex assign
            assert(Rinternals::typeof(sym) == SYMSXP);
            defineVar(sym, val, env);
            stack.push(val);
            break;
        }

        case BC_t::force_all: {
            for (size_t a = 0; a < numArgs; ++a) {
                SEXP arg = getArg(a);
                if (isBCProm(arg)) {
                    BCProm* prom = getBCProm(arg);
                    SEXP val = forcePromise(prom, arg);
                    stack.push(val);
                } else {
                    stack.push(arg);
                }
            }
            break;
        }

        case BC_t::force: {
            SEXP tos = stack.pop();
            assert(isBCProm(tos));
            BCProm* prom = getBCProm(tos);
            SEXP val = forcePromise(prom, tos);
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

            SEXP bcls = jit(body, arglist, env);
            stack.push(bcls);
            break;
        }

        case BC_t::add: {
            SEXP rhs = stack.pop();
            SEXP lhs = stack.pop();
            if (Rinternals::typeof(lhs) == REALSXP &&
                Rinternals::typeof(lhs) == REALSXP && Rf_length(lhs) == 1 &&
                Rf_length(rhs) == 1) {
                SEXP res = Rf_allocVector(REALSXP, 1);
                SET_NAMED(res, 1);
                REAL(res)[0] = REAL(lhs)[0] + REAL(rhs)[0];
                stack.push(res);
            } else {
                static SEXP op = getPrimitive("+");
                static CCODE primfun = getPrimfun("+");
                SEXP res = callPrimitive(primfun, getCurrentCall(), op, env,
                                         {lhs, rhs});
                stack.push(res);
            }
            break;
        }

        case BC_t::sub: {
            SEXP rhs = stack.pop();
            SEXP lhs = stack.pop();
            if (Rinternals::typeof(lhs) == REALSXP &&
                Rinternals::typeof(lhs) == REALSXP && Rf_length(lhs) == 1 &&
                Rf_length(rhs) == 1) {
                SEXP res = Rf_allocVector(REALSXP, 1);
                SET_NAMED(res, 1);
                REAL(res)[0] = REAL(lhs)[0] - REAL(rhs)[0];
                stack.push(res);
            } else {
                static SEXP op = getPrimitive("-");
                static CCODE primfun = getPrimfun("-");
                SEXP res = callPrimitive(primfun, getCurrentCall(), op, env,
                                         {lhs, rhs});
                stack.push(res);
            }
            break;
        }

        case BC_t::lt: {
            SEXP rhs = stack.pop();
            SEXP lhs = stack.pop();
            if (Rinternals::typeof(lhs) == REALSXP &&
                Rinternals::typeof(lhs) == REALSXP && Rf_length(lhs) == 1 &&
                Rf_length(rhs) == 1) {
                stack.push(REAL(lhs)[0] < REAL(rhs)[0] ? R_TrueValue
                                                       : R_FalseValue);
            } else {
                static SEXP op = getPrimitive("<");
                static CCODE primfun = getPrimfun("<");
                SEXP res = callPrimitive(primfun, getCurrentCall(), op, env,
                                         {lhs, rhs});
                stack.push(res);
            }
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

SEXP Interpreter::run(SEXP env) { return rirEval(fun, 0, env, 0, R_NilValue); }

void Interpreter::gcCallback(void (*forward_node)(SEXP)) {
    for (size_t i = 0; i < stack.size(); ++i) {
        SEXP e = stack.at(i);
        forward_node(e);
    }
}

} // rir
} // rjit
