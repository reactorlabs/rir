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

// ============================================================================
// Recycle argument lists. This is a bit of a premature optimization, but I
// wanted to experiment with it....

template <int size>
class ArglistCache {
    static constexpr int poolSize = 2;
    SEXP pool[poolSize];

  public:
    ArglistCache() {
        for (int i = 0; i < size; ++i) {
            pool[i] = nullptr;
        }
    }

    SEXP get(std::array<SEXP, size> args) {
        SEXP res = nullptr;
        for (int i = 0; i < poolSize; ++i) {
            if (pool[i]) {
                res = pool[i];
                pool[i] = nullptr;
                break;
            }
        }
        if (!res) {
            res = R_NilValue;
            for (int i = 0; i < size; ++i)
                res = CONS_NR(R_NilValue, res);
            Precious::add(res);
        }
        SEXP cur = res;
        for (int i = 0; i < size; ++i) {
            SETCAR(cur, args[i]);
            cur = CDR(cur);
        }
        return res;
    }

    void release(SEXP list) {
        for (int i = 0; i < poolSize; ++i) {
            if (!pool[i]) {
                pool[i] = list;
            }
        }
    }
};

static ArglistCache<1> ArglistCache1;
static ArglistCache<2> ArglistCache2;

// ============================================================================
// Common container class for various stacks in the interpreter

template <typename T>
class Stack {
    std::vector<T> stack;
    size_t size_ = 0;
    size_t capacity = 2048;

  public:
    Stack() { stack.resize(capacity); }

    void push(T s) {
        if (size_ == capacity) {
            capacity *= 2;
            stack.resize(capacity);
        }
        stack[size_++] = s;
    }

    bool empty() { return size_ == 0; }

    T pop() { return stack[--size_]; }
    void pop(size_t n) { size_ -= n; }

    T& peek(size_t offset) { return stack[size_ - 1 - offset]; }

    T set(size_t offset, T val) { return stack[size_ - 1 - offset] = val; }

    T& at(size_t pos) { return stack[pos]; }

    T& top() { return stack[size_ - 1]; }

    size_t size() { return size_; }
};

// ============================================================================
// Continuation is similar to R notion of Context.
// When we force a promise of call a function, all internal state of the
// interpreter is bundled in a continuation and pushed on the continuation
// stack

#pragma pack(push)
#pragma pack(0)
struct Continuation {
  public:
    Continuation() {}

    Function* fun;
    Code* code;
    BC_t* pc;
    SEXP env;
    size_t bp;
    num_args_t argsPassed;

    Continuation(Function* fun, Code* code, BC_t* pc, SEXP env, size_t bp,
                 num_args_t argsPassed)
        : fun(fun), code(code), pc(pc), env(env), bp(bp),
          argsPassed(argsPassed) {}
};
#pragma pack(pop)

// =============================================================================
// === Interpreter Datastructures

// The stacks

static Stack<SEXP> stack;
static Stack<Continuation> cont;
Stack<int> stacki;
Stack<fun_idx_t> callArgs;

// The internal state of the interpreter

// Current Env
SEXP env;
// Current Function being evaluated
Function* fun;
// Current Code Object (from the function) being evaluated
Code* cur;
// Current pc
BC_t* pc;
// number of args of the current function
num_args_t argsPassed;
// base pointer
size_t bp;

// =============================================================================

SEXP getClosure() { return stack.at(bp - 1); }

SEXP getArg(size_t idx) { return stack.at(bp + idx); }

void storeCurrentState() {
    cont.push(Continuation(fun, cur, pc, env, bp, argsPassed));
}

void setInternalState(Function* f, Code* c, SEXP e, num_args_t a, size_t b) {
    fun = f;
    cur = c;
    pc = cur->bc;
    env = e;
    argsPassed = a;
    bp = b;
}

void tailCallFunction(BCClosure* bcls) {
    fun = bcls->fun;
    cur = bcls->fun->code[0];
    pc = cur->bc;
}

void beginFunction(BCClosure* bcls, num_args_t a = 0) {
    assert(stack.top() == (SEXP)bcls);

    SEXP e = env;
    if (bcls->env)
        e = Rf_NewEnvironment(R_NilValue, R_NilValue, bcls->env);

    setInternalState(bcls->fun, bcls->fun->code[0], e, a, stack.size());
}

void beginPromise(BCProm* prom) {
    assert(stack.top() == (SEXP)prom);
    assert(prom->fun->code.size() > prom->idx);

    setInternalState(prom->fun, prom->fun->code[prom->idx], prom->env, 0,
                     stack.size());
}

void restoreCont() {
    Continuation c = cont.pop();
    fun = c.fun;
    cur = c.code;
    pc = c.pc;
    env = c.env;
    bp = c.bp;
    argsPassed = c.argsPassed;
}

SEXP callPrimitive1(SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP), SEXP call, SEXP op,
                    SEXP env, SEXP arg) {
    SEXP arglist = ArglistCache1.get({arg});
    SEXP res = primfun(call, op, arglist, env);
    ArglistCache1.release(arglist);
    return res;
}

SEXP callPrimitive2(SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP), SEXP call, SEXP op,
                    SEXP env, SEXP lhs, SEXP rhs) {
    SEXP arglist = ArglistCache2.get({lhs, rhs});
    SEXP res = primfun(call, op, arglist, env);
    ArglistCache2.release(arglist);
    return res;
}

SEXP callPrimitive(SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP), SEXP call, SEXP op,
                   SEXP env, num_args_t numargs) {
    if (numargs == 1)
        return callPrimitive1(primfun, call, op, env, stack.pop());

    if (numargs == 2) {
        SEXP rhs = stack.pop();
        SEXP lhs = stack.pop();
        return callPrimitive2(primfun, call, op, env, lhs, rhs);
    }

    // args are expected to be on the stack
    SEXP arglist = R_NilValue;
    for (num_args_t i = 0; i < numargs; ++i) {
        SEXP t = stack.pop();
        assert(TYPEOF(t) != BCProm::type);
        arglist = CONS_NR(t, arglist);
    }
    Protect prot;
    prot(arglist);

    return primfun(call, op, arglist, env);
}

// =============================================================================
// == Interpreter loop
//
SEXP evalFunction(Function* fun_, SEXP env_) {
    assert(pc == nullptr);
    fun = fun_;
    env = env_;

    // TODO: the top level is not a closure!
    stack.push(R_NilValue);

    setInternalState(fun, fun->code[0], env_, 0, 1);

    // std::cout << "====  Function ========\n";
    // for (auto c : fun->code) {
    //     c->print();
    // }
    // std::cout << "==== /Function ========\n\n";

    while (true) {
        BC bc = BC::advance(&pc);

        // bc.print();
        // std::cout << " (" << stack.size() << ")\n";

        switch (bc.bc) {
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
            pc = (BC_t*)((uintptr_t)pc + bc.immediateOffset());
            break;

        case BC_t::jmp_true:
            if (stack.pop() == R_TrueValue)
                pc = (BC_t*)((uintptr_t)pc + bc.immediateOffset());
            break;

        case BC_t::jmp_false:
            if (stack.pop() == R_FalseValue)
                pc = (BC_t*)((uintptr_t)pc + bc.immediateOffset());
            break;

        case BC_t::push: {
            SEXP c = bc.immediateConst();
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
            SEXP sym = bc.immediateConst();
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
                    // force promise
                    stack.push(val);

                    cont.push(Continuation(fun, cur, BC::rewind(pc, bc), env,
                                           bp, argsPassed));

                    beginPromise(prom);
                    break;
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
            SEXP sym = bc.immediateConst();
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
                    // force promise
                    stack.push(val);

                    cont.push(Continuation(fun, cur, BC::rewind(pc, bc), env,
                                           bp, argsPassed));

                    beginPromise(prom);
                    break;
                }
            }

            // WTF? is this just defensive programming or what?
            if (NAMED(val) == 0 && val != R_NilValue)
                SET_NAMED(val, 1);

            stack.push(val);
            break;
        }

        case BC_t::push_arg: {
            callArgs.push(bc.immediateFunIdx());
            break;
        }

        case BC_t::mkprom: {
            auto prom = new BCProm(fun, bc.immediateFunIdx(), env);
            assert(fun->code.size() > bc.immediateFunIdx());
            stack.push((SEXP)prom);
            break;
        }

        case BC_t::tail_call: {
            SEXP cls = getClosure();

            switch (TYPEOF(cls)) {
            case SPECIALSXP:
                assert(false);
                break;
            case BUILTINSXP: {
                num_args_t nargs = stack.size() - bp;
                SEXP op = stack.peek(nargs + 1);
                SEXP call = stack.peek(nargs + 2);

                SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP) = 
                            R_FunTab[cls->u.primsxp.offset].cfun;

                SEXP res = callPrimitive(primfun, call, op, env, nargs);

                stack.pop(3);
                stack.push(res);
                restoreCont();
                break;
            }
            case BCClosure::type: {
                BCClosure* bcls = (BCClosure*)cls;
                tailCallFunction(bcls);
                break;
            }
            default:
                assert(false);
                break;
            }
            break;
        }

        case BC_t::call: {
            num_args_t nargs = bc.immediateNumArgs();

            SEXP cls = stack.top();

            switch (TYPEOF(cls)) {
            case SPECIALSXP: {
                callArgs.pop(nargs);

                // call, op, args, rho
                SEXP (*primfun)(SEXP, SEXP, SEXP, SEXP) = 
                            R_FunTab[cls->u.primsxp.offset].cfun;

                SEXP call = cur->getAst(pc);
                SEXP op = cls;

                SEXP res = primfun(call, op, CDR(call), env);
                stack.push(res);
                break;
            }
            case BUILTINSXP: {
                stack.pop();
                SEXP call = cur->getAst(pc);
                SEXP op = cls;

                // This is a bit of a hack:
                // We compile an additional code object for eager argument
                // evaluation right before the first argument. For Builtins we
                // call into that instead. This Code object must end with
                // tail_call_eager.
                storeCurrentState();

                stack.push(call);
                stack.push(op);
                stack.push(cls);

                callArgs.pop(nargs - 1);
                fun_idx_t idx = callArgs.pop() - 1;
                Code* args = fun->code[idx];
                setInternalState(fun, args, env, nargs, stack.size());

                break;
            }
            case BCClosure::type: {
                BCClosure* bcls = (BCClosure*)cls;

                // TODO missing:
                assert(bcls->nargs == VARIADIC_ARGS || bcls->nargs == nargs);

                if (bcls->eager == true) {
                    // explanation, see case BUILTINSXP above
                    storeCurrentState();
                    callArgs.pop(nargs - 1);
                    fun_idx_t idx = callArgs.pop() - 1;
                    Code* args = fun->code[idx];
                    setInternalState(fun, args, env, nargs, stack.size());
                    break;
                }

                storeCurrentState();
                Function* curFun = fun;
                SEXP curEnv = env;

                beginFunction(bcls, nargs);

                for (int i = 0; i < nargs; ++i) {
                    auto prom = new BCProm(curFun, callArgs.peek(nargs - 1 - i),
                                           curEnv);
                    stack.push((SEXP)prom);
                }
                callArgs.pop(nargs);
                break;
            }
            default:
                assert(false);
                break;
            }
            break;
        }

        case BC_t::load_arg: {
            num_args_t a = bc.immediateNumArgs();
            assert(a < argsPassed);
            stack.push(getArg(a));
            break;
        }

        case BC_t::numargi: {
            stacki.push(argsPassed);
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

        case BC_t::call_name: {
            assert(false);
            break;
        }

        case BC_t::force_all: {
            unsigned forced = stack.size() - bp - argsPassed;
            if (forced == argsPassed)
                break;

            SEXP val = getArg(forced);
            stack.push(val);
            assert(TYPEOF(val) == BCProm::type);
            BCProm* prom = (BCProm*)val;

            cont.push(Continuation(fun, cur, BC::rewind(pc, bc), env, bp,
                                   argsPassed));
            beginPromise(prom);

            // std::cout << "====  Promise ========\n";
            // cur->print();
            // std::cout << "==== /Promise ========\n\n";
            break;
        }

        case BC_t::force: {
            SEXP val = stack.top();
            assert(TYPEOF(val) == BCProm::type);

            BCProm* prom = (BCProm*)val;
            assert(prom->fun->code.size() > prom->idx);

            storeCurrentState();
            beginPromise(prom);

            //            std::cout << "+++ force prom " << prom << "\n";
            // std::cout << "====  Promise ========\n";
            // cur->print();
            // std::cout << "==== /Promise ========\n\n";
            break;
        }

        case BC_t::pop:
            stack.pop();
            break;

        case BC_t::leave_prom: {
            SEXP res = stack.pop();
            SEXP cls = stack.pop();
            assert(TYPEOF(cls) == BCProm::type);
            BCProm* prom = (BCProm*)cls;
            assert(prom->fun->code[prom->idx] == cur);
            assert(!prom->val);
            prom->val = res;

            // TODO: this is a bit of a hack and we need to find a
            // better way. Since getvar and getfun implicitly forces
            // promises, we need to not leave anything on the stack upon
            // returning
            if (*cont.top().pc != BC_t::getvar &&
                *cont.top().pc != BC_t::getfun)
                stack.push(res);

            goto ret;
        }

        case BC_t::leave: {
            SEXP res = stack.pop();
            size_t frameSize = stack.size() - bp;
            stack.pop(frameSize);
            SEXP cls = stack.pop();

            if (cls != R_NilValue) {
                assert(TYPEOF(cls) == BCClosure::type);
                BCClosure* bcls = (BCClosure*)cls;
                assert(bcls->fun->code[0] == cur);
            }

            stack.push(res);
            goto ret;
        }

        case BC_t::ret: {
        ret:
            if (cont.empty()) {
                assert(stack.size() == 1);
                assert(bp == 1);
                goto done;
            }

            restoreCont();
            break;
        }

        case BC_t::pushi: {
            stacki.push(bc.immediate.i);
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
    pc = nullptr;
    return stack.top();
}
} // namespace

SEXP Interpreter::run(SEXP env) { return evalFunction(fun, env); }

void Interpreter::gcCallback(void (*forward_node)(SEXP)) {
    for (size_t i = 0; i < stack.size(); ++i) {
        SEXP e = stack.at(i);
        // TODO: that's a bit of a hack, we should fix it
        if (TYPEOF(e) == BCProm::type)
            forward_node(((BCProm*)e)->val);
        else if (TYPEOF(e) != BCClosure::type)
            forward_node(stack.at(i));
    }
    for (size_t i = 0; i < cont.size(); ++i) {
        forward_node(cont.at(i).env);
    }
}

} // rir
} // rjit
