#include "Interpreter.h"
#include "RIntlns.h"
#include "Function.h"
#include "Compiler.h"
#include "CodeStream.h"
#include "Runtime.h"
#include "Primitives.h"

#include <iostream>
#include <deque>
#include <array>

extern "C" Rboolean convertToLogicalNoNA(SEXP what, SEXP consts, int call);

namespace rjit {
namespace rir {

namespace {

BCClosure* jit(SEXP fun) {
    Compiler c(BODY(fun));
    BCClosure* cls = new BCClosure;
    cls->env = CLOENV(fun);
    cls->fun = c.finalize();
    cls->formals = FORMALS(fun);
    return cls;
}

template <typename T>
class Stack {
    std::deque<T> stack;

  public:
    void push(T s) { stack.push_back(s); }

    bool empty() { return stack.empty(); }

    T pop() {
        T res = stack.back();
        stack.pop_back();
        return res;
    }

    T peek(size_t offset) { return stack[stack.size() - 1 - offset]; }
    T set(size_t offset, T val) {
        return stack[stack.size() - 1 - offset] = val;
    }

    T at(size_t pos) { return stack[pos]; }

    T top() { return stack.back(); }

    size_t size() { return stack.size(); }
};

struct Continuation {
  public:
    Function* fun;
    Code* code;
    BC_t* pc;
    SEXP env;
    size_t sp;
    num_args_t numArgs;

    Continuation(Function* fun, Code* code, BC_t* pc, SEXP env, size_t sp,
                 num_args_t numArgs)
        : fun(fun), code(code), pc(pc), env(env), sp(sp), numArgs(numArgs) {}
};

SEXP evalFunction(Function* fun_, SEXP env) {

    Stack<SEXP> stack;
    Stack<Continuation> cont;

    Function* fun = fun_;
    Code* cur;
    BC_t* pc;

    num_args_t numArgs;
    size_t sp;

    auto setState = [&fun, &cur, &pc, &env, &sp, &numArgs, &stack](
        Function* fun_, Code* code, SEXP env, num_args_t a) {
        fun = fun_;
        cur = code;
        pc = cur->bc;
        env = env;
        numArgs = a;
        sp = stack.size() - numArgs;

    };

    auto storeCont = [&cont, &fun, &cur, &pc, &env, &sp, &numArgs]() {
        cont.push(Continuation(fun, cur, pc, env, sp, numArgs));
    };

    auto restoreCont = [&cont, &fun, &cur, &pc, &env, &sp, &numArgs]() {
        Continuation c = cont.pop();
        fun = c.fun;
        cur = c.code;
        pc = c.pc;
        env = c.env;
        sp = c.sp;
        numArgs = c.numArgs;
    };

    setState(fun, fun->code[0], env, 0);
    std::cout << "====  Function ========\n";
    for (auto c : fun->code) {
        c->print();
    }
    std::cout << "==== /Function ========\n\n";

    while (true) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
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

            std::cout << cond << "\n";
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

        case BC_t::push: {
            SEXP c = bc.immediateConst();
            stack.push(c);
            break;
        }

        case BC_t::getfun:
        // TODO
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
                // TODO
                assert(false);
            }

            if (NAMED(val) == 0 && val != R_NilValue)
                SET_NAMED(val, 1);

            stack.push(val);
            break;
        }

        case BC_t::mkprom: {
            auto prom = new BCProm(fun, bc.immediateFunIdx(), env);
            stack.push((SEXP)prom);
            break;
        }

        case BC_t::call: {
            num_args_t nargs = bc.immediateNumArgs();

            SEXP cls = stack.peek(nargs);
            BCClosure* bcls;

            if (TYPEOF(cls) == CLOSXP) {
                bcls = jit(cls);
            } else if (TYPEOF(cls) == SPECIALSXP) {
                bcls = Primitives::compilePrimitive(cls, nargs);
            } else {
                assert(TYPEOF(cls) == BCClosure::type);
                bcls = (BCClosure*)cls;
            }
            stack.set(nargs, (SEXP)bcls);

            storeCont();
            setState(bcls->fun, bcls->fun->code[0], bcls->env, nargs);

            std::cout << "====  Function ========\n";
            for (auto c : fun->code) {
                c->print();
            }
            std::cout << "==== /Function ========\n\n";

            break;
        }

        case BC_t::load_arg: {
            num_args_t a = bc.immediateNumArgs();
            assert(a < numArgs);
            stack.push(stack.at(sp + a));
            break;
        }

        case BC_t::check_numarg: {
            num_args_t a = bc.immediateNumArgs();
            assert(numArgs == a);
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

        case BC_t::force: {
            SEXP val = stack.top();
            assert(TYPEOF(val) == BCProm::type);

            BCProm* prom = (BCProm*)val;

            storeCont();
            setState(prom->fun, prom->fun->code[prom->idx], prom->env, 0);

            std::cout << "====  Promise ========\n";
            cur->print();
            std::cout << "==== /Promise ========\n\n";
            break;
        }

        case BC_t::pop:
            stack.pop();
            break;

        case BC_t::ret: {
            SEXP res = stack.pop();
            for (num_args_t i = 0; i < numArgs; ++i) {
                stack.pop();
            }

            if (!cont.empty()) {
                assert(TYPEOF(stack.top()) == BCClosure::type ||
                       TYPEOF(stack.top()) == BCProm::type);
                stack.pop();
                stack.push(res);
                restoreCont();
            } else {
                stack.push(res);
                goto done;
            }
            break;
        }

        case BC_t::mkclosure:
        case BC_t::num_of:
        case BC_t::invalid:
            assert(false);
        }
    }

done:
    return stack.top();
}
} // namespace

SEXP Interpreter::run(SEXP env) { return evalFunction(fun, env); }

} // rir
} // rjit
