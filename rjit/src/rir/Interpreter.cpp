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

    T at(size_t offset) { return stack[stack.size() - 1 - offset]; }

    T top() { return stack.back(); }
};

struct Continuation {
  public:
    Function* fun;
    Code* code;
    BC_t* pc;
    SEXP env;
    Continuation(Function* fun, Code* code, BC_t* pc, SEXP env)
        : fun(fun), code(code), pc(pc), env(env) {}
};

SEXP evalFunction(Function* f, SEXP env) {

    Stack<SEXP> stack;
    Stack<Continuation> cont;

    Code* cur;
    BC_t* pc;

    std::array<SEXP, MAX_NUM_ARGS> arg;

    auto setState = [&f, &cur, &pc, &env](Function* fun, Code* code, SEXP env) {
        f = fun;
        cur = code;
        pc = cur->bc;
        env = env;
    };

    auto storeCont = [&cont, &f, &cur, &pc, &env]() {
        cont.push(Continuation(f, cur, pc, env));
    };

    auto restoreCont = [&cont, &f, &cur, &pc, &env]() {
        Continuation c = cont.pop();
        f = c.fun;
        cur = c.code;
        pc = c.pc;
        env = c.env;
    };

    setState(f, f->code[0], env);
    for (auto c : f->code) {
        c->print();
    }

    while (true) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
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
            auto prom = new BCProm(f, bc.immediateFunIdx(), env);
            stack.push((SEXP)prom);
            break;
        }

        case BC_t::call: {
            num_args_t nargs = bc.immediateNumArgs();

            SEXP fun = stack.at(nargs);
            BCClosure* cls;

            if (TYPEOF(fun) == CLOSXP) {
                cls = jit(fun);
            } else if (TYPEOF(fun) == SPECIALSXP) {
                cls = Primitives::compilePrimitive(fun, nargs);
            } else {
                assert(TYPEOF(fun) == BCClosure::type);
                cls = (BCClosure*)fun;
            }

            arg.fill(nullptr);
            for (int i = nargs - 1; i >= 0; --i) {
                arg[i] = stack.pop();
            }
            stack.pop();

            storeCont();
            setState(cls->fun, cls->fun->code[0], cls->env);
            cur->print();
            break;
        }

        case BC_t::load_arg: {
            num_args_t a = bc.immediateNumArgs();
            stack.push(arg[a]);
            break;
        }

        case BC_t::get_ast: {
            SEXP t = stack.top();
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
            SEXP val = stack.pop();
            assert(TYPEOF(val) == BCProm::type);

            BCProm* prom = (BCProm*)val;

            storeCont();
            setState(prom->fun, prom->fun->code[prom->idx], prom->env);
            cur->print();
            break;
        }

        case BC_t::pop:
            stack.pop();
            break;

        case BC_t::ret:
            if (!cont.empty())
                restoreCont();
            else
                goto done;
            break;

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
