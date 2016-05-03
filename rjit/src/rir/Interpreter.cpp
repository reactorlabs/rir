#include "Interpreter.h"
#include "RIntlns.h"
#include "Function.h"
#include "Compiler.h"
#include "CodeStream.h"

#include <iostream>
#include <deque>

namespace rjit {
namespace rir {

namespace {

struct sxpinfo_struct {
    SEXPTYPE type : TYPE_BITS; /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
>------->------->-------     * -> warning: `type' is narrower than values
>------->------->-------     *              of its type
>------->------->-------     * when SEXPTYPE was an enum */
    unsigned int obj : 1;
    unsigned int named : 2;
    unsigned int gp : 16;
    unsigned int mark : 1;
    unsigned int debug : 1;
    unsigned int trace : 1; /* functions and memory tracing */
    unsigned int spare : 1; /* currently unused */
    unsigned int gcgen : 1; /* old generation number */
    unsigned int gccls : 3; /* node class */
};                          /*>-->-------    Tot: 32 */

struct BCProm {
    static constexpr SEXPTYPE type = 27;
    BCProm() { sxpinfo.type = type; }

    sxpinfo_struct sxpinfo = {0};
    SEXP attrib = R_NilValue;
    SEXP gengc_next_node, gengc_prev_node;

    fun_idx_t idx;
    Function* fun;
    SEXP env;
};

struct BCClosure {
    static constexpr SEXPTYPE type = 28;
    BCClosure() { sxpinfo.type = type; }

    sxpinfo_struct sxpinfo = {0};
    SEXP attrib = R_NilValue;
    SEXP gengc_next_node, gengc_prev_node;

    Function* fun;
    SEXP env;
    SEXP formals;
};

BCProm* makePromise(Function* fun, fun_idx_t idx, SEXP env) {
    BCProm* prom = new BCProm;
    prom->idx = idx;
    prom->fun = fun;
    prom->env = env;
    return prom;
}

BCClosure* jit(SEXP fun) {
    Compiler c(BODY(fun));
    BCClosure* cls = new BCClosure;
    cls->env = CLOENV(fun);
    cls->fun = c.finalize();
    cls->formals = FORMALS(fun);
    return cls;
}

BCClosure* getBuiltin(SEXP fun, num_args_t nargs) {
    Function* f = new Function;
    CodeStream cs(*f);
    switch (TYPEOF(fun)) {
    case SPECIALSXP: {
        long idx = (long)CAR(fun);

        if (idx == 11) {
            // do_begin
            if (nargs == 0) {
                cs << BC::push(R_NilValue);
            } else {
                while (--nargs > 0)
                    cs << BC::force() << BC::drop();
                cs << BC::force();
            }
        } else if (idx == 8) {
            // do_set
            // TODO
            assert(false);
        } else {
            assert(false);
        }
        break;
    }
    default:
        assert(false);
    }
    cs << BC::ret();
    cs.finalize();
    BCClosure* cls = new BCClosure;
    cls->env = CLOENV(fun);
    cls->fun = f;
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

    register SEXP tmp1;
    register SEXP tmp2;

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
    cur->print();

    while (true) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
        case BC_t::push:
            tmp1 = bc.immediateConst();
            stack.push(tmp1);
            break;

        case BC_t::getfun:
        // TODO
        case BC_t::getvar:
            tmp1 = bc.immediateConst();
            tmp2 = findVar(tmp1, env);
            R_Visible = TRUE;

            if (tmp2 == R_UnboundValue)
                assert(false and "Unbound var");
            else if (tmp2 == R_MissingArg)
                assert(false and "Missing argument");

            assert(TYPEOF(tmp2) != PROMSXP);
            if (TYPEOF(tmp2) == BCProm::type) {
                // TODO
                assert(false);
            }

            if (NAMED(tmp2) == 0 && tmp2 != R_NilValue)
                SET_NAMED(tmp2, 1);

            stack.push(tmp2);
            break;

        case BC_t::mkprom: {
            auto prom = makePromise(f, bc.immediateFunIdx(), env);
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
                cls = getBuiltin(fun, nargs);
            } else {
                assert(TYPEOF(fun) == BCClosure::type);
                cls = (BCClosure*)fun;
            }

            storeCont();
            setState(cls->fun, cls->fun->code[0], cls->env);
            cur->print();
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

        case BC_t::drop:
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
