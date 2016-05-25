#ifndef RIR_RUNTIME_H
#define RIR_RUNTIME_H

#include "Function.h"
#include "BC.h"

#define TYPE_BITS 5
struct sxpinfo_struct {
    SEXPTYPE type : TYPE_BITS; /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
                        * -> warning: `type' is narrower than values
                        *              of its type
                        * when SEXPTYPE was an enum */
    unsigned int obj : 1;
    unsigned int named : 2;
    unsigned int gp : 16;
    unsigned int mark : 1;
    unsigned int debug : 1;
    unsigned int trace : 1; /* functions and memory tracing */
    unsigned int spare : 1; /* currently unused */
    unsigned int gcgen : 1; /* old generation number */
    unsigned int gccls : 3; /* node class */
};                          /*		    Tot: 32 */

namespace rjit {
namespace rir {

struct BCProm {
    static constexpr SEXPTYPE type = 27;

    BCProm() { sxpinfo.type = type; }
    BCProm(Function* fun, fun_idx_t idx, SEXP env)
        : idx(idx), fun(fun), env(env) {
        sxpinfo.type = type;
        sxpinfo.mark = 1;
        assert(fun->code.size() > idx);
    }

    sxpinfo_struct sxpinfo = {0};
    //    SEXP attrib = R_NilValue;
    //    SEXP gengc_next_node, gengc_prev_node;

    fun_idx_t idx;
    Function* fun;
    SEXP env;
    SEXP val = nullptr;

    SEXP ast() { return fun->code[idx]->ast; }
};

struct BCClosure {
    static constexpr SEXPTYPE type = 28;
    BCClosure() : eager(false) {
        sxpinfo.type = type;
        sxpinfo.mark = 1;
    }

    sxpinfo_struct sxpinfo = {0};
    SEXP attrib = R_NilValue;
    SEXP gengc_next_node, gengc_prev_node;

    Function* fun;
    SEXP env;
    SEXP formals;

    num_args_t nargs;
    bool eager;
};

} // rir
} // rjit

#endif
