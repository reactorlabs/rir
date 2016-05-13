#ifndef RIR_RUNTIME_H
#define RIR_RUNTIME_H

#include "RIntlns.h"
#include "Function.h"
#include "BC.h"

namespace rjit {
namespace rir {

struct BCProm {
    static constexpr SEXPTYPE type = 27;

    BCProm() { sxpinfo.type = type; }
    BCProm(Function* fun, fun_idx_t idx, SEXP env)
        : idx(idx), fun(fun), env(env) {
        sxpinfo.type = type;
    }

    sxpinfo_struct sxpinfo = {0};
    SEXP attrib = R_NilValue;
    SEXP gengc_next_node, gengc_prev_node;

    fun_idx_t idx;
    Function* fun;
    SEXP env;

    SEXP ast() { return fun->ast[idx]; }
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

} // rir
} // rjit

#endif
