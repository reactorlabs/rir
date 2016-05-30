#ifndef RIR_RUNTIME_H
#define RIR_RUNTIME_H

#include "Function.h"
#include "BC.h"
#include "../RDefs.h"
#include "../RIntlns_inc.h"

namespace rjit {
namespace rir {

constexpr static int BCCodeType = 24; // RAWSXP

struct BCProm {
  public:
    static constexpr unsigned type = 0x9703;
    unsigned t : 16;
    fun_idx_t idx : 16;

    Function* fun;
    SEXP env;
    SEXP ast() { return fun->code[idx]->ast; }
    SEXP val(SEXP wrapper) { return _val; }
    void val(SEXP wrapper, SEXP aVal);
    BCProm(Function* fun, fun_idx_t idx, SEXP env)
        : t(type), idx(idx), fun(fun), env(env) {}

  private:
    SEXP _val = nullptr;
};

struct BCClosure {
  public:
    static constexpr unsigned type = 0xc105;
    unsigned t : 16;
    Function* fun;
    SEXP formals;
    num_args_t nargs;
    bool eager;
    SEXP env;
    BCClosure(Function* fun, SEXP formals, num_args_t nargs, bool eager,
              SEXP env)
        : t(type), fun(fun), formals(formals), nargs(nargs), eager(eager),
          env(env) {}
};

SEXP mkBCProm(Function* fun, fun_idx_t idx, SEXP env);
SEXP mkBCCls(Function* fun, SEXP formals, num_args_t nargs, bool eager,
             SEXP env);

inline BCProm* getBCProm(SEXP s) { return (BCProm*)Rinternals::raw(s); }

inline BCClosure* getBCCls(SEXP s) { return (BCClosure*)Rinternals::raw(s); }

inline bool isBCProm(SEXP s) {
    return Rinternals::typeof(s) == BCCodeType &&
           getBCProm(s)->t == BCProm::type;
}

inline bool isBCCls(SEXP s) {
    return Rinternals::typeof(s) == BCCodeType &&
           getBCCls(s)->t == BCClosure::type;
}

} // rir
} // rjit

#endif
