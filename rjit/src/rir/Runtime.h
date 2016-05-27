#ifndef RIR_RUNTIME_H
#define RIR_RUNTIME_H

#include "Function.h"
#include "BC.h"
#include "../RDefs.h"

extern "C" unsigned char* RAW(SEXP);

namespace rjit {
namespace rir {

constexpr static int BCCodeType = 24; // EXTPTRSXP

#pragma pack(push)
#pragma pack(0)
struct BCProm {
  public:
    static constexpr unsigned char type = 1;
    unsigned char t = type;
    Function* fun;
    fun_idx_t idx;
    SEXP env;
    SEXP ast() { return fun->code[idx]->ast; }
    SEXP val() { return _val; }
    void val(SEXP wrapper, SEXP aVal);
    BCProm(Function* fun, fun_idx_t idx, SEXP env)
        : fun(fun), idx(idx), env(env) {}

  private:
    SEXP _val = nullptr;
};

struct BCClosure {
  public:
    static constexpr unsigned char type = 2;
    unsigned char t = type;
    Function* fun;
    SEXP formals;
    num_args_t nargs;
    bool eager;
    SEXP env;
    BCClosure(Function* fun, SEXP formals, num_args_t nargs, bool eager,
              SEXP env)
        : fun(fun), formals(formals), nargs(nargs), eager(eager), env(env) {}
};

#pragma pack(pop)

SEXP mkBCProm(Function* fun, fun_idx_t idx, SEXP env);
SEXP mkBCCls(Function* fun, SEXP formals, num_args_t nargs, bool eager,
             SEXP env);

inline BCProm* getBCProm(SEXP s) { return (BCProm*)RAW(s); }

inline BCClosure* getBCCls(SEXP s) { return (BCClosure*)RAW(s); }

inline bool isBCProm(SEXP s) {
    return TYPEOF(s) == BCCodeType && getBCProm(s)->t == BCProm::type;
}

inline bool isBCCls(SEXP s) {
    return TYPEOF(s) == BCCodeType && getBCCls(s)->t == BCClosure::type;
}

} // rir
} // rjit

#endif
