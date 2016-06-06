#ifndef RIR_RUNTIME_H
#define RIR_RUNTIME_H

#include "Function.h"
#include "BC.h"
#include "../RDefs.h"
#include "../RIntlns_inc.h"

namespace rjit {
namespace rir {

constexpr static int BCCodeType = 24; // RAWSXP

#pragma pack(push)
#pragma pack(0)
struct BCProm {
  public:
    static constexpr short type = 0x9703;
    short t;
    fun_idx_t idx;

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
    static constexpr short type = 0xc105;

    enum class CC : char {
        envLazy,
        stackLazy,
        stackEager,
    };

    short t;
    CC cc;
    num_args_t nargs;

    Function* fun;
    SEXP formals;
    SEXP env;
    BCClosure(Function* fun, SEXP formals, num_args_t nargs, CC cc, SEXP env)
        : t(type), cc(cc), nargs(nargs), fun(fun), formals(formals), env(env) {}
};
#pragma pack(pop)

SEXP mkBCProm(Function* fun, fun_idx_t idx, SEXP env);
SEXP mkBCCls(Function* fun, SEXP formals, num_args_t nargs, BCClosure::CC cc,
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
