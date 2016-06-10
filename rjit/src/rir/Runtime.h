#ifndef RIR_RUNTIME_H
#define RIR_RUNTIME_H

#include "../RDefs.h"
#include "../RIntlns_inc.h"
#include "BC.h"
#include "Code.h"

namespace rjit {
namespace rir {

constexpr static int BCCodeType = 24; // RAWSXP

#pragma pack(push)
#pragma pack(0)
struct BCProm {
  public:
    static constexpr short type = 0x9703;
    short t;

    Code* fun;
    SEXP env;
    SEXP ast() { return fun->ast; }
    SEXP val(SEXP wrapper) { return _val; }
    void val(SEXP wrapper, SEXP aVal);
    BCProm(Code* fun, SEXP env) : t(type), fun(fun), env(env) {}

  private:
    SEXP _val = nullptr;
};

struct BCClosure {
  public:
    static constexpr short type = 0xc105;

    short t;
    Code::CC cc;
    num_args_t nargs;

    Code* fun;
    SEXP formals;
    SEXP env;
    BCClosure(Code* fun, SEXP formals, num_args_t nargs, Code::CC cc, SEXP env)
        : t(type), cc(cc), nargs(nargs), fun(fun), formals(formals), env(env) {}
};
#pragma pack(pop)

SEXP mkBCProm(Code* fun, SEXP env);
SEXP mkBCCls(Code* fun, SEXP formals, num_args_t nargs, Code::CC cc, SEXP env);

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
