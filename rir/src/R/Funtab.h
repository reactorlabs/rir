#ifndef RIR_FUNTAB_H
#define RIR_FUNTAB_H

#include "BuiltinIds.h"
#include "R/r.h"

#include <cassert>

static inline int getBuiltinNr(SEXP f) { return f->u.primsxp.offset; }

static inline CCODE getBuiltin(SEXP f) {
    return R_FunTab[getBuiltinNr(f)].cfun;
}

static inline const char* getBuiltinName(int i) { return R_FunTab[i].name; }
static inline const char* getBuiltinName(SEXP f) {
    return getBuiltinName(getBuiltinNr(f));
}

static inline int getBuiltinArity(SEXP f) {
    return R_FunTab[getBuiltinNr(f)].arity;
}

static inline int getFlag(int i) { return ((R_FunTab[i].eval) / 100) % 10; }
static inline int getFlag(SEXP f) { return getFlag(getBuiltinNr(f)); }

static inline SEXP getBuiltinFun(int id) {
    assert(R_FunTab[id].eval % 10 == 1 &&
           "Only use for BUILTINSXP");
    if (R_FunTab[id].eval % 100 / 10 == 0) {
        return Rf_install(getBuiltinName(id))->u.symsxp.value;
    } else {
        return Rf_install(getBuiltinName(id))->u.symsxp.internal;
    }

}

static inline SEXP getBuiltinFun(char const* name) {
    assert(R_FunTab[rir::blt(name)].eval % 10 == 1 &&
           "Only use for BUILTINSXP");
    if (R_FunTab[rir::blt(name)].eval % 100 / 10 == 0)
        return Rf_install(name)->u.symsxp.value;
    else
        return Rf_install(name)->u.symsxp.internal;
}

#endif
