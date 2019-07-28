#ifndef PIR_NATIVE_BUILTINS
#define PIR_NATIVE_BUILTINS

#include "R/r.h"
#include "jit/jit.h"

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
}

namespace rir {
namespace pir {

static const auto sxp = jit_type_void_ptr;

struct NativeBuiltin {
    const char* name;
    void* fun;
    size_t nargs;
    jit_type_t signature;
};

enum class BinopKind : int {
    ADD,
    MUL,
    SUB,
    DIV,
    EQ,
    NE,
    LT,
    LTE,
    GT,
    GTE,
    LAND,
    LOR,
};

struct NativeBuiltins {

    static NativeBuiltin forcePromise;

    static NativeBuiltin consNr;
    static NativeBuiltin consNrTagged;
    static NativeBuiltin consNrTaggedMissing;

    static NativeBuiltin ldvar;
    static NativeBuiltin ldvarCacheMiss;
    static NativeBuiltin stvar;
    static NativeBuiltin defvar;
    static NativeBuiltin starg;
    static NativeBuiltin ldfun;

    static NativeBuiltin setCar;

    static NativeBuiltin externalsxpSetEntry;

    static NativeBuiltin error;

    static NativeBuiltin createEnvironment;
    static NativeBuiltin createStubEnvironment;
    static NativeBuiltin createPromise;
    static NativeBuiltin createClosure;

    static NativeBuiltin newInt;
    static NativeBuiltin newLgl;
    static NativeBuiltin newReal;
    static NativeBuiltin newIntFromReal;
    static NativeBuiltin newLglFromReal;
    static NativeBuiltin newRealFromInt;

    static NativeBuiltin call;
    static NativeBuiltin callBuiltin;

    static NativeBuiltin notOp;
    static NativeBuiltin notEnv;
    static NativeBuiltin binop;
    static NativeBuiltin binopEnv;

    static NativeBuiltin asTest;
    static NativeBuiltin asLogical;

    static NativeBuiltin length;

    static NativeBuiltin deopt;

    static NativeBuiltin assertFail;

    static NativeBuiltin printValue;

    static NativeBuiltin extract11;
    static NativeBuiltin extract21;
};
}
}

#endif
