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

struct NativeBuiltins {

    static NativeBuiltin forcePromise;

    static NativeBuiltin consNr;
    static NativeBuiltin consNrTagged;
    static NativeBuiltin consNrTaggedMissing;

    static NativeBuiltin createEnvironment;

    static NativeBuiltin ldvar;
    static NativeBuiltin stvar;
    static NativeBuiltin ldfun;

    static NativeBuiltin error;

    static NativeBuiltin createPromise;

    static NativeBuiltin call;
    static NativeBuiltin callBuiltin;

    static NativeBuiltin newInt;
    static NativeBuiltin newReal;
};
}
}

#endif
