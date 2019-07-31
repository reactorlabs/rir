#ifndef PIR_NATIVE_BUILTINS
#define PIR_NATIVE_BUILTINS

#include "R/r_incl.h"
#include "R_ext/Boolean.h"
#include "jit/jit.h"
#include <cstddef>

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
}

namespace llvm {
class FunctionType;
}

namespace rir {
namespace pir {

static const auto sxp = jit_type_void_ptr;

struct NativeBuiltin {
    const char* name;
    void* fun;
    size_t nargs;
    jit_type_t signature;
    llvm::FunctionType* llvmSignature;
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
    static NativeBuiltin createBindingCell;
    static NativeBuiltin createMissingBindingCell;

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

    static NativeBuiltin is;
    static NativeBuiltin isMissing;
    static NativeBuiltin asTest;
    static NativeBuiltin asLogicalBlt;

    static NativeBuiltin length;

    static NativeBuiltin deopt;

    static NativeBuiltin assertFail;

    static NativeBuiltin printValue;

    static NativeBuiltin extract11;
    static NativeBuiltin extract21;

    static NativeBuiltin nativeCallTrampoline;
};
}
}

#endif
