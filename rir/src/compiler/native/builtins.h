#ifndef PIR_NATIVE_BUILTINS
#define PIR_NATIVE_BUILTINS

#include "R/r_incl.h"
#include "R_ext/Boolean.h"
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

struct NativeBuiltin {
    const char* name;
    void* fun;
    llvm::FunctionType* llvmSignature;
};

enum class BinopKind : int {
    ADD,
    MUL,
    SUB,
    DIV,
    IDIV,
    EQ,
    NE,
    LT,
    LTE,
    GT,
    GTE,
    LAND,
    LOR,
    COLON,
    MOD,
    POW,
};

enum class UnopKind : int {
    MINUS,
    PLUS,
};

struct NativeBuiltins {

    static NativeBuiltin forcePromise;

    static NativeBuiltin consNr;
    static NativeBuiltin createBindingCell;
    static NativeBuiltin createMissingBindingCell;

    static NativeBuiltin ldvar;
    static NativeBuiltin ldvarForUpdate;
    static NativeBuiltin ldvarCacheMiss;
    static NativeBuiltin stvar;
    static NativeBuiltin defvar;
    static NativeBuiltin starg;
    static NativeBuiltin ldfun;
    static NativeBuiltin chkfun;

    static NativeBuiltin setCar;
    static NativeBuiltin setCdr;
    static NativeBuiltin setTag;

    static NativeBuiltin externalsxpSetEntry;

    static NativeBuiltin error;
    static NativeBuiltin warn;

    static NativeBuiltin createEnvironment;
    static NativeBuiltin createStubEnvironment;
    static NativeBuiltin materializeEnvironment;
    static NativeBuiltin createPromise;
    static NativeBuiltin createClosure;

    static NativeBuiltin newInt;
    static NativeBuiltin newLgl;
    static NativeBuiltin newReal;
    static NativeBuiltin newIntFromReal;
    static NativeBuiltin newLglFromReal;
    static NativeBuiltin newRealFromInt;

    static NativeBuiltin makeVector;

    static NativeBuiltin call;
    static NativeBuiltin namedCall;
    static NativeBuiltin dotsCall;
    static NativeBuiltin callBuiltin;

    static NativeBuiltin notOp;
    static NativeBuiltin notEnv;
    static NativeBuiltin binop;
    static NativeBuiltin binopEnv;
    static NativeBuiltin unop;
    static NativeBuiltin unopEnv;

    static NativeBuiltin is;
    static NativeBuiltin isMissing;
    static NativeBuiltin asTest;
    static NativeBuiltin asLogicalBlt;

    static NativeBuiltin length;
    static NativeBuiltin forSeqSize;

    static NativeBuiltin deopt;

    static NativeBuiltin assertFail;

    static NativeBuiltin printValue;

    static NativeBuiltin extract11;
    static NativeBuiltin extract21;
    static NativeBuiltin extract21i;
    static NativeBuiltin extract21r;
    static NativeBuiltin extract12;
    static NativeBuiltin extract22;
    static NativeBuiltin extract22ii;
    static NativeBuiltin extract22rr;
    static NativeBuiltin extract13;

    static NativeBuiltin subassign11;
    static NativeBuiltin subassign21;
    static NativeBuiltin subassign21ii;
    static NativeBuiltin subassign21rr;
    static NativeBuiltin subassign21ir;
    static NativeBuiltin subassign21ri;
    static NativeBuiltin subassign12;
    static NativeBuiltin subassign22iii;
    static NativeBuiltin subassign22rrr;
    static NativeBuiltin subassign22rri;
    static NativeBuiltin subassign22iir;
    static NativeBuiltin subassign22;
    static NativeBuiltin subassign13;

    static NativeBuiltin asIntFloor;
    static NativeBuiltin asIntCeil;

    static NativeBuiltin nativeCallTrampoline;

    static NativeBuiltin initClosureContext;
    static NativeBuiltin endClosureContext;

    static NativeBuiltin recordDeopt;

    static NativeBuiltin matrixNrows;
    static NativeBuiltin matrixNcols;

    static NativeBuiltin sumr;
    static NativeBuiltin prodr;
};
}
}

#endif
