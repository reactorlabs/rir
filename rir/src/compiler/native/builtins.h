#ifndef PIR_NATIVE_BUILTINS
#define PIR_NATIVE_BUILTINS

#include "R/r_incl.h"
#include "R_ext/Boolean.h"

#include "llvm/IR/Attributes.h"

#include <cstddef>
#include <vector>

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
    std::vector<llvm::Attribute::AttrKind> attrs;
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

    enum class Id : uint8_t {
        forcePromise,
        consNr,
        createBindingCell,
        createMissingBindingCell,
        createEnvironment,
        createStubEnvironment,
        materializeEnvironment,
        ldvarForUpdate,
        ldvar,
        ldvarGlobal,
        ldvarCacheMiss,
        stvarSuper,
        stvar,
        stvari,
        stvarr,
        starg,
        setCar,
        setCdr,
        setTag,
        externalsxpSetEntry,
        defvar,
        ldfun,
        chkfun,
        warn,
        error,
        callBuiltin,
        call,
        namedCall,
        dotsCall,
        createPromise,
        createPromiseNoEnvEager,
        createPromiseNoEnv,
        createPromiseEager,
        createClosure,
        newIntFromReal,
        newRealFromInt,
        newInt,
        newIntDebug,
        newReal,
        unopEnv,
        unop,
        notEnv,
        notOp,
        binopEnv,
        binop,
        colon,
        isMissing,
        checkTrueFalse,
        asLogicalBlt,
        length,
        deopt,
        recordDeopt,
        assertFail,
        printValue,
        extract11,
        extract21,
        extract21i,
        extract21r,
        extract12,
        extract13,
        extract22,
        extract22ii,
        extract22rr,
        nativeCallTrampoline,
        subassign11,
        subassign21,
        subassign21ii,
        subassign21rr,
        subassign21ri,
        subassign21ir,
        subassign12,
        subassign13,
        subassign22,
        subassign22iii,
        subassign22rrr,
        subassign22rri,
        subassign22iir,
        forSeqSize,
        initClosureContext,
        endClosureContext,
        matrixNcols,
        matrixNrows,
        makeVector,
        prodr,
        sumr,
        colonInputEffects,
        colonCastLhs,
        colonCastRhs,
        names,
        setNames,
        xlength_,
        getAttrb,
        nonLocalReturn,
        clsEq,
        checkType,
        shallowDuplicate,
        sigsetjmp,

        // book keeping
        NUM_BUILTINS,
        FIRST = forcePromise,
        LAST = sigsetjmp
    };

    static constexpr unsigned long bindingsCacheFails = 2;

    static const NativeBuiltin& get(Id id) {
        return store[static_cast<size_t>(id)];
    }

    using BuiltinAction = std::function<void(const NativeBuiltin&)>;
    static void eachBuiltin(BuiltinAction it) {
        for (size_t i = static_cast<size_t>(Id::FIRST),
                    e = static_cast<size_t>(Id::LAST);
             i <= e; i++) {
            it(store[i]);
        }
    }

    static void initializeBuiltins();

  private:
    // For setting up - returns mutable reference
    static NativeBuiltin& get_(Id id) { return store[static_cast<size_t>(id)]; }

    static NativeBuiltin store[static_cast<size_t>(Id::NUM_BUILTINS)];
};

} // namespace pir
} // namespace rir

#endif
