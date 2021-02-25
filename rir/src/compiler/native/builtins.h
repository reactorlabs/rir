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

        ldvar,
        ldvarGlobal,
        ldvarForUpdate,
        ldvarCacheMiss,
        stvar,
        stvarSuper,
        stvari,
        stvarr,
        defvar,
        starg,
        ldfun,
        chkfun,

        setCar,
        setCdr,
        setTag,

        externalsxpSetEntry,

        error,
        warn,

        createEnvironment,
        createStubEnvironment,
        materializeEnvironment,
        createPromise,
        createPromiseNoEnv,
        createPromiseEager,
        createPromiseNoEnvEager,
        createClosure,

        newInt,
        newIntDebug,
        newReal,
        newIntFromReal,
        newRealFromInt,

        makeVector,

        call,
        namedCall,
        dotsCall,
        callBuiltin,

        notOp,
        notEnv,
        binop,
        binopEnv,
        unop,
        unopEnv,

        isMissing,
        checkTrueFalse,
        asLogicalBlt,

        length,
        forSeqSize,

        deopt,

        assertFail,

        printValue,

        extract11,
        extract21,
        extract21i,
        extract21r,
        extract12,
        extract22,
        extract22ii,
        extract22rr,
        extract13,

        subassign11,
        subassign21,
        subassign21ii,
        subassign21rr,
        subassign21ir,
        subassign21ri,
        subassign12,
        subassign22iii,
        subassign22rrr,
        subassign22rri,
        subassign22iir,
        subassign22,
        subassign13,

        nativeCallTrampoline,

        initClosureContext,
        endClosureContext,

        recordDeopt,

        matrixNrows,
        matrixNcols,

        sumr,
        prodr,

        colonInputEffects,
        colonCastLhs,
        colonCastRhs,

        colon,

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
#ifdef ENABLE_SLOWASSERT
        assert(initialized);
#endif
        return store[static_cast<size_t>(id)];
    }

    using BuiltinAction = std::function<void(const NativeBuiltin&)>;
    static void eachBuiltin(BuiltinAction it) {
#ifdef ENABLE_SLOWASSERT
        assert(initialized);
#endif
        for (size_t i = static_cast<size_t>(Id::FIRST),
                    e = static_cast<size_t>(Id::LAST);
             i <= e; i++) {
            it(store[i]);
        }
    }

  private:
    static NativeBuiltin& blt(Id id) { return store[static_cast<size_t>(id)]; }
    static NativeBuiltin store[static_cast<size_t>(Id::NUM_BUILTINS)];
    static bool initialized;
    static bool initializeBuiltins();
    friend void initializeTypes(llvm::LLVMContext& context);
};

} // namespace pir
} // namespace rir

#endif
