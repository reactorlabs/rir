#ifndef RIR_FUNCTION_H
#define RIR_FUNCTION_H

#include "Code.h"
#include "FunctionSignature.h"
#include "R/r.h"
#include "RirRuntimeObject.h"

namespace rir {

/**
 * Aliases for readability.
 */
typedef SEXP FunctionSEXP;

// Function magic constant is designed to help to distinguish between Function
// objects and normal EXTERNALSXPs. Normally this is not necessary, but a very
// creative user might try to assign arbitrary EXTERNAL to a closure which we
// would like to spot. Of course, such a creative user might actually put the
// magic in his vector too...
#define FUNCTION_MAGIC (unsigned)0xca11ab1e

/** A RIR function represents GNU R function.
 *
 *  Each function start with a header and some metadata. Then there are
 *  (GC traceable) pointers to the body and the compiled default arguments.
 *  If an argument has no default, the default arg is null.
 *
 *  A Function may be the result of optimizing another
 *  Function, in which case the origin field stores that
 *  Function as a SEXP pointer.
 *
 *  A Function source is stored in the body code object
 *
 */
#pragma pack(push)
#pragma pack(1)
struct Function : public RirRuntimeObject<Function, FUNCTION_MAGIC> {
    friend class FunctionCodeIterator;
    friend class ConstFunctionCodeIterator;

    static constexpr size_t NUM_PTRS = 3;

    Function(size_t functionSize, SEXP body_,
             const std::vector<SEXP>& defaultArgs)
        : RirRuntimeObject(
              // GC area starts at &locals and goes to the end of defaultArg_
              sizeof(Function) - NUM_PTRS * sizeof(FunctionSEXP),
              NUM_PTRS + defaultArgs.size()),
          size(functionSize), signature(nullptr), deopt(false), markOpt(false),
          numArgs(defaultArgs.size()) {
        origin(nullptr);
        next(nullptr);
        for (size_t i = 0; i < numArgs; ++i)
            setEntry(NUM_PTRS + i, defaultArgs[i]);
        body(body_);
    }

    Code* body() { return Code::unpack(getEntry(2)); }
    void body(SEXP body) { setEntry(2, body); }

    void disassemble(std::ostream&);

    FunctionSEXP origin() { return getEntry(0); }
    void origin(FunctionSEXP s) { setEntry(0, s); }

    FunctionSEXP next() { return getEntry(1); }
    void next(FunctionSEXP s) { setEntry(1, s); }

    Code* defaultArg(size_t i) const {
        assert(i < numArgs);
        if (!defaultArg_[i])
            return nullptr;
        return Code::unpack(defaultArg_[i]);
    }

    void registerInvocation() { body()->registerInvocation(); }
    size_t invocationCount() { return body()->funInvocationCount; }

    unsigned size; /// Size, in bytes, of the function and its data

    FunctionSignature* signature; /// pointer to this version's signature

    unsigned deopt : 1;
    unsigned markOpt : 1;
    unsigned spare : 30;

    unsigned numArgs;

  private:
    // !!! SEXPs traceable by the GC must be declared here !!!
    // locals contains: origin, next, body
    CodeSEXP locals[NUM_PTRS];
    CodeSEXP defaultArg_[];
};
#pragma pack(pop)
}

#endif
