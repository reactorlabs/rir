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

// TODO removed src reference, now each code has its own
/** A Function holds the RIR code for some GNU R function.
 *  Each function start with a header and a sequence of
 *  Code objects for the body and all of the promises
 *  in the code.
 *
 *  The header start with a magic constant. This is a
 *  temporary hack so that it is possible to differentiate
 *  an R int vector from a Function. Eventually, we will
 *  add a new SEXP type for this purpose.
 *
 *  The size of the function, in bytes, includes the size
 *  of all of its Code objects and is padded to a word
 *  boundary.
 *
 *  A Function may be the result of optimizing another
 *  Function, in which case the origin field stores that
 *  Function as a SEXP pointer.
 *
 *  A Function has a source AST, stored in src.
 *
 *  A Function has a number of Code objects, codeLen, stored
 *  inline in data.
 */
#pragma pack(push)
#pragma pack(1)
struct Function : public RirRuntimeObject<Function, FUNCTION_MAGIC> {
    friend class FunctionCodeIterator;
    friend class ConstFunctionCodeIterator;

    static constexpr size_t NUM_PTRS = 3;

    Function(size_t functionSize, SEXP body,
             const std::vector<SEXP>& defaultArgs)
        : RirRuntimeObject(
              // GC area starts just before the end of the Function
              sizeof(Function) - NUM_PTRS * sizeof(FunctionSEXP),
              // GC area includes the SEXPs before the Code objects array
              NUM_PTRS + defaultArgs.size()),
          size(functionSize), signature(nullptr), deopt(false), markOpt(false),
          numArgs(defaultArgs.size()) {
        origin(nullptr);
        next(nullptr);
        for (size_t i = 0; i < numArgs; ++i) {
            // Set codeObjects[i] to point to Code object c's container
            setEntry(NUM_PTRS + i, defaultArgs[i]);
        }
        setBody(body);
    }

    Code* body() { return Code::unpack(getEntry(2)); }
    void setBody(SEXP body) { setEntry(2, body); }

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
    // !!!   *before* the CodeSEXP array.                  !!!
    // !!! Furthermore, you need to update                 !!!
    // !!!     the CODEOBJ_OFFSET constant.                !!!

    CodeSEXP locals[NUM_PTRS];
    CodeSEXP defaultArg_[];
};
#pragma pack(pop)
}

#endif
