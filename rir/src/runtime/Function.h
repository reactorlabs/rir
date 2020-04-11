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

    static constexpr size_t NUM_PTRS = 1;

    Function(size_t functionSize, SEXP body_,
             const std::vector<SEXP>& defaultArgs,
             const FunctionSignature& signature)
        : RirRuntimeObject(
              // GC area starts at &locals and goes to the end of defaultArg_
              sizeof(Function) - NUM_PTRS * sizeof(FunctionSEXP),
              NUM_PTRS + defaultArgs.size()),
          size(functionSize), numArgs(defaultArgs.size()),
          signature_(signature) {
        for (size_t i = 0; i < numArgs; ++i)
            setEntry(NUM_PTRS + i, defaultArgs[i]);
        body(body_);
    }

    Code* body() const { return Code::unpack(getEntry(0)); }
    void body(SEXP body) { setEntry(0, body); }

    static Function* deserialize(SEXP refTable, R_inpstream_t inp);
    void serialize(SEXP refTable, R_outpstream_t out) const;
    void disassemble(std::ostream&);

    Code* defaultArg(size_t i) const {
        assert(i < numArgs);
        if (!defaultArg_[i])
            return nullptr;
        return Code::unpack(defaultArg_[i]);
    }

    void registerInvocation() { body()->registerInvocation(); }
    size_t invocationCount() { return body()->funInvocationCount; }
    void registerDeopt() { body()->registerDeopt(); }
    size_t deoptCount() { return body()->deoptCount; }

    unsigned size; /// Size, in bytes, of the function and its data

#define RIR_FUNCTION_FLAGS(V)                                                  \
    V(Deopt)                                                                   \
    V(MarkOpt)                                                                 \
    V(ForceInline)                                                             \
    V(DisableInline)                                                           \
    V(NotOptimizable)                                                          \
    V(NotInlineable)                                                           \
    V(Dead)                                                                    \
    V(InnerFunction)                                                           \
    V(DisableAllSpecialization)                                                \
    V(DisableArgumentTypeSpecialization)                                       \
    V(DisableNumArgumentsSepzialization)

    enum Flag {
#define V(F) F,
        RIR_FUNCTION_FLAGS(V)
#undef V

            FIRST = Deopt,
        LAST = DisableNumArgumentsSepzialization
    };
    EnumSet<Flag> flags;

    void inheritFlags(const Function* other) {
        static Flag inherited[] = {ForceInline, DisableInline,
                                   DisableAllSpecialization,
                                   DisableArgumentTypeSpecialization,
                                   DisableNumArgumentsSepzialization};
        auto f = other->flags;
        if (f.includes(DisableAllSpecialization))
            assert(!signature().assumptions.includes(
                Assumption::NoReflectiveArgument));
        for (auto flag : inherited)
            if (f.contains(flag))
                flags.set(flag);
    }

    void clearDisabledAssumptions(Assumptions& given) const;

    unsigned numArgs;

    const FunctionSignature& signature() const { return signature_; }

  private:
    FunctionSignature signature_; /// pointer to this version's signature

    // !!! SEXPs traceable by the GC must be declared here !!!
    // locals contains: origin, next, body
    CodeSEXP locals[NUM_PTRS];
    CodeSEXP defaultArg_[];
};
#pragma pack(pop)
} // namespace rir

#endif
