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
             const FunctionSignature& signature, const Context& ctx)
        : RirRuntimeObject(
              // GC area starts at &locals and goes to the end of defaultArg_
              sizeof(Function) - NUM_PTRS * sizeof(FunctionSEXP),
              NUM_PTRS + defaultArgs.size()),
          size(functionSize), numArgs_(defaultArgs.size()),
          signature_(signature), context_(ctx) {
        for (size_t i = 0; i < numArgs_; ++i)
            setEntry(NUM_PTRS + i, defaultArgs[i]);
        body(body_);
    }

    Code* body() const { return Code::unpack(getEntry(0)); }
    void body(SEXP body) { setEntry(0, body); }

    static Function* deserialize(SEXP refTable, R_inpstream_t inp);
    void serialize(SEXP refTable, R_outpstream_t out) const;
    void disassemble(std::ostream&);

    bool isOptimized() const {
        return signature_.optimization !=
               FunctionSignature::OptimizationLevel::Baseline;
    }

    Code* defaultArg(size_t i) const {
        assert(i < numArgs_);
        if (!defaultArg_[i])
            return nullptr;
        return Code::unpack(defaultArg_[i]);
    }

    size_t invocationCount() { return invocationCount_; }

    size_t deoptCount() { return deoptCount_; }
    void addDeoptCount(size_t n) { deoptCount_ += n; }

    static inline unsigned long rdtsc() {
        unsigned low, high;
        asm volatile("rdtsc" : "=a"(low), "=d"(high));
        return ((low) | ((uint64_t)(high) << 32));
    }
    static constexpr unsigned long MAX_TIME_MEASURE = 1e9;

    void unregisterInvocation() {
        invoked = 0;
        if (invocationCount_ > 0)
            invocationCount_--;
    }

    void registerInvocation() {
        if (execTime < MAX_TIME_MEASURE) {
            // constant increment for recursive functions
            if (invoked != 0)
                execTime += 5e5;
            else
                invoked = rdtsc();
        }

        if (invocationCount_ < UINT_MAX)
            invocationCount_++;
    }
    void registerEndInvocation() {
        if (invoked != 0) {
            execTime += rdtsc() - invoked;
            invoked = 0;
        }
    }
    unsigned long invocationTime() { return execTime; }
    void clearInvocationTime() { execTime = 0; }

    unsigned size; /// Size, in bytes, of the function and its data

#define RIR_FUNCTION_FLAGS(V)                                                  \
    V(Deopt)                                                                   \
    V(MarkOpt)                                                                 \
    V(ForceInline)                                                             \
    V(DisableInline)                                                           \
    V(DepromiseArgs)                                                           \
    V(NotOptimizable)                                                          \
    V(NotInlineable)                                                           \
    V(InnerFunction)                                                           \
    V(DisableAllSpecialization)                                                \
    V(DisableArgumentTypeSpecialization)                                       \
    V(NeedsFullEnv)                                                            \
    V(Reoptimize)                                                              \
    V(DisableNumArgumentsSpezialization)

    enum Flag {
#define V(F) F,
        RIR_FUNCTION_FLAGS(V)
#undef V

        FIRST = Deopt,
        LAST = DisableNumArgumentsSpezialization
    };
    EnumSet<Flag> flags;

    void inheritFlags(const Function* other) {
        static Flag inherited[] = {ForceInline,
                                   DisableInline,
                                   DisableAllSpecialization,
                                   DisableArgumentTypeSpecialization,
                                   DisableNumArgumentsSpezialization,
                                   DepromiseArgs};
        auto f = other->flags;
        for (auto flag : inherited)
            if (f.contains(flag))
                flags.set(flag);
    }

    void clearDisabledAssumptions(Context& given) const;

    unsigned nargs() const { return numArgs_; }
    unsigned expectedNargs() const { return numArgs_ - context().numMissing(); }

    const FunctionSignature& signature() const { return signature_; }
    const Context& context() const { return context_; }

    bool disabled() const { return flags.contains(Flag::Deopt); }
    bool pendingCompilation() const { return body()->pendingCompilation(); }

    void registerDeopt() {
        // Deopt counts are kept on the optimized versions
        assert(isOptimized());
        flags.set(Flag::Deopt);
        if (deoptCount_ < UINT_MAX)
            deoptCount_++;
    }

    void registerDeoptReason(DeoptReason::Reason r) {
        // Deopt reasons are counted in the baseline
        assert(!isOptimized());
        if (r == DeoptReason::DeadCall)
            deadCallReached_++;
        if (r == DeoptReason::EnvStubMaterialized)
            flags.set(NeedsFullEnv);
    }

    size_t deadCallReached() const {
        assert(!isOptimized());
        return deadCallReached_;
    }

  private:
    unsigned numArgs_;

    unsigned invocationCount_ = 0;

    unsigned deoptCount_ = 0;
    unsigned deadCallReached_ = 0;

    unsigned long invoked = 0;
    unsigned long execTime = 0;

    FunctionSignature signature_; /// pointer to this version's signature
    Context context_;

    // !!! SEXPs traceable by the GC must be declared here !!!
    // locals contains: body
    CodeSEXP locals[NUM_PTRS];
    CodeSEXP defaultArg_[];
};
#pragma pack(pop)
} // namespace rir

#endif
