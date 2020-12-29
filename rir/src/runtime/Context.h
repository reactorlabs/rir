#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <R/r_incl.h>
#include <array>
#include <cstring>
#include <iostream>

namespace rir {

enum class TypeAssumption {
    // Arg is already evaluated
    Arg0IsEager_,
    Arg1IsEager_,
    Arg2IsEager_,
    Arg3IsEager_,
    Arg4IsEager_,
    Arg5IsEager_,

    // Arg is not reflective
    Arg0IsNonRefl_,
    Arg1IsNonRefl_,
    Arg2IsNonRefl_,
    Arg3IsNonRefl_,
    Arg4IsNonRefl_,
    Arg5IsNonRefl_,

    // Arg is not an object
    Arg0IsNotObj_,
    Arg1IsNotObj_,
    Arg2IsNotObj_,
    Arg3IsNotObj_,
    Arg4IsNotObj_,
    Arg5IsNotObj_,

    // Arg is simple integer scalar
    Arg0IsSimpleInt_,
    Arg1IsSimpleInt_,
    Arg2IsSimpleInt_,
    Arg3IsSimpleInt_,
    Arg4IsSimpleInt_,
    Arg5IsSimpleInt_,

    // Arg is simple real scalar
    Arg0IsSimpleReal_,
    Arg1IsSimpleReal_,
    Arg2IsSimpleReal_,
    Arg3IsSimpleReal_,
    Arg4IsSimpleReal_,
    Arg5IsSimpleReal_,

    FIRST = Arg0IsEager_,
    LAST = Arg5IsSimpleReal_,
};

enum class Assumption {
    NoExplicitlyMissingArgs, // Explicitly missing, e.g. f(,,)
    CorrectOrderOfArguments, // Ie. the args are not named
    NotTooManyArguments,     // The number of args supplied is <= nargs
    StaticallyArgmatched,    // Arguments are statically matched

    FIRST = NoExplicitlyMissingArgs,
    LAST = StaticallyArgmatched,
};

struct Function;
namespace pir {
class ClosureVersion;
}

#pragma pack(push)
#pragma pack(1)
struct Context {
    typedef EnumSet<TypeAssumption, uint32_t> TypeFlags;
    typedef EnumSet<Assumption, uint8_t> Flags;

    constexpr static size_t MAX_MISSING = 255;
    // # of args with type assumptions
    constexpr static size_t NUM_TYPED_ARGS = 6;

    Context() = default;
    Context(const Context&) noexcept = default;

    explicit constexpr Context(const Flags& flags) : flags(flags) {}
    constexpr Context(const Flags& flags, uint8_t missing)
        : flags(flags), missing(missing) {}
    constexpr Context(const Flags& flags, const TypeFlags& typeFlags,
                      uint8_t missing)
        : flags(flags), typeFlags(typeFlags), missing(missing) {}
    explicit Context(void* pos) { memcpy((void*)this, pos, sizeof(*this)); }
    explicit Context(unsigned long val) {
        // Silence unused field warning
        (void)unused2;
        memcpy((void*)this, &val, sizeof(*this));
    }

    unsigned long toI() {
        static_assert(sizeof(*this) == sizeof(unsigned long), "");
        uint64_t m;
        memcpy(&m, this, sizeof(*this));
        return m;
    }

    RIR_INLINE void add(Assumption a) { flags.set(a); }
    RIR_INLINE void remove(Assumption a) { flags.reset(a); }
    RIR_INLINE bool includes(Assumption a) const { return flags.includes(a); }
    RIR_INLINE bool includes(const Flags& a) const { return flags.includes(a); }

#define TYPE_ASSUMPTIONS(Type)                                                 \
    static constexpr std::array<TypeAssumption, NUM_TYPED_ARGS>                \
        Type##Context = {                                                      \
            {TypeAssumption::Arg0Is##Type##_, TypeAssumption::Arg1Is##Type##_, \
             TypeAssumption::Arg2Is##Type##_, TypeAssumption::Arg3Is##Type##_, \
             TypeAssumption::Arg4Is##Type##_,                                  \
             TypeAssumption::Arg5Is##Type##_}};                                \
    RIR_INLINE bool is##Type(size_t i) const {                                 \
        if (i < NUM_TYPED_ARGS)                                                \
            if (typeFlags.includes(Type##Context[i]))                          \
                return true;                                                   \
        return false;                                                          \
    }                                                                          \
    RIR_INLINE void reset##Type(size_t i) {                                    \
        if (i < NUM_TYPED_ARGS)                                                \
            typeFlags.reset(Type##Context[i]);                                 \
    }                                                                          \
    RIR_INLINE void set##Type(size_t i) {                                      \
        if (i < NUM_TYPED_ARGS)                                                \
            typeFlags.set(Type##Context[i]);                                   \
    }
    TYPE_ASSUMPTIONS(Eager);
    TYPE_ASSUMPTIONS(NotObj);
    TYPE_ASSUMPTIONS(SimpleInt);
    TYPE_ASSUMPTIONS(SimpleReal);
    TYPE_ASSUMPTIONS(NonRefl);
#undef TYPE_ASSUMPTIONS

    static TypeFlags allEagerArgsFlags() {
        Context a;
        for (size_t i = 0; i < NUM_TYPED_ARGS; ++i)
            a.setEager(i);
        return a.typeFlags;
    }
    static TypeFlags allNonObjArgsFlags() {
        Context a;
        for (size_t i = 0; i < NUM_TYPED_ARGS; ++i)
            a.setNotObj(i);
        return a.typeFlags;
    }

    RIR_INLINE uint8_t numMissing() const { return missing; }

    RIR_INLINE void numMissing(long i) {
        assert(i < 255);
        missing = i;
    }

    RIR_INLINE bool empty() const {
        return flags.empty() && typeFlags.empty() && missing == 0;
    }

    RIR_INLINE size_t count() const {
        return flags.count() + typeFlags.count();
    }

    constexpr Context operator|(const Flags& other) const {
        return Context(other | flags, typeFlags, missing);
    }
    constexpr Context operator|(const TypeFlags& other) const {
        return Context(flags, other | typeFlags, missing);
    }
    constexpr Context operator|(const Context& other) const {

        if (missing != other.missing) {

            auto minContext = this;

            if (missing > other.missing) {
                minContext = &other;
            }

            if (minContext->flags.contains(
                    Assumption::NoExplicitlyMissingArgs)) {
                assert(false && "Contexts are not compatible for | operator");
            }
        }

        auto newMissing = other.missing > missing ? other.missing : missing;
        return Context(other.flags | flags, other.typeFlags | typeFlags,
                       newMissing);
    }
    constexpr Context operator&(const Context& other) const {
        if (missing != other.missing) {
            auto min = missing > other.missing ? other.missing : missing;
            return Context(other.flags & flags &
                               ~Flags(Assumption::NoExplicitlyMissingArgs),
                           other.typeFlags & typeFlags, min);
        }
        return Context(other.flags & flags, other.typeFlags & typeFlags,
                       missing);
    }

    RIR_INLINE bool operator<(const Context& other) const {
        if (*this == other)
            return false;

        // Order by number of assumptions! Important for dispatching.
        if (smaller(other))
            return true;
        if (other.smaller(*this))
            return false;

        // we need a complete order, smaller is only partial
        // (more assumptions = smaller context)
        if (flags.count() != other.flags.count())
            return flags.count() > other.flags.count();
        if (typeFlags.count() != other.typeFlags.count())
            return typeFlags.count() > other.typeFlags.count();
        if (missing != other.missing)
            return missing > other.missing;
        if (flags.to_i() != other.flags.to_i())
            return flags.to_i() > other.flags.to_i();
        return typeFlags.to_i() > other.typeFlags.to_i();
    }

    RIR_INLINE bool operator!=(const Context& other) const {
        return flags != other.flags || typeFlags != other.typeFlags ||
               missing != other.missing;
    }

    RIR_INLINE bool operator==(const Context& other) const {
        return flags == other.flags && typeFlags == other.typeFlags &&
               missing == other.missing;
    }

    bool smaller(const Context& other) const {
        // argdiff positive = "more than expected", negative = "less than"
        int argdiff = (int)other.missing - (int)missing;

        if (argdiff > 0 &&
            other.flags.contains(Assumption::NotTooManyArguments))
            return false;
        if (argdiff < 0 &&
            other.flags.contains(Assumption::NoExplicitlyMissingArgs))
            return false;

        return flags.includes(other.flags) &&
               typeFlags.includes(other.typeFlags);
    }

    bool isImproving(rir::Function*) const;
    bool isImproving(rir::pir::ClosureVersion*) const;
    bool isImproving(const Context& other, bool hasDotsFormals,
                     bool hasDefaultArgs) const;

    static Context deserialize(SEXP refTable, R_inpstream_t inp);
    void serialize(SEXP refTable, R_outpstream_t out) const;

    friend struct std::hash<rir::Context>;
    friend std::ostream& operator<<(std::ostream& out, const Context& a);

    void clearExcept(const Flags& filter) {
        flags = flags & filter;
        typeFlags.reset();
        missing = 0;
    }

    void clearTypeFlags() {
        typeFlags.reset();
    }

    void clearNargs() {
        missing = 0;
        flags.reset(Assumption::NoExplicitlyMissingArgs);
    }

    void clearObjFlags() {
        for (size_t i = 0; i < NUM_TYPED_ARGS; ++i)
            resetNotObj(i);
    }

    void setSpecializationLevel(int level);

  private:
    Flags flags;
    TypeFlags typeFlags;
    uint8_t missing = 0;
    uint16_t unused2 = 0;
};
#pragma pack(pop)

typedef uint32_t Immediate;
static_assert(sizeof(Context) == 2 * sizeof(Immediate),
              "Context needs to be 2 immediate args long");

std::ostream& operator<<(std::ostream& out, Assumption a);
std::ostream& operator<<(std::ostream& out, TypeAssumption a);

} // namespace rir

namespace std {
template <>
struct hash<rir::Context> {
    std::size_t operator()(const rir::Context& v) const {
        return hash_combine(
            hash_combine(hash_combine(0, v.flags.to_i()), v.typeFlags.to_i()),
            v.missing);
    }
};
} // namespace std

#endif
