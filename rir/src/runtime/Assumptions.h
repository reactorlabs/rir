#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <R/r.h>
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
    Arg6IsEager_,
    Arg7IsEager_,

    // Arg is not an object
    Arg0IsNotObj_,
    Arg1IsNotObj_,
    Arg2IsNotObj_,
    Arg3IsNotObj_,
    Arg4IsNotObj_,
    Arg5IsNotObj_,
    Arg6IsNotObj_,
    Arg7IsNotObj_,

    // Arg is simple integer scalar
    Arg0IsSimpleInt_,
    Arg1IsSimpleInt_,
    Arg2IsSimpleInt_,
    Arg3IsSimpleInt_,
    Arg4IsSimpleInt_,
    Arg5IsSimpleInt_,
    Arg6IsSimpleInt_,
    Arg7IsSimpleInt_,

    // Arg is simple real scalar
    Arg0IsSimpleReal_,
    Arg1IsSimpleReal_,
    Arg2IsSimpleReal_,
    Arg3IsSimpleReal_,
    Arg4IsSimpleReal_,
    Arg5IsSimpleReal_,
    Arg6IsSimpleReal_,
    Arg7IsSimpleReal_,

    FIRST = Arg0IsEager_,
    LAST = Arg7IsSimpleReal_,
};

enum class Assumption {
    NoExplicitlyMissingArgs, // Explicitly missing, e.g. f(,,)
    CorrectOrderOfArguments, // Ie. the args are not named
    NotTooManyArguments,     // The number of args supplied is <= nargs
    NoReflectiveArgument,    // Argument promises are not reflective
    StaticallyArgmatched,    // Arguments are statically matched

    FIRST = NoExplicitlyMissingArgs,
    LAST = StaticallyArgmatched,
};

#pragma pack(push)
#pragma pack(1)
struct Assumptions {
    typedef EnumSet<TypeAssumption, uint32_t> TypeFlags;
    typedef EnumSet<Assumption, uint8_t> Flags;

    constexpr static size_t MAX_MISSING = 255;
    // # of args with type assumptions
    constexpr static size_t NUM_TYPED_ARGS = 8;
    constexpr static size_t NUM_TYPED_ARGS_SPECULATE = 8;

    Assumptions() = default;
    Assumptions(const Assumptions&) noexcept = default;

    explicit constexpr Assumptions(const Flags& flags) : flags(flags) {}
    constexpr Assumptions(const Flags& flags, uint8_t missing)
        : flags(flags), missing(missing) {}
    constexpr Assumptions(const Flags& flags, const TypeFlags& typeFlags,
                          uint8_t missing)
        : flags(flags), typeFlags(typeFlags), missing(missing) {}
    explicit Assumptions(void* pos) {
        memcpy((void*)this, pos, sizeof(*this));
    }
    explicit Assumptions(unsigned long val) {
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
        Type##Assumptions = {                                                  \
            {TypeAssumption::Arg0Is##Type##_, TypeAssumption::Arg1Is##Type##_, \
             TypeAssumption::Arg2Is##Type##_, TypeAssumption::Arg3Is##Type##_, \
             TypeAssumption::Arg4Is##Type##_, TypeAssumption::Arg5Is##Type##_, \
             TypeAssumption::Arg6Is##Type##_,                                  \
             TypeAssumption::Arg7Is##Type##_}};                                \
    RIR_INLINE bool is##Type(size_t i) const {                                 \
        if (i < NUM_TYPED_ARGS_SPECULATE)                                      \
            if (typeFlags.includes(Type##Assumptions[i]))                      \
                return true;                                                   \
        return false;                                                          \
    }                                                                          \
    RIR_INLINE void set##Type(size_t i) {                                      \
        if (i < NUM_TYPED_ARGS_SPECULATE)                                      \
            typeFlags.set(Type##Assumptions[i]);                               \
    }
    TYPE_ASSUMPTIONS(Eager);
    TYPE_ASSUMPTIONS(NotObj);
    TYPE_ASSUMPTIONS(SimpleInt);
    TYPE_ASSUMPTIONS(SimpleReal);
#undef TYPE_ASSUMPTIONS

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

    constexpr Assumptions operator|(const Flags& other) const {
        return Assumptions(other | flags, typeFlags, missing);
    }
    constexpr Assumptions operator|(const TypeFlags& other) const {
        return Assumptions(flags, other | typeFlags, missing);
    }
    constexpr Assumptions operator|(const Assumptions& other) const {
        assert(missing == other.missing);
        return Assumptions(other.flags | flags, other.typeFlags | typeFlags,
                           missing);
    }

    RIR_INLINE bool operator<(const Assumptions& other) const {
        // Order by number of assumptions! Important for dispatching.
        if (*this != other) {
            if (subtype(other))
                return true;
            if (other.subtype(*this))
                return false;
        }

        // we need a complete order, subtype is only partial
        if (missing != other.missing)
            return missing < other.missing;
        if (flags.count() != other.flags.count())
            return flags.count() < other.flags.count();
        if (typeFlags.count() != other.typeFlags.count())
            return typeFlags.count() < other.typeFlags.count();
        if (flags.to_i() != other.flags.to_i())
            return flags.to_i() < other.flags.to_i();
        return typeFlags.to_i() < other.typeFlags.to_i();
    }

    RIR_INLINE bool operator!=(const Assumptions& other) const {
        return flags != other.flags || typeFlags != other.typeFlags ||
               missing != other.missing;
    }

    RIR_INLINE bool operator==(const Assumptions& other) const {
        return flags == other.flags && typeFlags == other.typeFlags &&
               missing == other.missing;
    }

    RIR_INLINE bool subtype(const Assumptions& other) const {
        return missing == other.missing && other.flags.includes(flags) &&
               other.typeFlags.includes(typeFlags);
    }

    static Assumptions deserialize(SEXP refTable, R_inpstream_t inp);
    void serialize(SEXP refTable, R_outpstream_t out) const;

    friend struct std::hash<rir::Assumptions>;
    friend std::ostream& operator<<(std::ostream& out, const Assumptions& a);

    void clearExcept(const Flags& filter) {
        flags = flags & filter;
        typeFlags.reset();
        missing = 0;
    }

    void clearTypeFlags() {
        typeFlags.reset();
        flags.reset(Assumption::NoReflectiveArgument);
    }

    void clearNargs() {
        missing = 0;
        flags.reset(Assumption::NoExplicitlyMissingArgs);
    }

  private:
    Flags flags;
    TypeFlags typeFlags;
    uint8_t missing = 0;
    uint16_t unused2 = 0;
};
#pragma pack(pop)

typedef uint32_t Immediate;
static_assert(sizeof(Assumptions) == 2 * sizeof(Immediate),
              "Assumptions needs to be 2 immediate args long");

std::ostream& operator<<(std::ostream& out, Assumption a);
std::ostream& operator<<(std::ostream& out, TypeAssumption a);

} // namespace rir

namespace std {
template <>
struct hash<rir::Assumptions> {
    std::size_t operator()(const rir::Assumptions& v) const {
        return hash_combine(
            hash_combine(hash_combine(0, v.flags.to_i()), v.typeFlags.to_i()),
            v.missing);
    }
};
} // namespace std

#endif
