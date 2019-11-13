#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <R/r.h>
#include <array>
#include <cstring>
#include <iostream>

namespace rir {

enum class Assumption {
    // Arg is already evaluated
    Arg0IsEager_,
    Arg1IsEager_,
    Arg2IsEager_,

    // Arg is not an object
    Arg0IsNotObj_,
    Arg1IsNotObj_,
    Arg2IsNotObj_,

    // Arg is simple integer scalar
    Arg0IsSimpleInt_,
    Arg1IsSimpleInt_,
    Arg2IsSimpleInt_,

    // Arg is simple real scalar
    Arg0IsSimpleReal_,
    Arg1IsSimpleReal_,
    Arg2IsSimpleReal_,

    NoExplicitlyMissingArgs, // Explicitly missing, e.g. f(,,)
    CorrectOrderOfArguments, // Ie. the args are not named
    NotTooManyArguments,     // The number of args supplied is <= nargs
    NoReflectiveArgument,    // Argument promises are not reflective
    StaticallyArgmatched,    // Arguments are statically matched

    FIRST = Arg0IsEager_,
    LAST = StaticallyArgmatched,
};

#pragma pack(push)
#pragma pack(1)
struct Assumptions {
    typedef EnumSet<Assumption, uint32_t> Flags;

    constexpr static size_t MAX_MISSING = 255;
    // # of args with type assumptions
    constexpr static size_t NUM_ARGS = 3;

    Assumptions() = default;
    Assumptions(const Assumptions&) noexcept = default;

    explicit constexpr Assumptions(const Flags& flags) : flags(flags) {}
    constexpr Assumptions(const Flags& flags, uint8_t missing)
        : flags(flags), missing(missing) {}
    explicit Assumptions(void* pos) {
        // Silences unused warning:
        (void)unused;
        (void)unused2;
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
    static constexpr std::array<Assumption, NUM_ARGS> Type##Assumptions = {    \
        {Assumption::Arg0Is##Type##_, Assumption::Arg1Is##Type##_,             \
         Assumption::Arg2Is##Type##_}};                                        \
    RIR_INLINE bool is##Type(size_t i) const {                                 \
        if (i < Type##Assumptions.size())                                      \
            if (flags.includes(Type##Assumptions[i]))                          \
                return true;                                                   \
        return false;                                                          \
    }                                                                          \
    RIR_INLINE void set##Type(size_t i) {                                      \
        if (i < Type##Assumptions.size())                                      \
            flags.set(Type##Assumptions[i]);                                   \
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

    RIR_INLINE bool empty() const { return flags.empty() && missing == 0; }

    RIR_INLINE size_t count() const { return flags.count(); }

    constexpr Assumptions operator|(const Flags& other) const {
        return Assumptions(flags | other, missing);
    }

    constexpr Assumptions operator|(const Assumptions& other) const {
        assert(missing == other.missing);
        return Assumptions(other.flags | flags, missing);
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
        return flags.to_i() < other.flags.to_i();
    }

    RIR_INLINE bool operator!=(const Assumptions& other) const {
        return flags != other.flags || missing != other.missing;
    }

    RIR_INLINE bool operator==(const Assumptions& other) const {
        return flags == other.flags && missing == other.missing;
    }

    RIR_INLINE bool subtype(const Assumptions& other) const {
        return missing == other.missing && other.flags.includes(flags);
    }

    static Assumptions deserialize(SEXP refTable, R_inpstream_t inp);
    void serialize(SEXP refTable, R_outpstream_t out) const;

    friend struct std::hash<rir::Assumptions>;
    friend std::ostream& operator<<(std::ostream& out, const Assumptions& a);

  private:
    Flags flags;
    uint8_t missing = 0;

    uint8_t unused = 0;
    uint16_t unused2 = 0;
};
#pragma pack(pop)

typedef uint32_t Immediate;
static_assert(sizeof(Assumptions) == 2 * sizeof(Immediate),
              "Assumptions needs to be 2 immediate args long");

std::ostream& operator<<(std::ostream& out, Assumption a);

} // namespace rir

namespace std {
template <>
struct hash<rir::Assumptions> {
    std::size_t operator()(const rir::Assumptions& v) const {
        return hash_combine(hash_combine(0, v.flags.to_i()), v.missing);
    }
};
} // namespace std

#endif
