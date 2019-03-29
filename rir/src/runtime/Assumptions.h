#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <array>
#include <cstring>
#include <iostream>

namespace rir {

enum class Assumption {
    // Arg is already evaluated
    Arg0IsEager_,
    Arg1IsEager_,
    Arg2IsEager_,
    Arg3IsEager_,
    Arg4IsEager_,

    // Arg is not an object
    Arg0IsNonObj_,
    Arg1IsNonObj_,
    Arg2IsNonObj_,
    Arg3IsNonObj_,

    NoExplicitlyMissingArgs, // Explicitly missing, e.g. f(,,)
    CorrectOrderOfArguments, // Ie. the args are not named
    NotTooFewArguments,      // The number of args supplied is as expected, ie.
                             //  supplied >= (nargs - missing)
                             //  Note: can still have explicitly missing args
    NotTooManyArguments,     // The number of args supplied is <= nargs

    FIRST = Arg0IsEager_,
    LAST = NotTooManyArguments
};

#pragma pack(push)
#pragma pack(1)
struct Assumptions {
    typedef EnumSet<Assumption, uint16_t> Flags;

    Assumptions() = default;
    Assumptions(const Assumptions&) noexcept = default;

    explicit constexpr Assumptions(const Flags& flags) : flags(flags) {}
    constexpr Assumptions(const Flags& flags, uint8_t missing)
        : flags(flags), missing(missing) {}
    explicit Assumptions(uint32_t i) {
        // Silences unused warning:
        (void)unused;
        memcpy(this, &i, sizeof(i));
    }

    constexpr static size_t MAX_MISSING = 255;

    RIR_INLINE void add(Assumption a) { flags.set(a); }
    RIR_INLINE void remove(Assumption a) { flags.reset(a); }
    RIR_INLINE bool includes(Assumption a) const { return flags.includes(a); }
    RIR_INLINE bool includes(const Flags& a) const { return flags.includes(a); }

    RIR_INLINE bool isEager(size_t i) const;
    RIR_INLINE void setEager(size_t i);
    RIR_INLINE void setEager();
    RIR_INLINE bool notObj(size_t i) const;
    RIR_INLINE void setNotObj(size_t i);
    RIR_INLINE void setNotObj();

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
        if (missing != other.missing)
            return missing > other.missing;
        if (flags.count() != other.flags.count())
            return flags.count() > other.flags.count();
        return flags < other.flags;
    }

    RIR_INLINE bool operator!=(const Assumptions& other) const {
        return flags != other.flags || missing != other.missing;
    }

    RIR_INLINE bool operator==(const Assumptions& other) const {
        return flags == other.flags && missing == other.missing;
    }

    RIR_INLINE bool subtype(const Assumptions& other) const {
        if (other.flags.includes(Assumption::NotTooFewArguments))
            return missing == other.missing && other.flags.includes(flags);
        return other.flags.includes(flags);
    }

    friend struct std::hash<rir::Assumptions>;
    friend std::ostream& operator<<(std::ostream& out, const Assumptions& a);

    static constexpr std::array<Assumption, 5> ObjAssumptions = {
        {Assumption::Arg0IsNonObj_, Assumption::Arg1IsNonObj_,
         Assumption::Arg2IsNonObj_, Assumption::Arg3IsEager_,
         Assumption::Arg4IsEager_}};
    static constexpr std::array<Assumption, 4> EagerAssumptions = {
        {Assumption::Arg0IsEager_, Assumption::Arg1IsEager_,
         Assumption::Arg2IsEager_, Assumption::Arg3IsNonObj_}};

  private:
    Flags flags;
    uint8_t missing = 0;
    uint8_t unused = 0;
};
#pragma pack(pop)

RIR_INLINE bool Assumptions::isEager(size_t i) const {
    if (i < EagerAssumptions.size())
        if (flags.includes(EagerAssumptions[i]))
            return true;

    return false;
}

RIR_INLINE void Assumptions::setEager() {
    for (size_t i = 0; i < EagerAssumptions.size(); ++i)
        flags.set(EagerAssumptions[i]);
}

RIR_INLINE void Assumptions::setEager(size_t i) {
    if (i < EagerAssumptions.size()) {
        flags.set(EagerAssumptions[i]);
    }
}

RIR_INLINE bool Assumptions::notObj(size_t i) const {
    if (i < ObjAssumptions.size())
        if (flags.includes(ObjAssumptions[i]))
            return true;

    return false;
}

RIR_INLINE void Assumptions::setNotObj() {
    for (size_t i = 0; i < ObjAssumptions.size(); ++i)
        flags.set(ObjAssumptions[i]);
}

RIR_INLINE void Assumptions::setNotObj(size_t i) {
    if (i < ObjAssumptions.size()) {
        flags.set(ObjAssumptions[i]);
    }
}

typedef uint32_t Immediate;
static_assert(sizeof(Assumptions) == sizeof(Immediate),
              "Assumptions needs to be one immediate arg size");

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
