#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <array>
#include <cstring>
#include <iostream>

namespace rir {

enum class Assumption {
    EagerArgs_, // All arguments are already evaluated

    Arg1IsEager_, // Arg1 is already evaluated
    Arg2IsEager_, // Arg2 is already evaluated
    Arg3IsEager_, // Arg3 is already evaluated

    NonObjectArgs_, // All arguments are not objects

    Arg1IsNonObj_, // Arg1 is not an object
    Arg2IsNonObj_, // Arg2 is not an object
    Arg3IsNonObj_, // Arg3 is not an object

    NoExplicitlyMissingArgs, // Explicitly missing, e.g. f(,,)
    CorrectOrderOfArguments, // Ie. the args are not named
    NotTooFewArguments,      // The number of args supplied is as expected, ie.
                             //  supplied >= (nargs - missing)
                             //  Note: can still have explicitly missing args
    NotTooManyArguments,     // The number of args supplied is <= nargs

    FIRST = EagerArgs_,
    LAST = NotTooManyArguments
};

#pragma pack(push)
#pragma pack(1)
struct Assumptions {
    typedef EnumSet<Assumption, uint16_t> Flags;

    Assumptions() {}
    explicit Assumptions(const Flags& flags) : flags(flags) {}
    Assumptions(const Flags& flags, uint8_t missing)
        : flags(flags), missing(missing) {}
    explicit Assumptions(uint32_t i) { memcpy(this, &i, sizeof(i)); }
    Assumptions(std::initializer_list<Assumption> assumptions)
        : flags(assumptions) {}

    constexpr static size_t MAX_MISSING = 255;

  private:
    Flags flags;
    uint8_t missing = 0;
    uint8_t unused = 0;

  public:
    void add(Assumption a) { flags.set(a); }
    void remove(Assumption a) { flags.reset(a); }
    bool includes(Assumption a) const { return flags.includes(a); }

    bool isEager(size_t i) const;
    void setEager(size_t i, bool);
    bool notObj(size_t i) const;
    void setNotObj(size_t i, bool);

    uint8_t numMissing() const { return missing; }

    void numMissing(long i) {
        assert(i < 255);
        missing = i;
    }

    bool empty() const { return flags.empty() && missing == 0; }

    size_t count() const { return flags.count(); }

    Assumptions operator|(const Assumptions& other) const {
        assert(missing == other.missing);
        return Assumptions(other.flags | flags, missing);
    }

    bool operator<(const Assumptions& other) const {
        // Order by number of assumptions! Important for dispatching.
        if (missing != other.missing)
            return missing > other.missing;
        if (flags.count() != other.flags.count())
            return flags.count() > other.flags.count();
        return flags < other.flags;
    }

    bool operator!=(const Assumptions& other) const {
        return flags != other.flags || missing != other.missing;
    }

    bool operator==(const Assumptions& other) const {
        return flags == other.flags && missing == other.missing;
    }

    bool subtype(const Assumptions& other) const {
        if (other.flags.includes(Assumption::NotTooFewArguments))
            return missing == other.missing && other.flags.includes(flags);
        return other.flags.includes(flags);
    }

    friend struct std::hash<rir::Assumptions>;
    friend std::ostream& operator<<(std::ostream& out, const Assumptions& a);
};
#pragma pack(pop)

typedef uint32_t Immediate;
static_assert(sizeof(Assumptions) == sizeof(Immediate),
              "Assumptions needs to be one immediate arg size");

std::ostream& operator<<(std::ostream& out, Assumption a);

} // namespace rir

namespace std {
template <>
struct hash<rir::Assumptions> {
    std::size_t operator()(const rir::Assumptions& v) const {
        using std::hash;
        return hash<unsigned long long>()(v.flags.to_i()) ^
               hash<unsigned long long>()(v.missing);
    }
};
} // namespace std

#endif
