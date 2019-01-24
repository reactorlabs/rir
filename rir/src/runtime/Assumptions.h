#ifndef PIR_ASSUMPTIONS_H
#define PIR_ASSUMPTIONS_H

#include "utils/EnumSet.h"

#include <array>
#include <cstring>
#include <iostream>

namespace rir {

enum class Assumption {
    EagerArgs_,

    Arg1IsEager_,
    Arg2IsEager_,
    Arg3IsEager_,

    NonObjectArgs_,

    Arg1IsNonObj_,
    Arg2IsNonObj_,
    Arg3IsNonObj_,

    NoMissingArguments,
    NotTooManyArguments,
    CorrectOrderOfArguments,

    FIRST = EagerArgs_,
    LAST = CorrectOrderOfArguments
};

#pragma pack(push)
#pragma pack(1)
struct Assumptions {
    typedef EnumSet<Assumption, uint16_t> Flags;

    Assumptions() {}
    Assumptions(const Flags& flags, uint8_t missing)
        : flags(flags), missing(missing) {}
    explicit Assumptions(uint32_t i) { memcpy(this, &i, sizeof(i)); }
    Assumptions(std::initializer_list<Assumption> assumptions)
        : flags(assumptions) {}

    constexpr static size_t MAX_MISSING = 8;

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

    bool isMissing(size_t arg) const {
        assert(arg < MAX_MISSING);
        return missing & (1UL << arg);
    }
    void setMissing(size_t arg) { missing |= (1UL << arg); }

    bool empty() const { return flags.empty() && missing == 0; }

    size_t count() const { return flags.count(); }

    Assumptions operator|(const Assumptions& other) const {
        assert(missing == other.missing);
        return Assumptions(other.flags | flags, missing);
    }

    bool operator<(const Assumptions& other) const {
        // Order by number of assumptions! Important for dispatching.
        if (missing != other.missing)
            return missing < other.missing;
        if (flags.count() != other.flags.count())
            return flags.count() < other.flags.count();
        return flags < other.flags;
    }

    bool operator!=(const Assumptions& other) const {
        return flags != other.flags || missing != other.missing;
    }

    bool operator==(const Assumptions& other) const {
        return flags == other.flags && missing == other.missing;
    }

    bool subtype(const Assumptions& other) const {
        return missing == other.missing && other.flags.includes(flags);
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
