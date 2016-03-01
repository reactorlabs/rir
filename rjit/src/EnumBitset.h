#ifndef ENUM_BITSET
#define ENUM_BITSET

#include <type_traits>

namespace rjit {

template <typename Enum>
class EnumBitset {

    typedef typename std::underlying_type<Enum>::type BASE;

    BASE set = 0;

    BASE toI(Enum e) const {
        BASE i = static_cast<BASE>(e);
        return 1 << i;
    }

  public:
    operator BASE() { return set; }

    EnumBitset() : set(0) {}

    EnumBitset(BASE init) : set(init) {}

    EnumBitset(Enum init) : set(toI(init)) {}

    void clear() { set = 0; }

    bool empty() const { return set == 0; }

    bool has(Enum e) const { return (set & toI(e)) != 0; }

    void insert(Enum e) { set |= toI(e); }

    void insert(const EnumBitset<Enum>& o) { set |= o.set; }

    bool operator==(const EnumBitset<Enum>& o) const { return set == o.set; }

    bool operator!=(EnumBitset<Enum> const& o) const { return set != o.set; }

    EnumBitset operator|(const EnumBitset<Enum>& o) const {
        return set | o.set;
    }

    EnumBitset& operator=(const EnumBitset<Enum>& o) {
        set = o.set;
        return *this;
    }

    EnumBitset& operator|=(const EnumBitset<Enum>& o) {
        set |= o.set;
        return *this;
    }
};
}

#endif
