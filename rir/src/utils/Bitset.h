#ifndef RIR_BITSET_H
#define RIR_BITSET_H

#include <initializer_list>

namespace rir {

template <typename STORE, typename BASE_TYPE>
class BitSet {
    static_assert((std::size_t)BASE_TYPE::max <= (sizeof(STORE) * 8),
                  "Maximal base type does not fit in bitfield");
    STORE set_;

    STORE asSet(const BASE_TYPE& t) const { return (1 << (std::size_t)t); }

  public:
    typedef STORE Store;
    typedef BASE_TYPE Base;

    BitSet() : set_(0) {}

    BitSet(const STORE& set) : set_(set) {}

    BitSet(const std::initializer_list<BASE_TYPE>& ts) : set_(0) {
        for (auto t : ts)
            set(t);
    }

    BitSet(const BASE_TYPE& t) : set_(asSet(t)) {}

    void set(BASE_TYPE t) {
        assert(t < BASE_TYPE::max);
        set_ |= asSet(t);
    }

    void clear(BASE_TYPE t) {
        assert(t < BASE_TYPE::max);
        set_ &= ~asSet(t);
    }

    bool contains(const BASE_TYPE& t) const {
        assert(t < BASE_TYPE::max);
        return set_ & asSet(t);
    }

    bool includes(const BitSet& s) const { return (s.set_ & set_) == s.set_; }

    void operator=(const BitSet& o) { set_ = o.set_; }

    bool operator==(const BASE_TYPE& t) const { return asSet(t) == set_; }

    bool operator==(const BitSet& s) const { return set_ == s.set_; }

    BitSet operator|(const BitSet& s) const { return BitSet(s.set_ | set_); }

    BitSet operator|(const BASE_TYPE& t) const { return *this | BitSet(t); }

    std::size_t size() const { return __builtin_popcount(set_); }
};
}

#endif
