#ifndef RIR_BITSET_H
#define RIR_BITSET_H

#include "common.h"
#include <bitset>
#include <initializer_list>
#include <limits>

namespace rir {

#pragma pack(push)
#pragma pack(1)

template <typename Element, typename Store = unsigned long>
class EnumSet {
    Store set_ = 0;

    static_assert(static_cast<size_t>(Element::FIRST) <=
                      static_cast<size_t>(Element::LAST),
                  "EnumSet needs an enum with first and last marker.");

    static_assert(std::numeric_limits<Store>::digits >
                      static_cast<size_t>(Element::LAST),
                  "Store type is not large enough to hold all enums");

    static bool boundscheck(const Element& e) {
        return e >= Element::FIRST && e <= Element::LAST;
    }

  public:
    typedef Store StoreType;

    static constexpr EnumSet None() { return EnumSet(); }

    static constexpr Store NoneI() { return static_cast<Store>(None()); }
    static constexpr Store AnyI() { return static_cast<Store>(Any()); }

    static constexpr EnumSet Any() {
        return ((1 << ((Store)(Element::LAST) + 1)) - 1) &
               ~((1 << (Store)Element::FIRST) - 1);
    }

    constexpr EnumSet() {}
    EnumSet(const EnumSet& other) noexcept = default;

    constexpr EnumSet(Element e) : set_(1UL << static_cast<size_t>(e)) {
        static_assert(sizeof(*this) == sizeof(Store),
                      "No room for extra stuff");
    }

    static_assert(!std::is_same<Element, Store>::value, "That is confusing");
    constexpr EnumSet(const Store& s) : set_(s) {}

    RIR_INLINE Element max() const {
        for (size_t i = static_cast<size_t>(Element::LAST) - 1;
             i >= static_cast<size_t>(Element::FIRST); i--) {
            Element e = static_cast<Element>(i);
            if (contains(e))
                return e;
        }
        assert(false && "EnumSet has no max because it's empty");
    }

    RIR_INLINE bool contains(const Element& e) const {
        assert(boundscheck(e));
        return !(*this & EnumSet(e)).empty();
    }

    RIR_INLINE void set(const Element& e) {
        assert(boundscheck(e));
        set_ |= 1UL << static_cast<size_t>(e);
    }

    RIR_INLINE void reset() { set_ = 0; }

    RIR_INLINE void reset(const Element& e) {
        assert(boundscheck(e));
        set_ &= ~(1UL << static_cast<size_t>(e));
    }

    RIR_INLINE bool intersects(const EnumSet& s) const {
        return !EnumSet(s.set_ & set_).empty();
    }

    RIR_INLINE bool constexpr includes(const EnumSet& s) const {
        return (s.set_ & set_) == s.set_;
    }

    RIR_INLINE bool operator==(const Element& t) const {
        return EnumSet(t).set_ == set_;
    }

    RIR_INLINE bool operator==(const EnumSet& s) const {
        return set_ == s.set_;
    }

    constexpr RIR_INLINE EnumSet operator~() const {
        return EnumSet(~set_ & static_cast<Store>(Any()));
    }

    constexpr EnumSet operator/(const EnumSet& other) const {
        return *this & ~other;
    }

    RIR_INLINE bool operator!=(const EnumSet& s) const {
        return set_ != s.set_;
    }

    RIR_INLINE bool operator!=(const Element& t) const {
        return EnumSet(t) != set_;
    }

    constexpr RIR_INLINE EnumSet operator&(const EnumSet& s) const {
        return EnumSet(s.set_ & set_);
    }

    constexpr RIR_INLINE EnumSet operator|(const EnumSet& s) const {
        return EnumSet(s.set_ | set_);
    }

    constexpr RIR_INLINE EnumSet operator|(const Element& t) const {
        return *this | EnumSet(t);
    }

    constexpr Store to_i() const { return set_; }

    explicit constexpr operator Store() const { return set_; }

    RIR_INLINE bool empty() const { return set_ == 0; }

    RIR_INLINE std::size_t count() const { return __builtin_popcount(set_); }

    struct Iterator {
      private:
        size_t i;
        const EnumSet& e;

      public:
        Iterator(const EnumSet& e, size_t i) : i(i), e(e) {}
        Iterator(const EnumSet& e)
            : i(static_cast<size_t>(Element::FIRST)), e(e) {
            seekNext();
        }

        void seekNext() {
            while (i <= static_cast<size_t>(Element::LAST) &&
                   !((1UL << i) & e.set_))
                i++;
        }

        Element operator*() { return Element(i); }

        bool operator!=(const Iterator& other) const {
            assert(e == other.e);
            return i != other.i;
        }

        void operator++() {
            i++;
            seekNext();
        }

        Iterator operator+(size_t n) const {
            Iterator o(e, i);
            for (size_t x = 0; x < n; ++x) {
                ++o;
            }
            return o;
        }
    };
    friend struct Iterator;

    Iterator begin() const { return Iterator(*this); }

    Iterator end() const {
        return Iterator(*this, static_cast<size_t>(Element::LAST) + 1);
    }
};

#pragma pack(pop)

} // namespace rir
#endif
