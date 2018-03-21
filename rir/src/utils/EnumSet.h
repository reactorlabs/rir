#ifndef RIR_BITSET_H
#define RIR_BITSET_H

#include <bitset>
#include <initializer_list>

namespace rir {

template <typename Element>
class EnumSet {
    typedef std::bitset<static_cast<size_t>(Element::LAST) + 1> Store;
    Store set_;

    static_assert(static_cast<size_t>(Element::FIRST) <=
                      static_cast<size_t>(Element::LAST),
                  "EnumSet needs an enum with first and last marker.");

    static bool boundscheck(const Element& e) {
        return e >= Element::FIRST && e <= Element::LAST;
    }

  public:
    EnumSet() {}

    EnumSet(const std::initializer_list<Element>& ts) {
        for (auto t : ts)
            set(t);
    }
    EnumSet(const Element& e) { set(e); }
    EnumSet(const Store& s) : set_(s) {}

    bool contains(const Element& e) const {
        assert(boundscheck(e));
        return set_ & singleton(e);
    }

    void set(const Element& e) {
        assert(boundscheck(e));
        set_.set(static_cast<size_t>(e));
    }

    void reset(const Element& e) {
        assert(boundscheck(e));
        set_.reset(static_cast<size_t>(e));
    }

    bool includes(const EnumSet& s) const { return (s.set_ & set_) == s.set_; }

    bool operator==(const Element& t) const { return EnumSet(t) == set_; }

    bool operator==(const EnumSet& s) const { return set_ == s.set_; }

    EnumSet operator|(const EnumSet& s) const { return EnumSet(s.set_ | set_); }

    EnumSet operator|(const Element& t) const { return *this | EnumSet(t); }

    bool empty() const { return set_.none(); }

    std::size_t count() const { return set_.count(); }

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
            while (i <= static_cast<size_t>(Element::LAST) && !e.set_.test(i))
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
}

#endif
