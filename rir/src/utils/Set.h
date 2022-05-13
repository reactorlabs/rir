#ifndef RIR_SET_H
#define RIR_SET_H

#include <algorithm>
#include <vector>

namespace rir {

template <typename T>
class SmallSet {
    std::vector<T> container;

  public:
    typedef typename std::vector<T>::iterator iterator;
    typedef typename std::vector<T>::const_iterator const_iterator;

    SmallSet() {}
    explicit SmallSet(std::initializer_list<T> in) : container(in) {}

    void insert(T t) {
        for (const auto& e : container)
            if (e == t)
                return;
        container.push_back(t);
    }

    void clear() { container.clear(); }

    size_t count(const T t) const { return includes(t) ? 1 : 0; }

    bool includes(const T t) const {
        for (const auto& e : container)
            if (e == t)
                return true;
        return false;
    }

    bool empty() const { return container.empty(); }

    bool operator!=(const SmallSet& other) const { return !(*this == other); }

    bool operator==(const SmallSet& other) const {
        if (container.size() != other.size())
            return false;

        for (const auto& e1 : container) {
            for (const auto& e2 : other.container) {
                if (e1 == e2)
                    goto op_equal_found_element;
            }
            return false;
        op_equal_found_element : {}
        }
        return true;
    }

    size_t size() const { return container.size(); }

    iterator find(const T& t) {
        return std::find(container.begin(), container.end(), t);
    }

    iterator erase(iterator e) { return container.erase(e); }
    void erase(T e) { erase(find(e)); }
    const_iterator erase(const_iterator e) { return container.erase(e); }

    iterator begin() { return container.begin(); }
    iterator end() { return container.end(); }
    const_iterator cbegin() const { return container.cbegin(); }
    const_iterator cend() const { return container.cend(); }
    const_iterator begin() const { return container.cbegin(); }
    const_iterator end() const { return container.cend(); }
};

} // namespace rir

#endif
