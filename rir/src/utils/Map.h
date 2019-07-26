#ifndef RIR_MAP_H
#define RIR_MAP_H

#include <cassert>
#include <functional>
#include <unordered_map>
#include <vector>

namespace rir {

template <typename K, typename V>
class SmallMap {
    typedef std::pair<K, V> Element;
    std::vector<Element> container;
    std::unordered_map<K, size_t> index;
    bool big = false;

  public:
    typedef typename std::vector<Element>::iterator iterator;
    typedef typename std::vector<Element>::const_iterator const_iterator;

    SmallMap() {}

    bool empty() const { return container.empty(); }
    size_t count(const K& k) const { return contains(k) ? 1 : 0; }

    void checkSize() {
        assert(!big);
        if (container.size() >= 8) {
            big = true;
            auto begin = container.begin();
            for (auto it = begin, end = container.end(); it != end; ++it)
                index[it->first] = it - begin;
        }
    }

    void set(const K& k, const V& v) {
        if (big) {
            auto p = index.find(k);
            if (p != index.end()) {
                container[p->second].second = v;
                return;
            }
        } else {
            for (auto& e : container) {
                if (e.first == k) {
                    e.second = v;
                    return;
                }
            }
        }
        if (!big)
            checkSize();
        if (big)
            index[k] = container.size();
        container.push_back(Element(k, v));
    }

    void insert(const K& k, const V& v) {
        SLOWASSERT(!contains(k));
        if (!big)
            checkSize();
        if (big)
            index[k] = container.size();
        container.push_back(Element(k, v));
    }

    const V& at(const K& k) const {
        if (big)
            return container[index.at(k)].second;

        for (const auto& e : container)
            if (e.first == k)
                return e.second;
        assert(false);
    }

    V& operator[](const K& k) {
        if (big) {
            auto p = index.find(k);
            if (p != index.end())
                return container[p->second].second;
        } else {
            for (auto& e : container)
                if (e.first == k)
                    return e.second;
        }

        if (!big)
            checkSize();
        if (big)
            index[k] = container.size();
        container.emplace_back(k, V());
        return container.back().second;
    }

    const V& get(const K& k, const V& otherwise) const {
        if (big) {
            auto p = index.find(k);
            if (p != index.end()) {
                return container[p->second].second;
            }
        } else {
            for (const auto& e : container)
                if (e.first == k)
                    return e.second;
        }
        return otherwise;
    }

    bool contains(const K& k) const {
        if (big)
            return index.count(k);
        for (const auto& e : container)
            if (e.first == k)
                return true;
        return false;
    }

    const_iterator find(const K& k) const {
        if (big) {
            auto idx = index.find(k);
            if (idx == index.end()) {
                return cend();
            } else {
                return cbegin() + idx->second;
            }
        }
        for (auto idx = cbegin(); idx != cend(); ++idx)
            if (idx->first == k)
                return idx;
        return cend();
    }

    void contains(const K& k, const std::function<void(V&)>& found,
                  const std::function<void()>& notFound) {
        if (big) {
            auto p = index.find(k);
            if (p == index.end())
                notFound();
            else
                found(container[p->second].second);
            return;
        }

        for (auto& e : container) {
            if (e.first == k) {
                found(e.second);
                return;
            }
        }
        notFound();
    }

    iterator begin() { return container.begin(); }
    iterator end() { return container.end(); }
    const_iterator cbegin() const { return container.cbegin(); }
    const_iterator cend() const { return container.cend(); }
    const_iterator begin() const { return container.cbegin(); }
    const_iterator end() const { return container.cend(); }
};

} // namespace rir

#endif
