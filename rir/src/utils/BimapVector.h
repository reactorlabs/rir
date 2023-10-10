//
// Created by Jakob Hain on 10/9/23.
//

#pragma once

#include <unordered_map>

/// Bimap of std::vector<T> and std::unordered_map<T, size_t>
template<typename T> class BimapVector {
    std::vector<T> ltr_;
    std::unordered_map<T, size_t> rtl_;

  public:
    BimapVector() = default;
    explicit BimapVector(std::vector<T> ltr_) : ltr_(ltr_) {
        for (size_t i = 0; i < ltr_.size(); i++) {
            rtl_[ltr_[i]] = i;
        }
    }

    const std::vector<T>& ltr() const { return ltr_; }
    const std::unordered_map<T, size_t>& rtl() const { return rtl_; }

    size_t size() const { return ltr_.size(); }
    bool empty() const { return ltr_.empty(); }
    bool count(const T& t) const { return rtl_.count(t); }
    const T& operator[](size_t i) const { return ltr_[i]; }
    size_t operator[](const T& t) const {
        assert(rtl_.count(t) && "BimapVector does not contain this element");
        return rtl_.at(t);
    }

    void push_back(const T& t) {
        assert(rtl_.count(t) == 0 && "BimapVector already contains this element");
        ltr_.push_back(t);
        rtl_[t] = ltr_.size() - 1;
    }

    bool operator==(const BimapVector<T>& other) const {
        return ltr_ == other.ltr_;
    }
};
