//
// Created by Jakob Hain on 10/9/23.
//

#pragma once

#include <unordered_map>

/// Bimap of `std::vector<T>` and `std::unordered_map<T, size_t>`.
/// The vector can have multiple copies of the same item, and the map will map
/// to the last occurrence.
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
    const T& at(size_t i) const {
        assert(i < ltr_.size() && "BimapVector index out of bounds");
        return ltr_.at(i);
    }
    size_t at(const T& t) const {
        assert(rtl_.count(t) && "BimapVector does not contain this element");
        return rtl_.at(t);
    }

    void push_back(const T& t) {
        ltr_.push_back(t);
        rtl_[t] = ltr_.size() - 1;
    }

    bool operator==(const BimapVector<T>& other) const {
        return ltr_ == other.ltr_;
    }
};
