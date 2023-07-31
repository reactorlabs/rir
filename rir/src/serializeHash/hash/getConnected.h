//
// Created by Jakob Hain on 7/23/23.
//

#pragma once

#include "R/r_incl.h"
#include <unordered_set>
#include <queue>

namespace rir {

struct ConnectedElem {
    SEXP sexp;
    bool isChild;

    bool operator==(const ConnectedElem& other) const {
        return sexp == other.sexp && isChild == other.isChild;
    }
    bool operator!=(const ConnectedElem& other) const {
        return sexp != other.sexp || isChild != other.isChild;
    }
};

} // namespace rir

namespace std {
template <>
struct hash<rir::ConnectedElem> {
    size_t operator()(const rir::ConnectedElem& e) const {
        return hash<SEXP>()(e.sexp) ^ hash<bool>()(e.isChild);
    }
};
} // namespace std

namespace rir {

/// Set of RIR SEXPs connected to another SEXP
class ConnectedSet {
    std::unordered_set<ConnectedElem> seen;

    friend ConnectedSet getConnected(SEXP root);
    friend class ConnectedCollector;
    ConnectedSet() : seen() {}
    bool insert(SEXP e, bool isChild) { return seen.insert({e, isChild}).second; }

  public:
    using const_iterator = std::unordered_set<ConnectedElem>::const_iterator;
    const_iterator begin() const { return seen.begin(); }
    const_iterator end() const { return seen.end(); }
};

/// Facade to add connected RIR SEXPs which is exposed to RIR objects.
class ConnectedCollector {
    /// Underlying connected set
    ConnectedSet& set;
    /// Next SEXPs to process: instead of recursing, we add nested SEXPs to this
    /// queue and then process them in a loop.
    std::queue<ConnectedElem>& worklist;

    ConnectedCollector(ConnectedSet& set, std::queue<ConnectedElem>& worklist)
        : set(set), worklist(worklist) {}

    friend ConnectedSet getConnected(SEXP root);

  public:
    /// Add connected objects in SEXP, which may or may not be a RIR object
    /// itself.
    void add(SEXP s, bool isChild) {
        if (set.insert(s, isChild)) {
            worklist.push({s, isChild});
        }
    }
    /// Add connected objects in SEXP in constant pool ([Pool])
    void addConstant(unsigned idx);
    /// Add connected objects in SEXP in source pool ([src_pool_at])
    void addSrc(unsigned idx);
    /// Add connected objects in SEXP which could be nullptr
    void addNullable(SEXP s, bool isChild) {
        if (s) {
            add(s, isChild);
        }
    }
};

/// Get RIR SEXPs connected to this SEXP. Used during recursive interning.
ConnectedSet getConnected(SEXP root);

} // namespace rir