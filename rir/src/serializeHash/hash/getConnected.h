//
// Created by Jakob Hain on 7/23/23.
//

#pragma once

#include "R/r_incl.h"
#include <unordered_set>
#include <queue>

namespace rir {

/// Set of RIR SEXPs connected to another SEXP
class ConnectedSet {
    std::unordered_set<SEXP> seen;

    friend ConnectedSet getConnected(SEXP root);
    friend class ConnectedCollector;
    ConnectedSet() : seen() {}
    bool insert(SEXP e) { return seen.insert(e).second; }

  public:
    using const_iterator = std::unordered_set<SEXP>::const_iterator;
    const_iterator begin() const { return seen.begin(); }
    const_iterator end() const { return seen.end(); }
};

/// Facade to add connected RIR SEXPs which is exposed to RIR objects.
class ConnectedCollector {
    /// Underlying connected set
    ConnectedSet& set;
    /// Next SEXPs to process: instead of recursing, we add nested SEXPs to this
    /// queue and then process them in a loop.
    std::queue<SEXP>& worklist;

    ConnectedCollector(ConnectedSet& set, std::queue<SEXP>& worklist)
        : set(set), worklist(worklist) {}

    friend ConnectedSet getConnected(SEXP root);

  public:
    /// Add connected objects in SEXP, which may or may not be a RIR object
    /// itself.
    void add(SEXP s) {
        if (set.insert(s)) {
            worklist.push(s);
        }
    }
    /// Add connected objects in SEXP in constant pool ([Pool])
    void addConstant(unsigned idx);
    /// Add connected objects in SEXP in source pool ([src_pool_at])
    void addSrc(unsigned idx);
    /// Add connected objects in SEXP which could be nullptr
    void addNullable(SEXP s) {
        if (s) {
            add(s);
        }
    }
};

/// Get RIR SEXPs connected to this SEXP. Used during recursive interning.
ConnectedSet getConnected(SEXP root);

} // namespace rir

