//
// Created by Jakob Hain on 8/15/23.
//

#pragma once

#include "R/r_incl.h"
#include <unordered_set>

namespace rir {

/// Set of RIR SEXPs connected to another SEXP
class ConnectedSet {
    std::unordered_set<SEXP> seen;

    friend ConnectedSet getConnectedOld(SEXP root);
    friend ConnectedSet getConnectedUni(SEXP root);
    friend ConnectedSet getConnected(SEXP root);
    friend class ConnectedCollectorOld;
    friend class ConnectedCollectorUni;
    ConnectedSet() : seen() {}
    bool insert(SEXP e) { return seen.insert(e).second; }

  public:
    using const_iterator = std::unordered_set<SEXP>::const_iterator;
    const_iterator begin() const { return seen.begin(); }
    const_iterator end() const { return seen.end(); }
};

/// Get RIR SEXPs connected to this SEXP. Used during recursive interning.
ConnectedSet getConnected(SEXP root);

} // namespace rir
