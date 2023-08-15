//
// Created by Jakob Hain on 7/23/23.
//

#pragma once

#include "R/r_incl.h"
#include "getConnected.h"
#include <queue>
#include <unordered_set>

namespace rir {

/// Facade to add connected RIR SEXPs which is exposed to RIR objects.
class ConnectedCollectorOld {
    /// Underlying connected set
    ConnectedSet& set;
    /// Next SEXPs to process: instead of recursing, we add nested SEXPs to this
    /// queue and then process them in a loop.
    std::queue<SEXP>& worklist;

    ConnectedCollectorOld(ConnectedSet& set, std::queue<SEXP>& worklist)
        : set(set), worklist(worklist) {}

    friend ConnectedSet getConnectedOld(SEXP root);

  public:
    /// Add connected objects in SEXP, which may or may not be a RIR object
    /// itself. isChild is currently unused but may be for an optimization
    /// later.
    void add(SEXP s, __attribute__((unused)) bool isChild = false) {
        if (set.insert(s)) {
            worklist.push(s);
        }
    }
    /// Add connected objects in SEXP in constant pool ([Pool])
    void addConstant(unsigned idx);
    /// Add connected objects in SEXP in source pool ([src_pool_at])
    void addSrc(unsigned idx);
    /// Add connected objects in SEXP which could be nullptr
    void addNullable(SEXP s, bool isChild = false) {
        if (s) {
            add(s, isChild);
        }
    }
};

/// Get RIR SEXPs connected to this SEXP. Used during recursive interning.
ConnectedSet getConnectedOld(SEXP root);

} // namespace rir