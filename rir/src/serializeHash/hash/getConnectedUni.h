//
// Created by Jakob Hain on 7/23/23.
//

#pragma once

#include "R/r_incl.h"
#include "getConnected.h"
#include "serializeHash/serializeUni.h"
#include <queue>
#include <unordered_set>

namespace rir {

/// Facade to add connected RIR SEXPs which is exposed to RIR objects.
class ConnectedCollectorUni : AbstractSerializer {
    /// Underlying connected set
    ConnectedSet& set;
    /// Next SEXPs to process: instead of recursing, we add nested SEXPs to this
    /// queue and then process them in a loop.
    std::queue<SEXP> worklist;

    explicit ConnectedCollectorUni(ConnectedSet& set)
        : set(set), worklist() {}

    SerializedRefs* refs() override { return nullptr; }
    void doGetConnected(SEXP root);
    friend ConnectedSet getConnectedUni(SEXP root);
  public:
    SerialOptions& serialOptions() const override;
    bool willWrite(const SerialFlags& flags) const override;
    void writeBytes(const void *data, size_t size,
                    const SerialFlags& flags) override {}
    void writeInt(int data, const SerialFlags& flags) override {}
    void write(SEXP s, const SerialFlags& flags) override;
};

/// Get RIR SEXPs connected to this SEXP. Used during recursive interning.
ConnectedSet getConnectedUni(SEXP root);

} // namespace rir