#include "phi_placement.h"
#include "../analysis/generic_static_analysis.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"

#include "utils/Set.h"

namespace rir {
namespace pir {

PhiPlacement::Phis
PhiPlacement::compute(ClosureVersion* cls,
                      const std::unordered_map<BB*, Value*>& writes,
                      const CFG& cfg, const DominanceGraph& dom,
                      const DominanceFrontier& dfrontier) {
    SmallSet<BB*> phis;
    SmallSet<BB*> defs;
    for (const auto& w : writes)
        defs.insert(w.first);

    // Textbook phi placement
    // see page 31 http://ssabook.gforge.inria.fr/latest/book.pdf
    while (!defs.empty()) {
        auto xi = defs.end() - 1;
        auto x = *xi;
        defs.erase(xi);
        auto df = dfrontier.at(x);
        for (auto y : df) {
            if (!phis.includes(y)) {
                phis.insert(y);
                if (!defs.includes(y)) {
                    defs.insert(y);
                }
            }
        }
    }

    // Depth first traversal to connect inputs with phis
    Phis computed;
    std::vector<bool> seen(cls->nextBBId, false);
    std::function<void(BB*, PhiInput)> computePhis = [&](BB* cur,
                                                         PhiInput input) {
        if (!cur)
            return;

        if (phis.includes(cur)) {
            if (input.aValue || input.otherPhi) {
                computed[cur].insert(input);
            }
        }

        if (seen[cur->id])
            return;
        seen[cur->id] = true;

        if (writes.count(cur)) {
            input.otherPhi = nullptr;
            input.aValue = writes.at(cur);
        } else if (phis.includes(cur)) {
            input.aValue = nullptr;
            input.otherPhi = cur;
        }
        input.inputBlock = cur;
        computePhis(cur->next0, input);
        computePhis(cur->next1, input);
    };
    computePhis(cls->entry, {});

    // Cleanup the resulting phi graph
    bool changed = true;
    auto cleanup = [&]() {
        for (auto ci = computed.begin(); ci != computed.end();) {
            // Remove dead inputs
            auto& inputs = ci->second;
            for (auto ii = inputs.begin(); ii != inputs.end();) {
                if (ii->otherPhi && !computed.count(ii->otherPhi)) {
                    ii = inputs.erase(ii);
                    changed = true;
                } else {
                    ii++;
                }
            }
            if (ci->second.size() == 0) {
                ci = computed.erase(ci);
            } else if (ci->second.size() == 1) {
                // Remove single input phis
                auto input1 = *ci->second.begin();
                // update all other phis which have us as input
                for (auto& c : computed) {
                    for (auto in : c.second) {
                        if (in.otherPhi == ci->first) {
                            in.otherPhi = input1.otherPhi;
                            in.aValue = input1.aValue;
                            changed = true;
                        }
                    }
                }
                ci = computed.erase(ci);
            } else {
                ci++;
            }
        }
    };
    while (changed) {
        changed = false;
        cleanup();
    }

    // Fail if not all phis are well formed
    for (auto& i : computed) {
        if (i.second.size() != cfg.immediatePredecessors(i.first).size())
            return {};
    }

    return computed;
}

} // namespace pir
} // namespace rir
