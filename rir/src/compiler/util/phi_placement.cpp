#include "phi_placement.h"
#include "../analysis/generic_static_analysis.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"

#include "utils/Set.h"

namespace rir {
namespace pir {

PhiPlacement::PhiPlacement(ClosureVersion* cls, BB* target,
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

    {
        std::unordered_map<BB*, PhiInput> pendingInput;
        DominatorTreeVisitor<>(dom).run(cls->entry, [&](BB* cur) {
            PhiInput input = {nullptr, nullptr, nullptr};
            if (pendingInput.count(cur))
                input = pendingInput.at(cur);

            if (phis.includes(cur)) {
                input.aValue = nullptr;
                input.otherPhi = cur;
            }

            if (writes.count(cur)) {
                input.otherPhi = nullptr;
                input.aValue = writes.at(cur);
            }
            input.inputBlock = cur;

            auto apply = [&](BB* next) {
                if (!next)
                    return;
                if (!input.otherPhi && !input.aValue)
                    return;
                if (phis.includes(next))
                    placement[next].insert(input);
                else
                    pendingInput[next] = input;
            };

            apply(cur->next0);
            apply(cur->next1);
        });

        if (phis.includes(target))
            targetPhi = target;
        else if (pendingInput.count(target))
            targetPhi = pendingInput.at(target).otherPhi;

        if (!targetPhi)
            return;
    }

    // Cleanup the resulting phi graph
    bool changed = true;
    auto cleanup = [&]() {
        for (auto ci = placement.begin(); ci != placement.end();) {
            // Remove dead inputs
            auto& inputs = ci->second;
            for (auto ii = inputs.begin(); ii != inputs.end();) {
                if (ii->otherPhi && !placement.count(ii->otherPhi)) {
                    ii = inputs.erase(ii);
                    changed = true;
                } else {
                    ii++;
                }
            }
            if (ci->second.size() == 0) {
                ci = placement.erase(ci);
            } else if (ci->second.size() == 1) {
                // Remove single input phis
                auto input1 = *ci->second.begin();
                // update all other phis which have us as input
                for (auto& c : placement) {
                    for (auto in : c.second) {
                        if (in.otherPhi == ci->first) {
                            in.otherPhi = input1.otherPhi;
                            in.aValue = input1.aValue;
                            changed = true;
                        }
                    }
                }
                if (targetPhi == ci->first)
                    targetPhi = input1.otherPhi;
                ci = placement.erase(ci);
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
    for (auto& i : placement) {
        if (i.second.size() != cfg.immediatePredecessors(i.first).size()) {
            placement.clear();
            return;
        }
    }

    success = !placement.empty();
}

} // namespace pir
} // namespace rir
