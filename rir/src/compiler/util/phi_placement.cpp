#include "phi_placement.h"
#include "../analysis/generic_static_analysis.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"

#include "utils/Set.h"

#include <sstream>
#include <string>

#include <list>

namespace rir {
namespace pir {

PhiPlacement::PhiPlacement(ClosureVersion* cls,
                           const std::unordered_map<BB*, Value*>& writes,
                           const DominanceGraph& dom,
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

    std::unordered_map<BB*, std::list<BB*>> dominatedByPhi;

    {
        std::unordered_map<BB*, PhiInput> pendingInput;
        DominatorTreeVisitor<>(dom).run(cls->entry, [&](BB* cur) {
            PhiInput input = {nullptr, nullptr, nullptr};
            if (pendingInput.count(cur)) {
                input = pendingInput.at(cur);
            }

            if (phis.includes(cur)) {
                input.aValue = nullptr;
                input.otherPhi = cur;
            }

            if (writes.count(cur)) {
                input.otherPhi = nullptr;
                input.aValue = writes.at(cur);
            }
            input.inputBlock = cur;

            if (!phis.includes(cur)) {
                if (input.otherPhi)
                    dominatingPhi[cur] = input.otherPhi;

            } else {
                dominatingPhi[cur] = cur;
            }

            auto apply = [&](BB* next) {
                if (!next)
                    return;
                if (!input.otherPhi && !input.aValue)
                    return;

                if (phis.includes(next)) {
                    placement[next].insert(input);
                } else {
                    pendingInput[next] = input;
                }
                                
            };

            for (auto suc : cur->successors())
                apply(suc);
        });
    }

    bool changed;

    for (auto it = dominatingPhi.begin(); it != dominatingPhi.end(); it++) {
        auto& l = dominatedByPhi[it->second];
        l.push_back(it->first);
    }

    auto updateDominatingPhisOnDeletion = [&](BB* deletedPhiNode) {
        auto& l = dominatedByPhi[deletedPhiNode];
        for (auto it = l.begin(); it != l.end(); it++) {
            dominatingPhi.erase(*it);
        }

        dominatedByPhi.erase(deletedPhiNode);
    };

    // Remove ill formed phis
    for (auto ci = placement.begin(); ci != placement.end();) {
        if (ci->second.size() != ci->first->predecessors().size()) {
            updateDominatingPhisOnDeletion(ci->first);
            ci = placement.erase(ci);

        } else {
            ci++;
        }
    }

    // Remove Broken phis (a broken phi is a phi which has an input pointing to
    // a phi that no longer exists)
    auto cleanupRemoveBrokenPhis = [&]() {
        for (auto ci = placement.begin(); ci != placement.end();) {
            auto& inputs = ci->second;

            auto isBroken = false;
            for (auto ii = inputs.begin(); ii != inputs.end(); ii++) {
                if (ii->otherPhi && !placement.count(ii->otherPhi)) {
                    isBroken = true;
                    break;
                }
            }

            if (isBroken) {
                updateDominatingPhisOnDeletion(ci->first);
                ci = placement.erase(ci);
                changed = true;
            } else {
                ci++;
            }
        }
    };

    changed = true;
    while (changed) {
        changed = false;
        cleanupRemoveBrokenPhis();
    }
}

} // namespace pir
} // namespace rir
