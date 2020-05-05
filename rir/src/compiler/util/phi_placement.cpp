#include "phi_placement.h"
#include "../analysis/generic_static_analysis.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"

#include "utils/Set.h"

#include <sstream>
#include <string>

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

            auto apply = [&](BB* next) {
                if (!next)
                    return;
                if (!input.otherPhi && !input.aValue)
                    return;

                if (phis.includes(next)) {
                    placement[next].insert(input);
                } else
                    pendingInput[next] = input;
            };

            for (auto suc : cur->successors())
                apply(suc);
        });
    }

    bool changed;

    // Remove ill formed phis
    for (auto ci = placement.begin(); ci != placement.end();) {
        if (ci->second.size() != ci->first->predecessors().size()) {
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

    // recompute dominatingPhi
    dominatingPhi.clear();
    Visitor::run(cls->entry, [&](BB* cur) {
        BB* next = cur;
        while (next != cls->entry) {

            if (placement.count(next)) {
                dominatingPhi[cur] = next;
                break;
            }
            next = dom.immediateDominator(next);
        }
    });

}

} // namespace pir
} // namespace rir
