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
                if (!phis.includes(cur))
                    dominatingPhi[cur] = pendingInput.at(cur).otherPhi;
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

    auto printPhis = [&]() {
        std::stringstream str("");

        str << " PLACEMENTS: ("
            << "\n";
        for (auto ci = placement.begin(); ci != placement.end(); ci++) {

            str << "PHI \n[";
            str << "target: " << ci->first->id;
            str << "\n";
            auto& inputs = ci->second;

            for (auto ii = inputs.begin(); ii != inputs.end(); ii++) {
                str << "\t{";
                str << "input: " << ii->inputBlock->id;
                str << ", otherPhi: ";
                if (ii->otherPhi != nullptr)
                    str << ii->otherPhi->id;
                str << "}";
                str << "\n";
            }
            str << "]";
            str << " \n\n";
        }

        str << ")"
            << "\n";

        return str.str();
    };

    auto phisInitial = printPhis();

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
                if (input1.otherPhi != ci->first) {
                    dominatingPhi[ci->first] = input1.otherPhi;
                    // update all other phis which have us as input
                    for (auto& c : placement) {
                        for (auto& in : c.second) {
                            if (in.otherPhi == ci->first) {
                                in.otherPhi = input1.otherPhi;
                                in.aValue = input1.aValue;
                                changed = true;
                            }
                        }
                    }
                }
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
        if (i.second.size() != i.first->predecessors().size()) {
            // TODO figure out why this happens
            placement.clear();
            dominatingPhi.clear();
            break;
        }
    }

    // // Remove one-input phis and relink
    // bool changed = true;
    // auto cleanupRemoveOneInputs = [&]() {
    //     for (auto ci = placement.begin(); ci != placement.end();) {

    //         if (ci->second.size() == 1) {
    //             // Remove single input phis
    //             auto input1 = *ci->second.begin();
    //             if (input1.otherPhi != ci->first) {
    //                 dominatingPhi[ci->first] = input1.otherPhi;
    //                 // update all other phis which have us as input
    //                 for (auto& c : placement) {
    //                     for (auto& in : c.second) {
    //                         if (in.otherPhi == ci->first) {
    //                             in.otherPhi = input1.otherPhi;
    //                             in.aValue = input1.aValue;
    //                             changed = true;
    //                         }
    //                     }
    //                 }
    //             }

    //             std::cerr << "---------- " << "id failed: " << ci->first->id
    //             << "\n";
    //             //assert(false);
    //             ci = placement.erase(ci);
    //         } else {
    //             ci++;
    //         }
    //     }
    // };

    // while (changed) {
    //     changed = false;
    //     cleanupRemoveOneInputs();
    // }

    // // Remove ill formed phis
    // for (auto ci = placement.begin(); ci != placement.end();) {
    //     if (ci->second.size() != ci->first->predecessors().size()) {
    //         assert(false);
    //         ci = placement.erase(ci);

    //     } else {
    //         ci++;
    //     }
    // }

    // // Remove Broken phis (a broken phi is a phi which has an input pointing
    // to
    // // a phi that no longer exists)
    // auto cleanupRemoveBrokenPhis = [&]() {
    //     for (auto ci = placement.begin(); ci != placement.end();) {
    //         auto& inputs = ci->second;

    //         auto isBroken = false;
    //         for (auto ii = inputs.begin(); ii != inputs.end();) {
    //             if (ii->otherPhi && !placement.count(ii->otherPhi)) {
    //                 isBroken = true;
    //                 break;
    //             }
    //         }

    //         if (isBroken) {
    //             ci = placement.erase(ci);
    //             assert(false);
    //             changed = true;
    //         } else {
    //             ci++;
    //         }
    //     }
    // };

    // while (changed) {
    //     changed = false;
    //     cleanupRemoveBrokenPhis();
    // }

    Visitor::run(cls->entry, [&](BB* cur) {
        auto count = 0;
        std::stringstream strDom("Dom: ");
        for (auto ci = placement.begin(); ci != placement.end(); ci++) {
            if (dom.strictlyDominates(ci->first, cur)) {
                strDom << ci->first->id << " -> " << cur->id << "\n";

                count++;
            }
        }

        if (count > 1) {
            // cls->print(std::cerr, true);
            std::cerr << "\n\n" << strDom.str();
            std::cerr << "\n\n";
            cls->printBBGraph(std::cerr, true);

            std::cerr << "phis initial: "
                      << "\n";
            std::cerr << phisInitial;

            std::cerr << "\n";
            auto phisFinal = printPhis();

            std::cerr << "phis final: "
                      << "\n";
            std::cerr << phisFinal;
            std::cerr << "\n";
        }
        std::cerr << "count: " << count;
        assert(count <= 1 && "count more than 1");
    });
}

} // namespace pir
} // namespace rir
