//
// Created by Jakob Hain on 8/15/23.
//

#include "getConnected.h"
#include "getConnectedOld.h"
#include "getConnectedUni.h"
#include "R/Printing.h"
#include <iostream>

namespace rir {

ConnectedSet getConnected(SEXP root) {
    auto set1 = getConnectedOld(root);
    auto set2 = getConnectedUni(root);
    std::unordered_set<SEXP> set1MinusSet2;
    std::unordered_set<SEXP> set2MinusSet1;
#ifdef ENABLE_SLOWASSERT
    std::set_difference(set1.begin(), set1.end(), set2.begin(), set2.end(),
                        std::inserter(set1MinusSet2, set1MinusSet2.begin()));
    std::set_difference(set2.begin(), set2.end(), set1.begin(), set1.end(),
                        std::inserter(set2MinusSet1, set2MinusSet1.begin()));
    if (!set1MinusSet2.empty()) {
        std::cerr << "getConnectedOld has more elements than getConnectedUni:\n";
        for (auto e : set1MinusSet2) {
            std::cerr << "  " << Print::dumpSexp(e, 75) << "\n";
        }
    }
    if (!set2MinusSet1.empty()) {
        std::cerr << "getConnectedUni has more elements than getConnectedOld:\n";
        for (auto e : set2MinusSet1) {
            std::cerr << "  " << Print::dumpSexp(e, 75) << "\n";
        }
    }
#endif

    return set2;
}

} // namespace rir