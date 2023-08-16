//
// Created by Jakob Hain on 8/15/23.
//

// Connected objects are currently different, not worth making them identical
// though as long as they both work the same
#define DEBUG_CONNECTED_DIFFERENCES 0

#include "getConnected.h"
#include "getConnectedOld.h"
#include "getConnectedUni.h"
#if DEBUG_CONNECTED_DIFFERENCES
#include "R/Printing.h"
#include <algorithm>
#include <iostream>
#endif

namespace rir {

ConnectedSet getConnected(SEXP root) {
#if defined(ENABLE_SLOWASSERT) || DEBUG_CONNECTED_DIFFERENCES
    auto set1 = getConnectedUni(root);
#endif
    auto set2 = getConnectedOld(root);
#if DEBUG_CONNECTED_DIFFERENCES
    std::unordered_set<SEXP> set1MinusSet2;
    std::unordered_set<SEXP> set2MinusSet1;
    std::set_difference(set1.begin(), set1.end(), set2.begin(), set2.end(),
                        std::inserter(set1MinusSet2, set1MinusSet2.begin()));
    std::set_difference(set2.begin(), set2.end(), set1.begin(), set1.end(),
                        std::inserter(set2MinusSet1, set2MinusSet1.begin()));
    if (!set1MinusSet2.empty()) {
        std::cerr << "getConnectedUni has " << set1MinusSet2.size() << " elements not in getConnectedOld:\n";
        for (auto e : set1MinusSet2) {
            std::cerr << "  " << Print::dumpSexp(e, 75) << "\n";
        }
    }
    if (!set2MinusSet1.empty()) {
        std::cerr << "getConnectedOld has " << set2MinusSet1.size() << " elements not in getConnectedUni:\n";
        for (auto e : set2MinusSet1) {
            std::cerr << "  " << Print::dumpSexp(e, 75) << "\n";
        }
    }
#elif defined(ENABLE_SLOWASSERT)
    (void)set1;
#endif

    return set2;
}

} // namespace rir