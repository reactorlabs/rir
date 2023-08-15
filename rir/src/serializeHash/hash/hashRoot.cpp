//
// Created by Jakob Hain on 8/15/23.
//

#include "hashRoot.h"
#include "hashRootOld.h"
#include "hashRootUni.h"
#include "R/Printing.h"
#include <iostream>

namespace rir {

UUID hashRoot(SEXP root) {
    auto uuid1 = hashRootOld(root);
    auto uuid2 = hashRootUni(root);
#ifdef ENABLE_SLOWASSERT
    if (uuid1 != uuid2) {
        std::cerr << "hashRootOld and hashRootUni disagree:\n";
        std::cerr << "  " << Print::dumpSexp(root, 500) << "\n";
    }
#endif

    return uuid2;
}

} // namespace rir