//
// Created by Jakob Hain on 8/15/23.
//

// Hashes are currently different, not worth making them identical though as
// long as they both work the same
#define DEBUG_HASH_DIFFERENCES 0

#include "hashRoot.h"
#include "hashRootOld.h"
#include "hashRootUni.h"
#if DEBUG_HASH_DIFFERENCES
#include "runtime/log/printRirObject.h"
#include <iostream>
#endif

namespace rir {

UUID hashRoot(SEXP root) {
#if defined(ENABLE_SLOWASSERT) || DEBUG_HASH_DIFFERENCES
    auto uuid1 = hashRootOld(root);
#endif
    auto uuid2 = hashRootUni(root);
#if DEBUG_HASH_DIFFERENCES
    if (uuid1 != uuid2) {
        std::cerr << "hashRootOld and hashRootUni disagree:\n";
        std::cerr << "  ";
        printRirObject(root, std::cerr);
        std::cerr << "\n";
    }
#elif defined(ENABLE_SLOWASSERT)
    (void)uuid1;
#endif

    return uuid2;
}

} // namespace rir