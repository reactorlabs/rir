
#ifndef DEBUG_POOL
#define DEBUG_POOL

#include "R/RVector.h"

#include <cassert>
#include <cstddef>
#include <vector>

#include "R/r.h"

#include <unordered_map>

namespace rir {

typedef unsigned DebugPoolIdx;

class DebugPool {
    // These are supposed to be unprotected
    static std::vector<SEXP> tmps;
    static std::vector<const char*> prefixes;

  public:
    static SEXP& tmpAt(DebugPoolIdx idx) { return DebugPool::tmps[idx]; }

    static DebugPoolIdx addTmp() {
        DebugPool::tmps.push_back(nullptr);
        return DebugPool::tmps.size() - 1;
    }

    static const char* prefixAt(DebugPoolIdx idx) {
        return DebugPool::prefixes[idx];
    }

    static DebugPoolIdx prefixIdx(const char* prefix) {
        for (DebugPoolIdx i = 0; i < DebugPool::prefixes.size(); i++) {
            if (strcmp(DebugPool::prefixes[i], prefix) == 0) {
                return i;
            }
        }
        DebugPool::prefixes.push_back(prefix);
        return DebugPool::prefixes.size() - 1;
    }
};
} // namespace rir

#endif
