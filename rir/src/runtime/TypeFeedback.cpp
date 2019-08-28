#include "TypeFeedback.h"
#include "R/r.h"
#include "runtime/Code.h"

#include <cassert>

namespace rir {

void ObservedCallees::record(Code* caller, SEXP callee) {
    if (taken < CounterOverflow)
        taken++;
    if (numTargets < MaxTargets) {
        int i = 0;
        for (; i < numTargets; ++i)
            if (caller->getExtraPoolEntry(targets[i]) == callee)
                break;
        if (i == numTargets) {
            auto idx = caller->addExtraPoolEntry(callee);
            targets[numTargets++] = idx;
        }
    }
}

SEXP ObservedCallees::getTarget(const Code* code, size_t pos) const {
    assert(pos < numTargets);
    return code->getExtraPoolEntry(targets[pos]);
}

} // namespace rir
