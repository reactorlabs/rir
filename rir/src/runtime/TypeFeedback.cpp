#include "TypeFeedback.h"
#include "../interpreter/instance.h"
#include "R/r.h"
#include "runtime/Code.h"

#include <cassert>

namespace rir {

ObservedType::ObservedType(R_bcstack_t* s)
    : sexptype((uint8_t)stackObjTypeof(s)), scalar(stackObjIsScalar(s)),
      object(s->tag == STACK_OBJ_SEXP && OBJECT(s->u.sxpval)),
      attribs(s->tag == STACK_OBJ_SEXP && ATTRIB(s->u.sxpval) != R_NilValue) {}
ObservedType::ObservedType(SEXP s)
    : sexptype((uint8_t)TYPEOF(s)), scalar(s->sxpinfo.scalar),
      object(OBJECT(s)), attribs(ATTRIB(s) != R_NilValue) {}

SEXP ObservedCallees::getTarget(const Code* code, size_t pos) const {
    assert(pos < numTargets);
    return code->getExtraPoolEntry(targets[pos]);
}

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

} // namespace rir
