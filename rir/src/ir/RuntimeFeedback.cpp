#include "RuntimeFeedback.h"
#include "R/r.h"
#include "runtime/Code.h"

#include <cassert>

namespace rir {
ObservedType::ObservedType(SEXP s)
    : sexptype((uint8_t)TYPEOF(s)), scalar(IS_SCALAR(s, TYPEOF(s))),
      object(OBJECT(s)), attribs(ATTRIB(s) != R_NilValue) {}

SEXP ObservedCallees::getTarget(const Code* code, size_t pos) const {
    assert(pos < numTargets);
    return code->getExtraPoolEntry(targets[pos]);
}

} // namespace rir
