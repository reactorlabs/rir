//
// Created by Jakob Hain on 7/26/23.
//

#include "rirObjectMagic.h"
#include "RirRuntimeObject.h"

namespace rir {

unsigned rirObjectMagic(SEXP rirObject) {
    assert(TYPEOF(rirObject) == EXTERNALSXP && "Not a RIR object");
    return ((rir_header*)STDVEC_DATAPTR(rirObject))->magic;
}

} // namespace rir