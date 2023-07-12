//
// Created by Jakob Hain on 7/11/23.
//

#include "contextualHashing.h"

namespace rir {

SEXP copyRefTable(SEXP refTable) {
    SEXP copy = CONS_NR(R_NilValue, Rf_allocVector(VECSXP, LENGTH(CDR(refTable))));
    SET_STDVEC_TRUELENGTH(CDR(copy), TRUELENGTH(CDR(refTable)));
    Rf_copyVector(CDR(copy), CDR(refTable));
    return copy;
}

} // namespace rir