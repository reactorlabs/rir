#include "interp_incl.h"

#include <R/r.h>

namespace rir {

void serializeRir(SEXP s, SEXP refTable, R_outpstream_t out) {
    WriteItem(rirDecompile(s), refTable, out);
}

SEXP deserializeRir(SEXP refTable, R_inpstream_t inp) {
    return ReadItem(refTable, inp);
}

}; // namespace rir
