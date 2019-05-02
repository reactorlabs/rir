#include "interp_incl.h"
#include "runtime/DispatchTable.h"
#include <R/r.h>

#define SERIALIZE_RIR

namespace rir {

void serializeRir(SEXP s, SEXP refTable, R_outpstream_t out) {
#ifdef SERIALIZE_RIR
    DispatchTable::unpack(s)->serialize(refTable, out);
#else
    WriteItem(rirDecompile(s), refTable, out);
#endif
}

SEXP deserializeRir(SEXP refTable, R_inpstream_t inp) {
#ifdef SERIALIZE_RIR
    return DispatchTable::deserialize(refTable, inp)->container();
#else
    return ReadItem(refTable, inp);
#endif
}

}; // namespace rir
