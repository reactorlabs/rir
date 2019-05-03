#include "interp_incl.h"
#include "runtime/DispatchTable.h"
#include <R/r.h>

#define SERIALIZE_RIR

namespace rir {

// Will serialize s if it's an instance of CLS
template <typename CLS>
static bool trySerialize(SEXP s, SEXP refTable, R_outpstream_t out) {
    if (CLS* b = CLS::check(s)) {
        OutInteger(out, b->info.magic);
        b->serialize(refTable, out);
        return true;
    } else {
        return false;
    }
}

void serializeRir(SEXP s, SEXP refTable, R_outpstream_t out) {
#ifdef SERIALIZE_RIR
    if (!trySerialize<DispatchTable>(s, refTable, out) &&
        !trySerialize<Code>(s, refTable, out) &&
        !trySerialize<Function>(s, refTable, out)) {
        std::cerr << "couldn't deserialize EXTERNALSXP: ";
        Rf_PrintValue(s);
        assert(false);
    }
#else
    WriteItem(rirDecompile(s), refTable, out);
#endif
}

SEXP deserializeRir(SEXP refTable, R_inpstream_t inp) {
#ifdef SERIALIZE_RIR
    unsigned code = InInteger(inp);
    switch (code) {
    case DISPATCH_TABLE_MAGIC:
        return DispatchTable::deserialize(refTable, inp)->container();
    case CODE_MAGIC:
        return Code::deserialize(refTable, inp)->container();
    case FUNCTION_MAGIC:
        return Function::deserialize(refTable, inp)->container();
    default:
        std::cerr << "couldn't deserialize EXTERNALSXP with code: 0x"
                  << std::hex << code << "\n";
        assert(false);
        return nullptr;
    }
#else
    return ReadItem(refTable, inp);
#endif
}

}; // namespace rir
