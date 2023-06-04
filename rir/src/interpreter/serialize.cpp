#include "R/Protect.h"
#include "R/r.h"
#include "api.h"
#include "compiler/parameter.h"
#include "interp_incl.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "utils/UUIDPool.h"

namespace rir {

bool pir::Parameter::RIR_PRESERVE =
    getenv("RIR_PRESERVE") ? atoi(getenv("RIR_PRESERVE")) : false;
unsigned pir::Parameter::RIR_SERIALIZE_CHAOS =
    getenv("RIR_SERIALIZE_CHAOS") ? atoi(getenv("RIR_SERIALIZE_CHAOS")) : 0;

static bool oldPreserve = false;

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
    if (pir::Parameter::RIR_PRESERVE) {
        OutInteger(out, EXTERNALSXP);
        if (!trySerialize<DispatchTable>(s, refTable, out) &&
            !trySerialize<Code>(s, refTable, out) &&
            !trySerialize<Function>(s, refTable, out) &&
            !trySerialize<ArglistOrder>(s, refTable, out) &&
            !trySerialize<LazyArglist>(s, refTable, out) &&
            !trySerialize<LazyEnvironment>(s, refTable, out) &&
            !trySerialize<PirTypeFeedback>(s, refTable, out)) {
            std::cerr << "couldn't deserialize EXTERNALSXP: ";
            Rf_PrintValue(s);
            assert(false);
        }
    } else {
        WriteItem(rirDecompile(s), refTable, out);
    }
}

SEXP deserializeRir(SEXP refTable, R_inpstream_t inp) {
    unsigned code = InInteger(inp);
    switch (code) {
    case DISPATCH_TABLE_MAGIC:
        return DispatchTable::deserialize(refTable, inp)->container();
    case CODE_MAGIC:
        return Code::deserialize(refTable, inp)->container();
    case FUNCTION_MAGIC:
        return Function::deserialize(refTable, inp)->container();
    case ARGLIST_ORDER_MAGIC:
        return ArglistOrder::deserialize(refTable, inp)->container();
    case LAZY_ARGS_MAGIC:
        return LazyArglist::deserialize(refTable, inp)->container();
    case LAZY_ENVIRONMENT_MAGIC:
        return LazyEnvironment::deserialize(refTable, inp)->container();
    case PIR_TYPE_FEEDBACK_MAGIC:
        return PirTypeFeedback::deserialize(refTable, inp)->container();
    default:
        std::cerr << "couldn't deserialize EXTERNALSXP with code: 0x"
                  << std::hex << code << "\n";
        assert(false);
    }
}

SEXP copyBySerial(SEXP x) {
    if (!pir::Parameter::RIR_SERIALIZE_CHAOS)
        return x;

    Protect p;
    oldPreserve = pir::Parameter::RIR_PRESERVE;
    pir::Parameter::RIR_PRESERVE = true;
    SEXP data = p(R_serialize(x, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
    SEXP copy = p(R_unserialize(data, R_NilValue));
#ifdef DO_INTERN
    copy = UUIDPool::intern(copy);
#endif
#if defined(ENABLE_SLOWASSERT) && defined(CHECK_COPY_BY_SERIAL)
    auto xHash = hashSexp(x);
    auto copyHash = hashSexp(copy);
    if (xHash != copyHash) {
        std::stringstream ss;
        ss << "hash mismatch after serializing: " << xHash << " != " << copyHash;
        Rf_warning(ss.str().c_str());
        Rf_PrintValue(x);
        Rf_PrintValue(copy);

        SEXP data2 = p(R_serialize(copy, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
        SEXP copy2 = p(R_unserialize(data2, R_NilValue));
        auto copyHash2 = hashSexp(copy2);
        if (copyHash != copyHash2) {
            std::stringstream ss2;
            ss2 << "copy hash is also different: " << copyHash2;
            Rf_warning(ss2.str().c_str());
            Rf_PrintValue(copy2);
        }
    }
#endif
    pir::Parameter::RIR_PRESERVE = oldPreserve;
    return copy;
}

} // namespace rir
