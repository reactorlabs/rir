#include "serialize.h"
#include "R/Protect.h"
#include "R/disableGc.h"
#include "api.h"
#include "compiler/parameter.h"
#include "hash/UUIDPool.h"
#include "interp_incl.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "utils/measuring.h"
#include <sstream>

namespace rir {

bool pir::Parameter::RIR_PRESERVE =
    getenv("RIR_PRESERVE") != nullptr && strtol(getenv("RIR_PRESERVE"), nullptr, 10);
unsigned pir::Parameter::RIR_SERIALIZE_CHAOS =
    getenv("RIR_SERIALIZE_CHAOS") ? strtol(getenv("RIR_SERIALIZE_CHAOS"), nullptr, 10) : 0;
bool pir::Parameter::DEBUG_SERIALIZE_LLVM =
    RIR_PRESERVE ||
    (getenv("DEBUG_SERIALIZE_LLVM") != nullptr && strtol(getenv("DEBUG_SERIALIZE_LLVM"), nullptr, 10));
bool pir::Parameter::PIR_MEASURE_SERIALIZATION =
    getenv("PIR_MEASURE_SERIALIZATION") != nullptr &&
    strtol(getenv("PIR_MEASURE_SERIALIZATION"), nullptr, 10);

// This is a magic constant in custom-r/src/main/saveload.c:defaultSaveVersion
static const int R_STREAM_DEFAULT_VERSION = 3;
static const R_pstream_format_t R_STREAM_FORMAT = R_pstream_xdr_format;

static bool _useHashes = false;
static UUID retrieveHash;

// Will serialize s if it's an instance of CLS
template <typename CLS>
static bool trySerialize(SEXP s, SEXP refTable, R_outpstream_t out) {
    if (CLS* b = CLS::check(s)) {
        OutInteger(out, b->info.magic);
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serialize.cpp: serializeRir", s, [&]{
            b->serialize(refTable, out);
        });
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
            std::cerr << "couldn't serialize EXTERNALSXP: ";
            Rf_PrintValue(s);
            assert(false);
        }
    } else {
        WriteItem(rirDecompile(s), refTable, out);
    }
}

SEXP deserializeRir(SEXP refTable, R_inpstream_t inp) {
    return Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serialize.cpp: deserializeRir", [&]{
        unsigned magic = InInteger(inp);
        switch (magic) {
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
            std::cerr << "couldn't deserialize EXTERNALSXP with magic code: 0x"
                      << std::hex << magic << "\n";
            assert(false);
        }
    }, [&](SEXP s){
        // TODO: Find out why this doesn't work for some nested code objects,
        //  and fix if possible.
        return false;
    });
}

SEXP copyBySerial(SEXP x) {
    if (!pir::Parameter::RIR_SERIALIZE_CHAOS)
        return x;

    return Measuring::timeEventIf<SEXP>(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serialize.cpp: copyBySerial", x, [&]{
        Protect p(x);
        auto oldPreserve = pir::Parameter::RIR_PRESERVE;
        pir::Parameter::RIR_PRESERVE = true;
        SEXP data =
            p(R_serialize(x, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
        SEXP copy =
            p(disableGc<SEXP>([&] { return R_unserialize(data, R_NilValue); }));
#ifdef DO_INTERN
        copy = UUIDPool::intern(copy, true, false);
#endif
#if defined(ENABLE_SLOWASSERT) && defined(CHECK_COPY_BY_SERIAL)
        auto xHash = hashRoot(x);
        auto copyHash = hashRoot(copy);
        if (xHash != copyHash) {
            std::stringstream ss;
            ss << "hash mismatch after serializing: " << xHash
               << " != " << copyHash;
            Rf_warning(ss.str().c_str());
            Rf_PrintValue(x);
            Rf_PrintValue(copy);

            SEXP data2 = p(R_serialize(copy, R_NilValue, R_NilValue, R_NilValue,
                                       R_NilValue));
            SEXP copy2 = p(R_unserialize(data2, R_NilValue));
            auto copyHash2 = hashRoot(copy2);
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
    });
}

static void rStreamOutChar(R_outpstream_t stream, int data) {
    auto buffer = (ByteBuffer*)stream->data;
    auto data2 = (unsigned char)data;
    buffer->putBytes(&data2, sizeof(unsigned char));
}

static void rStreamOutBytes(R_outpstream_t stream, void* data, int length) {
    auto buffer = (ByteBuffer*)stream->data;
    buffer->putBytes((uint8_t*)data, length);
}

static int rStreamInChar(R_inpstream_t stream) {
    auto buffer = (ByteBuffer*)stream->data;
    unsigned char c;
    buffer->getBytes(&c, sizeof(unsigned char));
    return c;
}

static void rStreamInBytes(R_inpstream_t stream, void* data, int length) {
    auto buffer = (ByteBuffer*)stream->data;
    buffer->getBytes((uint8_t*)data, length);
}

void serialize(SEXP sexp, ByteBuffer& buffer, bool useHashes) {
    assert(!retrieveHash && "bad state: should start deserializing SEXP with retrieve hash or deserialize a non-RIR SEXP before serializing another SEXP");
    disableGc([&] {
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serialize.cpp: serialize", sexp, [&]{
            auto oldPreserve = pir::Parameter::RIR_PRESERVE;
            auto oldUseHashes = _useHashes;
            pir::Parameter::RIR_PRESERVE = true;
            _useHashes = useHashes;
            struct R_outpstream_st out{};
            R_InitOutPStream(&out, (R_pstream_data_t)&buffer, R_STREAM_FORMAT,
                             R_STREAM_DEFAULT_VERSION, rStreamOutChar,
                             rStreamOutBytes, nullptr, nullptr);
            R_Serialize(sexp, &out);
            _useHashes = oldUseHashes;
            pir::Parameter::RIR_PRESERVE = oldPreserve;
        });
    });
}

SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes) {
    return deserialize(sexpBuffer, useHashes, UUID());
}

SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes, const UUID& newRetrieveHash) {
    assert(!retrieveHash && "bad state: should start deserializing SEXP with retrieve hash or deserialize a non-RIR SEXP before deserializing another SEXP");
    return disableGc<SEXP>([&] {
        return Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serialize.cpp: deserialize", [&]{
            auto oldPreserve = pir::Parameter::RIR_PRESERVE;
            auto oldUseHashes = _useHashes;
            pir::Parameter::RIR_PRESERVE = true;
            _useHashes = useHashes;
            retrieveHash = newRetrieveHash;
            struct R_inpstream_st in{};
            R_InitInPStream(&in, (R_pstream_data_t)&sexpBuffer, R_STREAM_FORMAT,
                            rStreamInChar, rStreamInBytes, nullptr, nullptr);
            SEXP sexp = R_Unserialize(&in);
            assert(!retrieveHash && "retrieve hash not filled");
            assert((!newRetrieveHash || UUIDPool::get(newRetrieveHash) == sexp) &&
                   "retrieve hash not filled with deserialized SEXP");
            _useHashes = oldUseHashes;
            pir::Parameter::RIR_PRESERVE = oldPreserve;
            return sexp;
        }, [&](SEXP s){
            // TODO: Find out why this doesn't work for some nested code objects,
            //  and fix if possible.
            return false;
        });
    });
}

bool useHashes(__attribute__((unused)) R_outpstream_t out) {
    // Trying to pretend we don't use a singleton...
    return _useHashes;
}

bool useHashes(__attribute__((unused)) R_inpstream_t in) {
    // Trying to pretend we don't use a singleton...
    return _useHashes;
}

void useRetrieveHashIfSet(__attribute__((unused)) R_inpstream_t inp, SEXP sexp) {
    if (retrieveHash) {
        UUIDPool::intern(sexp, retrieveHash, false, false);
        retrieveHash = UUID();
    }
}

} // namespace rir
