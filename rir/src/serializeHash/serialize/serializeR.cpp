#include "serialize.h"
#include "R/Protect.h"
#include "R/disableGc.h"
#include "api.h"
#include "compiler/parameter.h"
#include "serializeHash/hash/UUIDPool.h"
#include "interpreter/interp_incl.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "utils/measuring.h"
#include <sstream>

namespace rir {

bool pir::Parameter::RIR_PRESERVE =
    getenv("RIR_PRESERVE") != nullptr && strtol(getenv("RIR_PRESERVE"), nullptr, 10);
bool pir::Parameter::SERIALIZE_LLVM =
    RIR_PRESERVE ||
    (getenv("PIR_DEBUG_SERIALIZE_LLVM") != nullptr && strtol(getenv("PIR_DEBUG_SERIALIZE_LLVM"), nullptr, 10));

// This is a magic constant in custom-r/src/main/saveload.c:defaultSaveVersion
static const int R_STREAM_DEFAULT_VERSION = 3;
static const R_pstream_format_t R_STREAM_FORMAT = R_pstream_xdr_format;

static bool _useHashes = false;
static UUID retrieveHash;

// Will serialize s if it's an instance of CLS
template <typename CLS>
static bool trySerializeR(SEXP s, SEXP refTable, R_outpstream_t out) {
    if (CLS* b = CLS::check(s)) {
        OutInteger(out, b->info.magic);
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: rirSerializeHook", s, [&]{
            b->serializeR(refTable, out);
        });
        return true;
    } else {
        return false;
    }
}

void rirSerializeHook(SEXP s, SEXP refTable, R_outpstream_t out) {
    if (pir::Parameter::RIR_PRESERVE) {
        OutInteger(out, EXTERNALSXP);
        if (!trySerializeR<DispatchTable>(s, refTable, out) &&
            !trySerializeR<Code>(s, refTable, out) &&
            !trySerializeR<Function>(s, refTable, out) &&
            !trySerializeR<ArglistOrder>(s, refTable, out) &&
            !trySerializeR<LazyArglist>(s, refTable, out) &&
            !trySerializeR<LazyEnvironment>(s, refTable, out) &&
            !trySerializeR<PirTypeFeedback>(s, refTable, out)) {
            std::cerr << "couldn't serialize EXTERNALSXP: ";
            Rf_PrintValue(s);
            assert(false);
        }
    } else {
        WriteItem(rirDecompile(s), refTable, out);
    }
}

SEXP rirDeserializeHook(SEXP refTable, R_inpstream_t inp) {
    return Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: rirDeserializeHook", [&]{
        unsigned magic = InInteger(inp);
        switch (magic) {
        case DISPATCH_TABLE_MAGIC:
            return DispatchTable::deserializeR(refTable, inp)->container();
        case CODE_MAGIC:
            return Code::deserializeR(refTable, inp)->container();
        case FUNCTION_MAGIC:
            return Function::deserializeR(refTable, inp)->container();
        case ARGLIST_ORDER_MAGIC:
            return ArglistOrder::deserializeR(refTable, inp)->container();
        case LAZY_ARGS_MAGIC:
            return LazyArglist::deserializeR(refTable, inp)->container();
        case LAZY_ENVIRONMENT_MAGIC:
            return LazyEnvironment::deserializeR(refTable, inp)->container();
        case PIR_TYPE_FEEDBACK_MAGIC:
            return PirTypeFeedback::deserializeR(refTable, inp)->container();
        default:
            std::cerr << "unhandled RIR object magic: 0x" << std::hex << magic
                      << "\n";
            assert(false);
        }
    }, [&](SEXP s){
        // TODO: Find out why this doesn't work for some nested code objects,
        //  and fix if possible.
        return false;
    });
}

SEXP copyBySerialR(SEXP x) {
    if (!pir::Parameter::RIR_SERIALIZE_CHAOS)
        return x;

    return Measuring::timeEventIf2(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: copyBySerialR", x, [&]{
        Protect p(x);
        auto oldPreserve = pir::Parameter::RIR_PRESERVE;
        pir::Parameter::RIR_PRESERVE = true;
        SEXP copy;
        disableInterpreter([&]{
            SEXP data = p(R_serialize(x, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
            disableGc([&] { copy = p(R_unserialize(data, R_NilValue)); });
        });
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

            SEXP copy2;
            disableInterpreter([&]{
                SEXP data = p(R_serialize(copy, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
                disableGc([&]{ copy = p(R_unserialize(data2, R_NilValue)); });
            });
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

void serializeR(SEXP sexp, ByteBuffer& buffer, bool useHashes) {
    assert(!retrieveHash && "bad state: should start deserializing SEXP with retrieve hash or deserialize a non-RIR SEXP before serializing another SEXP");
    disableInterpreter([&]{
        disableGc([&] {
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: serializeR", sexp, [&]{
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
    });
}

SEXP deserializeR(ByteBuffer& sexpBuffer, bool useHashes, const UUID& newRetrieveHash) {
    assert(!retrieveHash && "bad state: should start deserializing SEXP with retrieve hash or deserialize a non-RIR SEXP before deserializing another SEXP");
    SEXP result;
    disableInterpreter([&]{
        disableGc([&] {
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: deserializeR", [&]{
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
                assert((!newRetrieveHash || UUIDPool::getHash(sexp) == newRetrieveHash) &&
                       "deserialized SEXP not given retrieve hash");
                _useHashes = oldUseHashes;
                pir::Parameter::RIR_PRESERVE = oldPreserve;
                return sexp;
            }, [&](SEXP s){
                // TODO: Find out why this doesn't work for some nested code objects,
                //  and fix if possible.
                return false;
            });
        });
    });
    return result;
}

SEXP deserializeR(ByteBuffer& sexpBuffer, bool useHashes) {
    return deserializeR(sexpBuffer, useHashes, UUID());
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
