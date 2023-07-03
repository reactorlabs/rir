#include "serialize.h"
#include "R/Protect.h"
#include "R/r.h"
#include "api.h"
#include "compiler/parameter.h"
#include "hash/UUIDPool.h"
#include "interp_incl.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"

namespace rir {

bool pir::Parameter::RIR_PRESERVE =
    getenv("RIR_PRESERVE") != nullptr && strtol(getenv("RIR_PRESERVE"), nullptr, 10);
unsigned pir::Parameter::RIR_SERIALIZE_CHAOS =
    getenv("RIR_SERIALIZE_CHAOS") ? strtol(getenv("RIR_SERIALIZE_CHAOS"), nullptr, 10) : 0;

// This is a magic constant in custom-r/src/main/saveload.c:defaultSaveVersion
static const int R_STREAM_DEFAULT_VERSION = 3;
static const R_pstream_format_t R_STREAM_FORMAT = R_pstream_xdr_format;

static bool _useHashes = false;
static bool _isHashing = false;
static std::queue<SEXP>* connectedWorklist = nullptr;
static const UUID* retrieveHash = nullptr;

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
    auto oldPreserve = pir::Parameter::RIR_PRESERVE;
    pir::Parameter::RIR_PRESERVE = true;
    SEXP data = p(R_serialize(x, R_NilValue, R_NilValue, R_NilValue, R_NilValue));
    SEXP copy = p(R_unserialize(data, R_NilValue));
#ifdef DO_INTERN
    copy = UUIDPool::intern(copy, true, false);
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

static void rStreamDiscardChar(R_outpstream_t stream, int data) {}

static void rStreamDiscardBytes(R_outpstream_t stream, void* data, int length) {}

static void rStreamHashChar(R_outpstream_t stream, int data) {
    auto hasher = (UUIDHasher*)stream->data;
    hasher->hashBytesOf<unsigned char>((unsigned char)data);
}

static void rStreamHashBytes(R_outpstream_t stream, void* data, int length) {
    auto hasher = (UUIDHasher*)stream->data;
    hasher->hashBytes(data, length);
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

R_outpstream_st nullOutputStream() {
    R_outpstream_st out{};
    R_InitOutPStream(
        &out,
        (R_pstream_data_t) nullptr,
        R_STREAM_FORMAT,
        R_STREAM_DEFAULT_VERSION,
        rStreamDiscardChar,
        rStreamDiscardBytes,
        nullptr,
        nullptr
    );
    return out;
}

UUID hashSexp(SEXP sexp, std::queue<SEXP>& worklist) {
    UUIDHasher hasher;
    hashSexp(sexp, hasher, worklist);
    return hasher.finalize();
}

UUID hashSexp(SEXP sexp) {
    UUIDHasher hasher;
    hashSexp(sexp, hasher);
    return hasher.finalize();
}

void hashSexp(SEXP sexp, UUIDHasher& hasher, std::queue<SEXP>& worklist) {
    auto oldPreserve = pir::Parameter::RIR_PRESERVE;
    auto oldUseHashes = _useHashes;
    auto oldIsHashing = _isHashing;
    auto oldConnectedWorklist = connectedWorklist;
    auto oldRetrieveHash = retrieveHash;
    pir::Parameter::RIR_PRESERVE = true;
    _useHashes = false;
    _isHashing = true;
    connectedWorklist = &worklist;
    retrieveHash = nullptr;
    struct R_outpstream_st out{};
    R_InitOutPStream(
        &out,
        (R_pstream_data_t)&hasher,
        R_STREAM_FORMAT,
        R_STREAM_DEFAULT_VERSION,
        rStreamHashChar,
        rStreamHashBytes,
        nullptr,
        nullptr
    );
    R_Serialize(sexp, &out);
    retrieveHash = oldRetrieveHash;
    connectedWorklist = oldConnectedWorklist;
    _isHashing = oldIsHashing;
    _useHashes = oldUseHashes;
    pir::Parameter::RIR_PRESERVE = oldPreserve;
}

void hashSexp(SEXP sexp, UUIDHasher& hasher) {
    auto oldPreserve = pir::Parameter::RIR_PRESERVE;
    auto oldUseHashes = _useHashes;
    auto oldIsHashing = _isHashing;
    auto oldConnectedWorklist = connectedWorklist;
    auto oldRetrieveHash = retrieveHash;
    pir::Parameter::RIR_PRESERVE = true;
    _useHashes = false;
    _isHashing = true;
    connectedWorklist = nullptr;
    retrieveHash = nullptr;
    struct R_outpstream_st out{};
    R_InitOutPStream(
        &out,
        (R_pstream_data_t)&hasher,
        R_STREAM_FORMAT,
        R_STREAM_DEFAULT_VERSION,
        rStreamHashChar,
        rStreamHashBytes,
        nullptr,
        nullptr
    );
    R_Serialize(sexp, &out);
    retrieveHash = oldRetrieveHash;
    connectedWorklist = oldConnectedWorklist;
    _isHashing = oldIsHashing;
    _useHashes = oldUseHashes;
    pir::Parameter::RIR_PRESERVE = oldPreserve;
}

void serialize(SEXP sexp, ByteBuffer& buffer, bool useHashes) {
    auto oldPreserve = pir::Parameter::RIR_PRESERVE;
    auto oldUseHashes = _useHashes;
    auto oldIsHashing = _isHashing;
    auto oldConnectedWorklist = connectedWorklist;
    auto oldRetrieveHash = retrieveHash;
    pir::Parameter::RIR_PRESERVE = true;
    _useHashes = useHashes;
    _isHashing = false;
    connectedWorklist = nullptr;
    retrieveHash = nullptr;
    struct R_outpstream_st out{};
    R_InitOutPStream(
        &out,
        (R_pstream_data_t)&buffer,
        R_STREAM_FORMAT,
        R_STREAM_DEFAULT_VERSION,
        rStreamOutChar,
        rStreamOutBytes,
        nullptr,
        nullptr
    );
    R_Serialize(sexp, &out);
    retrieveHash = oldRetrieveHash;
    connectedWorklist = oldConnectedWorklist;
    _isHashing = oldIsHashing;
    _useHashes = oldUseHashes;
    pir::Parameter::RIR_PRESERVE = oldPreserve;
}

SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes, const UUID* newRetrieveHash) {
    auto oldPreserve = pir::Parameter::RIR_PRESERVE;
    auto oldUseHashes = _useHashes;
    auto oldIsHashing = _isHashing;
    auto oldConnectedWorklist = connectedWorklist;
    auto oldRetrieveHash = retrieveHash;
    pir::Parameter::RIR_PRESERVE = true;
    _useHashes = useHashes;
    _isHashing = false;
    connectedWorklist = nullptr;
    retrieveHash = newRetrieveHash;
    struct R_inpstream_st in{};
    R_InitInPStream(
        &in,
        (R_pstream_data_t)&sexpBuffer,
        R_STREAM_FORMAT,
        rStreamInChar,
        rStreamInBytes,
        nullptr,
        nullptr
    );
    SEXP sexp = R_Unserialize(&in);
    assert(retrieveHash == nullptr && "retrieve hash not taken");
    retrieveHash = oldRetrieveHash;
    connectedWorklist = oldConnectedWorklist;
    _isHashing = oldIsHashing;
    _useHashes = oldUseHashes;
    pir::Parameter::RIR_PRESERVE = oldPreserve;
    return sexp;
}

bool useHashes(__attribute__((unused)) R_outpstream_t out) {
    // Trying to pretend we don't use a singleton...
    return _useHashes;
}

bool useHashes(__attribute__((unused)) R_inpstream_t in) {
    // Trying to pretend we don't use a singleton...
    return _useHashes;
}

bool isHashing(__attribute__((unused)) R_outpstream_t out) {
    // Trying to pretend we don't use a singleton...
    return _isHashing;
}

std::queue<SEXP>* worklist(__attribute__((unused)) R_outpstream_t out) {
    // Trying to pretend we don't use a singleton...
    return connectedWorklist;
}

void useRetrieveHashIfSet(__attribute__((unused)) R_inpstream_t inp, SEXP sexp) {
    if (retrieveHash) {
        UUIDPool::intern(sexp, *retrieveHash, false, false);
        retrieveHash = nullptr;
    }
}


} // namespace rir
