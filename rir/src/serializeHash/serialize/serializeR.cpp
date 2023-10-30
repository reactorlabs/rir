#include "serializeR.h"
#include "R/Protect.h"
#include "R/disableGc.h"
#include "api.h"
#include "compiler/parameter.h"
#include "interpreter/interp_incl.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "runtime/PoolStub.h"
#include "runtime/ProxyEnv.h"
#include "serialize.h"
#include "serializeHash/hash/UUIDPool.h"
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

/// Controls what data is serialized and what format some of it uses. The SEXP
/// must be deserialized with the same options it was serialized with.
///
/// Unfortunately, this is a global variable, because that is the easiest way to
/// thread these options through the GNU-R serialization API, and because we
/// already have a separate RIR serializer which stores these in the serializer
/// (the GNU-R serialization API is serializing children with only `out` and
/// `refTable`, so we can't just pass our serializer to children).
static SerialOptions* R_SERIAL_OPTIONS = nullptr;
/// Similar to R_SERIAL_OPTIONS, we store the retrieve hash for
/// deserialized RIR objects as a global. As a consequence, we can't deserialize
/// with before we consume the retrieve hash from a previous serialization.
static UUID R_SERIAL_RETRIEVE_HASH;

struct RSerializer : AbstractSerializer {
    /// Underlying R output stream
    R_outpstream_t out;
    /// Underlying R ref table
    SEXP refTable;

    RSerializer(R_outpstream_t out, SEXP refTable)
        : out(out), refTable(refTable) {}

    SerializedRefs* refs() override { return nullptr; }

    const SerialOptions& serialOptions() const override {
        return *R_SERIAL_OPTIONS;
    }
    bool willWrite(const SerialFlags& flags) const override {
        assert(R_SERIAL_OPTIONS && "not setup for serialization");
        return R_SERIAL_OPTIONS->willReadOrWrite(flags);
    }
    void writeBytes(const void *data, size_t size,
                    const SerialFlags& flags) override {
        if (!willWrite(flags)) {
            return;
        }

        OutBytes(out, data, (int)size);
    }
    void writeInt(int data, const SerialFlags& flags) override {
        if (!willWrite(flags)) {
            return;
        }

        OutInteger(out, data);
    }
    void write(SEXP s, const SerialFlags& flags) override {
        if (!willWrite(flags)) {
            return;
        }

        if (R_SERIAL_OPTIONS->useHashes) {
            if (!UUIDPool::tryWriteHash(s, out)) {
                WriteItem(s, refTable, out);
            }
        } else if (flags.contains(SerialFlag::MaybeNotRecordedCall)) {
            if (!UUIDPool::tryWriteHash(s, out)) {
                // Still serialize children via hashes
                R_SERIAL_OPTIONS->useHashes = true;
                WriteItem(s, refTable, out);
            }
        } else {
            WriteItem(s, refTable, out);
        }
    }
};

struct RDeserializer : AbstractDeserializer {
    /// Underlying R input stream
    R_inpstream_t inp = nullptr;
    /// Underlying R read-ref table
    SEXP refTable = nullptr;

    RDeserializer(R_inpstream_t inp, SEXP refTable)
        : inp(inp), refTable(refTable) {}

    DeserializedRefs* refs() override { return nullptr; }

    const SerialOptions& serialOptions() const override {
        return *R_SERIAL_OPTIONS;
    }

    bool willRead(const SerialFlags& flags) const override {
        assert(R_SERIAL_OPTIONS && "not setup for deserialization");
        return R_SERIAL_OPTIONS->willReadOrWrite(flags);
    }

    void readBytes(void *data, size_t size, const SerialFlags& flags) override {
        if (!willRead(flags)) {
            return;
        }

        InBytes(inp, data, (int)size);
    }

    int readInt(const SerialFlags& flags) override {
        if (!willRead(flags)) {
            return 0;
        }

        return InInteger(inp);
    }

    SEXP read(const SerialFlags& flags) override {
        if (!willRead(flags)) {
            return nullptr;
        }

        SEXP result;
        if (R_SERIAL_OPTIONS->useHashes) {
            result = UUIDPool::tryReadHash(inp);
            if (!result) {
                result = ReadItem(refTable, inp);
            }
        } else if (flags.contains(SerialFlag::MaybeNotRecordedCall)) {
            result = UUIDPool::tryReadHash(inp);
            if (!result) {
                // Still deserialize children via hashes
                R_SERIAL_OPTIONS->useHashes = true;
                result = ReadItem(refTable, inp);
                R_SERIAL_OPTIONS->useHashes = false;
            }
        } else {
            result = ReadItem(refTable, inp);
        }

        return result;
    }

    void addRef(SEXP sexp) override {
        AddReadRef(refTable, sexp);
        if (R_SERIAL_RETRIEVE_HASH && TYPEOF(sexp) == EXTERNALSXP) {
            UUIDPool::intern(sexp, R_SERIAL_RETRIEVE_HASH, false, false);
            R_SERIAL_RETRIEVE_HASH = UUID();
        }
    }
};

// Will serialize s if it's an instance of CLS
template <typename CLS>
static bool trySerializeR(SEXP s, SEXP refTable, R_outpstream_t out) {
    if (CLS* b = CLS::check(s)) {
        OutInteger(out, b->info.magic);
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: rirSerializeHook", s, [&]{
            RSerializer serializer(out, refTable);
            b->serialize(serializer);
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
            !trySerializeR<PirTypeFeedback>(s, refTable, out) &&
            !trySerializeR<TypeFeedback>(s, refTable, out) &&
            !trySerializeR<PoolStub>(s, refTable, out) &&
            !trySerializeR<ProxyEnv>(s, refTable, out)) {
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
        RDeserializer deserializer(inp, refTable);
        unsigned magic = InInteger(inp);
        switch (magic) {
        case DISPATCH_TABLE_MAGIC:
            return DispatchTable::deserialize(deserializer)->container();
        case CODE_MAGIC:
            return Code::deserialize(deserializer)->container();
        case FUNCTION_MAGIC:
            return Function::deserialize(deserializer)->container();
        case ARGLIST_ORDER_MAGIC:
            return ArglistOrder::deserialize(deserializer)->container();
        case LAZY_ARGS_MAGIC:
            return LazyArglist::deserialize(deserializer)->container();
        case LAZY_ENVIRONMENT_MAGIC:
            return LazyEnvironment::deserialize(deserializer)->container();
        case PIR_TYPE_FEEDBACK_MAGIC:
            return PirTypeFeedback::deserialize(deserializer)->container();
        case TYPEFEEDBACK_MAGIC:
            return TypeFeedback::deserialize(deserializer)->container();
        case POOL_STUB_MAGIC:
            return PoolStub::deserialize(deserializer)->container();
        case PROXY_ENV_MAGIC:
            return ProxyEnv::deserialize(deserializer)->container();
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

static SerialOptions* newRSerialOptions(bool useHashes) {
    return new SerialOptions{useHashes, useHashes, false, nullptr, SerialOptions::SourcePools()};
}

void serializeR(SEXP sexp, ByteBuffer& buffer, bool useHashes) {
    disableInterpreter([&]{
        disableGc([&] {
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: serializeR", sexp, [&]{
                auto oldPreserve = pir::Parameter::RIR_PRESERVE;
                auto oldSerialOptions = R_SERIAL_OPTIONS;
                pir::Parameter::RIR_PRESERVE = true;
                R_SERIAL_OPTIONS = newRSerialOptions(useHashes);

                struct R_outpstream_st out{};
                R_InitOutPStream(&out, (R_pstream_data_t)&buffer, R_STREAM_FORMAT,
                                 R_STREAM_DEFAULT_VERSION, rStreamOutChar,
                                 rStreamOutBytes, nullptr, nullptr);
                R_Serialize(sexp, &out);

                delete R_SERIAL_OPTIONS;
                R_SERIAL_OPTIONS = oldSerialOptions;
                pir::Parameter::RIR_PRESERVE = oldPreserve;
            });
        });
    });
}

SEXP deserializeR(const ByteBuffer& sexpBuffer, bool useHashes, const UUID& newRetrieveHash) {
    assert(!R_SERIAL_RETRIEVE_HASH &&
           "bad state: deserializing a different SEXP before we set the retrieve hash from last deserialization");
    SEXP result;
    disableInterpreter([&]{
        disableGc([&] {
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: deserializeR", [&]{
                auto oldPreserve = pir::Parameter::RIR_PRESERVE;
                auto oldSerialOptions = R_SERIAL_OPTIONS;
                pir::Parameter::RIR_PRESERVE = true;
                R_SERIAL_OPTIONS = newRSerialOptions(useHashes);
                R_SERIAL_RETRIEVE_HASH = newRetrieveHash;

                struct R_inpstream_st in{};
                R_InitInPStream(&in, (R_pstream_data_t)&sexpBuffer, R_STREAM_FORMAT,
                                rStreamInChar, rStreamInBytes, nullptr, nullptr);
                SEXP sexp = R_Unserialize(&in);

                assert(!R_SERIAL_RETRIEVE_HASH && "retrieve hash not filled");
                assert((!newRetrieveHash || UUIDPool::getHash(sexp) == newRetrieveHash) &&
                       "deserialized SEXP not given retrieve hash");

                delete R_SERIAL_OPTIONS;
                R_SERIAL_OPTIONS = oldSerialOptions;
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

SEXP deserializeR(const ByteBuffer& sexpBuffer, bool useHashes) {
    return deserializeR(sexpBuffer, useHashes, UUID());
}

SEXP copyBySerialR(SEXP x) {
    if (!pir::Parameter::RIR_SERIALIZE_CHAOS)
        return x;

    return Measuring::timeEventIf2(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeR.cpp: copyBySerialR", x, [&]{
        Protect p(x);

        auto oldOptions = R_SERIAL_OPTIONS;
        auto oldPreserve = pir::Parameter::RIR_PRESERVE;
        pir::Parameter::RIR_PRESERVE = true;
        R_SERIAL_OPTIONS = newRSerialOptions(false);

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

        delete R_SERIAL_OPTIONS;
        R_SERIAL_OPTIONS = oldOptions;
        pir::Parameter::RIR_PRESERVE = oldPreserve;

        return copy;
    });
}

} // namespace rir
