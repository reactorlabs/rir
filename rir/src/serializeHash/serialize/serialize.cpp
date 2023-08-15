#include "serialize.h"
#include "R/Protect.h"
#include "R/disableGc.h"
#include "compiler/parameter.h"
#include "compilerClientServer/CompilerServer.h"
#include "serializeHash/hash/UUIDPool.h"
#include "utils/measuring.h"

#define DEBUG_SERIALIZE_CONSISTENCY 1

namespace rir {

#if DEBUG_SERIALIZE_CONSISTENCY
static const uint64_t sexpBound = 0x123456789abcdef0;
static const uint64_t sexpEndBound = 0x123456789abcdef1;
static const uint64_t dataBound = 0xfedcba9876543210;
static const uint64_t intBound = 0xfedcba9876543211;
#endif

SerialOptions SerialOptions::DeepCopy{false, false, false, false, false};
SerialOptions SerialOptions::CompilerServer{true, false, false, false, true};
SerialOptions SerialOptions::CompilerClientRetrieve{false, false, false, false, true};
SerialOptions SerialOptions::CompilerClientSourceAndFeedback{false, false, false, true, true};
SerialOptions SerialOptions::CompilerClientSource{false, true, false, false, true};
SerialOptions SerialOptions::CompilerClientFeedback{false, false, true, false, true};

unsigned pir::Parameter::RIR_SERIALIZE_CHAOS =
    getenv("RIR_SERIALIZE_CHAOS") ? strtol(getenv("RIR_SERIALIZE_CHAOS"), nullptr, 10) : 0;
bool pir::Parameter::PIR_MEASURE_SERIALIZATION =
    getenv("PIR_MEASURE_SERIALIZATION") != nullptr &&
    strtol(getenv("PIR_MEASURE_SERIALIZATION"), nullptr, 10);

static bool shouldSkip(const SerialOptions& options, const SerialFlags& flags) {
    return
        (options.onlySource && !flags.contains(SerialFlag::InSource)) ||
        (options.onlyFeedback && !flags.contains(SerialFlag::InFeedback)) ||
        (options.onlySourceAndFeedback &&
         !flags.contains(SerialFlag::InSource) &&
         !flags.contains(SerialFlag::InFeedback)) ||
        (options.skipEnvLocks && !flags.contains(SerialFlag::NotEnvLock));
}

bool Serializer::willWrite(const rir::SerialFlags& flags) const {
    return !shouldSkip(options, flags);
}

void Serializer::writeBytes(const void* data, size_t size,
                            const SerialFlags& flags) {
    if (shouldSkip(options, flags)) {
        return;
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(dataBound);
    buffer.putLong(size);
    buffer.putInt(flags.id());
#endif

    buffer.putBytes((uint8_t*)data, size);
}

void Serializer::writeInt(int data, const SerialFlags& flags) {
    if (shouldSkip(options, flags)) {
        return;
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(intBound);
    buffer.putInt(flags.id());
#endif

    buffer.putInt(*reinterpret_cast<unsigned*>(&data));
}

void Serializer::write(SEXP s, const SerialFlags& flags) {
    assert(flags.contains(SerialFlag::MaybeSexp) &&
           "Serializing non SEXP with SEXP flag");

    if (shouldSkip(options, flags)) {
        return;
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(sexpBound);
    buffer.putInt(flags.id());
    auto type = TYPEOF(s);
    buffer.putInt(type);
#endif

    // If `useHashes` or this is a recorded call, either serialize via hash or
    // (if this can't be serialized via hash) serialize children via hash.
    // Otherwise serialize children regularly. If this is a recorded call and
    // `useHashes` is false, we have to construct a different serializer where
    // `useHashes` is true, but if `useHashes` is true we can use this one.
    // Either way we must call `writeInline` if we didn't write the hash
    // directly to not infinitely recurse.
    // TODO: Refactor UUIDPool methods into this (or somewhere else in
    //  serialize or serializeUni) and use separate readItem for recorded calls
    //  which may be may be null instead of just allowing null on the compiler
    //  server
    if (options.useHashes) {
        if (!UUIDPool::tryWriteHash(s, buffer)) {
            writeInline(s);
        }
    } else if (flags.contains(SerialFlag::MaybeNotRecordedCall)) {
        if (!UUIDPool::tryWriteHash(s, buffer)) {
            // Still serialize children via hashes
            auto innerOptions = options;
            innerOptions.useHashes = true;
            Serializer innerSerializer(buffer, innerOptions);
            innerSerializer.writeInline(s);
        }
    } else {
        writeInline(s);
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(sexpEndBound);
    assert(type == TYPEOF(s) && "sanity check failed, SEXP changed type after serialization?");
#endif
}

bool Deserializer::willRead(const rir::SerialFlags& flags) const {
    return !shouldSkip(options, flags);
}

void Deserializer::readBytes(void* data, size_t size, const SerialFlags& flags) {
    if (shouldSkip(options, flags)) {
        // TODO: Allow default data
        memset(data, 0, size);
        return;
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == dataBound && "serialize/deserialize data boundary mismatch");
    assert(buffer.getLong() == size && "serialize/deserialize data size mismatch");
    assert(buffer.getInt() == flags.id() && "serialize/deserialize data flags mismatch");
#endif

    buffer.getBytes((uint8_t*)data, size);
}

int Deserializer::readInt(const SerialFlags& flags) {
    if (shouldSkip(options, flags)) {
        // TODO: Allow default data
        return 0;
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == intBound && "serialize/deserialize int boundary mismatch");
    assert(buffer.getInt() == flags.id() && "serialize/deserialize int flags mismatch");
#endif

    auto result = buffer.getInt();
    return *reinterpret_cast<int*>(&result);
}

SEXP Deserializer::read(const SerialFlags& flags) {
    assert(flags.contains(SerialFlag::MaybeSexp) &&
           "Deserializing non SEXP with SEXP flag");

    if (shouldSkip(options, flags)) {
        return nullptr;
    }

    SEXP result;

#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == sexpBound &&
           "serialize/deserialize sexp boundary mismatch");
    assert(buffer.getInt() == flags.id() &&
           "serialize/deserialize sexp flags mismatch");
    auto expectedType = buffer.getInt();
#endif

    // If `useHashes` or this is a recorded call, either deserialize via hash or
    // (if this wasn't serialized via hash) deserialize children via hash.
    // Otherwise deserialize children regularly. If this is a recorded call and
    // `useHashes` is false, we have to construct a different deserializer where
    // `useHashes` is true, but if `useHashes` is true we can use this one.
    // Either way we must call `readInline` if we didn't read the hash directly
    // to not infinitely recurse.
    // TODO: Refactor UUIDPool methods into this (or somewhere else in
    //  serialize or serializeUni) and use separate readItem for recorded calls
    //  which may be may be null instead of just allowing null on the compiler
    //  server
    if (options.useHashes) {
        result = UUIDPool::tryReadHash(buffer);
        if (!result) {
            result = readInline();
        }
    } else if (flags.contains(SerialFlag::MaybeNotRecordedCall)) {
        result = UUIDPool::tryReadHash(buffer);
        if (!result) {
            // Still deserialize children via hashes
            auto innerOptions = options;
            innerOptions.useHashes = true;
            Deserializer innerDeserializer(buffer, innerOptions);
            result = innerDeserializer.readInline();
        }
    } else {
        result = readInline();
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == sexpEndBound &&
           "serialize/deserialize sexp end boundary mismatch");
    assert(expectedType == TYPEOF(result) &&
           "serialize/deserialize sexp type mismatch");
#endif

    return result;
}

void Deserializer::addRef(SEXP sexp) {
    AbstractDeserializer::addRef(sexp);
    if (retrieveHash && TYPEOF(sexp) == EXTERNALSXP) {
        // TODO: A bit hachy that we hardcode preserve to if the compiler server
        //  is running
        UUIDPool::intern(sexp, retrieveHash, CompilerServer::isRunning(), false);
        retrieveHash = UUID();
    }
}

void serialize(SEXP sexp, ByteBuffer& buffer, const SerialOptions& options) {
    disableInterpreter([&]{
        disableGc([&] {
            Serializer serializer(buffer, options);
            serializer.writeBytesOf(options);
            serializer.writeInline(sexp);
        });
    });
}

SEXP deserialize(ByteBuffer& buffer, const SerialOptions& options) {
    return deserialize(buffer, options, UUID());
}

SEXP deserialize(ByteBuffer& buffer, const SerialOptions& options,
                 const UUID& retrieveHash) {
    SEXP result;
    disableInterpreter([&]{
        disableGc([&] {
            Deserializer deserializer(buffer, options, retrieveHash);
            auto serializedOptions = deserializer.readBytesOf<SerialOptions>();
            assert(serializedOptions == options && "serialize/deserialize options mismatch");
            result = deserializer.readInline();
        });
    });
    return result;
}

SEXP copyBySerial(SEXP x) {
    if (!pir::Parameter::RIR_SERIALIZE_CHAOS)
        return x;

    return Measuring::timeEventIf2(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serialize.cpp: copyBySerial", x, [&]{
        Protect p(x);
        ByteBuffer buffer;
        serialize(x, buffer, SerialOptions::DeepCopy);
        return p(deserialize(buffer, SerialOptions::DeepCopy));
    });
}

} // namespace rir
