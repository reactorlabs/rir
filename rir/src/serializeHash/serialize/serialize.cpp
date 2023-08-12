#include "serialize.h"
#include "R/Protect.h"
#include "R/disableGc.h"
#include "compiler/parameter.h"
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

unsigned pir::Parameter::RIR_SERIALIZE_CHAOS =
    getenv("RIR_SERIALIZE_CHAOS") ? strtol(getenv("RIR_SERIALIZE_CHAOS"), nullptr, 10) : 0;
bool pir::Parameter::PIR_MEASURE_SERIALIZATION =
    getenv("PIR_MEASURE_SERIALIZATION") != nullptr &&
    strtol(getenv("PIR_MEASURE_SERIALIZATION"), nullptr, 10);

void Serializer::writeBytes(const void* data, size_t size, SerialFlags flags) {
#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(dataBound);
    buffer.putLong(size);
    buffer.putLong(flags.to_i());
#endif
    buffer.putBytes((uint8_t*)data, size);
}

void Serializer::writeInt(int data, rir::SerialFlags flags) {
#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(intBound);
    buffer.putLong(flags.to_i());
#endif
    buffer.putInt(*reinterpret_cast<unsigned*>(&data));
}

void Serializer::write(SEXP s, rir::SerialFlags flags) {
#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(sexpBound);
    buffer.putLong(flags.to_i());
    auto type = TYPEOF(s);
    buffer.putInt(type);
#endif
    if (useHashes) {
        // TODO: Refactor UUIDPool methods into this (or somewhere else in
        //  serializeUni)
        UUIDPool::writeItem(s, false, buffer, true);
    } else {
        writeInline(s);
    }
#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(sexpEndBound);
    assert(type == TYPEOF(s) && "sanity check failed, SEXP changed type after serialization?");
#endif
}

void Deserializer::readBytes(void* data, size_t size, SerialFlags flags) {
#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == dataBound && "serialize/deserialize data boundary mismatch");
    assert(buffer.getLong() == size && "serialize/deserialize data size mismatch");
    assert(buffer.getLong() == flags.to_i() && "serialize/deserialize data flags mismatch");
#endif
    buffer.getBytes((uint8_t*)data, size);
}

int Deserializer::readInt(rir::SerialFlags flags) {
#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == intBound && "serialize/deserialize int boundary mismatch");
    assert(buffer.getLong() == flags.to_i() && "serialize/deserialize int flags mismatch");
#endif
    auto result = buffer.getInt();
    return *reinterpret_cast<int*>(&result);
}

SEXP Deserializer::read(SerialFlags flags) {
#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == sexpBound && "serialize/deserialize sexp boundary mismatch");
    assert(buffer.getLong() == flags.to_i() && "serialize/deserialize sexp flags mismatch");
    auto expectedType = buffer.getInt();
    SEXP result;
    if (useHashes) {
        // TODO: Refactor UUIDPool methods into this (or somewhere else in
        //  serializeUni)
        result = UUIDPool::readItem(buffer, true);
    } else {
        result = readInline();
    }
    assert(buffer.getLong() == sexpEndBound && "serialize/deserialize sexp end boundary mismatch");
    assert(expectedType == TYPEOF(result) && "serialize/deserialize sexp type mismatch");
    return result;
#else
    if (useHashes) {
        // TODO: Refactor UUIDPool methods into this (or somewhere else in
        //  serializeUni)
        return UUIDPool::readItem(buffer, true);
    } else {
        return readInline();
    }
#endif
}

void Deserializer::addRef(SEXP sexp) {
    AbstractDeserializer::addRef(sexp);
    if (retrieveHash && TYPEOF(sexp) == EXTERNALSXP) {
        UUIDPool::intern(sexp, retrieveHash, false, false);
        retrieveHash = UUID();
    }
}

void serialize(SEXP sexp, ByteBuffer& buffer, bool useHashes) {
    disableInterpreter([&]{
        disableGc([&] {
            Serializer serializer(buffer, useHashes);
            serializer.AbstractSerializer::write(sexp);
        });
    });
}

SEXP deserialize(ByteBuffer& buffer, bool useHashes) {
    return deserialize(buffer, useHashes, UUID());
}

SEXP deserialize(ByteBuffer& buffer, bool useHashes, const UUID& retrieveHash) {
    SEXP result;
    disableInterpreter([&]{
        disableGc([&] {
            Deserializer deserializer(buffer, useHashes, retrieveHash);
            result = deserializer.AbstractDeserializer::read();
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
        serialize(x, buffer, false);
        return p(deserialize(buffer, false));
    });
}

} // namespace rir
