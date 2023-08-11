#include "serialize.h"
#include "R/Protect.h"
#include "R/disableGc.h"
#include "compiler/parameter.h"
#include "serializeHash/hash/UUIDPool.h"
#include "utils/measuring.h"

namespace rir {

unsigned pir::Parameter::RIR_SERIALIZE_CHAOS =
    getenv("RIR_SERIALIZE_CHAOS") ? strtol(getenv("RIR_SERIALIZE_CHAOS"), nullptr, 10) : 0;
bool pir::Parameter::PIR_MEASURE_SERIALIZATION =
    getenv("PIR_MEASURE_SERIALIZATION") != nullptr &&
    strtol(getenv("PIR_MEASURE_SERIALIZATION"), nullptr, 10);

void Serializer::writeBytes(const void* data, size_t size, SerialFlags flags) {
    buffer.putBytes((uint8_t*)data, size);
}

void Serializer::writeInt(int data, rir::SerialFlags flags) {
    buffer.putInt(*reinterpret_cast<unsigned*>(&data));
}

void Serializer::write(SEXP s, rir::SerialFlags flags) {
    if (useHashes) {
        // TODO: Refactor UUIDPool methods into this (or somewhere else in
        //  serializeUni)
        UUIDPool::writeItem(s, false, buffer, true);
    } else {
        writeInline(s);
    }
}

void Deserializer::readBytes(void* data, size_t size, SerialFlags flags) {
    buffer.getBytes((uint8_t*)data, size);
}

int Deserializer::readInt(rir::SerialFlags flags) {
    auto result = buffer.getInt();
    return *reinterpret_cast<int*>(&result);
}

SEXP Deserializer::read(SerialFlags flags) {
    if (useHashes) {
        // TODO: Refactor UUIDPool methods into this (or somewhere else in
        //  serializeUni)
        return UUIDPool::readItem(buffer, true);
    } else {
        return readInline();
    }
}

void Deserializer::addRef(SEXP sexp) {
    AbstractDeserializer::addRef(sexp);
    if (retrieveHash && TYPEOF(sexp) == EXTERNALSXP) {
        UUIDPool::intern(sexp, retrieveHash, false, false);
        retrieveHash = UUID();
    }
}

void serialize(SEXP sexp, ByteBuffer& buffer, bool useHashes) {
    disableGc([&] {
        Serializer serializer(buffer, useHashes);
        serializer.AbstractSerializer::write(sexp);
    });
}

SEXP deserialize(ByteBuffer& buffer, bool useHashes) {
    return deserialize(buffer, useHashes, UUID());
}

SEXP deserialize(ByteBuffer& buffer, bool useHashes, const UUID& retrieveHash) {
    return disableGc2([&] {
        Deserializer deserializer(buffer, useHashes, retrieveHash);
        return deserializer.AbstractDeserializer::read();
    });
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
