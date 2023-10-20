#include "serialize.h"
#include "R/Protect.h"
#include "R/disableGc.h"
#include "compiler/parameter.h"
#include "compilerClientServer/CompilerServer.h"
#include "runtime/Code.h"
#include "runtime/ExtraPoolStub.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/hash/hashAst.h"
#include "utils/measuring.h"

/// This adds padding to each serialize call, but immediately raises an
/// assertion failure when a deserialize call deserializes a region which was
/// not serialized with the same type/size and flags in a serialize call.
///
/// Regardless of whether this is enabled, we always serialize and check options
/// because that is cheap (only a few bytes at the start of serializing the
/// root, whereas this adds padding to each child and even non-SEXP fields)
#define DEBUG_SERIALIZE_CONSISTENCY 1

namespace rir {

#if DEBUG_SERIALIZE_CONSISTENCY
static const uint64_t sexpBound = 0x123456789abcdef0;
static const uint64_t sexpEndBound = 0x123456789abcdef1;
static const uint64_t dataBound = 0xfedcba9876543210;
static const uint64_t intBound = 0xfedcba9876543211;
#endif

SerialOptions SerialOptions::DeepCopy{false, false, false, false, SerialOptions::ExtraPool()};

SerialOptions SerialOptions::CompilerServer(bool intern) {
    return SerialOptions{intern, intern, false, true, SerialOptions::ExtraPool()};
}

SerialOptions SerialOptions::CompilerClient(bool intern, Code* codeWithPool, SEXP decompiledClosure) {
    return SerialOptions{intern, intern, false, true, SerialOptions::ExtraPool(codeWithPool, decompiledClosure)};
}

SerialOptions SerialOptions::CompilerClientRetrieve{false, true, false, true, SerialOptions::ExtraPool()};
SerialOptions SerialOptions::SourceAndFeedback{false, true, true, true, SerialOptions::ExtraPool()};

unsigned pir::Parameter::RIR_SERIALIZE_CHAOS =
    getenv("RIR_SERIALIZE_CHAOS") ? strtol(getenv("RIR_SERIALIZE_CHAOS"), nullptr, 10) : 0;
bool pir::Parameter::PIR_MEASURE_SERIALIZATION =
    getenv("PIR_MEASURE_SERIALIZATION") != nullptr &&
    strtol(getenv("PIR_MEASURE_SERIALIZATION"), nullptr, 10);

SerialOptions::ExtraPool::ExtraPool(Code* codeWithPool, SEXP decompiledClosure)
    : sourceHash(hashDecompiled(decompiledClosure)), map() {
    for (unsigned i = 0; i < codeWithPool->extraPoolSize; i++) {
        map.push_back(codeWithPool->getExtraPoolEntry(i));
    }
}

bool SerialOptions::ExtraPool::isStub(SEXP stub) const {
    auto rirStub = ExtraPoolStub::check(stub);
    return rirStub && rirStub->sourceHash == sourceHash;
}

bool SerialOptions::ExtraPool::isEntry(SEXP entry) const {
    return map.count(entry);
}

SEXP SerialOptions::ExtraPool::entry(SEXP stub) const {
    assert(isStub(stub) && "not a stub for this extra pool");
    return map.at(ExtraPoolStub::unpack(stub)->index);
}

SEXP SerialOptions::ExtraPool::stub(SEXP entry) const {
    assert(isEntry(entry) && "not an entry in this extra pool");
    return ExtraPoolStub::create(sourceHash, map.at(entry));
}

SerialOptions SerialOptions::deserializeCompatible(AbstractDeserializer& deserializer) {
    SerialOptions options;
    options.useHashes = deserializer.readBytesOf<bool>();
    options.onlySourceAndFeedback = deserializer.readBytesOf<bool>();
    options.skipEnvLocks = deserializer.readBytesOf<bool>();
    return options;
}

void SerialOptions::serializeCompatible(AbstractSerializer& serializer) const {
    serializer.writeBytesOf(useHashes);
    serializer.writeBytesOf(onlySourceAndFeedback);
    serializer.writeBytesOf(skipEnvLocks);
}

bool SerialOptions::areCompatibleWith(const rir::SerialOptions& other) const {
    return useHashes == other.useHashes &&
           onlySourceAndFeedback == other.onlySourceAndFeedback &&
           skipEnvLocks == other.skipEnvLocks;
}

bool SerialOptions::willReadOrWrite(const SerialFlags& flags) const {
    return
        (!onlySourceAndFeedback ||
         flags.contains(SerialFlag::InSource) ||
         flags.contains(SerialFlag::InFeedback)) &&
        (!skipEnvLocks || flags.contains(SerialFlag::NotEnvLock));
}

bool Serializer::willWrite(const rir::SerialFlags& flags) const {
    return options.willReadOrWrite(flags);
}

void Serializer::writeBytes(const void* data, size_t size,
                            const SerialFlags& flags) {
    if (!willWrite(flags)) {
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
    if (!willWrite(flags)) {
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

    if (!willWrite(flags)) {
        return;
    }

    // If this is a stubbed extra pool entry, serialize the stub instead
    if (options.extraPool.isEntry(s)) {
        s = options.extraPool.stub(s);
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    buffer.putLong(sexpBound);
    buffer.putInt(flags.id());
    auto type = TYPEOF(s);
    buffer.putInt(type);
#endif

    // If `useHashes` or `useHashesForRecordedCalls` depending on flags, either
    // serialize via hash or (if this can't be serialized via hash) serialize
    // children via hash. Otherwise serialize children regularly. If this is a
    // recorded call, `useHashesForRecordedCalls` is ture, and `useHashes` is
    // false, we have to construct a different serializer where `useHashes` is
    // true, but if `useHashes` is true we can use this one. Either way we must
    // call `writeInline` if we didn't write the hash directly to not infinitely
    // recurse.
    if (options.useHashes) {
        if (!UUIDPool::tryWriteHash(s, buffer)) {
            writeInline(s);
        }
    } else if (options.useHashesForRecordedCalls &&
               flags.contains(SerialFlag::MaybeNotRecordedCall)) {
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
    return options.willReadOrWrite(flags);
}

void Deserializer::readBytes(void* data, size_t size, const SerialFlags& flags) {
    if (!willRead(flags)) {
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
    if (!willRead(flags)) {
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

    if (!willRead(flags)) {
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

    // If `useHashes` or `useHashesForRecordedCalls` depending on flags, either
    // deserialize via hash or (if this wasn't serialized via hash) deserialize
    // children via hash. Otherwise deserialize children regularly. If this is a
    // recorded call, `useHashesForRecordedCalls` is true, and `useHashes` is
    // false, we have to construct a different deserializer where `useHashes` is
    // true, but if `useHashes` is true we can use this one. Either way we must
    // call `readInline` if we didn't read the hash directly to not infinitely
    // recurse.
    if (options.useHashes) {
        result = UUIDPool::tryReadHash(buffer);
        if (!result) {
            result = readInline();
        }
    } else if (options.useHashesForRecordedCalls &&
               flags.contains(SerialFlag::MaybeNotRecordedCall)) {
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

    // If this is a stubbed extra pool entry, deserialize the stub instead
    if (options.extraPool.isStub(result)) {
        result = options.extraPool.entry(result);
    }

    return result;
}

void Deserializer::addRef(SEXP sexp) {
    AbstractDeserializer::addRef(sexp);
    if (retrieveHash && UUIDPool::internable(sexp)) {
        // TODO: Hacky that we hardcode preserve to whether the compiler server
        //  is running
        UUIDPool::intern(sexp, retrieveHash, CompilerServer::isRunning(), false);
        retrieveHash = UUID();
    }
}

void serialize(SEXP sexp, ByteBuffer& buffer, const SerialOptions& options) {
    disableInterpreter([&]{
        disableGc([&] {
            Serializer serializer(buffer, options);
            serializer.writeInline(sexp);
        });
    });
}

SEXP deserialize(const ByteBuffer& buffer, const SerialOptions& options) {
    return deserialize(buffer, options, UUID());
}

SEXP deserialize(const ByteBuffer& buffer, const SerialOptions& options,
                 const UUID& retrieveHash) {
    SEXP result;
    disableInterpreter([&]{
        disableGc([&] {
            Deserializer deserializer(buffer, options, retrieveHash);
            result = deserializer.readInline();

            assert(!deserializer.retrieveHash && "retrieve hash not filled");
            assert((!retrieveHash || UUIDPool::getHash(result) == retrieveHash) &&
                   "deserialized SEXP not given retrieve hash");
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
