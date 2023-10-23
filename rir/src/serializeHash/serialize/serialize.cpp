#include "serialize.h"
#include "R/Printing.h"
#include "R/Protect.h"
#include "R/Symbols.h"
#include "R/disableGc.h"
#include "compiler/parameter.h"
#include "compilerClientServer/CompilerServer.h"
#include "runtime/PoolStub.h"
#include "serializeHash/globals.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/hash/hashAst.h"
#include "traceSerialize.h"
#include "utils/measuring.h"
#include <algorithm>

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

SerialOptions SerialOptions::DeepCopy{false, false, false, nullptr, SerialOptions::SourcePools()};

SerialOptions SerialOptions::CompilerServer(bool intern) {
    return SerialOptions{intern, intern, false, nullptr, SerialOptions::SourcePools()};
}

SerialOptions SerialOptions::CompilerClient(bool intern, Function* function,
                                            SEXP decompiledClosure) {
    // TODO: Fix closure stubs and then set
    //  closureEnvAndIfSetWeTryToSerializeLocalEnvsAsStubs
    return SerialOptions{intern, intern, false, nullptr, SerialOptions::SourcePools(function, decompiledClosure)};
}

SerialOptions SerialOptions::CompilerClientRetrieve{false, true, false, nullptr, SerialOptions::SourcePools()};
SerialOptions SerialOptions::SourceAndFeedback{false, true, true, nullptr, SerialOptions::SourcePools()};

unsigned pir::Parameter::RIR_SERIALIZE_CHAOS =
    getenv("RIR_SERIALIZE_CHAOS") ? strtol(getenv("RIR_SERIALIZE_CHAOS"), nullptr, 10) : 0;
bool pir::Parameter::PIR_MEASURE_SERIALIZATION =
    getenv("PIR_MEASURE_SERIALIZATION") != nullptr &&
    strtol(getenv("PIR_MEASURE_SERIALIZATION"), nullptr, 10);

SerialOptions::SourcePools::SourcePools(Function* function,
                                        SEXP decompiledClosure)
    : sourceHash(hashDecompiled(decompiledClosure)), poolSeparatorIndices(),
      map() {
    auto body = function->body();
    for (unsigned i = 0; i < body->extraPoolSize; i++) {
        map.push_back(body->getExtraPoolEntry(i));
    }
    for (unsigned defaultArgIdx = 0; defaultArgIdx < function->nargs();
         defaultArgIdx++) {
        poolSeparatorIndices.push_back(map.size());
        if (auto defaultArg = function->defaultArg(defaultArgIdx)) {
            for (unsigned i = 0; i < defaultArg->extraPoolSize; i++) {
                map.push_back(defaultArg->getExtraPoolEntry(i));
            }
        }
    }
}

bool SerialOptions::SourcePools::isStub(SEXP stub) const {
    auto rirStub = PoolStub::check(stub);
    return rirStub && rirStub->sourceHash == sourceHash;
}

bool SerialOptions::SourcePools::isEntry(SEXP entry) const {
    return map.count(entry);
}

SEXP SerialOptions::SourcePools::entry(SEXP stub) const {
    assert(isStub(stub) && "not a stub for this extra pool");
    auto index = PoolStub::unpack(stub)->index;
    auto defaultArgIdx = PoolStub::unpack(stub)->defaultArgIdx;
    auto absoluteIndex = defaultArgIdx == UINT32_MAX ? index : (index + poolSeparatorIndices[defaultArgIdx]);
    return map.at(absoluteIndex);
}

SEXP SerialOptions::SourcePools::stub(SEXP entry) const {
    assert(isEntry(entry) && "not an entry in this extra pool");
    auto absoluteIndex = (unsigned)map.at(entry);
    auto poolSeparator = std::upper_bound(poolSeparatorIndices.begin(),
                                          poolSeparatorIndices.end(), absoluteIndex);
    auto index = poolSeparator == poolSeparatorIndices.begin()
                     ? absoluteIndex
                     : absoluteIndex - *(poolSeparator - 1);
    // The `- 1` may wrap around, we want body to have index `UINT32_MAX`
    auto defaultArgIdx = std::distance(poolSeparatorIndices.begin(),
                                       poolSeparator) - 1;
    return PoolStub::create(sourceHash, defaultArgIdx, index);
}

SerialOptions SerialOptions::deserializeCompatible(AbstractDeserializer& deserializer) {
    SerialOptions options;
    options.useHashes = deserializer.readBytesOf<bool>();
    options.onlySourceAndFeedback = deserializer.readBytesOf<bool>();
    return options;
}

void SerialOptions::serializeCompatible(AbstractSerializer& serializer) const {
    serializer.writeBytesOf(useHashes);
    serializer.writeBytesOf(onlySourceAndFeedback);
}

bool SerialOptions::areCompatibleWith(const rir::SerialOptions& other) const {
    return useHashes == other.useHashes &&
           onlySourceAndFeedback == other.onlySourceAndFeedback;
}

bool SerialOptions::willReadOrWrite(const SerialFlags& flags) const {
    return
        (!onlySourceAndFeedback ||
         flags.contains(SerialFlag::InSource) ||
         flags.contains(SerialFlag::InFeedback));
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

    // If this is a stubbed pool entry, serialize the stub instead
    if (options.sourcePools.isEntry(s)) {
        s = options.sourcePools.stub(s);
    } else if (s == options.closureEnvAndIfSetWeTryToSerializeLocalEnvsAsStubs) {
        s = symbol::closureEnvStub;
    } else if (options.closureEnvAndIfSetWeTryToSerializeLocalEnvsAsStubs &&
               TYPEOF(s) == ENVSXP && !globalsSet.count(s) &&
               !R_IsPackageEnv(s) && !R_IsNamespaceEnv(s)) {
        std::cerr << "WARNING: pointerStubLocalEnvs isn't implemented, and "
                  << "we're serializing a local env: " << Print::dumpSexp(s)
                  << std::endl;
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
               !flags.contains(SerialFlag::MaybeNotRecordedCall)) {
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

#ifdef DEBUG_SERIALIZE_CONSISTENCY
static void checkFlagConsistency(const char* deserializedType,
                                 unsigned deserializedId, const SerialFlags& flags) {
    if (deserializedId != flags.id()) {
        std::cerr << "serialize/deserialize " << deserializedType
                  << " flags mismatch: " << deserializedId << "(";
        if (deserializedId + 1 < SerialFlags::ById.size()) {
            std::cerr << SerialFlags::ById[deserializedId];
        } else {
            std::cerr << "???";
        }
        std::cerr <<  ")" << " vs " << flags.id() << " (" << flags << ")" << std::endl;
        assert(false && "serialize/deserialize flags mismatch");
    }
}
#endif

void Deserializer::readBytes(void* data, size_t size, const SerialFlags& flags) {
    if (!willRead(flags)) {
        memset(data, 0, size);
        return;
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == dataBound && "serialize/deserialize data boundary mismatch");
    assert(buffer.getLong() == size && "serialize/deserialize data size mismatch");
    checkFlagConsistency("data", buffer.getInt(), flags);
#endif

    buffer.getBytes((uint8_t*)data, size);
}

int Deserializer::readInt(const SerialFlags& flags) {
    if (!willRead(flags)) {
        return 0;
    }

#if DEBUG_SERIALIZE_CONSISTENCY
    assert(buffer.getLong() == intBound && "serialize/deserialize int boundary mismatch");
    checkFlagConsistency("int", buffer.getInt(), flags);
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
    checkFlagConsistency("sexp", buffer.getInt(), flags);
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
               !flags.contains(SerialFlag::MaybeNotRecordedCall)) {
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

    // If this is a stub, deserialize the stubbed value instead
    if (options.closureEnvAndIfSetWeTryToSerializeLocalEnvsAsStubs &&
        result == symbol::closureEnvStub) {
        result = options.closureEnvAndIfSetWeTryToSerializeLocalEnvsAsStubs;
    } else if (options.sourcePools.isStub(result)) {
        result = options.sourcePools.entry(result);
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
            if (pir::Parameter::PIR_TRACE_SERIALIZATION) {
                auto oldWritePos = buffer.getWritePos();
                auto sexpPrint = Print::dumpSexp(sexp, 80);
                std::cerr << "+ serialize " << sexpPrint << std::endl;
                TraceSerializer traceSerializer(serializer);
                traceSerializer.writeInline(sexp);
                std::cerr << "+ serialized "
                          << buffer.getWritePos() - oldWritePos << " bytes, "
                          << sexpPrint << std::endl;
            } else {
                serializer.writeInline(sexp);
            }
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
            if (pir::Parameter::PIR_TRACE_SERIALIZATION) {
                auto oldReadPos = buffer.getReadPos();
                std::cerr << "- deserialize" << std::endl;
                TraceDeserializer traceDeserializer(deserializer);
                result = traceDeserializer.readInline();
                std::cerr << "- deserialized "
                          << buffer.getReadPos() - oldReadPos << " bytes, "
                          << Print::dumpSexp(result, 80) << std::endl;
            } else {
                result = deserializer.readInline();
            }

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
