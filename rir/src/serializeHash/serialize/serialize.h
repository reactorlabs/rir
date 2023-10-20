//
// Created by Jakob Hain on 6/27/23.
//

#pragma once

#include "R/r_incl.h"
#include "serializeHash/hash/UUID.h"
#include "serializeHash/serializeUni.h"
#include "utils/BimapVector.h"
#include "utils/ByteBuffer.h"

namespace rir {

struct Code;

/// Controls what data is serialized / deserialized and what format some of it
/// uses. The same options data is serialized with, it must also be deserialized
/// with.
struct SerialOptions {
    class ExtraPool {
        UUID sourceHash;
        BimapVector<SEXP> map;

      public:
        ExtraPool() : sourceHash(), map() {}
        ExtraPool(Code* codeWithPool, SEXP decompiledClosure);
        explicit operator bool() const { return (bool)sourceHash; }

        bool isEntry(SEXP entry) const;
        bool isStub(SEXP stub) const;
        SEXP entry(SEXP stub) const;
        SEXP stub(SEXP entry) const;
    };

    /// Whether to serialize connected RIR objects as UUIDs instead of their
    /// full content, besides recorded calls, which are serialized as UUIDs
    /// depending on `useHashesForRecordedCalls`.
    bool useHashes;
    /// Whether to serialize recorded calls as UUIDs instead of their full
    /// content.
    bool useHashesForRecordedCalls;
    /// Whether to only serialize source and feedback (no optimized code).
    bool onlySourceAndFeedback;
    /// Whether to skip serializing environment locks
    bool skipEnvLocks;
    /// If nonempty, we serialize the corresponding SEXPs with extra pool stubs
    ExtraPool extraPool;

    /// Don't serialize the extra pool, since we are only serializing to check
    /// compatibility and that isn't used
    static SerialOptions deserializeCompatible(AbstractDeserializer& deserializer);
    /// Don't serialize the extra pool, since we are only serializing to check
    /// compatibility and that isn't used
    void serializeCompatible(AbstractSerializer& serializer) const;
    /// Check equality of everything except the extra pool
    bool areCompatibleWith(const SerialOptions& other) const;

    bool willReadOrWrite(const SerialFlags& flags) const;

    /// Serialize everything, no hashes, environment locks
    static SerialOptions DeepCopy;
    /// Serialize everything, no hashes, no environment locks
    static SerialOptions CompilerServer(bool intern);
    /// Serialize everything, no hashes, no environment locks.
    /// Serialize and deserialize the pool entries from stubs
    static SerialOptions CompilerClient(bool intern, Code* codeWithPool,
                                        SEXP decompiledClosure);
    //  TODO: Remove both of the below
    /// Serialize everything, hashes for recorded calls, no environment locks
    static SerialOptions CompilerClientRetrieve;
    /// Serialize only source and feedback, no hashes, no environment locks
    static SerialOptions SourceAndFeedback;
};

class Serializer : public AbstractSerializer {
    /// Underlying byte buffer
    ByteBuffer& buffer;
    /// Ref table for recursively-serialized SEXPs
    SerializedRefs refs_;
    /// Controls what data is serialized and what format some of it uses. The
    /// corresponding deserializer must have the same options.
    SerialOptions options;

    SerializedRefs* refs() override { return &refs_; }

    Serializer(ByteBuffer& buffer, const SerialOptions& options)
        : buffer(buffer), refs_(), options(options) {
        options.serializeCompatible(*this);
    }
    friend void serialize(SEXP sexp, ByteBuffer& buffer,
                          const SerialOptions& options);
  public:
    bool willWrite(const SerialFlags& flags) const override;
    void writeBytes(const void *data, size_t size, const SerialFlags& flags) override;
    void writeInt(int data, const SerialFlags& flags) override;
    void write(SEXP s, const SerialFlags& flags) override;
};

class Deserializer : public AbstractDeserializer {
    /// Underlying byte buffer
    const ByteBuffer& buffer;
    /// Ref table for recursively-(de)serialized SEXPs
    DeserializedRefs refs_;
    /// Controls what data is deserialized and what format some of it uses. The
    /// corresponding serializer must have the same options.
    SerialOptions options;
    /// If set, the first rir object deserialized will use this hash
    UUID retrieveHash;

    DeserializedRefs* refs() override { return &refs_; }

    Deserializer(const ByteBuffer& buffer, const SerialOptions& options,
                 const UUID& retrieveHash = UUID())
        : buffer(buffer), refs_(), options(options),
          retrieveHash(retrieveHash) {
        auto serializedOptions = SerialOptions::deserializeCompatible(*this);
        assert(serializedOptions.areCompatibleWith(options) &&
               "serialize/deserialize options incompatible (not equal)");
    }
    friend SEXP deserialize(const ByteBuffer& sexpBuffer,
                            const SerialOptions& options,
                            const UUID& retrieveHash);
  public:
    bool willRead(const SerialFlags& flags) const override;
    void readBytes(void *data, size_t size, const SerialFlags& flags) override;
    int readInt(const SerialFlags& flags) override;
    SEXP read(const SerialFlags& flags) override;
    void addRef(SEXP sexp) override;
};

/// Serialize a SEXP (doesn't have to be RIR) into the buffer, using RIR's
/// custom serialization format.
///
/// The corresponding call to deserialize MUST have the same options.
/// Additionally, if options.useHashes is true, connected RIR objects are
/// serialized as UUIDs instead of their full content, and these SEXP MUST be
/// interned and preserved because they must be retrievable when deserialized.
void serialize(SEXP sexp, ByteBuffer& buffer, const SerialOptions& options);
/// Deserialize an SEXP (doesn't have to be RIR) from the buffer, using RIR's
/// custom serialization format.
///
/// The corresponding call to serialize MUST have had the same options.
/// Additionally, if options.useHashes is true, connected RIR objects MUST be
/// retrievable.
SEXP deserialize(const ByteBuffer& sexpBuffer, const SerialOptions& options);
/// Equivalent to
/// `deserialize(const ByteBuffer& sexpBuffer, const SerialOptions& options)`, except
/// if the hash is non-null, the first deserialized internable SEXP will be
/// interned with it before being fully deserialized. This function is
/// used/needed to support deserializing recursive hashed structures.
///
/// \see deserialize(const ByteBuffer& sexpBuffer, const SerialOptions& options)
SEXP deserialize(const ByteBuffer& sexpBuffer, const SerialOptions& options,
                 const UUID& retrieveHash);

/// Will serialize and deserialize the SEXP, returning a deep copy, using RIR's
/// custom serialization format.
SEXP copyBySerial(SEXP x);

} // namespace rir