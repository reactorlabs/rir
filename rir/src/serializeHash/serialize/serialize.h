//
// Created by Jakob Hain on 6/27/23.
//

#pragma once

#include "R/r_incl.h"
#include "serializeHash/hash/UUID.h"
#include "serializeHash/serializeUni.h"
#include "utils/ByteBuffer.h"

namespace rir {

/// Controls what data is serialized / deserialized and what format some of it
/// uses. The same options data is serialized with, it must also be deserialized
/// with.
struct SerialOptions {
    /// Whether to serialize connected RIR objects as UUIDs instead of their
    /// full content. However, recorded calls are always serialized as UUIDs.
    bool useHashes;
    /// Whether to only serialize source (no optimized code or feedback).
    bool onlySource;
    /// Whether to only serialize feedback (no optimized code or source).
    bool onlyFeedback;
    /// Whether to only serialize source and feedback (no optimized code). This
    /// is different than passing onlySource and onlyFeedback, because that
    /// would serialize data which is both source and feedback, this serializes
    /// data which is either source or feedback (negated "and" confusion). Of
    /// course, if onlySource or onlyFeedback it set, that makes
    /// onlySourceAndFeedback irrelevant.
    bool onlySourceAndFeedback;

    /// Serialize everything, without hashes
    static SerialOptions DeepCopy;
    /// Serialize everything, with hashes
    static SerialOptions CompilerServer;
    /// Serialize everything, without hashes
    /// TODO: use hashes or something because this is probably too much
    ///  unnecessary data again
    static SerialOptions CompilerClientRetrieve;
    /// Serialize only source and feedback, without hashes
    static SerialOptions CompilerClientSourceAndFeedback;
    /// Serialize only source, without hashes
    static SerialOptions CompilerClientSource;
    /// Serialize only feedback, without hashes
    static SerialOptions CompilerClientFeedback;
};

class Serializer : public AbstractSerializer {
    /// Underlying byte buffer
    ByteBuffer& buffer;
    /// Ref table for recursively-serialized SEXPs
    SerializedRefs refs_;
    /// Controls what data is serialized and what format some of it uses. The
    /// corresponding deserializer must have the same options.
    SerialOptions options;

    Serializer(ByteBuffer& buffer, SerialOptions options)
        : buffer(buffer), refs_(), options(options) {}
    SerializedRefs* refs() override { return &refs_; }

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
    ByteBuffer& buffer;
    /// Ref table for recursively-(de)serialized SEXPs
    DeserializedRefs refs_;
    /// Controls what data is deserialized and what format some of it uses. The
    /// corresponding serializer must have the same options.
    SerialOptions options;
    /// If set, the first rir object deserialized will use this hash
    UUID retrieveHash;

    Deserializer(ByteBuffer& buffer, SerialOptions options,
                 const UUID& retrieveHash = UUID())
        : buffer(buffer), refs_(), options(options),
          retrieveHash(retrieveHash) {}
    DeserializedRefs* refs() override { return &refs_; }

    friend SEXP deserialize(ByteBuffer& sexpBuffer,
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
SEXP deserialize(ByteBuffer& sexpBuffer, const SerialOptions& options);
/// Equivalent to
/// `deserialize(ByteBuffer& sexpBuffer, const SerialOptions& options)`, except
/// if the hash is non-null, the first deserialized internable SEXP will be
/// interned with it before being fully deserialized. This function is
/// used/needed to support deserializing recursive hashed structures.
///
/// @see deserialize(ByteBuffer& sexpBuffer, const SerialOptions& options)
SEXP deserialize(ByteBuffer& sexpBuffer, const SerialOptions& options,
                 const UUID& retrieveHash);

/// Will serialize and deserialize the SEXP, returning a deep copy, using RIR's
/// custom serialization format.
SEXP copyBySerial(SEXP x);

} // namespace rir