//
// Created by Jakob Hain on 6/27/23.
//

#pragma once

#include "R/r_incl.h"
#include "serializeHash/hash/UUID.h"
#include "serializeHash/serializeUni.h"
#include "utils/ByteBuffer.h"

namespace rir {

class Serializer : public AbstractSerializer {
    /// Underlying byte buffer
    ByteBuffer& buffer;
    /// Ref table for recursively-serialized SEXPs
    SerializedRefs refs_;
    /// Whether to serialize connected RIR objects as UUIDs instead of their
    /// full content
    bool useHashes;

    Serializer(ByteBuffer& buffer, bool useHashes)
        : buffer(buffer), refs_(), useHashes(useHashes) {}
    SerializedRefs* refs() override { return &refs_; }

    friend void serialize(SEXP sexp, ByteBuffer& buffer, bool useHashes);
  public:
    void writeBytes(const void *data, size_t size, SerialFlags flags) override;
    void writeInt(int data, SerialFlags flags) override;
    void write(SEXP s, SerialFlags flags) override;
};

class Deserializer : public AbstractDeserializer {
    /// Underlying byte buffer
    ByteBuffer& buffer;
    /// Ref table for recursively-(de)serialized SEXPs
    DeserializedRefs refs_;
    /// Whether to deserialize connected RIR objects from UUIDs instead of their
    /// full content
    bool useHashes;
    /// If set, the first rir SEXP deserialized will assume this hash
    UUID retrieveHash;

    Deserializer(ByteBuffer& buffer, bool useHashes, const UUID& retrieveHash)
        : buffer(buffer), refs_(), useHashes(useHashes),
          retrieveHash(retrieveHash) {}
    DeserializedRefs* refs() override { return &refs_; }

    friend SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes,
                            const UUID& retrieveHash);
  public:
    void readBytes(void *data, size_t size, SerialFlags flags) override;
    int readInt(SerialFlags flags) override;
    SEXP read(SerialFlags flags) override;
    void addRef(SEXP sexp) override;
};

/// Serialize a SEXP (doesn't have to be RIR) into the buffer, using RIR's
/// custom serialization format.
///
/// If useHashes is true, connected RIR objects are serialized as UUIDs
/// instead of their full content. The corresponding call to deserialize MUST be
/// done with `useHashes=true` as well, AND the SEXP must have already been
/// recursively interned and preserved.
void serialize(SEXP sexp, ByteBuffer& buffer, bool useHashes);
/// Deserialize an SEXP (doesn't have to be RIR) from the buffer, using RIR's
/// custom serialization format.
///
/// If useHashes is true, connected RIR objects are deserialized from UUIDs
/// and retrieved from the UUIDPool. If the UUIDs aren't in the pool, this
/// sends a request to compiler server, and fails if it isn't connected or we
/// can't get a response. The corresponding call to serialize MUST have been
/// done with `useHashes=true` as well.
SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes);
/// Equivalent to `deserialize(ByteBuffer& sexpBuffer, bool useHashes)`, except
/// if the hash is non-null, the first deserialized internable SEXP will be
/// interned with it before being fully deserialized. This function is
/// used/needed to support deserializing recursive hashed structures.
///
/// @see deserialize(ByteBuffer& sexpBuffer, bool useHashes)
SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes, const UUID& retrieveHash);

/// Will serialize and deserialize the SEXP, returning a deep copy, using RIR's
/// custom serialization format.
SEXP copyBySerial(SEXP x);

} // namespace rir