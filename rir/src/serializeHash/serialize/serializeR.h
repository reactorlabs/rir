//
// Created by Jakob Hain on 6/27/23.
//

#pragma once

#include "R/r_incl.h"
#include "serializeHash/hash/UUID.h"
#include "utils/ByteBuffer.h"

namespace rir {

class ConnectedWorklist;

/// Function passed to GNU-R, use `serialize` instead
void rirSerializeHook(SEXP s, SEXP refTable, R_outpstream_t out);
/// Function passed to GNU-R, use `deserialize` instead
SEXP rirDeserializeHook(SEXP refTable, R_inpstream_t inp);
/// Will serialize and deserialize the SEXP, returning a deep copy, using R's
/// serialization format.
SEXP copyBySerialR(SEXP x);

/// Serialize a SEXP (doesn't have to be RIR) into the buffer, using R's
/// serialization format.
///
/// If useHashes is true, connected RIR objects are serialized as UUIDs
/// instead of their full content. The corresponding call to deserialize MUST be
/// done with `useHashes=true` as well, AND the SEXP must have already been
/// recursively interned and preserved.
void serializeR(SEXP sexp, ByteBuffer& buffer, bool useHashes);
/// Deserialize an SEXP (doesn't have to be RIR) from the buffer, using R's
/// serialization format.
///
/// If useHashes is true, connected RIR objects are deserialized from UUIDs
/// and retrieved from the UUIDPool. If the UUIDs aren't in the pool, this
/// sends a request to compiler peer, and fails if it isn't connected or we
/// can't get a response. The corresponding call to serialize MUST have been
/// done with `useHashes=true` as well.
SEXP deserializeR(ByteBuffer& sexpBuffer, bool useHashes);
/// Equivalent to `deserializeR(ByteBuffer& sexpBuffer, bool useHashes)`, except
/// the first deserialized internable SEXP will also be interned with that hash
/// before being fully deserialized. This function is used/needed to support
/// deserializing recursive hashed structures.
///
/// @see deserialize(ByteBuffer& sexpBuffer, bool useHashes)
SEXP deserializeR(ByteBuffer& sexpBuffer, bool useHashes, const UUID& retrieveHash);

/// Whether to use hashes when serializing in the current stream
bool useHashes(R_outpstream_t out);
/// Whether to use hashes when deserializing in the current stream
bool useHashes(R_inpstream_t in);
/// If `retrieveHash` is set, interns SEXP with it and unsets it.
void useRetrieveHashIfSet(R_inpstream_t inp, SEXP sexp);

} // namespace rir