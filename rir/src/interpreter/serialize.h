//
// Created by Jakob Hain on 6/27/23.
//

#pragma once

#include "R/r_incl.h"
#include "hash/RirUID.h"
#include "utils/ByteBuffer.h"

namespace rir {

class ConnectedWorklist;

/// Function passed to GNU-R, use `serialize` instead
void serializeRir(SEXP s, SEXP refTable, R_outpstream_t out);
/// Function passed to GNU-R, use `deserialize` instead
SEXP deserializeRir(SEXP refTable, R_inpstream_t inp);
/// Will serialize and deserialize the SEXP, returning a deep copy.
SEXP copyBySerial(SEXP x);

/// An output stream which simply discards its output
R_outpstream_st nullOutputStream();

/// Hash the semantics-altering mutable parts of an SEXP.
UUID smallHashSexp(SEXP sexp);
/// Hash an SEXP (doesn't have to be RIR) into a RirUID, by serializing it but
/// EVP-MD hashing ("fancy XOR"-ing) the bits instead of collecting them, and
/// add connected RIR object containers to the worklist.
RirUID hashSexp(SEXP sexp, ConnectedWorklist& connected);
/// Hash an SEXP (doesn't have to be RIR) into a RirUID, by serializing it but
/// EVP-MD hashing ("fancy XOR"-ing) the bits instead of collecting them.
RirUID hashSexp(SEXP sexp);
/// Serialize a SEXP (doesn't have to be RIR) into the buffer.
///
/// If useHashes is true, connected RIR objects are serialized as RirUIDs
/// instead of their full content. The corresponding call to deserialize MUST be
/// done with `useHashes=true` as well, AND the SEXP must have already been
/// recursively interned and preserved.
void serialize(SEXP sexp, ByteBuffer& buffer, bool useHashes);
/// Deserialize an SEXP (doesn't have to be RIR) from the buffer
///
/// If useHashes is true, connected RIR objects are deserialized from RirUIDs
/// and retrieved from the RirUIDPool. If the RirUIDs aren't in the pool, this
/// sends a request to compiler server, and fails if it isn't connected or we
/// can't get a response. The corresponding call to serialize MUST have been
/// done with `useHashes=true` as well.
SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes);
/// Equivalent to `deserialize(ByteBuffer& sexpBuffer, bool useHashes)`, except
/// the first deserialized internable SEXP will also be interned with that hash
/// before being fully deserialized. This function is used/needed to support
/// deserializing recursive hashed structures.
///
/// @see deserialize(ByteBuffer& sexpBuffer, bool useHashes)
SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes, const RirUID& retrieveHash);

/// Whether to use hashes when serializing in the current stream
bool useHashes(R_outpstream_t out);
/// Whether to use hashes when deserializing in the current stream
bool useHashes(R_inpstream_t in);
/// If true we're hashing, otherwise we're actually serializing
bool isHashing(R_outpstream_t out);
/// If true we're hashing, and only hashing the immutable parts of an SEXP
bool isOnlyBigHashing(R_outpstream_t out);
/// If true we're hashing, and only hashing the semantics-altering mutable parts
/// of an SEXP
bool isOnlySmallHashing(R_outpstream_t out);
/// Connected worklist for the current stream, or `nullptr` if there is none
ConnectedWorklist* connected(R_outpstream_t out);
/// If `retrieveHash` is set, interns SEXP with it and unsets it.
void useRetrieveHashIfSet(R_inpstream_t inp, SEXP sexp);

} // namespace rir