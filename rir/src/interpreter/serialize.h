//
// Created by Jakob Hain on 6/27/23.
//

#pragma once

#include "R/r_incl.h"
#include "hash/UUID.h"
#include "utils/ByteBuffer.h"
#include <queue>

namespace rir {

/// Function passed to GNU-R, use `serialize` instead
void serializeRir(SEXP s, SEXP refTable, R_outpstream_t out);
/// Function passed to GNU-R, use `deserialize` instead
SEXP deserializeRir(SEXP refTable, R_inpstream_t inp);
/// Will serialize and deserialize the SEXP, returning a deep copy.
SEXP copyBySerial(SEXP x);

/// An output stream which simply discards its output
R_outpstream_st nullOutputStream();

/// Hash an SEXP (doesn't have to be RIR) into a UUID, by serializing it but
/// XORing the bits instead of collecting them, and add connected RIR object
/// containers to the worklist.
UUID hashSexp(SEXP sexp, std::queue<SEXP>& worklist);
/// Hash an SEXP (doesn't have to be RIR) into a UUID, by serializing it but
/// XORing the bits instead of collecting them.
UUID hashSexp(SEXP sexp);
/// Hash an SEXP (doesn't have to be RIR) into the hasher, by serializing it but
/// XORing the bits instead of collecting them, and add connected RIR object
/// containers to the worklist.
///
/// @see hashSexp(SEXP sexp, UUIDHasher& hasher)
void hashSexp(SEXP sexp, UUIDHasher& hasher, std::queue<SEXP>& worklist);
/// Hash an SEXP (doesn't have to be RIR) into the hasher, by serializing it but
/// XORing the bits instead of collecting them.
///
/// It's specifically important that the compiler-client request hash contains
/// parts of the SEXP, like feedback, which we DON'T get by calling hashSexp.
/// This is because we use hashSexp for interning and we don't want interned
/// SEXPs to change hash, but when the request SEXP changes, we genuinely want
/// it to alter the response. We really need to look over what is mutable and
/// what isn't, and how we are going to do different kinds of hashing for
/// different purposes.
void hashSexp(SEXP sexp, UUIDHasher& hasher);
/// Serialize a SEXP (doesn't have to be RIR) into the buffer.
///
/// If useHashes is true, connected RIR objects are serialized as UUIDs instead
/// of their full content, with a "server UUID" to denote where to find them.
/// The corresponding call to deserialize MUST be done with `useHashes=true` as
/// well, AND the SEXP must have already been recursively interned and
/// preserved.
void serialize(SEXP sexp, ByteBuffer& buffer, bool useHashes);
/// Deserialize an SEXP (doesn't have to be RIR) from the buffer
///
/// If useHashes is true, connected RIR objects are deserialized from UUIDs
/// and an attached "peer UUID" instead of their full content, and retrieved
/// from the UUIDPool. If the UUIDs aren't in the pool, this sends a request to
/// the peer with the "peer UUID" (also in the deserialized data), and fails if
/// the peer isn't connected or we can't get a response. The corresponding call
/// to serialize MUST have been done with `useHashes=true` as well.
SEXP deserialize(ByteBuffer& sexpBuffer, bool useHashes);

/// Whether to use hashes when serializing in the current stream
bool useHashes(R_outpstream_t out);
/// Whether to use hashes when deserializing in the current stream
bool useHashes(R_inpstream_t in);
/// If true we're hashing, otherwise we're actually serializing
bool isHashing(R_outpstream_t out);
/// Worklist for the current stream
std::queue<SEXP>* worklist(R_outpstream_t out);

} // namespace rir