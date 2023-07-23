//
// Created by Jakob Hain on 7/21/23.
//

#pragma once

#include "R/r_incl.h"
#include "UUID.h"
#include <unordered_set>
#include <queue>

namespace rir {

class ConnectedWorklist {
    std::unordered_set<SEXP> seen;
    std::queue<SEXP> worklist;

    friend class Hasher;
    void insert(SEXP e);
  public:
    SEXP pop();
};

class Hasher {
    UUID::Hasher& hasher;
    std::queue<SEXP>& worklist;
    ConnectedWorklist* connected;

    Hasher(UUID::Hasher& hasher, std::queue<SEXP>& worklist,
           ConnectedWorklist* connected)
        : hasher(hasher), worklist(worklist), connected(connected) {}

    friend void hashRoot(SEXP root, UUID::Hasher& uuidHasher,
                         ConnectedWorklist* connected);
  public:
    template<typename T> void hashBytesOf(T c) {
        hasher.hashBytesOf(c);
    }
    void hashBytesOfCString(const char* c) {
        hasher.hashBytesOfCString(c);
    }
    void hashBytes(const void* data, size_t size) {
        hasher.hashBytes(data, size);
    }
    /// Add connected SEXP without hashing
    void addConnected(SEXP s);
    void hash(SEXP s);
    void hashConstant(unsigned idx);
    void hashSrc(unsigned idx);
    void hashNullable(SEXP s) {
        hashBytesOf<bool>(s != nullptr);
        if (s) {
            hash(s);
        }
    }
};

/// Hash an SEXP (doesn't have to be RIR) into a UUID, by serializing it but
/// EVP-MD hashing ("fancy XOR"-ing) the bits instead of collecting them, and
/// add connected RIR object containers to the worklist.
void hashRoot(SEXP root, UUID::Hasher& uuidHasher,
              ConnectedWorklist* connected);
/// Hash an SEXP (doesn't have to be RIR) into a UUID, by serializing it but
/// EVP-MD hashing ("fancy XOR"-ing) the bits instead of collecting them, and
/// add connected RIR object containers to the worklist.
UUID hashRoot(SEXP sexp, ConnectedWorklist& connected);
/// Hash an SEXP (doesn't have to be RIR) into a UUID, by serializing it but
/// EVP-MD hashing ("fancy XOR"-ing) the bits instead of collecting them.
UUID hashRoot(SEXP sexp);

} // namespace rir