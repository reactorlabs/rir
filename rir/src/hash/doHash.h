//
// Created by Jakob Hain on 7/21/23.
//

#pragma once

#include "R/r_incl.h"
#include "UUID.h"
#include <queue>

namespace rir {

class Hasher {
    UUID::Hasher& hasher;
    std::queue<SEXP>& worklist;

    Hasher(UUID::Hasher& hasher, std::queue<SEXP>& worklist)
        : hasher(hasher), worklist(worklist) {}

    friend void hashRoot(SEXP root, UUID::Hasher& hasher);
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
    void hash(SEXP s) {
        worklist.push(s);
    }
};

void hashRoot(SEXP root, UUID::Hasher& hasher);

} // namespace rir