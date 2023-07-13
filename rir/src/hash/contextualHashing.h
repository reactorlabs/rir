//
// Created by Jakob Hain on 7/11/23.
//

#pragma once

#include "R/r.h"
#include "hash/RirUID.h"
#include "interpreter/serialize.h"

namespace rir {

SEXP copyRefTable(SEXP refTable);

__attribute__((unused)) static inline void
bigHash(R_outpstream_t out, SEXP refTable,
        const std::function<void(R_outpstream_t out, SEXP refTable)>& code) {
    // Big hashing or regular serialization = run normally
    // Small hashing = skip (there's never a worklist with small hashing)
    if (!isOnlySmallHashing(out)) {
        code(out, refTable);
    }
}

__attribute__((unused)) static inline void
smallHash(R_outpstream_t out, SEXP refTable,
          const std::function<void(R_outpstream_t out, SEXP refTable)>& code) {
    // Big hashing = don't add to hash, but do add to worklist
    // Small hashing or regular serialization = run normally
    if (isOnlyBigHashing(out)) {
        auto nullOut = nullOutputStream();
        code(&nullOut, refTable);
    } else {
        code(out, refTable);
    }
}

__attribute__((unused)) static inline void
noHash(R_outpstream_t out, SEXP refTable,
       const std::function<void(R_outpstream_t out, SEXP refTable)>& code) {
    // Big hashing = don't add to hash, but do add to worklist
    // Small hashing = skip (there's never a worklist with small hashing)
    if (isHashing(out)) {
        auto nullOut = nullOutputStream();
        code(&nullOut, refTable);
    } else {
        code(out, refTable);
    }
}

} // namespace rir

#define BIG_HASH(code) bigHash(out, refTable, [&](R_outpstream_t out, SEXP refTable) code)
#define SMALL_HASH(code) smallHash(out, refTable, [&](R_outpstream_t out, SEXP refTable) code)
#define NO_HASH(code) noHash(out, refTable, [&](R_outpstream_t out, SEXP refTable) code)