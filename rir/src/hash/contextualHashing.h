//
// Created by Jakob Hain on 7/11/23.
//

#pragma once

#include "R/r.h"
#include "hash/RirUID.h"
#include "interpreter/serialize.h"

namespace rir {

__attribute__((unused)) static inline void
bigHash(R_outpstream_t out,
        const std::function<void(R_outpstream_t out)>& code) {
    // Big hashing or regular serialization = run normally
    // Small hashing = skip (there's never a worklist with small hashing)
    if (!isOnlySmallHashing(out)) {
        code(out);
    }
}

__attribute__((unused)) static inline void
smallHash(R_outpstream_t out,
          const std::function<void(R_outpstream_t out)>& code) {
    // Big hashing = don't add to hash, but do add to worklist
    // Small hashing or regular serialization = run normally
    if (isOnlyBigHashing(out)) {
        if (connected(out)) {
            auto nullOut = nullOutputStream();
            code(&nullOut);
        }
    } else {
        code(out);
    }
}

__attribute__((unused)) static inline void
noHash(R_outpstream_t out,
       const std::function<void(R_outpstream_t out)>& code) {
    // Big hashing = don't add to hash, but do add to worklist
    // Small hashing = skip (there's never a worklist with small hashing)
    if (isHashing(out)) {
        if (connected(out)) {
            auto nullOut = nullOutputStream();
            code(&nullOut);
        }
    } else {
        code(out);
    }
}

} // namespace rir

#define BIG_HASH(code) bigHash(out, [&](R_outpstream_t out) code)
#define SMALL_HASH(code) smallHash(out, [&](R_outpstream_t out) code)
#define NO_HASH(code) noHash(out, [&](R_outpstream_t out) code)