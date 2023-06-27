//
// Created by Jakob Hain on 6/1/23.
//

#include "UUIDPool.h"
#include "CompilerClient.h"
#include "R/Serialize.h"
#include "api.h"
#include "interpreter/serialize.h"
#include <queue>

namespace rir {

std::unordered_map<UUID, SEXP> UUIDPool::interned;
std::unordered_map<SEXP, UUID> UUIDPool::hashes;
std::unordered_set<SEXP> UUIDPool::preserved;
std::unordered_map<UUID, ByteBuffer> UUIDPool::serialized;

#ifdef DO_INTERN
void UUIDPool::uninternGcd(SEXP e) {
    assert(!preserved.count(e));
    auto hash = hashes.at(e);
    interned.erase(hash);
    serialized.erase(hash);
    hashes.erase(e);
}
#endif

SEXP UUIDPool::intern(SEXP e, const UUID& hash, bool preserve) {
#ifdef DO_INTERN
    PROTECT(e);
    SLOWASSERT(hashSexp(e) == hash && "SEXP hash isn't deterministic or `hash` in `UUIDPool::intern(e, hash)` is wrong");
    UNPROTECT(1);
    if (interned.count(hash)) {
        auto sexp = interned.at(hash);
        if (preserve && !preserved.count(sexp)) {
            R_PreserveObject(sexp);
            preserved.insert(sexp);
        }
        return sexp;
    }
    if (preserve) {
        R_PreserveObject(e);
        preserved.insert(e);
    } else {
        switch (TYPEOF(e)) {
        case NILSXP:
        case ENVSXP:
        case EXTPTRSXP:
        case BCODESXP:
        case EXTERNALSXP:
            R_RegisterCFinalizerEx(e, uninternGcd, (Rboolean) true);
            break;
        default:
            // can't register finalizer, hopefully these don't get gcd
            break;
        }
    }
    interned[hash] = e;
    hashes[e] = hash;
#endif
    return e;
}

SEXP UUIDPool::intern(SEXP e, bool recursive, bool preserve) {
#ifdef DO_INTERN
    if (recursive) {
        std::queue<SEXP> worklist;
        auto ret = intern(e, hashSexp(e, worklist), preserve);
        while (!worklist.empty()) {
            e = worklist.front();
            worklist.pop();
            intern(e, hashSexp(e, worklist), preserve);
        }
        return ret;
    } else {
        return intern(e, hashSexp(e), preserve);
    }
#else
    return e;
#endif
}

SEXP UUIDPool::get(const UUID& hash) {
#ifdef DO_INTERN
    if (interned.count(hash)) {
        return interned.at(hash);
    }
#endif
    return nullptr;
}

SEXP UUIDPool::readItem(SEXP ref_table, R_inpstream_t in) {
    if (useHashes(in)) {
        UUID hash;
        InBytes(in, &hash, sizeof(hash));
        if (interned.count(hash)) {
            return interned.at(hash);
        }
        if (CompilerClient::isRunning()) {
            auto sexp = CompilerClient::retrieve(hash);
            if (sexp) {
                return intern(sexp, hash, false);
            }
            Rf_error("SEXP deserialized from hash which we don't have, and server also doesn't have it");
        }
        Rf_error("SEXP deserialized from hash which we don't have, and no server");
    } else {
        return intern(ReadItem(ref_table, in), false, false);
    }
}

void UUIDPool::writeItem(SEXP sexp, SEXP ref_table, R_outpstream_t out) {
    assert(!worklist(out) || !useHashes(out));
    auto wl = worklist(out);
    if (wl && !hashes.count(sexp)) {
        wl->push(sexp);
    }
    if (useHashes(out)) {
        assert(hashes.count(sexp) && "SEXP not interned");
        auto hash = hashes.at(sexp);
        OutBytes(out, &hash, sizeof(hash));
    } else {
        WriteItem(sexp, ref_table, out);
    }
}

} // namespace rir