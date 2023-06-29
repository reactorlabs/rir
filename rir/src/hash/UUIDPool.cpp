//
// Created by Jakob Hain on 6/1/23.
//

#include "UUIDPool.h"
#include "CompilerClient.h"
#include "R/Serialize.h"
#include "api.h"
#include "interpreter/serialize.h"
#include <queue>

// Can change this to log interned and uninterned hashes and pointers
#define LOG(stmt) if (false) stmt

namespace rir {

std::unordered_map<UUID, SEXP> UUIDPool::interned;
std::unordered_map<SEXP, UUID> UUIDPool::hashes;
std::unordered_map<SEXP, SEXP> UUIDPool::nextToIntern;
std::unordered_map<SEXP, SEXP> UUIDPool::prevToIntern;
std::unordered_set<SEXP> UUIDPool::preserved;
std::unordered_map<UUID, ByteBuffer> UUIDPool::serialized;

#ifdef DO_INTERN
static void registerFinalizerIfPossible(SEXP e, R_CFinalizer_t finalizer) {
    switch (TYPEOF(e)) {
    case NILSXP:
    case ENVSXP:
    case EXTPTRSXP:
    case BCODESXP:
    case EXTERNALSXP:
        R_RegisterCFinalizerEx(e, finalizer, (Rboolean) true);
        break;
    default:
        // can't register finalizer, assume these don't get gcd
        break;
    }

}

void UUIDPool::uninternGcd(SEXP e) {
    assert(
        !preserved.count(e) &&
        "SEXP should not be preserved if it's getting uninterned because it was gcd?"
    );

    // Remove hash
    assert(hashes.count(e) && "SEXP was never interned");
    // Why does cppcheck think this is unused?
    // cppcheck-suppress unreadVariable
    auto hash = hashes.at(e);
    hashes.erase(e);
    assert(interned.count(hash) && "SEXP was interned, but the corresponding UUID is empty");

    // Remove from the intern list for this UUID. If this is the first entry,
    // update the interned UUID to point to the next SEXP. If there is no next,
    // erase the interned UUID since there are no live SEXPs with that hash
    // anymore.
    if (prevToIntern.count(e)) {
        // This isn't the first entry in the list with this UUID

        // Linked list intermediate removal algorithm
        auto prev = prevToIntern.at(e);
        prevToIntern.erase(e);
        assert(nextToIntern.count(prev) && nextToIntern.at(prev) == e);
        if (nextToIntern.count(e)) {
            auto next = nextToIntern.at(e);
            nextToIntern.erase(e);
            assert(prevToIntern.count(next) && prevToIntern.at(next) == e);
            nextToIntern.at(prev) = next;
            prevToIntern.at(next) = prev;
        } else {
            nextToIntern.erase(prev);
        }
        LOG(std::cout << "GC intern: " << hash << " -> " << e << "\n");
    } else if (nextToIntern.count(e)) {
        // This is the first entry in the list with this UUID, and there is
        // another entry

        // Linked list head removal algorithm
        auto next = nextToIntern.at(e);
        nextToIntern.erase(e);
        assert(prevToIntern.count(next) && prevToIntern.at(next) == e);
        prevToIntern.erase(next);

        // Replace interned at UUID with the next SEXP
        interned.at(hash) = next;
        LOG(std::cout << "Switch intern: " << hash << " -> was " << e << " now " << next << "\n");
    } else {
        // This is the first and only entry in the list with this UUID

        // Erase interned at UUID
        interned.erase(hash);
        serialized.erase(hash);
        LOG(std::cout << "Remove intern: " << hash << " -> " << e << "\n");
    }
}
#endif

SEXP UUIDPool::intern(SEXP e, const UUID& hash, bool preserve) {
#ifdef DO_INTERN
    PROTECT(e);
    SLOWASSERT(hashSexp(e) == hash && "SEXP hash isn't deterministic or `hash` in `UUIDPool::intern(e, hash)` is wrong");
    UNPROTECT(1);
    if (interned.count(hash)) {
        auto existing = interned.at(hash);
        if (!hashes.count(e)) {
            LOG(std::cout << "Reuse intern: " << hash << " -> " << e << "\n");
            hashes[e] = hash;

            // Add to intern list for this UUID
            auto oldLast = existing;
            while (nextToIntern.count(oldLast)) {
                oldLast = nextToIntern.at(oldLast);
            }
            nextToIntern[oldLast] = e;
            prevToIntern[e] = oldLast;

            registerFinalizerIfPossible(e, uninternGcd);
        }
        e = existing;
        if (preserve && !preserved.count(e)) {
            R_PreserveObject(e);
            preserved.insert(e);
        }
        return e;
    }
    if (preserve) {
        R_PreserveObject(e);
        preserved.insert(e);
    } else {
        registerFinalizerIfPossible(e, uninternGcd);
    }
    if (hashes.count(e)) {
        std::cerr << "SEXP UUID changed from " << hashes.at(e) << " to "
                  << hash << ": " << e << "\n";
        Rf_PrintValue(e);
        assert(false);
    }
    LOG(std::cout << "New intern: " << hash << " -> " << e << "\n");
    interned[hash] = e;
    hashes[e] = hash;
#endif
    return e;
}

SEXP UUIDPool::intern(SEXP e, bool recursive, bool preserve) {
#ifdef DO_INTERN
    if (hashes.count(e) && !recursive) {
        // Already interned, don't compute hash
        if (preserve && !preserved.count(e)) {
            R_PreserveObject(e);
            preserved.insert(e);
        }
        return e;
    }
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
        // Why does cppcheck think this is unused?
        // cppcheck-suppress unreadVariable
        auto hash = hashes.at(sexp);
        OutBytes(out, &hash, sizeof(hash));
    } else {
        WriteItem(sexp, ref_table, out);
    }
}

} // namespace rir