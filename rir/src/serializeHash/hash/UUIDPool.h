//
// Created by Jakob Hain on 6/1/23.
//

#pragma once

#include "R/r.h"
#include "UUID.h"
#include "bc/BC_inc.h"
#include "interpreter/instance.h"
#include "utils/ByteBuffer.h"

#include <unordered_map>
#include <unordered_set>
#include <queue>

#define DO_INTERN

namespace rir {

/// A global set of SEXPs identified by a unique UUID computed by hash.
/// Structurally equivalent SEXPs will have the same UUID, and structurally
/// different SEXPs will, with extremely high probability, have different UUIDs.
/// "Structurally equivalent" means that an SEXP's UUID is independent of its
/// address in memory, and even different R sessions can identify structurally-
/// equivalent SEXPs by the same UUID.
///
/// The UUID is computed by hashing the SEXP's serialized form. When serializing
/// an SEXP, we only serialize hashes to connected RIR objects, to avoid
/// serializing copies of SEXPs we already have and then effectively duplicating
/// them by deserializing. However, when we serialize an SEXP to compute its
/// hash, we always serialize the connected objects, because some of those
/// connections may be cyclic and we a) need to handle this via refs (we use R's
/// ref-table) and b) want the refs to be deterministic (which requires the
/// "hash" of the connected object to be different than what we get from hashing
/// object directly, because the numbers and expansion of the refs differ).
///
/// Each SEXP in the set has a WeakRef finalizer which will remove the SEXP when
/// it's garbage collected, so the pool won't continually increase in size.
/// Sometimes SEXPs need to be remembered (by the compiler server), in which
/// case `UUIDPool::intern(,,true)` will preserve them so they never get freed.
class UUIDPool {
    static bool isInitialized;
    static std::unordered_map<UUID, SEXP> interned;
    static std::unordered_map<SEXP, UUID> hashes;
    /// This and `prevToIntern` effectively form multiple double-linked lists of
    /// SEXPs with the same UUID hash (one list for each hash) in the order we
    /// would assign them to be the "interned" SEXP for the UUID; when the
    /// "interned" SEXP gets gcd, we replace it with the next SEXP in the list,
    /// otherwise we remove the UUID because there is no longer a corresponding
    /// live SEXP.
    static std::unordered_map<SEXP, SEXP> nextToIntern;
    /// See `nextToIntern` doc
    static std::unordered_map<SEXP, SEXP> prevToIntern;
    static std::unordered_set<SEXP> preserved;

#ifdef DO_INTERN
    static void printInternedIfNecessary(SEXP sexp, const UUID& hash);
    static void unintern(SEXP e, bool isGettingGcd = false);
    static void uninternGcd(SEXP e);
#endif

  public:
    static void initialize();

    /// Whether the SEXP can be interned, and is serialized as a hash when
    /// interned SEXPs are.
    static bool internable(SEXP sexp);

    /// Intern the SEXP when we already know its hash, not recursively.
    ///
    /// \see UUIDPool::intern(SEXP, bool, bool)
    static SEXP intern(SEXP e, const UUID& hash, bool preserve,
                       bool isSexpComplete = true);
    /// Will hash the SEXP and:
    /// - If not in the pool, will add it *and* if `recursive` is set,
    ///   recursively intern connected SEXPs. Then returns the original SEXP
    /// - If already in the pool, returns the existing SEXP
    static SEXP intern(SEXP e, bool recursive, bool preserve);
    /// If SEXP is in the intern pool, re-compute its hash and remove/re-add it.
    /// Returns a different SEXP if there already exists an interned SEXP with
    /// the recomputed hash.
    static SEXP reintern(SEXP e);

    /// Gets the interned SEXP by hash, or nullptr if not interned
    static SEXP get(const UUID& hash);
    /// Gets the SEXP if interned locally, otherwise sends a request to the
    /// compiler peer. If the compiler peer doesn't have it, calls `Rf_error` on
    /// the client and returns `R_NilValue` on the server.
    static SEXP retrieve(const UUID& hash);
    /// Gets the SEXP's memoized hash, or the null hash if the SEXP was never
    /// interned
    static const UUID& getHash(SEXP sexp);

    /// \see tryReadHash(const ByteBuffer&)
    static SEXP tryReadHash(R_inpstream_t in);
    /// \see tryWriteHash(SEXP, ByteBuffer&)
    static bool tryWriteHash(SEXP sexp, R_outpstream_t out);
    /// Reads a boolean. If `true`, reads a hash and returns the interned SEXP,
    /// fetching from the compiler peer if necessary. If `false`, returns
    /// `nullptr`.
    ///
    /// If this is the compiler server and the compiler client doesn't have the
    /// hash, it will return `R_NilValue`. If this is the client and the server
    /// doesn't have the hash, it will `Rf_error`. This is the same behavior of
    /// `UUIDPool::readItem`.
    static SEXP tryReadHash(const ByteBuffer& buf);

    /// If the SEXP is internable, writes `true`, writes its hash, then returns
    /// `true`. Otherwise, writes `false`, then returns `false`.
    ///
    /// This will intern the SEXP if it's not already interned, unlike
    /// `writeItem` which will error.
    static bool tryWriteHash(SEXP sexp, ByteBuffer& buf);
    /// Calls `tryReadHash`, otherwise reads normally.
    static SEXP readItem(const ByteBuffer& buf, bool useHashes);
    /// Calls `tryWriteHash`, otherwise writes normally. `isChild` is unused,
    /// but may be an optimization in the future.
    static void writeItem(SEXP sexp, bool isChild, ByteBuffer& buf, bool useHashes);
};

} // namespace rir