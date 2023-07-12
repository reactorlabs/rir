//
// Created by Jakob Hain on 6/1/23.
//

#pragma once

#include "R/r.h"
#include "RirUID.h"
#include "bc/BC_inc.h"
#include "interpreter/instance.h"
#include "utils/ByteBuffer.h"
#include "utils/Set.h"

#include <unordered_map>
#include <unordered_set>
#include <queue>

#define DO_INTERN

namespace rir {

/// A global set of SEXPs identified by a unique UID computed by hash.
/// Structurally equivalent SEXPs will have the same UID, and structurally
/// different SEXPs will, with extremely high probability, have different UIDs.
/// "Structurally equivalent" means that an SEXP's UID is independent of its
/// address in memory, and even different R sessions can identify structurally-
/// equivalent SEXPs by the same UID.
///
/// The UID is computed by hashing the SEXP's serialized form. When serializing
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
/// it's garbage collected, so the pool won't continually increase in size. When
/// SEXPs need to be remembered (by the compiler server), they must be
/// explicitly preserved.
class RirUIDPool {
    static std::unordered_map<UUID, SmallSet<SEXP>> interned;
    static std::unordered_map<SEXP, UUID> hashes;
    static std::unordered_set<SEXP> preserved;

#ifdef DO_INTERN
    static void uninternGcd(SEXP e);
#endif

  public:
    /// Intern the SEXP when we already know its hash, not recursively.
    ///
    /// @see RirUIDPool::intern(SEXP, bool, bool)
    static SEXP intern(SEXP e, const RirUID& uuid, bool preserve,
                       bool expectHashToBeTheSame = true);
    /// Will hash the SEXP and:
    /// - If not in the pool, will add it *and* if `recursive` is set,
    ///   recursively intern connected SEXPs. Then returns the original SEXP
    /// - If already in the pool, returns the existing SEXP
    static SEXP intern(SEXP e, bool recursive, bool preserve);
    /// Gets the interned SEXP by hash, or nullptr if not interned
    static SEXP get(const RirUID& hash);
    /// Gets the first live interned SEXP with the big hash, or nullptr if there
    /// are none
    static SEXP getAny(const UUID& bigHash);
    /// Gets the SEXP's memoized hash, or the null hash if the SEXP was never
    /// interned
    static RirUID getHash(SEXP sexp);
    /// When deserializing with `useHashes=true`, reads a hash, then looks it up
    /// in the intern pool. If the SEXP isn't in the intern pool, fetches it
    /// from the compiler server. If the compiler server isn't connected or
    /// doesn't have the SEXP, `Rf_error`s.
    ///
    /// Otherwise, Calls `ReadItem` to read the SEXP as usual.
    static SEXP readItem(SEXP ref_table, R_inpstream_t in);
    /// When deserializing with `useHashes=true`, reads a hash, then looks it up
    /// in the intern pool. If the SEXP isn't in the intern pool, fetches it
    /// from the compiler server. If the compiler server isn't connected or
    /// doesn't have the SEXP, `Rf_error`s.
    ///
    /// Otherwise, Calls `rir::deserialize` to read the SEXP as usual.
    static SEXP readItem(ByteBuffer& buf, bool useHashes);
    /// When serializing with `useHashes=true`, asserts that the SEXP is
    /// interned (required for `useHashes=true`) and writes the SEXP's hash.
    ///
    /// When "serializing" to compute the hash and serializing with
    /// `useHashes=false`, calls `WriteItem` to write the SEXP as usual.
    static void writeItem(SEXP sexp, SEXP ref_table, R_outpstream_t out);
    /// When serializing with `useHashes=true`, asserts that the SEXP is
    /// interned (required for `useHashes=true`) and writes the SEXP's hash.
    ///
    /// When "serializing" to compute the hash and serializing with
    /// `useHashes=false`, calls `rir::serialize` to write the SEXP as usual.
    static void writeItem(SEXP sexp, ByteBuffer& buf, bool useHashes);
    /// Serializes an AST, so that the hash won't change when we are hashing,
    /// since it may if we call `writeItem` even though the AST itself doesn't
    /// change
    static void writeAst(SEXP src, SEXP ref_table, R_outpstream_t out);
};

/// Would be an inner class but we can't: https://stackoverflow.com/a/951245
class ConnectedWorklist {
    std::unordered_set<SEXP> seen;

    friend class RirUIDPool;
    void insert(SEXP e) { seen.insert(e); }
    SEXP pop() {
        auto it = seen.begin();
        if (it == seen.end()) {
            return nullptr;
        }
        SEXP e = *it;
        seen.erase(it);
        return e;
    }
};

} // namespace rir