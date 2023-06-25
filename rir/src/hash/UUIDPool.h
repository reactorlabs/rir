//
// Created by Jakob Hain on 6/1/23.
//

#pragma once

#include "R/r.h"
#include "UUID.h"
#include "bc/BC_inc.h"
#include "interpreter/instance.h"

#include <unordered_map>

#define DO_INTERN

namespace rir {

/// A pool of SEXPs with a UUID.
/// When we deserialize some SEXPs, after deserialization we will check their
///    hash and try to reuse an SEXP already interned if possible. Otherwise we
///    store ("intern") for future deserializations.
class UUIDPool {
    static std::unordered_map<UUID, SEXP> interned;

  public:
    /// Intern the SEXP, except we already know its hash
    static SEXP intern(SEXP e, const UUID& uuid);
    /// Will hash the SEXP and then, if we've already interned, return the
    /// existing version. Otherwise we will insert it into the pool and return
    /// it as-is.
    static SEXP intern(SEXP e);
    /// Gets the interned value by hash, or nullptr if not interned
    static SEXP get(const UUID& hash);
    // Currently unused
    /* /// Reads item and interns, possibly returning the already-interned version.
    ///
    /// The SEXP MUST NOT contain any references to external SEXPs.
    static SEXP readItem(SEXP ref_table, R_inpstream_t in);
    /// Interns and then writes the item, possibly writing the already-interned
    /// version (though they should write the exact same data).
    ///
    /// The SEXP MUST NOT contain any references to external SEXPs.
    static void writeItem(SEXP sexp, SEXP ref_table, R_outpstream_t out); */
};

} // namespace rir