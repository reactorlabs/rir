//
// Created by Jakob Hain on 6/1/23.
//

#pragma once

#include "R/r.h"
#include "UUID.h"
#include "bc/BC_inc.h"
#include "interpreter/instance.h"

#include <unordered_map>
#include <unordered_set>

/// If not defined, we won't actually intern anything.
/// Importantly, by default we intern some deserialized SEXPs. Since that is the
/// only thing we intern, this is effectively the flag to disable this feature
/// (if we ever intern anything else maybe we'll have a separate flag)
#define DO_INTERN

namespace rir {

/// A pool of SEXPs with a UUID.
/// When we deserialize some SEXPs, after deserialization we will check their
///    hash and try to reuse an SEXP already interned if possible. Otherwise we
//     will intern for future deserializations.
class UUIDPool {
    static std::unordered_map<UUID, SEXP> interned;

  public:
    /// Will hash the SEXP and then, if we've already interned, return the
    ///    existing version. Otherwise we will insert it into the pool and
    ///    return it as-is.
    static SEXP intern(SEXP e);
    /// Read item and intern
    static SEXP readItem(SEXP ref_table, R_inpstream_t in);
    /// Write item, ensuring that it will actually be reused in redundant
    /// readItem calls even on a separate process
    /// TODO: implement
    static SEXP writeItem(SEXP sexp, SEXP ref_table, R_outpstream_t out);
};

} // namespace rir