//
// Created by Jakob Hain on 7/21/23.
//

#pragma once

#include "R/r_incl.h"
#include "UUID.h"
#include "serializeHash/serializeUni.h"
#include <queue>
#include <unordered_set>

namespace rir {

/* /// SEXP->UUID hasher which is exposed to RIR objects so that they can hash
/// themselves
class HasherUni : Serializer {
    /// Underlying UUID hasher
    UUID::Hasher& hasher;
    /// Next SEXPs to process: instead of recursing, we add nested SEXPs to this
    /// queue and then process them in a loop. This is different semantics than
    /// actually recursing, but it doesn't matter because hashes are still the
    /// same quality and consistent.
    SerialWorklist& worklist;

    Hasher(UUID::Hasher& hasher, Worklist& worklist)
        : hasher(hasher), worklist(worklist) {}

    friend UUID hashRoot(SEXP root);
  public:
    /// Hash raw data, can't contain any references
    template<typename T> void hashBytesOf(T c) {
        hasher.hashBytesOf(c);
    }
    /// Hash raw data, can't contain any references
    void hashBytes(const void* data, size_t size) {
        hasher.hashBytes(data, size);
    }
    /// Hash SEXP. ASTs hash differently and faster
    void hash(SEXP s, bool isAst = false) {
        worklist.push({s, isAst});
    }
    /// Hash SEXP in constant pool ([Pool])
    void hashConstant(unsigned idx);
    /// Hash SEXP in source pool ([src_pool_at])
    void hashSrc(unsigned idx);
    /// Hash SEXP which could be nullptr
    void hashNullable(SEXP s, bool isAst = false) {
        hashBytesOf<bool>(s != nullptr);
        if (s) {
            hash(s, isAst);
        }
    }
};

/// Hash an SEXP (doesn't have to be RIR) into a UUID, by serializing it but
/// EVP-MD hashing ("fancy XOR"-ing) the bits instead of collecting them.
/// <br /><br />
/// This is called `hashRoot` to signify that we hash other SEXPs after this
/// one, which is relevant when we hash cyclic references: later occurrences of
/// the same SEXP are replaced by refs, but the location of these refs differ
/// depending on which SEXP is the root. You can think of the SEXP and all its
/// connected SEXPs as a graph, and hashRoot` creates a view of the graph with
/// this one at the center; if we call `hashRoot` with a different SEXP in the
/// connected graph, even though we have the same graph, we get a different view
/// and thus a different hash.
UUID hashRoot(SEXP root); */

} // namespace rir