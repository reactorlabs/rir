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

/// SEXP->UUID hasher which is exposed to RIR objects so that they can hash
/// themselves
class HasherUni : AbstractSerializer {
    using Worklist = std::queue<SEXP>;

    /// Underlying UUID hasher
    UUID::Hasher& hasher;
    // SEXPs already processed; we serialize these as refs instead of recursing.
    SerializedRefs refs_;
    /// Next SEXPs to process: instead of recursing, we add nested SEXPs to this
    /// queue and then process them in a loop. This is different semantics than
    /// actually recursing, but it doesn't matter because hashes are still the
    /// same quality and consistent. We still hash ASTs immediately since those
    /// are hashed with a different function.
    Worklist worklist;

    explicit HasherUni(UUID::Hasher& hasher)
        : hasher(hasher), refs_(), worklist() {}
    SerializedRefs* refs() override { return &refs_; }

    void doHashRoot(SEXP root);
    friend UUID hashRootUni(SEXP root);
  public:
    bool willWrite(const SerialFlags& flags) const override;
    void writeBytes(const void *data, size_t size, const SerialFlags& flags) override;
    void writeInt(int data, const SerialFlags& flags) override;
    void write(SEXP s, const SerialFlags& flags) override;
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
UUID hashRootUni(SEXP root);

} // namespace rir