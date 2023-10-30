//
// Created by Jakob Hain on 10/9/23.
//

#pragma once

#include "R/r_incl.h"
#include "RirRuntimeObject.h"
#include "serializeHash/hash/getConnectedOld.h"
#include "serializeHash/hash/hashRootOld.h"
#include "serializeHash/serializeUni.h"
#include <cstddef>
#include <functional>

namespace rir {

struct Function;

#define POOL_STUB_MAGIC 0xec17a101

/// Stub for an SEXP in a local pool when we send it to the compiler server,
///   because the server only needs minimal information about the SEXP (like its
///   identity), so we don't want to send all of its data. For example, we
///   replace extra pool entries with stubs when sending RIR code to the server,
///   and then the server creates pushes and static calls and other bytecode
///   instructions for these SEXPs without caring about their content. When the
///   data is deserialized back to the client, we convert the stubs back into
///   their stubbed values.

class PoolStub :
    public RirRuntimeObject<PoolStub, POOL_STUB_MAGIC> {
  public:
    /// Unique hash to identify the source
    UUID sourceHash;
    /// UNSIGNED_MAX if this is the function body's pool, otherwise this is the
    /// default argument at the index's pool
    unsigned defaultArgIdx;
    size_t index;

    PoolStub(const UUID& sourceHash, unsigned defaultArgIdx, size_t index);
    /// Create an SEXP stubbing the given pool entry
    static SEXP create(const UUID& sourceHash, unsigned defaultArgIdx,
                       size_t index);

    /// Add stubs to source pool entries to the target code's pool until it's
    /// `size`.
    static void pad(const UUID& sourceHash, size_t sourceBodyPoolSize,
                    const std::vector<size_t>& sourceDefaultArgPoolSizes,
                    Function* targetFunction);

    void print(std::ostream& out) const;
    static PoolStub* deserialize(AbstractDeserializer& deserializer);
    void serialize(AbstractSerializer& serializer) const;
    void hash(HasherOld& hasher) const;
    void addConnected(ConnectedCollectorOld& collector) const;

};

} // namespace rir
