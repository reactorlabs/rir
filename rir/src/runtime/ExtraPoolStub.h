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

struct Code;

#define EXTRA_POOL_STUB_MAGIC 0xec17a101

class ExtraPoolStub :
    public RirRuntimeObject<ExtraPoolStub, EXTRA_POOL_STUB_MAGIC> {
  public:
    /// Unique hash to identify the source pool
    UUID sourceHash;
    size_t index;

    ExtraPoolStub(const UUID& sourceHash, size_t index);
    /// Create an SEXP stubbing the given extra pool entry
    static SEXP create(const UUID& sourceHash, size_t index);

    /// Add stubs to source pool entries to the target code's pool until it's
    /// `size`.
    static void pad(const UUID& sourceHash, size_t sourcePoolSize,
                    Code* targetCodeWithPool);

    void print(std::ostream& out) const;
    static ExtraPoolStub* deserialize(AbstractDeserializer& deserializer);
    void serialize(AbstractSerializer& serializer) const;
    void hash(HasherOld& hasher) const;
    void addConnected(ConnectedCollectorOld& collector) const;

};

} // namespace rir
