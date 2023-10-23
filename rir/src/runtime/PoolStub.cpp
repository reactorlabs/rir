//
// Created by Jakob Hain on 10/9/23.
//

#include "PoolStub.h"
#include "runtime/Function.h"

namespace rir {

PoolStub::PoolStub(const UUID& sourceHash, unsigned defaultArgIdx, size_t index)
    : RirRuntimeObject(0, 0),
      sourceHash(sourceHash),
      defaultArgIdx(defaultArgIdx),
      index(index) {
      assert(sourceHash && "sourceHash must be non-null");
}

SEXP PoolStub::create(const UUID& sourceHash, unsigned defaultArgIdx,
                      size_t index) {
    auto store = Rf_allocVector(EXTERNALSXP, sizeof(PoolStub));
    new (DATAPTR(store)) PoolStub(sourceHash, defaultArgIdx, index);
    return store;
}

void PoolStub::print(std::ostream& out) const {
    out << "(" << sourceHash << ", " << index << ")";
}

PoolStub* PoolStub::deserialize(AbstractDeserializer& deserializer) {
    UUID sourceHash;
    deserializer.readBytes(&sourceHash, sizeof(UUID));
    auto poolType = deserializer.readBytesOf<unsigned>();
    auto index = deserializer.readBytesOf<size_t>();
    auto store = create(sourceHash, poolType, index);
    return unpack(store);
}

void PoolStub::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytes(&sourceHash, sizeof(UUID));
    serializer.writeBytesOf(defaultArgIdx);
    serializer.writeBytesOf(index);
}

void PoolStub::hash(HasherOld& hasher) const {
    hasher.hashBytes(&sourceHash, sizeof(UUID));
    hasher.hashBytesOf(defaultArgIdx);
    hasher.hashBytesOf(index);
}

void PoolStub::addConnected(__attribute__((unused)) ConnectedCollectorOld& collector) const {
    // Nothing to add
}

void PoolStub::pad(const UUID& sourceHash, size_t sourceBodyPoolSize,
                   const std::vector<size_t>& sourceDefaultArgPoolSizes,
                   Function* targetFunction) {
    auto targetBody = targetFunction->body();
    for (auto i = (size_t)targetBody->extraPoolSize; i < sourceBodyPoolSize;
         i++) {
        targetBody->addExtraPoolEntry(create(sourceHash, UINT32_MAX, i));
    }
    for (unsigned defaultArgIdx = 0;
         defaultArgIdx < sourceDefaultArgPoolSizes.size(); defaultArgIdx++) {
        auto sourceDefaultArgPoolSize = sourceDefaultArgPoolSizes[defaultArgIdx];
        if (sourceDefaultArgPoolSize > 0) {
            auto targetDefaultArg = targetFunction->defaultArg(defaultArgIdx);
            assert(targetDefaultArg &&
                   "target default arg is NULL but source default arg has pool "
                   "entries");
            for (auto i = (size_t)targetDefaultArg->extraPoolSize;
                 i < sourceDefaultArgPoolSize; i++) {
                targetDefaultArg->addExtraPoolEntry(create(sourceHash, defaultArgIdx, i));
            }
        }
    }
}

} // namespace rir