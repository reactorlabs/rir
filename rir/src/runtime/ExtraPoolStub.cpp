//
// Created by Jakob Hain on 10/9/23.
//

#include "ExtraPoolStub.h"
#include "runtime/Code.h"

namespace rir {

ExtraPoolStub::ExtraPoolStub(const UUID& sourceHash, size_t index)
    : RirRuntimeObject(0, 0),
      sourceHash(sourceHash),
      index(index) {
      assert(sourceHash && "sourceHash must be non-null");
}

SEXP ExtraPoolStub::create(const UUID& sourceHash, size_t index) {
    auto store = Rf_allocVector(EXTERNALSXP, sizeof(ExtraPoolStub));
    new (DATAPTR(store)) ExtraPoolStub(sourceHash, index);
    return store;
}

void ExtraPoolStub::print(std::ostream& out) const {
    out << "(" << sourceHash << ", " << index << ")";
}

ExtraPoolStub* ExtraPoolStub::deserialize(AbstractDeserializer& deserializer) {
    UUID sourceHash;
    deserializer.readBytes(&sourceHash, sizeof(UUID));
    auto index = deserializer.readBytesOf<size_t>();
    auto store = create(sourceHash, index);
    return unpack(store);
}

void ExtraPoolStub::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytes(&sourceHash, sizeof(UUID));
    serializer.writeBytesOf(index);
}

void ExtraPoolStub::hash(HasherOld& hasher) const {
    hasher.hashBytes(&sourceHash, sizeof(UUID));
    hasher.hashBytesOf(index);
}

void ExtraPoolStub::addConnected(__attribute__((unused)) ConnectedCollectorOld& collector) const {
    // Nothing to add
}

void ExtraPoolStub::pad(const UUID& sourceHash, size_t sourcePoolSize,
                        Code* targetCodeWithPool) {
    for (auto i = (size_t)targetCodeWithPool->extraPoolSize; i < sourcePoolSize;
         i++) {
        targetCodeWithPool->addExtraPoolEntry(create(sourceHash, i));
    }
}

} // namespace rir