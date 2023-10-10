//
// Created by Jakob Hain on 10/9/23.
//

#include "ExtraPoolStub.h"
#include "runtime/Code.h"

namespace rir {

ExtraPoolStub::ExtraPoolStub(uintptr_t codeWithPoolAddr, size_t index)
    : RirRuntimeObject(0, 0),
      codeWithPoolAddr(codeWithPoolAddr),
      index(index) {
      assert(codeWithPoolAddr != 0 && "codeWithPoolAddr must be non-null");
}

SEXP ExtraPoolStub::create(uintptr_t codeWithPoolAddr, size_t index) {
    auto store = Rf_allocVector(EXTERNALSXP, sizeof(ExtraPoolStub));
    new (DATAPTR(store)) ExtraPoolStub(codeWithPoolAddr, index);
    return store;
}

void ExtraPoolStub::print(std::ostream& out) const {
    out << "(" << (void*)codeWithPoolAddr << ", " << index << ")";
}

ExtraPoolStub* ExtraPoolStub::deserialize(AbstractDeserializer& deserializer) {
    auto codeWithPoolAddr = deserializer.readBytesOf<uintptr_t>();
    auto index = deserializer.readBytesOf<size_t>();
    auto store = create(codeWithPoolAddr, index);
    return unpack(store);
}

void ExtraPoolStub::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf(codeWithPoolAddr);
    serializer.writeBytesOf(index);
}

void ExtraPoolStub::hash(HasherOld& hasher) const {
    hasher.hashBytesOf(codeWithPoolAddr);
    hasher.hashBytesOf(index);
}

void ExtraPoolStub::addConnected(__attribute__((unused)) ConnectedCollectorOld& collector) const {
    // Nothing to add
}

void ExtraPoolStub::pad(uintptr_t sourceCodeWithPoolAddr, size_t sourcePoolSize,
                        Code* targetCodeWithPool) {
    for (auto i = (size_t)targetCodeWithPool->extraPoolSize; i < sourcePoolSize;
         i++) {
        targetCodeWithPool->addExtraPoolEntry(create(sourceCodeWithPoolAddr, i));
    }
}

} // namespace rir