#include "Deoptimization.h"
#include "runtime/Code.h"
#include "hash/UUID.h"
#include "hash/UUIDPool.h"
#include "utils/ByteBuffer.h"

namespace rir {

void FrameInfo::deserialize(ByteBuffer& buf) {
    code = Code::unpack(UUIDPool::readItem(buf, true));
    pc = code->code() + buf.getInt();
    stackSize = (size_t)buf.getInt();
    inPromise = (bool)buf.getInt();
}

void FrameInfo::serialize(ByteBuffer& buf) const {
    UUIDPool::writeItem(code->container(), buf, true);
    buf.putInt((uint32_t)(pc - code->code()));
    buf.putInt((uint32_t)stackSize);
    buf.putInt((uint32_t)inPromise);
}

void FrameInfo::internRecursive() const {
    UUIDPool::intern(code->container(), true, false);
}

void FrameInfo::preserve() const {
    R_PreserveObject(code->container());
}

SEXP DeoptMetadata::container() const {
    // cppcheck-suppress thisSubtraction
    SEXP result = (SEXP)((uintptr_t)this - sizeof(VECTOR_SEXPREC));
    assert(TYPEOF(result) == RAWSXP && "DeoptMetadata not embedded in container, or corrupt.");
    return result;
}

DeoptMetadata* DeoptMetadata::deserialize(ByteBuffer& buf) {
    auto numFrames = (size_t)buf.getInt();
    auto size = sizeof(DeoptMetadata) + numFrames * sizeof(FrameInfo);
    SEXP store = Rf_allocVector(RAWSXP, (int)size);
    auto m = new (DATAPTR(store)) DeoptMetadata;
    m->numFrames = numFrames;
    for (size_t i = 0; i < numFrames; ++i) {
        m->frames[i].deserialize(buf);
    }
    return m;
}

void DeoptMetadata::serialize(ByteBuffer& buf) const {
    buf.putInt((uint32_t)numFrames);
    for (size_t i = 0; i < numFrames; ++i) {
        frames[i].serialize(buf);
    }
}

void DeoptMetadata::internRecursive() const {
    for (size_t i = 0; i < numFrames; ++i) {
        frames[i].internRecursive();
    }
}

void DeoptMetadata::preserve() const {
    R_PreserveObject(this->container());
    for (size_t i = 0; i < numFrames; ++i) {
        frames[i].preserve();
    }
}

void DeoptMetadata::print(std::ostream& out) const {
    for (size_t i = 0; i < numFrames; ++i) {
        auto f = frames[i];
        out << f.code << "+" << f.pc - f.code->code() << " s" << f.stackSize;
        if (i < numFrames - 1)
            out << ", ";
    }
}

} // namespace rir
