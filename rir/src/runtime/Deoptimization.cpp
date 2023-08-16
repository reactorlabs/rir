#include "Deoptimization.h"
#include "runtime/Code.h"
#include "serializeHash/hash/UUID.h"
#include "serializeHash/hash/UUIDPool.h"
#include "utils/ByteBuffer.h"

namespace rir {

void FrameInfo::deserialize(const ByteBuffer& buf) {
    code = Code::unpack(UUIDPool::readItem(buf, true));
    pc = code->code() + buf.getInt();
    stackSize = (size_t)buf.getInt();
    inPromise = (bool)buf.getInt();
}

void FrameInfo::serialize(ByteBuffer& buf) const {
    UUIDPool::writeItem(code->container(), false, buf, true);
    buf.putInt((uint32_t)(pc - code->code()));
    buf.putInt((uint32_t)stackSize);
    buf.putInt((uint32_t)inPromise);
}

void FrameInfo::internRecursive() const {
    UUIDPool::intern(code->container(), true, false);
}

void FrameInfo::gcAttach(Code* outer) const {
    outer->addExtraPoolEntry(code->container());
}

SEXP DeoptMetadata::container() const {
    // cppcheck-suppress thisSubtraction
    SEXP result = (SEXP)((uintptr_t)this - sizeof(VECTOR_SEXPREC));
    assert(TYPEOF(result) == RAWSXP && "DeoptMetadata not embedded in container, or corrupt.");
    return result;
}

DeoptMetadata* DeoptMetadata::deserialize(const ByteBuffer& buf) {
    auto numFrames = (size_t)buf.getInt();
    auto size = sizeof(DeoptMetadata) + numFrames * sizeof(FrameInfo);
    SEXP store = Rf_allocVector(RAWSXP, (int)size);
    PROTECT(store);
    auto m = new (DATAPTR(store)) DeoptMetadata;
    m->numFrames = numFrames;
    for (size_t i = 0; i < numFrames; ++i) {
        m->frames[i].deserialize(buf);
        PROTECT(m->frames[i].code->container());
    }
    UNPROTECT(1 + m->numFrames);
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

void DeoptMetadata::gcAttach(Code* outer) const {
    outer->addExtraPoolEntry(this->container());
    for (size_t i = 0; i < numFrames; ++i) {
        frames[i].gcAttach(outer);
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
