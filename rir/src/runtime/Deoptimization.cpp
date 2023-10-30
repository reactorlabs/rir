#include "Deoptimization.h"
#include "runtime/Code.h"
#include "serializeHash/hash/UUID.h"
#include "serializeHash/hash/UUIDPool.h"
#include "utils/ByteBuffer.h"

namespace rir {

void FrameInfo::deserialize(const ByteBuffer& buf,
                            const SerialOptions& serialOpts) {
    code = Code::unpack(rir::deserialize(buf, serialOpts));
    pc = code->code() + buf.getInt();
    stackSize = (size_t)buf.getInt();
    inPromise = (bool)buf.getInt();
}

void FrameInfo::serialize(ByteBuffer& buf,
                          const SerialOptions& serialOpts) const {
    rir::serialize(code->container(), buf, serialOpts);
    buf.putInt((uint32_t)(pc - code->code()));
    buf.putInt((uint32_t)stackSize);
    buf.putInt((uint32_t)inPromise);
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

DeoptMetadata* DeoptMetadata::deserialize(const ByteBuffer& buf,
                                          const SerialOptions& serialOpts) {
    auto numFrames = (size_t)buf.getInt();
    auto size = sizeof(DeoptMetadata) + numFrames * sizeof(FrameInfo);
    SEXP store = Rf_allocVector(RAWSXP, (int)size);
    PROTECT(store);
    auto m = new (DATAPTR(store)) DeoptMetadata;
    m->numFrames = numFrames;
    for (size_t i = 0; i < numFrames; ++i) {
        m->frames[i].deserialize(buf, serialOpts);
        PROTECT(m->frames[i].code->container());
    }
    UNPROTECT(1 + m->numFrames);
    return m;
}

void DeoptMetadata::serialize(ByteBuffer& buf,
                              const SerialOptions& serialOpts) const {
    buf.putInt((uint32_t)numFrames);
    for (size_t i = 0; i < numFrames; ++i) {
        frames[i].serialize(buf, serialOpts);
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
