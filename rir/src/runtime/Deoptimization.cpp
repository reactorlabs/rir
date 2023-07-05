#include "Deoptimization.h"
#include "api.h"
#include "runtime/Code.h"
#include "hash/UUID.h"
#include "hash/UUIDPool.h"
#include "interpreter/serialize.h"
#include "utils/ByteBuffer.h"

namespace rir {

void FrameInfo::deserialize(ByteBuffer& buf) {
    UUID codeUuid;
    buf.getBytes((uint8_t*)&codeUuid, sizeof(codeUuid));
    code = Code::unpack(rir::deserialize(buf, true));
    pc = code->code() + buf.getInt();
    stackSize = (size_t)buf.getInt();
    inPromise = (bool)buf.getInt();
}

void FrameInfo::serialize(ByteBuffer& buf) const {
    rir::serialize(code->container(), buf, true);
    buf.putInt((uint32_t)(pc - code->code()));
    buf.putInt((uint32_t)stackSize);
    buf.putInt((uint32_t)inPromise);
}

void FrameInfo::internRecursive() const {
    UUIDPool::intern(code->container(), true, false);
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


void DeoptMetadata::print(std::ostream& out) const {
    for (size_t i = 0; i < numFrames; ++i) {
        auto f = frames[i];
        out << f.code << "+" << f.pc - f.code->code() << " s" << f.stackSize;
        if (i < numFrames - 1)
            out << ", ";
    }
}

} // namespace rir
