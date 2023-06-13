//
// Created by Jakob Hain on 6/1/23.
//

#include "UUIDPool.h"
#include "R/Serialize.h"
#include "api.h"

namespace rir {

std::unordered_map<UUID, SEXP> UUIDPool::interned;

SEXP UUIDPool::intern(SEXP e, UUID hash) {
#ifdef DO_INTERN
    PROTECT(e);
    SLOWASSERT(hashSexp(e) == hash && "SEXP hash isn't deterministic or `hash` in `UUIDPool::intern(e, hash)` is wrong");
    UNPROTECT(1);
    if (interned.count(hash)) {
        return interned.at(hash);
    }
    // Object will be permanently preserved since it's permanently interned
    R_PreserveObject(e);
    interned[hash] = e;
#endif
    return e;
}

SEXP UUIDPool::intern(SEXP e) {
#ifdef DO_INTERN
    return intern(e, hashSexp(e));
#else
    return e;
#endif
}

/* /// Wrap data to also get UUID while deserializing
struct RStreamWrapper {
    R_inpstream_t stream;
    UUIDHasher hasher;

    explicit RStreamWrapper(R_inpstream_t stream) : stream(stream) {}
    UUID finalize() { return hasher.finalize(); }
};

static int rStreamWrapInChar(R_inpstream_t hashIn) {
    auto streamWrapper = (RStreamWrapper*)hashIn->data;
    auto in = streamWrapper->stream;
    auto hasher = &streamWrapper->hasher;

    auto data = in->InChar(in);
    hasher->hashBytesOf<unsigned char>((unsigned char)data);
    return data;
}

static void rStreamWrapInBytes(R_inpstream_t hashIn, void* data, int size) {
    auto streamWrapper = (RStreamWrapper*)hashIn->data;
    auto in = streamWrapper->stream;
    auto hasher = &streamWrapper->hasher;

    in->InBytes(in, data, size);
    hasher->hashBytes(data, size);
}

// Currently unused
SEXP UUIDPool::readItem(SEXP ref_table, R_inpstream_t in) {
    RStreamWrapper streamWrapper{in};
    R_inpstream_st hashIn{};
    R_InitInPStream(
        &hashIn,
        (R_pstream_data_t)&streamWrapper,
        in->type,
        rStreamWrapInChar,
        rStreamWrapInBytes,
        in->InPersistHookFunc,
        in->InPersistHookData
    );
    SEXP sexp = ReadItem(ref_table, &hashIn);
    return intern(sexp, streamWrapper.finalize());
}

void UUIDPool::writeItem(SEXP sexp, SEXP ref_table, R_outpstream_t out) {
    WriteItem(intern(sexp), ref_table, out);
} */

} // namespace rir