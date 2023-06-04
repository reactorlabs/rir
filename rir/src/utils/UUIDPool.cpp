//
// Created by Jakob Hain on 6/1/23.
//

#include "UUIDPool.h"
#include "api.h"

namespace rir {

std::unordered_map<UUID, SEXP> UUIDPool::interned;

SEXP UUIDPool::intern(SEXP e, UUID hash) {
#ifdef DO_INTERN
    PROTECT(e);
    SLOWASSERT(hashSexp(e) == hashSexp(e) && "SEXP hash isn't deterministic or `hash` in `UUIDPool::intern(e, hash)` is wrong");
    UNPROTECT(1);
    if (interned.count(hash)) {
        return interned.at(hash);
    }
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

struct RStreamAndHasher {
    R_inpstream_t stream;
    UUIDHasher hasher;

    explicit RStreamAndHasher(R_inpstream_t stream) : stream(stream) {}
    const UUID& uuid() const { return hasher.uuid(); }
};

static int rStreamInChar(R_inpstream_t hashIn) {
    auto streamAndHasher = (RStreamAndHasher*)hashIn->data;
    auto in = streamAndHasher->stream;
    auto hasher = &streamAndHasher->hasher;

    auto data = in->InChar(in);
    hasher->hashUChar((unsigned char)data);
    return data;
}

static void rStreamInBytes(R_inpstream_t hashIn, void* data, int size) {
    auto streamAndHasher = (RStreamAndHasher*)hashIn->data;
    auto in = streamAndHasher->stream;
    auto hasher = &streamAndHasher->hasher;

    in->InBytes(in, data, size);
    hasher->hashBytes(data, size);
}

SEXP UUIDPool::readItem(SEXP ref_table, R_inpstream_t in) {
    RStreamAndHasher streamAndHasher{in};
    R_inpstream_st hashIn{};
    R_InitInPStream(
        &hashIn,
        (R_pstream_data_t)&streamAndHasher,
        in->type,
        rStreamInChar,
        rStreamInBytes,
        in->InPersistHookFunc,
        in->InPersistHookData
    );
    SEXP sexp = ReadItem(ref_table, &hashIn);
    return intern(sexp, streamAndHasher.uuid());
}

void UUIDPool::writeItem(SEXP sexp, SEXP ref_table, R_outpstream_t out) {
    WriteItem(sexp, ref_table, out);
}


} // namespace rir