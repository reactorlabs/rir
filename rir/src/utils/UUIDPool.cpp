//
// Created by Jakob Hain on 6/1/23.
//

#include "UUIDPool.h"
#include "ByteBuffer.h"
#include "api.h"

namespace rir {

std::unordered_map<UUID, SEXP> UUIDPool::interned;

#ifdef DO_INTERN
/// Hash the SEXP in a way that ignores pointers
static UUID hashSexp(SEXP e) {
    ByteBuffer buffer;
    serialize(e, buffer);
    return UUID::hash(buffer.data(), buffer.size());
}
#endif

SEXP UUIDPool::intern(SEXP e) {
#ifdef DO_INTERN
    UUID uuid = hashSexp(e);
    if (interned.count(uuid)) {
        return interned.at(uuid);
    }
    interned[uuid] = e;
#endif
    return e;
}

SEXP UUIDPool::readItem(SEXP ref_table, R_inpstream_t in) {
    return intern(ReadItem(ref_table, in));
}

} // namespace rir