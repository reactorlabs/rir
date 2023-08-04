#include "UUID.h"
#include "R/Serialize.h"

#include "xxhash.h"
#include <iomanip>
#include <sstream>

namespace rir {

UUID UUID::hash(const void* data, size_t size) {
    UUID::Hasher hasher;
    hasher.hashBytes(data, size);
    return hasher.finalize();
}

UUID UUID::deserialize(__attribute__((unused)) SEXP _refTable, R_inpstream_t inp) {
    UUID uuid;
    InBytes(inp, &uuid.high, sizeof(uuid.high));
    InBytes(inp, &uuid.low, sizeof(uuid.low));
    return uuid;
}

void UUID::serialize(__attribute__((unused)) SEXP _refTable, R_outpstream_t out) const {
    OutBytes(out, &high, sizeof(high));
    OutBytes(out, &low, sizeof(low));
}

std::string UUID::str() const {
    std::ostringstream str;
    str << std::setfill('0') << std::setw(sizeof(high) + sizeof(low))
        << std::right << std::hex << high << low << std::dec;
    return str.str();
}

std::ostream& operator<<(std::ostream& stream, const UUID& uuid) {
    stream << "0x" << uuid.str();
    return stream;
}

UUID::operator bool() const {
    return high || low;
}

bool UUID::operator==(const UUID& other) const {
    return high == other.high && low == other.low;
}

bool UUID::operator!=(const UUID& other) const {
    return high != other.high || low != other.low;
}

UUID::Hasher::Hasher() : state(XXH3_createState()), finalized(false) {
    assert(state && "Failed to create hash state");

    if (XXH3_128bits_reset(state) == XXH_ERROR) {
        XXH3_freeState(state);
        assert(false && "Failed to initialize hash state as 128 bits");
    }
}

UUID::Hasher::~Hasher() {
    assert(finalized && "UUID::Hasher was not finalized");
}

void UUID::Hasher::hashBytesOfCString(const char* c) {
    hashBytes(c, strlen(c));
}

void UUID::Hasher::hashBytes(const void* data, size_t size) {
    assert(!finalized && "UUID::Hasher was already finalized");
    
    if (XXH3_128bits_update(state, data, size) == XXH_ERROR) {
        XXH3_freeState(state);
        assert(false && "Failed to update hash state");
    }
}

UUID UUID::Hasher::finalize() {
    assert(!finalized && "UUID::Hasher was already finalized");
    finalized = true;
    
    auto digest = XXH3_128bits_digest(state);
    UUID uuid{digest.high64, digest.low64};
    XXH3_freeState(state);
    return uuid;
}

} // namespace rir

namespace std {
std::size_t hash<rir::UUID>::operator()(const rir::UUID& v) const {
    return v.high ^ v.low;
}
} // namespace std
