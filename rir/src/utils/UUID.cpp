#include "UUID.h"
#include "R/Serialize.h"

#include <sstream>

namespace rir {

// Generates a UUID by hashing the data
UUID UUID::hash(const void* data, size_t size) {
    UUID uuid;
    while (size > sizeof(uint64_t) * 2) {
        uuid.msb ^= *(uint64_t*)data;
        uuid.lsb ^= *(uint64_t*)((uintptr_t)data + sizeof(uint64_t));
        data = (void*)((uintptr_t)data + sizeof(uint64_t) * 2);
        size -= sizeof(uint64_t) * 2;
    }
    // region manual-case 0-16 boilerplate
    switch (size) {
        case 0:
            break;
        case 1:
            uuid.msb ^= *(uint8_t*)data;
            break;
        case 2:
            uuid.msb ^= *(uint16_t*)data;
            break;
        case 3:
            uuid.msb ^= *(uint16_t*)data;
            uuid.msb ^= (uint32_t)*(uint8_t*)((uintptr_t)data + sizeof(uint16_t)) << 16;
            break;
        case 4:
            uuid.msb ^= *(uint32_t*)data;
            break;
        case 5:
            uuid.msb ^= *(uint32_t*)data;
            uuid.msb ^= (uint64_t)*(uint8_t*)((uintptr_t)data + sizeof(uint32_t)) << 32;
            break;
        case 6:
            uuid.msb ^= *(uint32_t*)data;
            uuid.msb ^= (uint64_t)*(uint16_t*)((uintptr_t)data + sizeof(uint32_t)) << 32;
            break;
        case 7:
            uuid.msb ^= *(uint32_t*)data;
            uuid.msb ^= (uint64_t)*(uint16_t*)((uintptr_t)data + sizeof(uint32_t)) << 32;
            uuid.msb ^= (uint64_t)*(uint8_t*)((uintptr_t)data + sizeof(uint32_t) + sizeof(uint16_t)) << 48;
            break;
        case 8:
            uuid.msb ^= *(uint64_t*)data;
            break;
        case 9:
            uuid.msb ^= *(uint64_t*)data;
            uuid.lsb ^= *(uint8_t*)((uintptr_t)data + sizeof(uint64_t));
            break;
        case 10:
            uuid.msb ^= *(uint64_t*)data;
            uuid.lsb ^= *(uint16_t*)((uintptr_t)data + sizeof(uint64_t));
            break;
        case 11:
            uuid.msb ^= *(uint64_t*)data;
            uuid.lsb ^= *(uint16_t*)((uintptr_t)data + sizeof(uint64_t));
            uuid.lsb ^= (uint32_t)*(uint8_t*)((uintptr_t)data + sizeof(uint64_t) + sizeof(uint16_t)) << 16;
            break;
        case 12:
            uuid.msb ^= *(uint64_t*)data;
            uuid.lsb ^= *(uint32_t*)((uintptr_t)data + sizeof(uint64_t));
            break;
        case 13:
            uuid.msb ^= *(uint64_t*)data;
            uuid.lsb ^= *(uint32_t*)((uintptr_t)data + sizeof(uint64_t));
            uuid.lsb ^= (uint64_t)*(uint8_t*)((uintptr_t)data + sizeof(uint64_t) + sizeof(uint32_t)) << 32;
            break;
        case 14:
            uuid.msb ^= *(uint64_t*)data;
            uuid.lsb ^= *(uint32_t*)((uintptr_t)data + sizeof(uint64_t));
            uuid.lsb ^= (uint64_t)*(uint16_t*)((uintptr_t)data + sizeof(uint64_t) + sizeof(uint32_t)) << 32;
            break;
        case 15:
            uuid.msb ^= *(uint64_t*)data;
            uuid.lsb ^= *(uint32_t*)((uintptr_t)data + sizeof(uint64_t));
            uuid.lsb ^= (uint64_t)*(uint16_t*)((uintptr_t)data + sizeof(uint64_t) + sizeof(uint32_t)) << 32;
            uuid.lsb ^= (uint64_t)*(uint8_t*)((uintptr_t)data + sizeof(uint64_t) + sizeof(uint32_t) + sizeof(uint16_t)) << 48;
            break;
        default:
            assert(false);
    }
    // endregion
    return uuid;
}

UUID UUID::deserialize(__attribute__((unused)) SEXP _refTable, R_inpstream_t inp) {
    UUID uuid;
    InBytes(inp, &uuid.msb, sizeof(uuid.msb));
    InBytes(inp, &uuid.lsb, sizeof(uuid.lsb));
    return uuid;
}

void UUID::serialize(__attribute__((unused)) SEXP _refTable, R_outpstream_t out) const {
    OutBytes(out, &msb, sizeof(msb));
    OutBytes(out, &lsb, sizeof(lsb));
}

std::string UUID::str() const {
    std::ostringstream str;
    str << std::hex << msb << lsb;
    return str.str();
}

std::ostream& operator<<(std::ostream& stream, const UUID& uuid) {
    stream << "UUID(" << uuid.str() << ")";
    return stream;
}

bool UUID::operator==(const UUID& other) const {
    return msb == other.msb && lsb == other.lsb;
}

bool UUID::operator!=(const UUID& other) const {
    return !(*this == other);
}

void UUIDHasher::hashBytes(const void* data, size_t size) {
    // XORs each byte to the UUID over and over, preserving offset so that
    // multiple calls to hashBytes over the same sequence of bytes produces the
    // same result as a single call to hashBytes over the entire sequence.
    // ---
    // The actual implementation is a bit optimized. Maybe the compiler is smart
    // enough to do this automatically, but I'm not sure:
    // - First we XOR bytes until offset == 0 again
    //   - Case where offset < 64-bits (8 bytes, sizeof(uint64_t))
    while (offset != 0 && offset < sizeof(uint64_t)) {
        if (size == 0) {
            break;
        }
        _uuid.msb ^= (uint64_t)*(uint8_t*)data << (offset * 8);
        offset++;
        data = (void*)((uintptr_t)data + 1);
        size--;
    }
    //  - Case where offset < 128-bits (16 bytes, sizeof(uint64_t * 2), sizeof(UUID))).
    //    If offset is already 0 both this and the above are skipped.
    while (offset != 0) {
        if (size == 0) {
            break;
        }
        _uuid.lsb ^= (uint64_t)*(uint8_t*)data << ((offset - sizeof(uint64_t)) * 8);
        offset++;
        data = (void*)((uintptr_t)data + 1);
        size--;
        if (offset == sizeof(uint64_t) * 2) {
            offset = 0;
        }
    }
    // - Next we can XOR 128-bit (16 byte, sizeof(uint64_t) * 2, sizeof(UUID))
    //   chunks at a time, until we have less than 128 bits left
    while (size >= sizeof(uint64_t) * 2) {
        _uuid.msb ^= *(uint64_t*)data;
        _uuid.lsb ^= *(uint64_t*)((uintptr_t)data + sizeof(uint64_t));
        data = (void*)((uintptr_t)data + sizeof(uint64_t) * 2);
        size -= sizeof(uint64_t) * 2;
    }
    // - Finally we XOR the remaining bytes, one at a time
    while (size > 0) {
        if (offset < sizeof(uint64_t)) {
            _uuid.msb ^= (uint64_t)*(uint8_t*)data << (offset * 8);
        } else {
            _uuid.lsb ^= (uint64_t)*(uint8_t*)data << ((offset - sizeof(uint64_t)) * 8);
        }
        offset++;
        data = (void*)((uintptr_t)data + 1);
        size--;
    }
}

} // namespace rir

namespace std {
std::size_t hash<rir::UUID>::operator()(const rir::UUID& v) const {
    return v.msb ^ v.lsb;
}
} // namespace std
