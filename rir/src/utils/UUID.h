#pragma once

#include <R/r.h>

#include <string>

namespace rir {

class UUIDHasher;

/// A 128-bit UUID
#pragma pack(push, 1)
class UUID {
    uint64_t msb;
    uint64_t lsb;

    UUID() : msb(0), lsb(0) {}
    UUID(uint64_t msb, uint64_t lsb) : msb(msb), lsb(lsb) {}

  public:
    /// Generates a UUID by hashing the data
    static UUID hash(const void* data, size_t size);
    static UUID deserialize(__attribute__((unused)) SEXP refTable, R_inpstream_t inp);
    void serialize(SEXP refTable, R_outpstream_t out) const;
    std::string str() const;

    friend std::ostream& operator<<(std::ostream&, const UUID&);
    bool operator==(const UUID& other) const;
    bool operator!=(const UUID& other) const;
    friend struct std::hash<UUID>;

    friend class UUIDHasher;
};
#pragma pack(pop)

class UUIDHasher {
    UUID _uuid;
    size_t offset = 0;

  public:
    UUIDHasher() = default;
    template<typename T> void hashBytesOf(T c) { hashBytes(&c, sizeof(T)); }
    void hashBytes(const void* data, size_t size);
    const UUID& uuid() const { return _uuid; }
};

} // namespace rir

namespace std {
template <>
struct hash<rir::UUID> {
    std::size_t operator()(const rir::UUID& v) const;
};
} // namespace std
