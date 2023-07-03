#pragma once

#include "R/r.h"

#include <string>
#include <openssl/evp.h>

namespace rir {

class UUIDHasher;

/// A 256-bit UUID
#pragma pack(push, 1)
class UUID {
    uint64_t a;
    uint64_t b;
    uint64_t c;
    uint64_t d;

    UUID(uint64_t a, uint64_t b, uint64_t c, uint64_t d)
        : a(a), b(b), c(c), d(d) {}

  public:
    /// The null UUID (0x0)
    UUID() : a(0), b(0), c(0), d(0) {}
    /// Generates a UUID for the data
    static UUID hash(const void* data, size_t size);
    /// Deserialize a UUID from the R stream
    static UUID deserialize(__attribute__((unused)) SEXP refTable, R_inpstream_t inp);
    /// Serialize a UUID to the R stream
    void serialize(SEXP refTable, R_outpstream_t out) const;
    /// Print the UUID as a hexadecimal string
    std::string str() const;

    friend std::ostream& operator<<(std::ostream&, const UUID&);
    /// `false` iff this is the null UUID (0x0)
    operator bool() const;
    bool operator==(const UUID& other) const;
    bool operator!=(const UUID& other) const;
    friend struct std::hash<UUID>;

    friend class UUIDHasher;
};
#pragma pack(pop)

/// Create a UUID for a stream of data
class UUIDHasher {
    EVP_MD_CTX* ctx;
    bool finalized;

  public:
    UUIDHasher();
    ~UUIDHasher();
    /// Hash the data-structure, which should not contain any references
    template<typename T> void hashBytesOf(T c) { hashBytes(&c, sizeof(T)); }
    /// Hash the data, which should not contain any references
    void hashBytes(const void* data, size_t size);
    /// Get the UUID. After calling this, you can't call hashBytes anymore.
    UUID finalize();
};

} // namespace rir

namespace std {
template <>
struct hash<rir::UUID> {
    std::size_t operator()(const rir::UUID& v) const;
};
} // namespace std
