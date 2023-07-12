//
// Created by Jakob Hain on 7/10/23.
//

#pragma once

#include "hash/UUID.h"

namespace rir {

/// A unique identifier for a rir object.
///
/// Consists of a "big ID" consisting of the EVP hash of the immutable data,
/// and a "small ID" consisting of the hash of the mutable semantic data.
/// Mutable non-semantic data, such as function default args, is not included.
#pragma pack(push, 1)
struct RirUID {
    /// Create a RirUID by hashing data
    struct Hasher;
    /// The big ID
    UUID big;
    /// The small ID
    UUID small;
    /// Create a RirUID from big and small IDs
    RirUID(const UUID& big, const UUID& small) : big(big), small(small) {}
    /// The null RirUID (0x0)
    RirUID() : big(UUID()), small(UUID()) {}
    /// Deserialize a RirUID from the R stream
    static RirUID deserialize(__attribute__((unused)) SEXP refTable, R_inpstream_t inp);
    /// Serialize a RirUID to the R stream
    void serialize(SEXP refTable, R_outpstream_t out) const;

    friend std::ostream& operator<<(std::ostream&, const RirUID&);
    /// `false` iff this is the null RirUID
    operator bool() const;
    bool operator==(const RirUID& other) const;
    bool operator!=(const RirUID& other) const;
    friend struct std::hash<RirUID>;
};
#pragma pack(pop)

struct RirUID::Hasher {
    UUID::Hasher big;
    UUID::Hasher small;

    /// Get the RirUID. After calling this, you can't call hashBytes anymore.
    RirUID finalize();
};

} // namespace rir

namespace std {
template <>
struct hash<rir::RirUID> {
    std::size_t operator()(const rir::RirUID& v) const;
};
} // namespace std
