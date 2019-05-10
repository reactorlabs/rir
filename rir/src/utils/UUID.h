#pragma once

#include <R/r.h>

#define UUID_SIZE 64

namespace rir {

class UUID {
    char data[UUID_SIZE] = {};

    UUID() {}

  public:
    // Generates a random UUID
    static UUID random();
    static UUID deserialize(SEXP refTable, R_inpstream_t inp);
    void serialize(SEXP refTable, R_outpstream_t out) const;

    bool operator==(const UUID& other) const;
    UUID operator^(const UUID& other) const;
    friend struct std::hash<UUID>;
};

}; // namespace rir

namespace std {
template <>
struct hash<rir::UUID> {
    std::size_t operator()(const rir::UUID& v) const { return *(size_t*)v; }
};
} // namespace std
