#pragma once

#include <R/r.h>

namespace rir {

class UUID {
    size_t uuid;

    UUID() {}
    explicit UUID(size_t v) : uuid(v) {}

  public:
    // Generates a random UUID
    static UUID random();
    static UUID deserialize(SEXP refTable, R_inpstream_t inp);
    void serialize(SEXP refTable, R_outpstream_t out) const;
    std::string str();

    bool operator==(const UUID& other) const;
    friend struct std::hash<UUID>;
};

}; // namespace rir

namespace std {
template <>
struct hash<rir::UUID> {
    std::size_t operator()(const rir::UUID& v) const { return v.uuid; }
};
} // namespace std
