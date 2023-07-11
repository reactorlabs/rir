#include "UUID.h"
#include "R/Serialize.h"

#include <sstream>
#include <iomanip>

namespace rir {

UUID UUID::hash(const void* data, size_t size) {
    UUID::Hasher hasher;
    hasher.hashBytes(data, size);
    return hasher.finalize();
}

UUID UUID::deserialize(__attribute__((unused)) SEXP _refTable, R_inpstream_t inp) {
    UUID uuid;
    InBytes(inp, &uuid.a, sizeof(uuid.a));
    InBytes(inp, &uuid.b, sizeof(uuid.b));
    InBytes(inp, &uuid.c, sizeof(uuid.c));
    InBytes(inp, &uuid.d, sizeof(uuid.d));
    return uuid;
}

void UUID::serialize(__attribute__((unused)) SEXP _refTable, R_outpstream_t out) const {
    OutBytes(out, &a, sizeof(a));
    OutBytes(out, &b, sizeof(b));
    OutBytes(out, &c, sizeof(c));
    OutBytes(out, &d, sizeof(d));
}

std::string UUID::str() const {
    std::ostringstream str;
    str << std::setfill('0') << std::setw(sizeof(a)) << std::right
        << std::hex << a << b << c << d << std::dec;
    return str.str();
}

std::ostream& operator<<(std::ostream& stream, const UUID& uuid) {
    stream << "0x" << uuid.str();
    return stream;
}

UUID::operator bool() const {
    return a || b || c || d;
}

bool UUID::operator==(const UUID& other) const {
    return a == other.a && b == other.b && c == other.c && d == other.d;
}

bool UUID::operator!=(const UUID& other) const {
    return a != other.a || b != other.b || c != other.c || d != other.d;
}

UUID::Hasher::Hasher() : ctx(EVP_MD_CTX_new()), finalized(false) {
    if (!ctx) {
        assert(false && "Failed to create EVP_MD_CTX");
    }
    if (EVP_DigestInit_ex(ctx, EVP_sha256(), nullptr) != 1) {
        assert(false && "Failed to initialize EVP_MD_CTX");
    }
}

UUID::Hasher::~Hasher() {
    assert(finalized && "UUID::Hasher was not finalized");
}

void UUID::Hasher::hashBytes(const void* data, size_t size) {
    // Update the context with new data
    if (EVP_DigestUpdate(ctx, data, size) != 1) {
        assert(false && "Failed to update hash with new data");
    }
}

UUID UUID::Hasher::finalize() {
    unsigned int len = EVP_MD_size(EVP_sha256());
    unsigned char result[EVP_MAX_MD_SIZE];  // Holds the final hash

    if (EVP_DigestFinal_ex(ctx, result, &len) != 1) {
        assert(false && "Failed to finalize hash");
    }

    UUID uuid(
        *(reinterpret_cast<uint64_t*>(&result[0])),
        *(reinterpret_cast<uint64_t*>(&result[8])),
        *(reinterpret_cast<uint64_t*>(&result[16])),
        *(reinterpret_cast<uint64_t*>(&result[24]))
    );

    EVP_MD_CTX_free(ctx);
    finalized = true;

    return uuid;
}

} // namespace rir

namespace std {
std::size_t hash<rir::UUID>::operator()(const rir::UUID& v) const {
    return v.a ^ v.b ^ v.c ^ v.d;
}
} // namespace std
