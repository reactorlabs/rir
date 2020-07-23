#include "UUID.h"
#include "R/Serialize.h"
#include <sstream>
#include <stdlib.h>
#include <time.h>

namespace rir {

UUID UUID::null() { return UUID(); }

// Generates a random UUID
UUID UUID::random() {
    // Dumb algorithm
    UUID uuid;
    for (int i = 0; i < UUID_SIZE; i++) {
        uuid.data[i] = (char)(rand() % 256);
    }
    return uuid;
}

UUID UUID::deserialize(SEXP refTable, R_inpstream_t inp) {
    UUID uuid;
    InBytes(inp, &uuid.data, UUID_SIZE);
    return uuid;
}

void UUID::serialize(SEXP refTable, R_outpstream_t out) const {
    OutBytes(out, &data, UUID_SIZE);
}

size_t UUID::hash() const {
    size_t result = 0;
    for (int byteIndex = 0; byteIndex < sizeof(size_t); byteIndex++) {
        unsigned char byte = (unsigned char)data[byteIndex];
        result |= (size_t)byte << (byteIndex * 8);
    }
    return result;
}

std::string UUID::str() const {
    std::ostringstream str;
    for (int i = 0; i < 8; i++) {
        if (i != 0)
            str << " ";
        str << (int)data[i];
    }
    return str.str();
}

bool UUID::operator==(const UUID& other) const {
    for (int i = 0; i < UUID_SIZE; i++) {
        if (data[i] != other.data[i])
            return false;
    }
    return true;
}

bool UUID::operator!=(const UUID& other) const { return !(*this == other); }

UUID UUID::operator^(const UUID& other) const {
    UUID uuid;
    for (int i = 0; i < UUID_SIZE; i++) {
        uuid.data[i] = data[i] ^ other.data[i];
    }
    return uuid;
}

}; // namespace rir
