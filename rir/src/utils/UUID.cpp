#include "UUID.h"
#include "R/Serialize.h"
#include <stdlib.h>
#include <time.h>

namespace rir {

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

bool UUID::operator==(const UUID& other) const {
    for (int i = 0; i < UUID_SIZE; i++) {
        if (data[i] != other.data[i])
            return false;
    }
    return true;
}

}; // namespace rir
