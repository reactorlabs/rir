#include "UUID.h"
#include "R/Serialize.h"

#include <sstream>

namespace rir {

static size_t nextUuid = 0;

// Generates a random UUID
UUID UUID::random() { return UUID(++nextUuid); }

UUID UUID::deserialize(SEXP refTable, R_inpstream_t inp) {
    UUID uuid;
    InBytes(inp, &uuid.uuid, sizeof(uuid.uuid));
    return uuid;
}

void UUID::serialize(SEXP refTable, R_outpstream_t out) const {
    OutBytes(out, &uuid, sizeof(uuid));
}

std::string UUID::str() {
    std::ostringstream str;
    str << uuid;
    return str.str();
}

bool UUID::operator==(const UUID& other) const { return uuid == other.uuid; }

} // namespace rir
