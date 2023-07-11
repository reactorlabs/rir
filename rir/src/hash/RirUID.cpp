//
// Created by Jakob Hain on 7/10/23.
//

#include "RirUID.h"
#include <ostream>

namespace rir {

RirUID RirUID::deserialize(SEXP refTable, R_inpstream_t inp) {
    auto big = UUID::deserialize(refTable, inp);
    auto small = UUID::deserialize(refTable, inp);
    return {big, small};
}

void RirUID::serialize(SEXP refTable, R_outpstream_t out) const {
    big.serialize(refTable, out);
    small.serialize(refTable, out);
}

std::ostream& operator<<(std::ostream& out, const RirUID& uid) {
    out << "[" << uid.big << ", " << uid.small << "]";
    return out;
}

RirUID::operator bool() const {
    return big || small;
}

bool RirUID::operator==(const RirUID& other) const {
    return big == other.big && small == other.small;
}

bool RirUID::operator!=(const RirUID& other) const {
    return big != other.big || small != other.small;
}

RirUID RirUID::Hasher::finalize() {
    return {big.finalize(), small.finalize()};
}

} // namespace rir

namespace std {
std::size_t hash<rir::RirUID>::operator()(const rir::RirUID& v) const {
    return hash<rir::UUID>()(v.big) ^ hash<rir::UUID>()(v.small);
}
} // namespace std
