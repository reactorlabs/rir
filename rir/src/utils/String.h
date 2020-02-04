#ifndef RIR_STRING_H
#define RIR_STRING_H

namespace rir {

static constexpr bool inline staticStringEqual(char const* a, char const* b) {
    return *a == *b && (*a == '\0' || staticStringEqual(a + 1, b + 1));
}

} // namespace rir

#endif
