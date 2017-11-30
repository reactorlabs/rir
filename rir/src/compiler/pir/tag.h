#ifndef COMPILER_TAG_H
#define COMPILER_TAG_H

#include "instruction_list.h"
#include "value_list.h"

#include <cstdint>

namespace rir {
namespace pir {

enum class Tag : uint8_t {
    Unused,
#define V(I) I,
    COMPILER_INSTRUCTIONS(V)
#undef V
#define V(I) I,
        COMPILER_VALUES(V)
#undef V
};

const char* TagToStr(Tag t);
}
}

#endif
