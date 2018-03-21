#ifndef COMPILER_TAG_H
#define COMPILER_TAG_H

#include "instruction_list.h"
#include "value_list.h"

#include <cstdint>

namespace rir {
namespace pir {

/*
 * Every Value has a unique tag, to be able to safely downcast values and
 * instructions (without relying on RTTI). Instructions are values too.
 *
 */
enum class Tag : uint8_t {
    _UNUSED_,
#define V(I) I,
    COMPILER_INSTRUCTIONS(V)
#undef V
#define V(I) I,
        COMPILER_VALUES(V)
#undef V
};

const char* tagToStr(Tag t);
}
}

#endif
