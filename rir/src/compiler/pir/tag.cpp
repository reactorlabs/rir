#include "tag.h"

#include <cassert>

namespace rir {
namespace pir {

const char* tagToStr(Tag tag) {
    switch (tag) {
#define V(I)                                                                   \
    case Tag::I:                                                               \
        return #I;
        COMPILER_INSTRUCTIONS(V)
#undef V
#define V(I)                                                                   \
    case Tag::I:                                                               \
        return #I;
        COMPILER_VALUES(V)
#undef V
    case Tag::_UNUSED_:
        assert(false);
    }
    assert(false);
    return "";
}
}
}
