#ifndef COMPILER_CODE_H
#define COMPILER_CODE_H

#include "pir.h"

namespace rir {
namespace pir {

/*
 * A piece of code, starting at the BB entry.
 *
 * Currently: either a Promise or a function.
 *
 */
class Code {
  public:

    enum class Tag : uint8_t {
        Closure,
        Promise,
        _UNUSED_,
    };

    Tag tag;
    BB* entry;

    size_t nextBBId = 0;

    Code(Tag tag) : tag(tag) {}
    void print(std::ostream&);
    ~Code();
};

}
}

#endif
