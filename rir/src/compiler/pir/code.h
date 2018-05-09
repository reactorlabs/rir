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
    BB* entry;

    size_t maxBBId = 0;

    Code();
    void print(std::ostream&);
    ~Code();
};
}
}

#endif
