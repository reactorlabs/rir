#ifndef COMPILER_CODE_H
#define COMPILER_CODE_H

#include "pir.h"

namespace rir {
namespace pir {

class Code {
  public:
    BB* entry;

    Code();
    void print(std::ostream&);
    void print() { print(std::cerr); }
    ~Code();
};
}
}

#endif
