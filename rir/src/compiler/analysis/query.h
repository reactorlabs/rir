#ifndef COMPILER_PIR_QUERY_H
#define COMPILER_PIR_QUERY_H

#include "../pir/pir.h"

namespace rir {
namespace pir {

class Query {
  public:
    static bool pure(Code* c);
    static Value* getReturn(Code* c);
};
}
}

#endif
