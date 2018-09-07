#ifndef COMPILER_PIR_QUERY_H
#define COMPILER_PIR_QUERY_H

#include "../pir/pir.h"

#include <unordered_set>

namespace rir {
namespace pir {

/*
 * Simple queries, that should be O(n) to compute
 *
 */
class Query {
  public:
    static bool pure(Code* c);
    static bool noEnv(Code* c);
    static bool envOnlyBeforeDeopt(Code* c);
    static bool noDeopt(Code* c);
    static std::unordered_set<Value*> returned(Code* c);
};
}
}

#endif
