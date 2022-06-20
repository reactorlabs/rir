#ifndef COMPILER_PIR_QUERY_H
#define COMPILER_PIR_QUERY_H

#include "../pir/pir.h"

#include <unordered_set>

namespace rir {
struct Code;
struct Function;
namespace pir {

/*
 * Simple queries, that should be O(n) to compute
 *
 */
class Query {
  public:
    static bool pure(Code* c);
    static bool pureExceptDeopt(Code* c);
    static bool noEnv(Code* c);
    static bool noParentEnv(Code* c);
    static bool noEnvSpec(Code* c);
    static bool noDeopt(Code* c);
    static PirType returnType(Code* c);
    static std::unordered_set<Value*> returned(Code* c);
};

} // namespace pir
} // namespace rir

#endif
