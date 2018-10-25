#ifndef COMPILER_PIR_INSERT_CAST_H
#define COMPILER_PIR_INSERT_CAST_H

#include "../pir/pir.h"

namespace rir {
namespace pir {

/* This pass inserts casts to adjust input types, to argument types of
 * instructions.
 *
 * For example if we get a potentially lazy value from a load, that flows into
 * an eager instruction, then it inserts a force instruction.
 *
 */
class InsertCast {
    BB* start;
    Value* env;
    void apply(BB* b);

  public:
    static pir::Instruction* cast(pir::Value* v, PirType t, Value* env);

    InsertCast(BB* s, Value* e) : start(s), env(e) {}
    void operator()();
};
}
}

#endif
