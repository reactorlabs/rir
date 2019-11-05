#ifndef COMPILER_PIR_INSERT_CAST_H
#define COMPILER_PIR_INSERT_CAST_H

#include "../analysis/available_checkpoints.h"
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
    Code* code;
    Value* env;
    void apply(BB* b, AvailableCheckpoints& cp);

  public:
    static pir::Instruction* cast(pir::Value* v, PirType t, Value* env);

    InsertCast(Code* s, Value* e) : code(s), env(e) {}
    void operator()();
};
}
}

#endif
