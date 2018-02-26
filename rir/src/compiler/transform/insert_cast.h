#ifndef COMPILER_PIR_INSERT_CAST_H
#define COMPILER_PIR_INSERT_CAST_H

#include "../pir/pir.h"

namespace rir {
namespace pir {

class InsertCast {
    BB* start;

  public:
    static pir::Instruction* cast(pir::Value* v, PirType t);

    InsertCast(BB* s) : start(s) {}
    void operator()();
    void apply(BB* b);
};
}
}

#endif
