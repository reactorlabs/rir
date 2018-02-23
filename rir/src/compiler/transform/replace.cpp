#include "replace.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

void Replace::usesOfValue(Instruction* i, Value* old, Value* rpl) {
    i->map_arg([&](Value* v, PirType t) {
        if (v == old)
            return rpl;
        return v->replaceRefs(old, rpl);
    });
}

void Replace::usesOfValue(BB* start, Value* old, Value* rpl) {
    Visitor::run(start, [&](BB* bb) {
        for (auto i : *bb) {
            usesOfValue(i, old, rpl);
        }
    });
}
}
}
