#include "promise.h"
#include "compiler/pir/bb.h"
#include "compiler/pir/instruction.h"
#include "compiler/util/visitor.h"
#include "interpreter/instance.h"
#include "ir/BC.h"

namespace rir {
namespace pir {

Promise::Promise(ClosureVersion* owner, unsigned id, rir::Code* rirSrc)
    : id(id), owner(owner), rirSrc_(rirSrc), srcPoolIdx_(rirSrc->src) {
    assert(src_pool_at(globalContext(), srcPoolIdx_));
}

unsigned Promise::srcPoolIdx() const { return srcPoolIdx_; }

LdFunctionEnv* Promise::env() const {
    LdFunctionEnv* e = nullptr;
    Visitor::run(entry, [&](Instruction* i) {
        if (auto ld = LdFunctionEnv::Cast(i)) {
            assert(!e);
            e = ld;
        }
    });
    return e;
}

} // namespace pir
} // namespace rir
