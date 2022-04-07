#include "promise.h"
#include "bc/BC.h"
#include "closure_version.h"
#include "compiler/pir/bb.h"
#include "compiler/pir/instruction.h"
#include "compiler/util/visitor.h"
#include "interpreter/instance.h"

namespace rir {
namespace pir {

Promise::Promise(ClosureVersion* owner, unsigned id, rir::Code* rirSrc)
    : id(id), owner(owner), rirSrc_(rirSrc), srcPoolIdx_(rirSrc->src) {
    assert(src_pool_at(srcPoolIdx_));
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

bool Promise::trivial() const {
    auto bb = entry;
    if (bb->isEmpty())
        bb = bb->next();
    for (auto i : *bb) {
        switch (i->tag) {
        case Tag::Visible:
        case Tag::Return:
            break;
        default:
            return false;
        }
    }
    return true;
}

void Promise::printName(std::ostream& out) const {
    out << owner->name() << "_p" << id;
}

} // namespace pir
} // namespace rir
