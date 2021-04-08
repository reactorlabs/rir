#include "promise.h"
#include "compiler/pir/bb.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "compiler/util/visitor.h"
#include "interpreter/instance.h"
#include "ir/BC.h"

namespace rir {
namespace pir {

Promise::Promise(ClosureVersion* owner, unsigned id, SEXP expression)
    : id(id), owner(owner), expression_(expression) {}

unsigned Promise::srcPoolIdx() const {
    return src_pool_add(globalContext(), expression());
}

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
        case Tag::LdConst:
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
    out << owner->name() << "_" << id;
}

} // namespace pir
} // namespace rir
