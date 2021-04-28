#include "query.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/Symbols.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

bool Query::noDeopt(Code* c) {
    return Visitor::check(c->entry,
                          [](Instruction* i) { return !Deopt::Cast(i); });
}

bool Query::noEnv(Code* c) {
    return Visitor::check(c->entry,
                          [](Instruction* i) { return !MkEnv::Cast(i); });
}

bool Query::noParentEnv(Code* c) {
    return Visitor::check(c->entry,
                          [](Instruction* i) {
          return !i->hasEnv() || i->env() != Env::notClosed();
    });
}

bool Query::noEnvSpec(Code* c) {
    return Visitor::check(c->entry, [](Instruction* i) {
        if (MkEnv::Cast(i) && !MkEnv::Cast(i)->stub) {
            auto env = MkEnv::Cast(i);
            return env->bb()->isDeopt();
        }
        return true;
    });
}

bool Query::pure(Code* c) {
    return Visitor::check(
        c->entry, [&](Instruction* i) { return !i->hasStrongEffects(); });
}

bool Query::pureExceptDeopt(Code* c) {
    return Visitor::check(c->entry, [&](Instruction* i) {
        return !i->hasStrongEffects() || Deopt::Cast(i);
    });
}

PirType Query::returnType(Code* c) {
    PirType ret = PirType::bottom();
    bool first = false;
    Visitor::run(c->entry, [&](BB* bb) {
        if (bb->isExit()) {
            if (auto r = Return::Cast(bb->last())) {
                auto t = r->arg(0).val()->type;
                if (first) {
                    first = false;
                    ret = t;
                } else {
                    ret = ret | t;
                }
            }
        }
    });
    return ret;
}

std::unordered_set<Value*> Query::returned(Code* c) {
    std::unordered_set<Value*> returned;
    Visitor::run(c->entry, [&](BB* bb) {
        if (!bb->isEmpty() && Return::Cast(bb->last()))
            returned.insert(bb->last()->arg(0).val());
    });
    return returned;
}

} // namespace pir
} // namespace rir
