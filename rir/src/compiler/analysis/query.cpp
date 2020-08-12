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

std::unordered_set<Value*> Query::returned(Code* c) {
    std::unordered_set<Value*> returned;
    Visitor::run(c->entry, [&](BB* bb) {
        if (!bb->isEmpty() && Return::Cast(bb->last()))
            returned.insert(bb->last()->arg(0).val());
    });
    return returned;
}

bool Query::needsPromargs(rir::Function* f) { return needsPromargs(f->body()); }

bool Query::needsPromargs(rir::Code* c) {
    auto pc = c->code();
    std::vector<BC::FunIdx> promises;
    while (pc != c->endCode()) {
        auto bc = BC::advance(&pc, c);
        if (bc.bc == Opcode::ldfun_) {
            auto sym = Pool::get(bc.immediate.pool);
            if (sym == symbol::UseMethod || sym == symbol::Recall ||
                sym == symbol::eval || sym == symbol::standardGeneric ||
                sym == symbol::dispatchGeneric)
                return true;
        }
        bc.addMyPromArgsTo(promises);
    }
    for (auto& p : promises)
        if (needsPromargs(c->getPromise(p)))
            return true;

    return false;
}

} // namespace pir
} // namespace rir
