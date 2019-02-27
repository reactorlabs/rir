#include "query.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

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

bool Query::noEnvSpec(Code* c) {
    return Visitor::check(c->entry, [](Instruction* i) {
        if (MkEnv::Cast(i) && !MkEnv::Cast(i)->stub) {
            auto env = MkEnv::Cast(i);
            if (!env->hasSingleUse())
                return false;
            auto it = i->bb()->begin();
            while (it != i->bb()->end()) {
                if (auto fs = FrameState::Cast(*it))
                    if (fs->env() == env)
                        return true;
                it++;
            }
            return false;
        }
        return true;
    });
}

bool Query::pure(Code* c) {
    return Visitor::check(c->entry, [](Instruction* i) {
        return !i->hasEffect() && !i->changesEnv();
    });
}

std::unordered_set<Value*> Query::returned(Code* c) {
    std::unordered_set<Value*> returned;
    Visitor::run(c->entry, [&](Instruction* i) {
        Return::Cast(
            i, [&](Return* ret) { returned.insert(ret->arg<0>().val()); });
    });
    return returned;
}

} // namespace pir
} // namespace rir
