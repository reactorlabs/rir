#include "query.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

bool Query::noEnv(Code* c) {
    return Visitor::check(c->entry, [](BB* bb) {
        for (auto i : *bb)
            if (MkEnv::Cast(i))
                return false;
        return true;
    });
}

bool Query::pure(Code* c) {
    return Visitor::check(c->entry, [](BB* bb) {
        for (auto i : *bb)
            if (i->mightIO() || i->changesEnv())
                return false;
        return true;
    });
}

std::set<Value*> Query::returned(Code* c) {
    std::set<Value*> returned;
    Visitor::run(c->entry, [&](BB* bb) {
        for (auto i : *bb) {
            auto ret = Return::Cast(i);
            if (ret)
                returned.insert(ret->arg<0>());
        }
    });
    return returned;
}
}
}
