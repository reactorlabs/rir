#include "env.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Env::printRef(std::ostream& out) {
    if (this == theContext())
        out << "?";
    else
        assert(false);
}

bool Env::isEnv(Value* v) {
    return Env::Cast(v) || MkEnv::Cast(v) || LdFunctionEnv::Cast(v);
}

Value* Env::parentEnv(Value* e) {
    assert(isEnv(e));
    if (Cast(e))
        return Cast(e)->parent;
    if (MkEnv::Cast(e))
        return MkEnv::Cast(e)->parent();
    assert(false);
    return nullptr;
}

bool Env::isParentEnv(Value* a, Value* b) {
    if (a == b)
        return false;
    b = parentEnv(b);
    while (b != nullptr) {
        if (a == b)
            return true;
        b = parentEnv(b);
    }
    return false;
}
}
}
