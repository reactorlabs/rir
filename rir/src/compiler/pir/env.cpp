#include "env.h"
#include "R/Printing.h"
#include "pir_impl.h"

#include <cassert>

namespace rir {
namespace pir {

void Env::printRef(std::ostream& out) const {
    if (this == notClosed()) {
        out << "?";
        return;
    } else if (this == elided()) {
        out << "elided";
        return;
    } else if (this == nil()) {
        out << "nil";
        return;
    }

    assert(rho);
    if (rho == R_GlobalEnv) {
        out << "GlobalEnv";
    } else if (rho == R_BaseNamespace) {
        out << "BaseNamespace";
    } else {
        auto val = Print::dumpSexp(rho);
        out << val.substr(0, val.length() - 1);
    }
}

bool Env::isStaticEnv(Value* v) {
    return Env::Cast(v) && v != Env::notClosed() && v != Env::nil() &&
           v != Env::elided();
}

bool Env::isPirEnv(Value* v) {
    return MkEnv::Cast(v) || LdFunctionEnv::Cast(v);
}

bool Env::isAnyEnv(Value* v) {
    return Env::Cast(v) || MkEnv::Cast(v) || LdFunctionEnv::Cast(v) ||
           MaterializeEnv::Cast(v);
}

Value* Env::parentEnv(Value* e) {
    assert(isAnyEnv(e));
    if (Cast(e))
        return Cast(e)->parent;
    if (MkEnv::Cast(e))
        return MkEnv::Cast(e)->lexicalEnv();
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
} // namespace pir
} // namespace rir
