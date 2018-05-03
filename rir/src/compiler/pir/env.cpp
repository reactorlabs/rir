#include "env.h"
#include "pir_impl.h"

#include "utils/capture_out.h"

#include <cassert>

namespace rir {
namespace pir {

void Env::printRef(std::ostream& out) {
    if (this == notClosed()) {
        out << "?";
        return;
    }
    if (this == nil()) {
        out << "nil";
        return;
    }
    assert(rho);
    std::string val;
    {
        CaptureOut rec;
        Rf_PrintValue(rho);
        val = rec();
    }
    out << val.substr(0, val.length() - 1);
}

bool Env::isStaticEnv(Value* v) {
    return Env::Cast(v) && v != Env::notClosed() && v != Env::nil();
}

bool Env::isPirEnv(Value* v) {
    return MkEnv::Cast(v) || LdFunctionEnv::Cast(v);
}

bool Env::isAnyEnv(Value* v) {
    return Env::Cast(v) || MkEnv::Cast(v) || LdFunctionEnv::Cast(v);
}

Value* Env::parentEnv(Value* e) {
    assert(isAnyEnv(e));
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
