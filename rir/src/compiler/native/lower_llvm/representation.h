#pragma once

#include "llvm_imports.h"

#include "types_llvm.h"
#include <vector>

namespace rir {
namespace pir {

using namespace llvm;

struct Representation {
    enum Type { Bottom, Integer, Real, Sexp };

    Representation() : t(Bottom) {}
    // cppcheck-suppress noExplicitConstructor
    Representation(Type t) : t(t) {}
    explicit Representation(llvm::Type* jt) {
        if (jt == t::Void)
            t = Bottom;
        else if (jt == t::Int)
            t = Integer;
        else if (jt == t::Double)
            t = Real;
        else if (jt == t::SEXP)
            t = Sexp;
        else {
            jt->print(outs());
            outs() << "\n";
            assert(false);
        }
    }
    Type t;
    operator llvm::Type*() {
        switch (t) {
        case Representation::Bottom:
            return t::Void;
        case Representation::Integer:
            return t::Int;
        case Representation::Real:
            return t::Double;
        case Representation::Sexp:
            return t::SEXP;
        }
        assert(false);
        return nullptr;
    }
    bool merge(Representation& other) {
        if (t < other.t) {
            *this = other;
            return true;
        }
        return false;
    }
    bool operator==(const Representation& other) const { return t == other.t; }
    bool operator!=(const Representation& other) const {
        return !(*this == other);
    }
};

static Representation representationOf(PirType t) {
    if (t.isA(NativeType::test))
        return Representation::Integer;
    if (t.isA(PirType(RType::logical).scalar()))
        return Representation::Integer;
    if (t.isA(PirType(RType::integer).scalar()))
        return Representation::Integer;
    if (t.isA(PirType(RType::real).scalar()))
        return Representation::Real;
    return Representation::Sexp;
}

static Representation representationOf(Value* v) {
    return representationOf(v->type);
}

} // namespace pir
} // namespace rir