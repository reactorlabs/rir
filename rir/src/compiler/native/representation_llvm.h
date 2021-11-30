#ifndef PIR_COMPILER_REPRESENTATION_LLVM_H
#define PIR_COMPILER_REPRESENTATION_LLVM_H

#include "types_llvm.h"

#include "compiler/pir/pir.h"

#include <iostream>

namespace rir {
namespace pir {

struct Rep {
    enum Type {
        Bottom,
        i32,
        f64,
        SEXP,
        DeoptReason,
        Invalid,
    };
    Rep() : t(Bottom) {}
    // cppcheck-suppress noExplicitConstructor
    Rep(Type t) : t(t) {}

    explicit Rep(llvm::Type* jt) {
        if (jt == t::Void)
            t = Bottom;
        else if (jt == t::Int)
            t = i32;
        else if (jt == t::Double)
            t = f64;
        else if (jt == t::SEXP)
            t = SEXP;
        else if (jt == t::DeoptReasonPtr)
            t = DeoptReason;
        else {
            jt->print(llvm::outs());
            llvm::outs() << "\n";
            assert(false);
        }
    }
    Type t;

    llvm::Type* toLlvm() const {
        switch (t) {
        case Rep::i32:
            return t::Int;
        case Rep::f64:
            return t::Double;
        case Rep::SEXP:
            return t::SEXP;
        case Rep::DeoptReason:
            return t::DeoptReasonPtr;
        case Rep::Bottom:
        case Rep::Invalid:
            break;
        }
        assert(false);
        return nullptr;
    }

    bool merge(const Rep& other) {
        if (t < other.t) {
            if (t == Rep::DeoptReason) {
                t = Rep::Invalid;
                return false;
            }
            t = other.t;
            return true;
        }
        return false;
    }

    bool operator<(const Rep& other) const { return t < other.t; }
    bool operator==(const Rep& other) const { return t == other.t; }
    bool operator!=(const Rep& other) const { return !(*this == other); }

    static Rep Of(PirType t);
    static Rep Of(pir::Value* v);

    friend std::ostream& operator<<(std::ostream& out, const Rep& r) {
        switch (r.t) {
        case Rep::Bottom:
            out << "?";
            break;
        case Rep::i32:
            out << "i32";
            break;
        case Rep::f64:
            out << "f64";
            break;
        case Rep::SEXP:
            out << "SEXP";
            break;
        case Rep::DeoptReason:
            out << "DeoptReason*";
            break;
        case Rep::Invalid:
            out << "invalid";
            break;
        }
        return out;
    }
};

} // namespace pir
} // namespace rir

#endif
