#ifndef COMPILER_TYPE_H
#define COMPILER_TYPE_H

#include <cassert>
#include <cstdint>
#include <iostream>
#include <vector>

#include "utils/Bitset.h"

#include "R/r.h"

namespace rir {
namespace pir {

enum class RType : uint8_t {
    unused,

    nil,
    cons,

    symbol,
    chars,

    logical,
    integer,
    dbls,
    strs,
    vecs,

    raw,

    closure,
    prom,

    code,
    env,
    ast,

    max
};

enum class NativeType : uint8_t {
    unused,

    test,

    max
};

enum class TypeFlags : uint8_t {
    unused,

    lazy,
    missing,
    obj,
    is_scalar,
    rtype,

    max
};

/*
 * Values in PIR are either types from R (RType), or native types (NativeType).
 *
 * In both cases we use union types, represented by a bitset.
 *
 * There is an additional flags bitset, that adds some modifiers.
 *
 * As an example, an R integer, that is potentially promised, has:
 * 
 *  - flag : TypeFlag::rtype | TypeFlag::lazy
 *  - t_.r : RType::integer 
 *
 * A machine boolean has type:
 *
 *  - flag : ()
 *  - t_.n : NativeType::test
 *
 * An R value (not a promise), has:
 *
 *  - flag : TypeFlag::rtype
 *  - t_.r : RType::symbol | ... | RType::ast
 *
 */
struct PirType {
    typedef BitSet<uint32_t, RType> RTypeSet;
    typedef BitSet<uint8_t, NativeType> NativeTypeSet;
    typedef BitSet<uint8_t, TypeFlags> FlagSet;

    FlagSet flags_;

    union Type {
        RTypeSet r;
        NativeTypeSet n;
        Type(RTypeSet r) : r(r) {}
        Type(NativeTypeSet n) : n(n) {}
    };
    Type t_;

    static FlagSet defaultRTypeFlags() { return FlagSet() | TypeFlags::rtype; }

    PirType() : PirType(RTypeSet()) {}
    PirType(const RType& t) : flags_(defaultRTypeFlags()), t_(t) {
        assert(t > RType::unused && t < RType::max);
    }
    PirType(const NativeType& t) : t_(t) {
        assert(t > NativeType::unused && t < NativeType::max);
    }
    PirType(const RTypeSet& t) : flags_(defaultRTypeFlags()), t_(t) {}
    PirType(const NativeTypeSet& t) : t_(t) {}
    PirType(SEXP);

    void operator=(const PirType& o) {
        flags_ = o.flags_;
        if (flags_.includes(TypeFlags::rtype))
            t_.r = o.t_.r;
        else
            t_.n = o.t_.n;
    }

    static PirType num() {
        return PirType(RType::logical) | RType::integer | RType::dbls;
    }
    static PirType val() {
        return PirType(vecs() | list() | RType::symbol | RType::chars |
                       RType::raw | RType::closure | RType::prom | RType::code |
                       RType::env | RType::ast);
        // TODO: for now we ignore object systems..
        //    .orObj();
    }
    static PirType vecs() { return num() | RType::strs | RType::vecs; }

    static PirType valOrMissing() { return val().orMissing(); }
    static PirType valOrLazy() { return val().orLazy(); }
    static PirType list() { return PirType(RType::cons) | RType::nil; }
    static PirType any() { return val().orLazy().orMissing(); }

    bool maybeMissing() const { return flags_.includes(TypeFlags::missing); }
    bool maybeLazy() const { return flags_.includes(TypeFlags::lazy); }
    bool maybeObj() const { return flags_.includes(TypeFlags::obj); }
    bool isScalar() const { return flags_.includes(TypeFlags::is_scalar); }
    bool isRType() const { return flags_.includes(TypeFlags::rtype); }

    PirType scalar() const {
        assert(isRType());
        PirType t = *this;
        t.flags_.set(TypeFlags::is_scalar);
        return t;
    }

    PirType orObj() const {
        assert(isRType());
        PirType t = *this;
        t.flags_.set(TypeFlags::obj);
        return t;
    }

    PirType orMissing() const {
        assert(isRType());
        PirType t = *this;
        t.flags_.set(TypeFlags::missing);
        return t;
    }

    PirType orLazy() const {
        assert(isRType());
        PirType t = *this;
        t.flags_.set(TypeFlags::lazy);
        return t;
    }

    PirType baseType() const {
        assert(isRType());
        return PirType(t_.r);
    }

    static const PirType voyd() { return NativeTypeSet(); }

    static const PirType missing() { return bottom().orMissing(); }

    static const PirType bottom() { return PirType(RTypeSet()); }

    PirType operator|(const PirType& o) const {
        assert(isRType() == o.isRType());
        if (!isRType()) {
            return t_.n | o.t_.n;
        }
        PirType r = t_.r | o.t_.r;
        if (o.maybeLazy())
            r.flags_.set(TypeFlags::lazy);
        if (o.maybeMissing())
            r.flags_.set(TypeFlags::missing);
        if (isScalar() && o.isScalar())
            r.flags_.set(TypeFlags::is_scalar);
        else
            r.flags_.clear(TypeFlags::is_scalar);
        if (o.maybeObj())
            r.flags_.set(TypeFlags::obj);

        return r;
    }

    bool operator==(const NativeType& o) const {
        return !isRType() && t_.n == o;
    }

    bool operator!=(const PirType& o) const { return !(*this == o); }

    bool operator==(const PirType& o) const {
        return flags_ == o.flags_ &&
               (isRType() ? t_.r == o.t_.r : t_.n == o.t_.n);
    }

    bool operator>=(const PirType& o) const {
        if (isRType() != o.isRType()) {
            return false;
        }
        if (!isRType()) {
            return t_.n.includes(o.t_.n);
        }
        if ((!maybeLazy() && o.maybeLazy()) ||
            (!maybeMissing() && o.maybeMissing()) ||
            (!maybeObj() && o.maybeObj()) || (isScalar() && !o.isScalar())) {
            return false;
        }
        return t_.r.includes(o.t_.r);
    }

    void print();
};

inline std::ostream& operator<<(std::ostream& out, NativeType t) {
    switch (t) {
    case NativeType::test:
        out << "t";
        break;
    case NativeType::unused:
    case NativeType::max:
        assert(false);
        break;
    }
    return out;
}

inline std::ostream& operator<<(std::ostream& out, RType t) {
    switch (t) {
    case RType::ast:
        out << "ast";
        break;
    case RType::raw:
        out << "raw";
        break;
    case RType::vecs:
        out << "vec";
        break;
    case RType::chars:
        out << "char";
        break;
    case RType::dbls:
        out << "dble";
        break;
    case RType::strs:
        out << "str";
        break;
    case RType::env:
        out << "env";
        break;
    case RType::code:
        out << "code";
        break;
    case RType::cons:
        out << "cons";
        break;
    case RType::prom:
        out << "prom";
        break;
    case RType::nil:
        out << "nil";
        break;
    case RType::closure:
        out << "cls";
        break;
    case RType::symbol:
        out << "sym";
        break;
    case RType::integer:
        out << "int";
        break;
    case RType::logical:
        out << "lgl";
        break;
    case RType::unused:
    case RType::max:
        assert(false);
        break;
    }
    return out;
}

inline std::ostream& operator<<(std::ostream& out, PirType t) {
    if (!t.isRType()) {
        if (t.t_.n.size() == 0) {
            out << "void";
            return out;
        }

        std::vector<NativeType> ts;
        for (NativeType bt = NativeType::unused; bt != NativeType::max;
             bt = (NativeType)((size_t)bt + 1)) {
            if (t.t_.n.includes(bt))
                ts.push_back(bt);
        }
        if (ts.size() > 1)
            out << "(";
        for (auto i = ts.begin(); i != ts.end(); ++i) {
            out << *i;
            if (i + 1 != ts.end())
                out << "|";
        }
        if (ts.size() > 1)
            out << ")";
        return out;
    }

    if (t.baseType() >= PirType::val().baseType()) {
        out << "val";
    } else {
        std::vector<RType> ts;
        for (RType bt = RType::unused; bt != RType::max;
             bt = (RType)((size_t)bt + 1)) {
            if (t.t_.r.includes(bt))
                ts.push_back(bt);
        }
        if (ts.size() != 1)
            out << "(";
        for (auto i = ts.begin(); i != ts.end(); ++i) {
            out << *i;
            if (i + 1 != ts.end())
                out << "|";
        }
        if (ts.size() != 1)
            out << ")";
    }

    if (t.isScalar())
        out << "$";
    if (t.maybeObj())
        out << "&";
    if (t.maybeLazy())
        out << "^";
    if (t.maybeMissing())
        out << "?";

    return out;
}
}
}

#endif
