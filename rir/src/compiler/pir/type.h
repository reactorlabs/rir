#ifndef COMPILER_TYPE_H
#define COMPILER_TYPE_H

#include <cassert>
#include <cstdint>
#include <iostream>
#include <vector>

#include "R/r_incl.h"
#include "runtime/Context.h"
#include "runtime/TypeFeedback.h"
#include "utils/EnumSet.h"

namespace rir {
namespace pir {

/*
 * Values in PIR are either types from R (RType), or native types (NativeType).
 *
 * In both cases we use union types, represented by a bitset.
 *
 * There is an additional flags bitset, that adds some modifiers.
 *
 * As an example, an R integer, that is potentially promised, has:
 *
 *  - flags_ : TypeFlag::rtype | TypeFlag::lazy
 *  - t_.r   : RType::integer
 *
 * A machine boolean has type:
 *
 *  - flags_ : ()
 *  - t_.n   : NativeType::test
 *
 * An R value (not a promise), has:
 *
 *  - flags_ : TypeFlag::rtype
 *  - t_.r   : RType::symbol | ... | RType::ast
 *
 */

enum class RType : uint8_t {
    _UNUSED_,

    nil,
    list,

    sym,
    chr,

    logical,
    integer,
    real,
    str,
    vec,
    cplx,

    raw,

    closure,
    prom,

    missing,
    unbound,

    code,
    env,
    ast,
    expressions,

    dots,
    expandedDots,

    other,

    FIRST = nil,
    LAST = other
};

enum class NativeType : uint8_t {
    _UNUSED_,

    checkpoint,
    frameState,
    context,

    FIRST = checkpoint,
    LAST = context,
};

enum class TypeFlags : uint8_t {
    _UNUSED_,

    lazy,
    promiseWrapped,
    maybeNotScalar,
    maybeObject,
    maybeAttrib,
    maybeNAOrNaN,
    rtype,

    FIRST = lazy,
    LAST = rtype
};

/*
 * A PirType can either represent a union of R types or of native types.
 *
 * `a :> b` is implemented by `a.isSuper(b)`. The primitive types are enumerated
 * by RType and NativeType respectively.
 *
 * TypeFlags are additional features. The element `rtype` of the type flags is
 * abused to store, if the type is an R type or native type.
 *
 * `a.flags_.includes(b.flags_)` is a necessary condition for `a :> b`.
 *
 * The "BaseType" is the (union) R or native type, stripped of all flags.
 *
 */

struct PirType {
    typedef EnumSet<RType, uint32_t> RTypeSet;
    typedef EnumSet<NativeType, uint32_t> NativeTypeSet;
    typedef EnumSet<TypeFlags, uint32_t> FlagSet;

    FlagSet flags_;

    union Type {
        RTypeSet r;
        NativeTypeSet n;
        constexpr Type(RTypeSet r) : r(r) {}
        constexpr Type(NativeTypeSet n) : n(n) {}
    };
    Type t_;

    static constexpr FlagSet defaultRTypeFlags() {
        return FlagSet() | TypeFlags::maybeNotScalar | TypeFlags::maybeNAOrNaN |
               TypeFlags::rtype;
    }

    static constexpr FlagSet topRTypeFlags() {
        return FlagSet() | TypeFlags::lazy | TypeFlags::promiseWrapped |
               TypeFlags::maybeObject | TypeFlags::maybeAttrib |
               TypeFlags::maybeNotScalar | TypeFlags::maybeNAOrNaN |
               TypeFlags::rtype;
    }
    static constexpr FlagSet optimisticRTypeFlags() {
        return FlagSet() | TypeFlags::rtype;
    }

    static constexpr PirType optimistic() {
        PirType t;
        t.flags_ = optimisticRTypeFlags();
        return t;
    }

    constexpr PirType() : flags_(topRTypeFlags()), t_(RTypeSet()) {}

    // cppcheck-suppress noExplicitConstructor
    constexpr PirType(const RType& t)
        : flags_(defaultRTypeFlags()), t_(RTypeSet(t)) {}
    // cppcheck-suppress noExplicitConstructor
    constexpr PirType(const RTypeSet& t) : flags_(defaultRTypeFlags()), t_(t) {}
    constexpr PirType(const RTypeSet& t, const FlagSet& f) : flags_(f), t_(t) {}

    // cppcheck-suppress noExplicitConstructor
    constexpr PirType(const NativeType& t) : t_(NativeTypeSet(t)) {}
    // cppcheck-suppress noExplicitConstructor
    constexpr PirType(const NativeTypeSet& t) : t_(t) {}

    explicit PirType(SEXP);
    constexpr PirType(const PirType& other)
        : flags_(other.flags_), t_(other.t_) {}

    explicit PirType(uint64_t);
    uint64_t serialize() {
        uint64_t i;
        static_assert(sizeof(*this) <= sizeof(uint64_t), "PirType is too big");
        memcpy(&i, this, sizeof(*this));
        return i;
    }

    PirType& operator=(const PirType& o) {
        flags_ = o.flags_;
        if (isRType())
            t_.r = o.t_.r;
        else
            t_.n = o.t_.n;
        return *this;
    }

    inline PirType mergeWithConversion(const PirType& other) const {
        assert(isRType() && other.isRType());

        auto res = *this | other;

        auto fixup = [&](PirType a, PirType b) {
            auto isInt = PirType() | RType::integer;
            auto isReal = PirType() | RType::real;
            auto isIntIsh = PirType() | RType::integer | RType::logical;
            auto isNum =
                PirType() | RType::real | RType::integer | RType::logical;
            if (a.isA(isIntIsh) && b.isA(isInt)) {
                res = res & isInt;
                return true;
            } else if (a.isA(isNum) && b.isA(isReal)) {
                res = res & isReal;
                return true;
            }
            return false;
        };

        if (!isVoid() && !other.isVoid())
            fixup(*this, other) || fixup(other, *this);

        return res;
    }

    void merge(const ObservedValues& other);
    void merge(SEXPTYPE t);

    static constexpr PirType intRealLgl() {
        return PirType(RType::integer) | RType::real | RType::logical;
    }
    static constexpr PirType intReal() {
        return PirType(RType::integer) | RType::real;
    }
    static constexpr PirType num() {
        return intReal() | RType::logical | RType::cplx;
    }
    static constexpr PirType atomOrSimpleVec() {
        return num() | RType::sym | RType::chr | RType::str | RType::code;
    }
    static constexpr PirType val() {
        return PirType(vecs() | list() | RType::sym | RType::chr | RType::raw |
                       RType::closure | RType::prom | RType::code | RType::env |
                       RType::missing | RType::unbound | RType::ast |
                       RType::dots | RType::other)
            .orObject()
            .orAttribs();
    }
    static constexpr PirType vecs() {
        return num() | RType::str | RType::raw | RType::vec |
               RType::expressions;
    }
    static constexpr PirType closure() { return RType::closure; }

    static constexpr PirType dotsArg() {
        return (PirType() | RType::missing | RType::dots).notPromiseWrapped();
    }

    static constexpr PirType simpleScalarInt() {
        return PirType(RType::integer).simpleScalar();
    }

    static constexpr PirType simpleScalarReal() {
        return PirType(RType::real).simpleScalar();
    }

    static constexpr PirType simpleScalarLogical() {
        return PirType(RType::logical).simpleScalar();
    }
    static constexpr PirType test() {
        return simpleScalarLogical().notNAOrNaN();
    }

    static constexpr PirType simpleScalarString() {
        return PirType(RType::str).simpleScalar();
    }

    static constexpr PirType anySimpleScalar() {
        return (PirType(RType::integer) | RType::real | RType::logical)
            .scalar();
    }

    static constexpr PirType simpleVector() { return PirType(RType::vec); }

    constexpr bool unboxable() {
        return isA(simpleScalarLogical()) || isA(simpleScalarInt()) ||
               isA(simpleScalarReal());
    }

    static constexpr PirType promiseWrappedVal() {
        return val().orPromiseWrapped();
    }

    static constexpr PirType valOrLazy() { return val().orLazy(); }
    static constexpr PirType list() {
        return PirType(RType::list) | RType::nil;
    }
    // Note: This includes any R type, but not native types
    static constexpr PirType any() { return val().orLazy(); }

    RIR_INLINE constexpr bool maybeMissing() const {
        if (!isRType())
            return false;
        return t_.r.includes(RType::missing);
    }
    RIR_INLINE constexpr bool maybeLazy() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::lazy);
    }
    RIR_INLINE constexpr bool maybePromiseWrapped() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::promiseWrapped) ||
               flags_.includes(TypeFlags::lazy);
    }
    RIR_INLINE constexpr bool maybeNAOrNaN() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::maybeNAOrNaN);
    }
    RIR_INLINE constexpr bool isSimpleScalar() const {
        return isScalar() && !maybeHasAttrs();
    }
    RIR_INLINE constexpr bool isScalar() const {
        if (!isRType())
            return true;
        return !flags_.includes(TypeFlags::maybeNotScalar);
    }
    RIR_INLINE constexpr bool isRType() const {
        return flags_.includes(TypeFlags::rtype);
    }
    RIR_INLINE constexpr bool isRType(const RType& o) const {
        return isRType() && t_.r == o;
    }
    RIR_INLINE constexpr bool maybe(PirType type) const {
        auto inter = (*this & type);
        return !inter.isVoid();
    }
    RIR_INLINE constexpr bool maybe(RType type) const {
        return isRType() && t_.r.includes(type);
    }
    RIR_INLINE constexpr bool maybeObj() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::maybeObject);
    }
    RIR_INLINE constexpr bool maybeHasAttrs() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::maybeAttrib);
    }

    RIR_INLINE constexpr PirType operator|(const PirType& o) const {
        assert(isRType() == o.isRType());

        PirType r;
        if (isRType())
            r.t_ = t_.r | o.t_.r;
        else
            r.t_ = t_.n | o.t_.n;

        r.flags_ = flags_ | o.flags_;
        return r;
    }

    RIR_INLINE constexpr PirType operator&(const PirType& o) const {
        assert(isRType() == o.isRType());

        PirType r;
        if (isRType())
            r.t_ = t_.r & o.t_.r;
        else
            r.t_ = t_.n & o.t_.n;

        r.flags_ = flags_ & o.flags_;
        return r;
    }

    bool maybeReferenceCounted() const {
        static constexpr RTypeSet refcount =
            RTypeSet() | RType::logical | RType::integer | RType::real |
            RType::str | RType::vec | RType::cplx;
        return isRType() && t_.r.intersects(refcount);
    }

    PirType constexpr notObject() const {
        assert(isRType());
        return PirType(t_.r, flags_ & ~FlagSet(TypeFlags::maybeObject));
    }

    PirType constexpr noAttribs() const {
        assert(isRType());
        return PirType(t_.r, flags_ & ~(FlagSet() | TypeFlags::maybeAttrib |
                                        TypeFlags::maybeObject));
    }

    PirType constexpr notMissing() const {
        assert(isRType());
        return PirType(t_.r & ~RTypeSet(RType::missing), flags_);
    }

    RIR_INLINE constexpr PirType notNAOrNaN() const {
        assert(isRType());
        return PirType(t_.r, flags_ & ~FlagSet(TypeFlags::maybeNAOrNaN));
    }

    RIR_INLINE constexpr PirType scalar() const {
        assert(isRType());
        return PirType(t_.r, flags_ & ~FlagSet(TypeFlags::maybeNotScalar));
    }

    RIR_INLINE constexpr PirType simpleScalar() const {
        return scalar().noAttribs();
    }

    RIR_INLINE constexpr PirType notT(RType t) const {
        assert(isRType());
        return PirType(t_.r & ~RTypeSet(t), flags_);
    }

    RIR_INLINE constexpr PirType orT(RType t) const {
        assert(isRType());
        return PirType(t_.r | t, flags_);
    }

    RIR_INLINE constexpr PirType orNAOrNaN() const {
        assert(isRType());
        return PirType(t_.r, flags_ | TypeFlags::maybeNAOrNaN);
    }

    RIR_INLINE constexpr PirType orNotScalar() const {
        assert(isRType());
        return PirType(t_.r, flags_ | TypeFlags::maybeNotScalar);
    }

    RIR_INLINE constexpr PirType orPromiseWrapped() const {
        assert(isRType());
        return PirType(t_.r, flags_ | TypeFlags::promiseWrapped);
    }

    RIR_INLINE constexpr PirType orLazy() const {
        assert(isRType());
        return PirType(t_.r,
                       flags_ | TypeFlags::lazy | TypeFlags::promiseWrapped);
    }

    RIR_INLINE constexpr PirType orObject() const {
        assert(isRType());
        return PirType(t_.r, flags_ | TypeFlags::maybeObject |
                                 TypeFlags::maybeAttrib);
    }

    RIR_INLINE constexpr PirType orAttribs() const {
        assert(isRType());
        return PirType(t_.r, flags_ | TypeFlags::maybeAttrib);
    }

    PirType constexpr notPromiseWrapped() const {
        return PirType(t_.r, flags_ & ~(FlagSet() | TypeFlags::lazy |
                                        TypeFlags::promiseWrapped));
    }

    PirType constexpr notLazy() const {
        return PirType(t_.r, flags_ & ~FlagSet(TypeFlags::lazy));
    }

    PirType constexpr forced() const {
        if (!maybePromiseWrapped())
            return *this;
        if (!maybeLazy())
            return PirType(t_.r, flags_ & ~FlagSet(TypeFlags::promiseWrapped));
        return PirType(
            // forcing can return the missing marker value
            t_.r | RType::missing,
            flags_ & ~(FlagSet(TypeFlags::lazy) | TypeFlags::promiseWrapped));
    }

    RIR_INLINE constexpr PirType baseType() const {
        assert(isRType());
        return PirType(t_.r);
    }

    // Type of <this>[<idx>] or <this>[<idx>, <idx>]
    PirType subsetType(PirType idx) const {
        assert(isRType());
        if (isA(PirType(RType::nil).orAttribs())) {
            // NULL
            return RType::nil;
        }
        if (isA((num() | RType::str | RType::list | RType::code).orAttribs())) {
            // If the index is out of bounds, NA is returned (even if both args
            // are non-NA) so we must add orNAOrNaN()
            if (idx.isA(PirType(RType::str).scalar()))
                return scalar().orAttribs().orNAOrNaN();
            // e.g. c(1,2,3)[-1] returns c(2,3)
            return orNotScalar().orNAOrNaN();
        } else if (isA(RType::vec)) {
            return PirType(RType::vec);
        } else if (isA(PirType(RType::vec).orAttribs())) {
            return PirType(RType::vec).orAttribs();
        } else if (!maybeHasAttrs() && !PirType(RType::prom).isA(*this)) {
            // Something else
            return val().notMissing();
        } else {
            // Possible object
            return valOrLazy();
        }
    }

    // Type of <this>[[<idx>]] or <this>[[<idx>, <idx>]]
    PirType extractType(PirType idx) const {
        assert(isRType());
        if (isA(PirType(RType::nil).orAttribs())) {
            // NULL
            return RType::nil;
        }
        if (isA((num() | RType::str | RType::list | RType::code).orAttribs())) {
            return scalar();
        } else if (isA(PirType(RType::vec).orAttribs())) {
            return val().notMissing();
        } else if (!maybeObj() && !PirType(RType::prom).isA(*this)) {
            // Something else
            return val().notMissing();
        } else {
            // Possible object
            return valOrLazy();
        }
    }

    // Type of c(<this>, ...numArgs)
    PirType collectionType(int numArgs) const {
        assert(isRType());
        if (isA(RType::nil)) {
            return RType::nil;
        } else if (isA(num() | RType::str | RType::nil)) {
            PirType t = *this;
            t.t_.r.reset(RType::nil);
            if (numArgs > 1) {
                // The orNAOrNaN is only needed because we don't check NA or NaN
                // on vectors, technically the vector doesn't contain NA or NaN
                t = t.orNotScalar().orNAOrNaN();
            }
            return t;
        } else if (t_.r.contains(RType::prom) ||
                   t_.r.contains(RType::expandedDots)) {
            return val();
        } else {
            return forced().notObject().orNotScalar().orNAOrNaN() | RType::vec;
        }
    }

    RIR_INLINE void setNotScalar() { *this = orNotScalar(); }
    RIR_INLINE void setNotMissing() { *this = notMissing(); }
    RIR_INLINE void setNotObject() { *this = notObject(); }
    RIR_INLINE void setNoAttribs() { *this = noAttribs(); }
    RIR_INLINE void setNotNAOrNaN() { *this = notNAOrNaN(); }
    RIR_INLINE void setMaybeNAOrNaN() { *this = orNAOrNaN(); }
    RIR_INLINE void setScalar() { *this = scalar(); }

    RIR_INLINE void setScalar(RType rtype) {
        setScalar();
        t_.r = RTypeSet(rtype);
    }

    constexpr bool isVoid() const {
        return isRType() ? t_.r.empty() : t_.n.empty();
    }

    static const PirType voyd() { return PirType(NativeTypeSet()); }
    static const PirType bottom() { return optimistic(); }

    void fromContext(const Context&, unsigned arg, unsigned nargs,
                     bool forced = false);

    RIR_INLINE bool operator==(const NativeType& o) const {
        return !isRType() && t_.n == o;
    }

    RIR_INLINE bool operator!=(const PirType& o) const { return !(*this == o); }

    RIR_INLINE bool operator==(const PirType& o) const {
        return flags_ == o.flags_ &&
               (isRType() ? t_.r == o.t_.r : t_.n == o.t_.n);
    }

    constexpr bool isA(const PirType& o) const { return o.isSuper(*this); }

    constexpr bool isSuper(const PirType& o) const {
        if (isRType() != o.isRType()) {
            return false;
        }
        if (!isRType()) {
            return t_.n.includes(o.t_.n);
        }
        if ((!maybeLazy() && o.maybeLazy()) ||
            (!maybePromiseWrapped() && o.maybePromiseWrapped()) ||
            (!maybeObj() && o.maybeObj()) ||
            (!maybeHasAttrs() && o.maybeHasAttrs()) ||
            (!maybeNAOrNaN() && o.maybeNAOrNaN()) ||
            (isScalar() && !o.isScalar())) {
            return false;
        }
        return t_.r.includes(o.t_.r);
    }

    // Is val an instance of this type?
    bool isInstance(SEXP val) const;

    void print(std::ostream& out = std::cout) const;

    size_t hash() const {
        return hash_combine(flags_.to_i(),
                            isRType() ? t_.r.to_i() : t_.n.to_i());
    }
};

inline std::ostream& operator<<(std::ostream& out, NativeType t) {
    switch (t) {
    case NativeType::context:
        out << "ct";
        break;
    case NativeType::checkpoint:
        out << "cp";
        break;
    case NativeType::frameState:
        out << "fs";
        break;
    case NativeType::_UNUSED_:
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
    case RType::vec:
        out << "vec";
        break;
    case RType::chr:
        out << "char";
        break;
    case RType::real:
        out << "real";
        break;
    case RType::cplx:
        out << "complex";
        break;
    case RType::str:
        out << "str";
        break;
    case RType::env:
        out << "env";
        break;
    case RType::code:
        out << "code";
        break;
    case RType::expressions:
        out << "expressions";
        break;
    case RType::list:
        out << "list";
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
    case RType::sym:
        out << "sym";
        break;
    case RType::integer:
        out << "int";
        break;
    case RType::logical:
        out << "lgl";
        break;
    case RType::missing:
        out << "miss";
        break;
    case RType::dots:
        out << "dots";
        break;
    case RType::expandedDots:
        out << "*dots";
        break;
    case RType::other:
        out << "other";
        break;
    case RType::unbound:
        out << "_";
        break;
    case RType::_UNUSED_:
        assert(false);
        break;
    }
    return out;
}

inline std::ostream& operator<<(std::ostream& out, PirType t) {
    if (!t.isRType()) {
        if (t.t_.n.empty()) {
            out << "void";
            return out;
        }

        if (t.t_.n.count() > 1)
            out << "(";
        for (auto e = t.t_.n.begin(); e != t.t_.n.end(); ++e) {
            out << *e;
            if (e + 1 != t.t_.n.end())
                out << "|";
        }
        if (t.t_.n.count() > 1)
            out << ")";
        return out;
    }

    // If the base type is at least a value, then it's a value
    if (t.isRType() && PirType::val().notMissing().baseType() == t.baseType()) {
        out << "val";
    } else if (t.isRType() && PirType::val().baseType() == t.baseType()) {
        out << "val?";
    } else if (t.isRType() &&
               PirType::num().notMissing().baseType() == t.baseType()) {
        out << "num";
    } else {
        if (t.t_.r.count() > 1)
            out << "(";
        for (auto i = t.t_.r.begin(); i != t.t_.r.end(); ++i) {
            out << *i;
            if (i + 1 != t.t_.r.end())
                out << "|";
        }
        if (t.t_.r.count() > 1)
            out << ")";
    }

    if (t.isScalar())
        out << "$";
    if (!t.maybeNAOrNaN())
        out << "#";
    if (t.maybeLazy())
        out << "^";
    else if (t.maybePromiseWrapped())
        out << "~";
    if (!t.maybeHasAttrs())
        out << "\"";
    else if (!t.maybeObj())
        out << "'";

    return out;
}
} // namespace pir
} // namespace rir

namespace std {
template <>
struct hash<rir::pir::PirType> {
    size_t operator()(const rir::pir::PirType& t) const { return t.hash(); }
};
}

#endif
