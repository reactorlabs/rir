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

#include "aa.h"

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
    special,
    builtin,
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
    deoptReason,
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
    maybeNotFastVecelt,
    maybeAttrib,

    maybeNAOrNaN,
    maybeMissing,
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
    typedef EnumSet<NativeType, uint8_t> NativeTypeSet;
    typedef EnumSet<TypeFlags, uint16_t> FlagSet;

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
               TypeFlags::maybeObject | TypeFlags::maybeNotFastVecelt |
               TypeFlags::maybeAttrib | TypeFlags::maybeNotScalar |
               TypeFlags::maybeNAOrNaN | TypeFlags::rtype;
    }
    static constexpr FlagSet optimisticRTypeFlags() {
        return FlagSet() | TypeFlags::rtype;
    }

    static PirType optimistic() {
        PirType t;
        t.flags_ = optimisticRTypeFlags();
        return t;
    }

    constexpr PirType() : PirType(RTypeSet(), topRTypeFlags()) {}

    // cppcheck-suppress noExplicitConstructor
    constexpr PirType(const RType& t)
        : PirType(RTypeSet(t), defaultRTypeFlags()) {}
    // cppcheck-suppress noExplicitConstructor
    constexpr PirType(const RTypeSet& t) : PirType(t, defaultRTypeFlags()) {}

    constexpr PirType(const RTypeSet& t, const FlagSet& f) : flags_(f), t_(t) {
        ensureMissingInvariant(t_.r, flags_);
    }

    // cppcheck-suppress noExplicitConstructor
    constexpr PirType(const NativeType& t) : t_(NativeTypeSet(t)) {
        assert(!isRType());
    }
    // cppcheck-suppress noExplicitConstructor
    constexpr PirType(const NativeTypeSet& t) : t_(t) {}

    explicit PirType(SEXP);

    constexpr PirType(const PirType& other)
        : flags_(other.flags_), t_(other.t_) {

        // relax ctx
        AA::singleton().copyInfo(&other, this);
    }

    ~PirType() {
        // relax ctx
        AA::singleton().unregisterType(this);
    }

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

        // relax ctx
        AA::singleton().copyInfo(&o, this);
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

    PirType orSexpTypes(const PirType& other) const {
        assert(isRType() && other.isRType());
        auto ret = PirType(t_.r | other.t_.r, flags_);

        // relax ctx
        AA::singleton().copyInfo(&other, &ret);

        return ret;
    }

    void merge(const ObservedValues& other);
    void merge(SEXPTYPE t);

    static PirType intReal() { return PirType(RType::integer) | RType::real; }
    static PirType intRealLgl() { return intReal() | RType::logical; }
    static PirType num() { return intRealLgl() | RType::cplx; }
    static PirType atomOrSimpleVec() {
        return num() | RType::sym | RType::chr | RType::str | RType::code;
    }
    static PirType val() {
        return PirType(vecs() | list() | RType::sym | RType::chr | RType::raw |
                       RType::closure | RType::special | RType::builtin |
                       RType::prom | RType::code | RType::env | RType::unbound |
                       RType::ast | RType::dots | RType::other)
            .orMaybeMissing()
            .orNAOrNaN()
            .orAttribsOrObj();
    }
    static PirType vecs() {
        return num() | RType::str | RType::raw | RType::vec |
               RType::expressions;
    }
    static PirType closure() {
        return PirType(RType::closure).orAttribsOrObj();
    }
    static PirType special() {
        return PirType(RType::special).orAttribsOrObj();
    }
    static PirType builtin() {
        return PirType(RType::builtin).orAttribsOrObj();
    }
    static PirType function() {
        return closure() | RType::special | RType::builtin;
    }

    static PirType env() { return PirType(RType::env).orAttribsOrObj(); }

    static PirType theMissingValue() {
        return PirType(RType::missing).orMaybeMissing();
    }

    static PirType dotsArg() {
        return (PirType() | RType::dots).notPromiseWrapped().orMaybeMissing();
    }

    static PirType simpleScalarInt() {
        return PirType(RType::integer).simpleScalar();
    }

    static PirType simpleScalarReal() {
        return PirType(RType::real).simpleScalar();
    }

    static PirType simpleScalarLogical() {
        return PirType(RType::logical).simpleScalar();
    }
    static PirType test() { return simpleScalarLogical().notNAOrNaN(); }

    static PirType simpleScalarString() {
        return PirType(RType::str).simpleScalar();
    }

    static PirType anySimpleScalar() {
        return (PirType(RType::integer) | RType::real | RType::logical)
            .simpleScalar();
    }

    static PirType simpleVector() { return PirType(RType::vec); }

    bool unboxable() const {
        return isA(simpleScalarLogical()) || isA(simpleScalarInt()) ||
               isA(simpleScalarReal());
    }

    static PirType promiseWrappedVal() { return val().orPromiseWrapped(); }

    static PirType valOrLazy() { return val().orLazy(); }
    static PirType list() { return PirType(RType::list) | RType::nil; }
    // Note: This includes any R type, but not native types
    static PirType any() { return val().orLazy(); }

    inline bool maybeMissing() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::maybeMissing);
    }
    inline constexpr bool maybeLazy() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::lazy);
    }
    inline constexpr bool maybePromiseWrapped() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::promiseWrapped) ||
               flags_.includes(TypeFlags::lazy);
    }
    inline constexpr bool maybeNAOrNaN() const {
        if (!isRType())
            return false;
        return flags_.includes(TypeFlags::maybeNAOrNaN);
    }
    inline constexpr bool isSimpleScalar() const {
        return isScalar() && !maybeHasAttrs();
    }

    inline constexpr bool isScalar() const {
        if (!isRType())
            return true;
        return !flags_.includes(TypeFlags::maybeNotScalar);
    }
    inline constexpr bool isRType() const {
        return flags_.includes(TypeFlags::rtype);
    }
    inline constexpr bool isRType(const RType& o) const {
        return isRType() && t_.r == o;
    }
    inline bool maybe(PirType type) const {
        auto inter = (*this & type);
        return !inter.isVoid();
    }
    inline constexpr bool maybe(RType type) const {
        return isRType() && t_.r.includes(type);
    }
    inline constexpr bool maybeObj(bool record = true) const {
        if (!isRType())
            return false;
        auto res = flags_.includes(TypeFlags::maybeObject);

        if (record && !res) {
            AA::singleton().recordNotObject(this);
        }

        assert(!res || (flags_.includes(TypeFlags::maybeAttrib) &&
                        flags_.includes(TypeFlags::maybeNotFastVecelt)));
        return res;
    }

    inline constexpr bool maybeNotFastVecelt() const {
        if (!isRType())
            return false;
        auto res = flags_.includes(TypeFlags::maybeNotFastVecelt);
        assert(!res || flags_.includes(TypeFlags::maybeAttrib));
        return res;
    }

    inline constexpr bool maybeHasAttrs() const {
        if (!isRType())
            return false;
        auto res = flags_.includes(TypeFlags::maybeAttrib);
        assert(res || (!flags_.includes(TypeFlags::maybeNotFastVecelt) &&
                       !flags_.includes(TypeFlags::maybeObject)));
        return res;
    }

    inline constexpr bool isTheMissingValue() const {
        if (!isRType())
            return false;
        return *this == theMissingValue();
    }

    inline constexpr bool maybeTheMissingValue() const {
        if (!isRType())
            return false;

        return flags_.includes(TypeFlags::maybeMissing);
    }

    inline constexpr bool evaluatesToMissing() const {
        if (!isRType())
            return false;

        return t_.r == RTypeSet(RType::missing);
    }

    inline constexpr bool maybeEvaluatesToMissing() const {
        if (!isRType())
            return false;

        return t_.r.includes(RType::missing);
    }

    inline constexpr void
    ensureMissingInvariant(RTypeSet& r, PirType::FlagSet& flags) const {

        if (!flags.includes(TypeFlags::lazy) &&
            !flags.includes(TypeFlags::promiseWrapped)

        ) {
            if (r.includes(RType::missing) ||
                flags.includes(TypeFlags::maybeMissing)) {
                flags.set(TypeFlags::maybeMissing);
                r.set(RType::missing);
            }
        }
    }

    inline PirType operator|(const PirType& o) const {

        assert(isRType() == o.isRType());

        PirType r;
        if (isRType())
            r.t_ = notMissing().t_.r | o.notMissing().t_.r;
        else
            r.t_ = t_.n | o.t_.n;

        r.flags_ = flags_ | o.flags_;

        if (r.isRType())
            ensureMissingInvariant(r.t_.r, r.flags_);

        // relax ctx
        AA::singleton().copyInfo(&o, &r);

        return r;
    }

    inline PirType operator&(const PirType& o) const {
        assert(isRType() == o.isRType());

        PirType r;
        if (isRType())
            r.t_ = notMissing().t_.r & o.notMissing().t_.r;
        else
            r.t_ = t_.n & o.t_.n;

        r.flags_ = flags_ & o.flags_;

        if (r.isRType())
            ensureMissingInvariant(r.t_.r, r.flags_);

        // relax ctx
        AA::singleton().copyInfo(&o, &r);

        return r;
    }

    bool maybeReferenceCounted() const {
        static constexpr RTypeSet refcount =
            RTypeSet() | RType::logical | RType::integer | RType::real |
            RType::str | RType::vec | RType::cplx;
        return isRType() && t_.r.intersects(refcount);
    }

    PirType notObject() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ & ~FlagSet(TypeFlags::maybeObject));

        // relax ctx
        AA::singleton().copyInfo(this, &ret);

        return ret;
    }

    PirType noAttribsOrObject() const {
        assert(isRType());
        auto ret =
            PirType(t_.r, flags_ & ~(FlagSet() | TypeFlags::maybeNotFastVecelt |
                                     TypeFlags::maybeAttrib))
                .notObject();

        // relax ctx
        AA::singleton().copyInfo(this, &ret);

        return ret;
    }

    PirType notMissing() const {
        assert(isRType());

        auto newType = t_.r;
        if (!maybePromiseWrapped()) {
            newType.reset(RType::missing);
        }

        auto ret = PirType(newType, flags_ & ~FlagSet(TypeFlags::maybeMissing));

        // relax ctx
        AA::singleton().copyInfo(this, &ret);

        return ret;
    }

    inline PirType notNAOrNaN() const {
        assert(isRType());

        auto ret = PirType(t_.r, flags_ & ~FlagSet(TypeFlags::maybeNAOrNaN));
        // relax ctx
        AA::singleton().copyInfo(this, &ret);

        return ret;
    }

    inline PirType scalar() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ & ~FlagSet(TypeFlags::maybeNotScalar));

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType simpleScalar() const { return scalar().noAttribsOrObject(); }

    inline PirType notT(RType t) const {
        assert(isRType());
        auto ret = PirType(t_.r & ~RTypeSet(t), flags_);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);

        return ret;
    }

    inline PirType orT(RType t) const {
        assert(isRType());
        auto ret = PirType(t_.r | t, flags_);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orNAOrNaN() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ | TypeFlags::maybeNAOrNaN);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orNotScalar() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ | TypeFlags::maybeNotScalar);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orPromiseWrapped() const {
        assert(isRType());

        if (maybePromiseWrapped())
            return *this;

        auto ret = PirType(t_.r, flags_ | TypeFlags::promiseWrapped);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orFullyPromiseWrapped() const {
        assert(isRType());

        if (maybePromiseWrapped())
            return *this;

        auto ret = PirType(t_.r, (flags_ | TypeFlags::promiseWrapped) &
                                     ~(FlagSet() | TypeFlags::maybeMissing));

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orLazy() const {
        assert(isRType());
        auto ret =
            PirType(t_.r, flags_ | TypeFlags::lazy | TypeFlags::promiseWrapped);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orObject() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ | TypeFlags::maybeObject |
                                     TypeFlags::maybeAttrib |
                                     TypeFlags::maybeNotFastVecelt);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orNotFastVecelt() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ | TypeFlags::maybeNotFastVecelt |
                                     TypeFlags::maybeAttrib);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orAttribsOrObj() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ | TypeFlags::maybeAttrib |
                                     TypeFlags::maybeNotFastVecelt |
                                     TypeFlags::maybeObject);
        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orNameAttrs() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ | TypeFlags::maybeAttrib |
                                     TypeFlags::maybeNotFastVecelt);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orMaybeMissing() const {
        assert(isRType());
        auto newType = t_.r;
        auto newFlags = flags_ | TypeFlags::maybeMissing;
        ensureMissingInvariant(newType, newFlags);
        auto ret = PirType(newType, newFlags);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType orFastVecelt() const {
        assert(isRType());
        return orAttribsOrObj().fastVecelt();
    }

    inline PirType fastVecelt() const {
        assert(isRType());
        auto ret = PirType(t_.r, flags_ & ~(FlagSet() | TypeFlags::maybeObject |
                                            TypeFlags::maybeNotFastVecelt));

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    PirType notPromiseWrapped() const {
        auto ret = PirType(t_.r, flags_ & ~(FlagSet() | TypeFlags::lazy |
                                            TypeFlags::promiseWrapped));
        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    PirType notLazy() const {
        auto ret = PirType(t_.r, flags_ & ~FlagSet(TypeFlags::lazy));

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    PirType forced() const {
        if (!maybePromiseWrapped())
            return *this;

        auto newFlags = flags_;
        auto newType = t_.r;

        newFlags =
            newFlags & ~(FlagSet(TypeFlags::lazy) | TypeFlags::promiseWrapped);

        ensureMissingInvariant(newType, newFlags);

        auto ret = PirType(newType, newFlags);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    inline PirType baseType() const {
        assert(isRType());
        auto ret = PirType(t_.r);

        // relax ctx
        AA::singleton().copyInfo(this, &ret);
        return ret;
    }

    // Type of <this>[<idx>] or <this>[<idx>, <idx>]
    PirType subsetType(PirType idx) const {
        assert(isRType());
        if (isA(PirType(RType::nil).orAttribsOrObj())) {
            // NULL
            return RType::nil;
        }
        if (isA((num() | RType::str | RType::list | RType::code)
                    .orAttribsOrObj())) {
            // If the index is out of bounds, NA is returned (even if both args
            // are non-NA) so we must add orNAOrNaN()
            if (idx.isA(PirType(RType::str).scalar()))
                return scalar().orAttribsOrObj().orNAOrNaN();
            // e.g. c(1,2,3)[-1] returns c(2,3)
            return orNotScalar().orNAOrNaN();
        } else if (isA(RType::vec)) {
            return PirType(RType::vec);
        } else if (isA(PirType(RType::vec).orAttribsOrObj())) {
            return PirType(RType::vec).orAttribsOrObj();
        } else if (!maybeNotFastVecelt() && !PirType(RType::prom).isA(*this)) {
            // Something else
            return val().notMissing();
        } else {
            // Possible object
            return val();
        }
    }

    // Type of <this>[[<idx>]] or <this>[[<idx>, <idx>]]
    PirType extractType(PirType idx) const {
        assert(isRType());
        if (isA(PirType(RType::nil).orAttribsOrObj())) {
            // NULL
            return RType::nil;
        }
        if (isA((num() | RType::str | RType::list).orAttribsOrObj())) {
            return simpleScalar();
        } else if (isA(PirType(RType::vec))) {
            return val().notMissing();
        } else if (!maybeNotFastVecelt() && !PirType(RType::prom).isA(*this)) {
            // Something else
            return val().notMissing();
        } else {
            // Possible object
            return val();
        }
    }

    // Type of c(<this>, ...numArgs)
    PirType collectionType(int numArgs) const {
        assert(isRType());
        if (isA(RType::nil)) {
            return RType::nil;
        } else if (isA(num() | RType::str | RType::nil)) {
            PirType t = *this;
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

    constexpr bool isVoid() const {
        return isRType() ? t_.r.empty() && !maybeMissing() : t_.n.empty();
    }

    static const PirType voyd() { return PirType(NativeTypeSet()); }
    static const PirType bottom() { return optimistic(); }

    void fromContext(const Context&, unsigned arg, unsigned nargs,
                     bool forced = false);

    inline bool operator==(const NativeType& o) const {
        return !isRType() && t_.n == o;
    }

    inline bool operator!=(const PirType& o) const { return !(*this == o); }

    inline bool operator==(const PirType& o) const {
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
            (!maybeObj(false) && o.maybeObj(false)) ||
            (!maybeNotFastVecelt() && o.maybeNotFastVecelt()) ||
            (!maybeHasAttrs() && o.maybeHasAttrs()) ||
            (!maybeNAOrNaN() && o.maybeNAOrNaN()) ||
            (isScalar() && !o.isScalar()) ||
            (!maybeMissing() && o.maybeMissing())) {
            return false;
        }
        // before we had: t_.r.includes(o.t_.r)
        // but, we want this case to be accepted: int~ | missing <isSuper> int |
        // missing
        return notMissing().t_.r.includes(o.notMissing().t_.r);
    }

    // Is val an instance of this type?
    bool isInstance(SEXP val) const;

    void print(std::ostream& out = std::cout) const;

    size_t hash() const {
        return hash_combine(flags_.to_i(),
                            isRType() ? t_.r.to_i() : t_.n.to_i());
    }

    // Composite values are pseudo instruction that produce a "logical" PIR
    // value. At runtime it is represented by all the PIR argument values
    // individually.
    bool isCompositeValue() { return isA(NativeType::frameState); }

    // Some PIR instructions produce PIR values that have no runtime
    // representation at all. E.g. contexts.
    bool isVirtualValue() {
        return isA(NativeType::context) || isA(NativeType::checkpoint);
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
    case NativeType::deoptReason:
        out << "dr";
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
    case RType::special:
        out << "spec";
        break;
    case RType::builtin:
        out << "blt";
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

    if (t.isTheMissingValue()) {
        out << "missing";
        return out;
    }

    // If the base type is at least a value, then it's a value
    if (PirType::val().notMissing().baseType() == t.baseType()) {
        out << "val";
    } else if (PirType::val().baseType() == t.baseType()) {
        out << "val?";
    } else if (PirType::num().notMissing().baseType() == t.baseType()) {
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
    if (!t.maybeHasAttrs()) {
        out << "-";
    } else {
        if (!t.maybeNotFastVecelt()) {
            assert(!t.maybeObj());
            out << "_";
        } else if (!t.maybeObj()) {
            out << "+";
        }
    }

    if (t.maybePromiseWrapped() && t.maybeMissing())
        out << " | miss";

    return out;
}

} // namespace pir
} // namespace rir

namespace std {
template <>
struct hash<rir::pir::PirType> {
    size_t operator()(const rir::pir::PirType& t) const { return t.hash(); }
};
} // namespace std

#endif
