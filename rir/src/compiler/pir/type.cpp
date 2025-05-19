#include "type.h"
#include "../parameter.h"
#include "R/r.h"
#include "runtime/LazyEnvironment.h"

namespace rir {
namespace pir {

unsigned Parameter::RIR_CHECK_PIR_TYPES =
    getenv("RIR_CHECK_PIR_TYPES") ? atoi(getenv("RIR_CHECK_PIR_TYPES")) : 0;

void PirType::print(std::ostream& out) const { out << *this << "\n"; }

void PirType::merge(SEXPTYPE sexptype) {
    assert(isRType());

    switch (sexptype) {
    case NILSXP:
        t_.r.set(RType::nil);
        break;
    case SYMSXP:
        t_.r.set(RType::sym);
        break;
    case LISTSXP:
        t_.r.set(RType::list);
        break;
    case CLOSXP:
        t_.r.set(RType::closure);
        break;
    case SPECIALSXP:
        t_.r.set(RType::special);
        break;
    case BUILTINSXP:
        t_.r.set(RType::builtin);
        break;
    case ENVSXP:
        t_.r.set(RType::env);
        break;
    case PROMSXP:
        flags_.set(TypeFlags::promiseWrapped);
        flags_.set(TypeFlags::lazy);
        t_.r = PirType::any().t_.r;
        break;
    case EXPRSXP:
        t_.r.set(RType::ast);
        break;
    case LANGSXP:
        t_.r.set(RType::code);
        break;
    case CHARSXP:
        t_.r.set(RType::chr);
        break;
    case LGLSXP:
        t_.r.set(RType::logical);
        break;
    case INTSXP:
        t_.r.set(RType::integer);
        break;
    case REALSXP:
        t_.r.set(RType::real);
        break;
    case STRSXP:
        t_.r.set(RType::str);
        break;
    case VECSXP:
        t_.r.set(RType::vec);
        break;
    case RAWSXP:
        t_.r.set(RType::raw);
        break;
    case BCODESXP:
    case EXTERNALSXP:
        t_.r.set(RType::code);
        break;
    case CPLXSXP:
        t_.r.set(RType::cplx);
        break;
    case DOTSXP:
        t_.r.set(RType::dots);
        break;
    case ANYSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case S4SXP:
        t_.r.set(RType::other);
        break;
    default:
        std::cerr << "unknown type: " << sexptype << "\n";
        assert(false);
        break;
    }
}

// For non-scalars, it takes too long to determine whether they
// contain NaN for the benefit, so we simple assume they do
static const R_xlen_t MAX_SIZE_OF_VECTOR_FOR_NAN_CHECK = 1;

static bool maybeContainsNAOrNaN(SEXP vector) {
    if (TYPEOF(vector) == CHARSXP) {
        return vector == NA_STRING;
    } else if (TYPEOF(vector) == INTSXP || TYPEOF(vector) == REALSXP ||
               TYPEOF(vector) == LGLSXP || TYPEOF(vector) == CPLXSXP ||
               TYPEOF(vector) == STRSXP) {
        if (XLENGTH(vector) > MAX_SIZE_OF_VECTOR_FOR_NAN_CHECK) {
            return true;
        }
        for (int i = 0; i < XLENGTH(vector); i++) {
            switch (TYPEOF(vector)) {
            case INTSXP:
                if (INTEGER(vector)[i] == NA_INTEGER)
                    return true;
                break;
            case REALSXP:
                if (ISNAN(REAL(vector)[i]))
                    return true;
                break;
            case LGLSXP:
                if (LOGICAL(vector)[i] == NA_LOGICAL)
                    return true;
                break;
            case CPLXSXP:
                if (ISNAN(COMPLEX(vector)[i].i))
                    return true;
                break;
            case STRSXP:
                if (STRING_ELT(vector, i) == NA_STRING)
                    return true;
                break;
            default:
                assert(false);
            }
        }
        return false;
    } else {
        // Not a type which can represent NaN
        return false;
    }
}

PirType::PirType(SEXP e) : flags_(topRTypeFlags()), t_(RTypeSet()) {

    if (e == R_MissingArg) {
        *this = theMissingValue();
        return;
    }

    // these are set by merge below
    flags_.reset(TypeFlags::promiseWrapped);
    flags_.reset(TypeFlags::lazy);

    if (TYPEOF(e) == PROMSXP) {
        flags_.set(TypeFlags::promiseWrapped);
        if (PRVALUE(e) != R_UnboundValue) {
            e = PRVALUE(e);
            if (e == R_MissingArg)
                t_.r.set(RType::missing);
        } else {
            flags_.set(TypeFlags::lazy);
            t_.r.set(RType::missing);
        }
    }

    if (e == R_UnboundValue)
        t_.r.set(RType::unbound);

    if (e != R_UnboundValue && e != R_MissingArg)
        merge(TYPEOF(e));

    if (TYPEOF(e) == PROMSXP)
        return;

    if (!Rf_isObject(e))
        flags_.reset(TypeFlags::maybeObject);
    if (fastVeceltOk(e))
        flags_.reset(TypeFlags::maybeNotFastVecelt);
    if (ATTRIB(e) == R_NilValue)
        flags_.reset(TypeFlags::maybeAttrib);
    auto t = TYPEOF(e);
    if (t != LISTSXP && t != EXTERNALSXP && t != BCODESXP && t != LANGSXP)
        if (Rf_xlength(e) == 1)
            flags_.reset(TypeFlags::maybeNotScalar);
    if (!maybeContainsNAOrNaN(e))
        flags_.reset(TypeFlags::maybeNAOrNaN);
}

PirType::PirType(uint64_t i) : PirType() {
    static_assert(sizeof(*this) <= sizeof(uint64_t), "PirType is too big");
    memcpy(reinterpret_cast<void*>(this), &i, sizeof(*this));
    assert((isRType() || !t_.n.empty()) && "corrupted pir type");
}

void PirType::merge(const ObservedValues& other) {
    assert(other.numTypes);

    if (other.attribs)
        flags_.set(TypeFlags::maybeAttrib);
    if (other.notScalar)
        flags_.set(TypeFlags::maybeNotScalar);
    if (other.object)
        flags_.set(TypeFlags::maybeObject);
    if (other.notFastVecelt)
        flags_.set(TypeFlags::maybeNotFastVecelt);
    assert(other.attribs || (!other.notFastVecelt && !other.object));

    flags_.set(TypeFlags::maybeNAOrNaN);
    for (size_t i = 0; i < other.numTypes; ++i)
        merge(other.seen[i]);

    if (other.numTypes == ObservedValues::MaxTypes)
        *this = orSexpTypes(any());
}

bool PirType::isInstance(SEXP val) const {
    if (isRType()) {
        if (TYPEOF(val) == PROMSXP) {
            assert(!Rf_isObject(val));
            if (maybePromiseWrapped() && !maybeLazy()) {
                auto v = PRVALUE(val);
                return v != R_UnboundValue &&
                       notMissing().forced().isInstance(v);
            }
            return maybe(RType::prom) || (maybeLazy() && maybePromiseWrapped());
        }
        if (val == R_DotsSymbol)
            return maybe(RType::dots);
        if (LazyEnvironment::check(val))
            return PirType(RType::env).isA(*this);
        if (*this == PirType::test()) {
            return IS_SIMPLE_SCALAR(val, LGLSXP) &&
                   LOGICAL(val)[0] != NA_LOGICAL;
        }
        return PirType(val).isA(*this);
    } else {
        std::cerr << "can't check val is instance of " << *this << ", value:\n";
        Rf_PrintValue(val);
        assert(false);
    }
}

PirType PirType::fromContext(const Context& assumptions, unsigned arg,
                             unsigned nargs, bool afterForce) const {
    PirType type = *this;
    auto i = arg;
    if (!afterForce &&
        assumptions.includes(Assumption::NoExplicitlyMissingArgs) &&
        arg < nargs - assumptions.numMissing())
        type = type & type.notMissing();

    if (assumptions.isEager(i))
        type = type & type.notLazy();

    if (afterForce)
        type = type & type.forced();

    if (assumptions.isEager(i) || afterForce) {
        type = type & type.notLazy();
        if (assumptions.isNotObj(i))
            type = type & type.notMissing().notObject();

        auto simpleType = [](PirType s) { return s.orPromiseWrapped(); };

        if (assumptions.isSimpleReal(i)) {
            type = type & simpleType(PirType::simpleScalarReal());
        }
        if (assumptions.isSimpleInt(i)) {
            type = type & simpleType(PirType::simpleScalarInt());
        }
    }
    // well, if the intersection of context info and current type is void, we
    // probably made a wrong speculation. it's most probably a bug somewhere,
    // but I don't want to make it an assert, since it can happen depending on
    // the pass order...
    if (!type.isVoid())
        return type;
    else
        return *this;
}

} // namespace pir
} // namespace rir
