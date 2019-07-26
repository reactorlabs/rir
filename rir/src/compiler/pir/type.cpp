#include "type.h"
#include "../../interpreter/LazyEnvironment.h"
#include "../parameter.h"
#include "R/r.h"

extern "C" Rboolean(Rf_isObject)(SEXP s);

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
        t_.r.set(RType::cons);
        break;
    case CLOSXP:
    // TODO: maybe have different types for those three?
    case SPECIALSXP:
    case BUILTINSXP:
        t_.r.set(RType::closure);
        break;
    case ENVSXP:
        t_.r.set(RType::env);
        break;
    case PROMSXP:
        flags_.set(TypeFlags::lazy);
        flags_.set(TypeFlags::promiseWrapped);
        t_.r = RTypeSet::Any();
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

PirType::PirType(SEXP e) : flags_(defaultRTypeFlags()), t_(RTypeSet()) {
    if (e == R_MissingArg)
        t_.r.set(RType::missing);
    else if (e == R_UnboundValue)
        t_.r.set(RType::unbound);
    else
        merge(TYPEOF(e));

    if (!Rf_isObject(e)) {
        flags_.reset(TypeFlags::maybeObject);
    }

    if (PirType::vecs().isSuper(*this)) {
        if (Rf_length(e) == 1)
            flags_.reset(TypeFlags::maybeNotScalar);
    }
}

PirType::PirType(const void* pos) : PirType() {
    memcpy(reinterpret_cast<void*>(this), pos, sizeof(*this));
    assert((isRType() || !t_.n.empty()) && "corrupted pir type");
}

void PirType::merge(const ObservedValues& other) {
    assert(other.numTypes);

    if (other.numTypes == ObservedValues::MaxTypes) {
        *this = *this | any();
        flags_.set(TypeFlags::maybeObject);
        flags_.set(TypeFlags::maybeNotScalar);
        return;
    }

    for (size_t i = 0; i < other.numTypes; ++i) {
        const auto& record = other.seen[i];
        if (record.object)
            flags_.set(TypeFlags::maybeObject);
        if (!record.scalar)
            flags_.set(TypeFlags::maybeNotScalar);

        merge(record.sexptype);
    }
}

bool PirType::isInstance(SEXP val) const {
    if (isRType()) {
        if (TYPEOF(val) == PROMSXP) {
            assert(!Rf_isObject(val));
            return maybePromiseWrapped() || maybeLazy() ||
                   PirType(RType::prom).isA(*this);
        }
        if (LazyEnvironment::check(val))
            return PirType(RType::env).isA(*this);
        return PirType(val).isA(*this);
    } else if (*this == NativeType::test) {
        return IS_SIMPLE_SCALAR(val, LGLSXP) && *LOGICAL(val) != NA_LOGICAL;
    } else {
        std::cerr << "can't check val is instance of " << *this << ", value:\n";
        Rf_PrintValue(val);
        assert(false);
    }
}
}
}
