#include "type.h"

extern "C" Rboolean(Rf_isObject)(SEXP s);

namespace rir {
namespace pir {

void PirType::print() { std::cout << *this << "\n"; }

PirType::PirType(SEXP e) : flags_(defaultRTypeFlags()), t_(RTypeSet()) {
    switch (TYPEOF(e)) {
    case NILSXP:
        t_.r.set(RType::nil);
        break;
    case SYMSXP:
        t_.r.set(RType::symbol);
        break;
    case LISTSXP:
        t_.r.set(RType::cons);
        break;
    case CLOSXP:
        t_.r.set(RType::closure);
        break;
    case ENVSXP:
        t_.r.set(RType::env);
        break;
    case PROMSXP:
        t_.r.set(RType::prom);
        break;
    case LANGSXP:
        t_.r.set(RType::ast);
        break;
    case CHARSXP:
        t_.r.set(RType::chars);
        break;
    case LGLSXP:
        t_.r.set(RType::logical);
        break;
    case INTSXP:
        t_.r.set(RType::integer);
        break;
    case REALSXP:
        t_.r.set(RType::dbls);
        break;
    case STRSXP:
        t_.r.set(RType::strs);
        break;
    case VECSXP:
        t_.r.set(RType::vecs);
        break;
    case RAWSXP:
        t_.r.set(RType::raw);
        break;
    case SPECIALSXP:
    case BUILTINSXP:
    case DOTSXP:
    case ANYSXP:
    case CPLXSXP:
    case EXPRSXP:
    case BCODESXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case S4SXP:
        assert(false);
    }

    if (Rf_isObject(e)) {
        flags_.set(TypeFlags::obj);
    }

    if (PirType::vecs() >= *this) {
        if (Rf_length(e) == 1)
            flags_.set(TypeFlags::is_scalar);
    }
}
}
}
