#include "type.h"
#include "R/r.h"

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
        t_.r.set(RType::prom);
        break;
    case EXPRSXP:
        t_.r.set(RType::ast);
        // fall through
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
        t_.r = val().t_.r;
    }

    // if (Rf_isObject(e)) {
    //     flags_.set(TypeFlags::obj);
    // }

    if (PirType::vecs().isSuper(*this)) {
        if (Rf_length(e) == 1)
            flags_.set(TypeFlags::is_scalar);
    }
}
}
}
