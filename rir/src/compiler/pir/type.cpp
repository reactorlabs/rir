#include "type.h"
#include "../../interpreter/LazyEnvironment.h"
#include "../parameter.h"
#include "R/r.h"

extern "C" Rboolean(Rf_isObject)(SEXP s);

namespace rir {
namespace pir {

unsigned Parameter::RIR_CHECK_PIR_TYPES =
    getenv("RIR_CHECK_PIR_TYPES") ? atoi(getenv("RIR_CHECK_PIR_TYPES")) : 0;

static PirType::Type parseBaseType(const std::string& inp, bool& isRType) {
    if (inp == "val") {
        isRType = true;
        return PirType::val().t_;
    } else if (inp == "ct") {
        isRType = false;
        return PirType::NativeTypeSet(NativeType::context);
    } else if (inp == "t") {
        isRType = false;
        return PirType::NativeTypeSet(NativeType::test);
    } else if (inp == "cp") {
        isRType = false;
        return PirType::NativeTypeSet(NativeType::checkpoint);
    } else if (inp == "fs") {
        isRType = false;
        return PirType::NativeTypeSet(NativeType::frameState);
    } else if (inp == "ast") {
        isRType = true;
        return PirType::RTypeSet(RType::ast);
    } else if (inp == "raw") {
        isRType = true;
        return PirType::RTypeSet(RType::raw);
    } else if (inp == "vec") {
        isRType = true;
        return PirType::RTypeSet(RType::vec);
    } else if (inp == "char") {
        isRType = true;
        return PirType::RTypeSet(RType::chr);
    } else if (inp == "real") {
        isRType = true;
        return PirType::RTypeSet(RType::real);
    } else if (inp == "complex") {
        isRType = true;
        return PirType::RTypeSet(RType::cplx);
    } else if (inp == "str") {
        isRType = true;
        return PirType::RTypeSet(RType::str);
    } else if (inp == "env") {
        isRType = true;
        return PirType::RTypeSet(RType::env);
    } else if (inp == "code") {
        isRType = true;
        return PirType::RTypeSet(RType::code);
    } else if (inp == "cons") {
        isRType = true;
        return PirType::RTypeSet(RType::cons);
    } else if (inp == "prom") {
        isRType = true;
        return PirType::RTypeSet(RType::prom);
    } else if (inp == "nil") {
        isRType = true;
        return PirType::RTypeSet(RType::nil);
    } else if (inp == "cls") {
        isRType = true;
        return PirType::RTypeSet(RType::closure);
    } else if (inp == "sym") {
        isRType = true;
        return PirType::RTypeSet(RType::sym);
    } else if (inp == "int") {
        isRType = true;
        return PirType::RTypeSet(RType::integer);
    } else if (inp == "lgl") {
        isRType = true;
        return PirType::RTypeSet(RType::logical);
    } else if (inp == "miss") {
        isRType = true;
        return PirType::RTypeSet(RType::missing);
    } else if (inp == "_") {
        isRType = true;
        return PirType::RTypeSet(RType::unbound);
    } else {
        Rf_error("couldn't parse pir type");
        return PirType::RTypeSet(RType::unbound);
    }
}

PirType PirType::parse(const std::string& inp) {
    std::string s = inp;
    FlagSet flags = FlagSet(TypeFlags::maybeNotScalar) | TypeFlags::maybeObject;
    while (true) {
        char suf = s[s.size() - 1];
        bool hasFlag = true;
        switch (suf) {
        case '$':
            flags.reset(TypeFlags::maybeNotScalar);
            break;
        case '^':
            flags.set(TypeFlags::lazy);
            break;
        case '~':
            flags.set(TypeFlags::promiseWrapped);
            break;
        case '\'':
            flags.reset(TypeFlags::maybeObject);
            break;
        default:
            hasFlag = false;
            break;
        }
        if (!hasFlag)
            break;
        s = s.substr(0, s.size() - 1);
    }
    bool isRType = true;
    ;
    Type base = parseBaseType(s, isRType);
    if (isRType)
        flags.set(TypeFlags::rtype);
    return PirType(base, flags);
}

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
