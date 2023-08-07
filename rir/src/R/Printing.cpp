#include "R/Printing.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "runtime/ArglistOrder.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "runtime/PirTypeFeedback.h"

#include <iomanip>
#include <sstream>

namespace rir {

std::string Print::sexptype2char(SEXPTYPE type) {
    switch (type) {
    case NILSXP:
        return "nil";
    case SYMSXP:
        return "sym";
    case LISTSXP:
        return "lst";
    case CLOSXP:
        return "cls";
    case ENVSXP:
        return "env";
    case PROMSXP:
        return "prom";
    case LANGSXP:
        return "lang";
    case SPECIALSXP:
        return "spec";
    case BUILTINSXP:
        return "blt";
    case CHARSXP:
        return "char";
    case LGLSXP:
        return "lgl";
    case INTSXP:
        return "int";
    case REALSXP:
        return "real";
    case CPLXSXP:
        return "cplx";
    case STRSXP:
        return "str";
    case DOTSXP:
        return "dot";
    case ANYSXP:
        return "any";
    case VECSXP:
        return "vec";
    case EXPRSXP:
        return "expr";
    case BCODESXP:
        return "bc";
    case EXTERNALSXP:
        return "rir";
    case EXTPTRSXP:
        return "extptr";
    case WEAKREFSXP:
        return "weakref";
    case S4SXP:
        return "s4";
    case RAWSXP:
        return "raw";
    case NEWSXP:
        return "new";
    case FREESXP:
        return "free";
    default:
        return "<unknown>";
    }
}

std::string Print::trim(std::string s, size_t n) {
    if (s.length() <= n)
        return s;

    return s.substr(0, n - 4) + "|...";
}

std::string Print::dumpPROMSXP(SEXP s, size_t length) {
    std::stringstream ss;
    ss << "<prom";
    if (PRVALUE(s) != R_UnboundValue)
        ss << " val=" << dumpSexp(PRVALUE(s), length);
    ss << " " << dumpSexp(PRCODE(s), length);
    // No PRENV(s) because we don't want to materialize if lazy
    ss << " env=" << dumpSexp(s->u.promsxp.env, length) << ">";
    return ss.str();
}

std::string Print::dumpCLOSXP(SEXP s, size_t length) {
    std::stringstream ss;
    ss << "function(";
    auto f = FORMALS(s);
    while (f != R_NilValue) {
        if (TAG(f) != R_NilValue)
            ss << dumpSexp(TAG(f), length);
        if (CAR(f) != R_MissingArg)
            ss << "=" << dumpSexp(CAR(f), length);
        f = CDR(f);
        if (f != R_NilValue)
            ss << ", ";
    }
    ss << ") " << dumpSexp(BODY(s), length);
    ss << " env=" << dumpSexp(CLOENV(s), length);
    return ss.str();
}

std::string Print::dumpLISTSXP(SEXP s, size_t limit, size_t length) {
    std::stringstream ss;
    ss << "<" << sexptype2char(TYPEOF(s));

    auto unsafe = unsafeTags(s);
    if (unsafe.length())
        ss << " |" << unsafe;

    size_t out = 6 + unsafe.length();
    while (s != R_NilValue && out < limit) {
        ss << " ";
        out++;
        if (TAG(s) != R_NilValue) {
            auto e = dumpSexp(TAG(s), length);
            ss << e << "=";
            out += 1 + e.length();
        }
        auto e = dumpSexp(CAR(s), length);
        ss << e;
        out += e.length();
        s = CDR(s);
    }
    ss << ">";
    return ss.str();
}

std::string Print::dumpLANGSXP(SEXP s, size_t length) {
    std::stringstream ss;
    if (s != R_NilValue) {
        ss << dumpSexp(CAR(s), length);
        s = CDR(s);
    }
    ss << "(";
    while (s != R_NilValue) {
        if (TAG(s) != R_NilValue)
            ss << dumpSexp(TAG(s), length) << "=";
        ss << dumpSexp(CAR(s), length);
        s = CDR(s);
        if (s != R_NilValue)
            ss << ", ";
    }
    ss << ")";
    return ss.str();
}

std::string Print::dumpVector(SEXP s, size_t limit, size_t length) {
    std::stringstream ss;

    auto unsafe = unsafeTags(s);
    if (unsafe.empty()) {

        if (size_t n = STDVEC_LENGTH(s)) {
            if (n == 1 && TYPEOF(s) != VECSXP) {
                switch (TYPEOF(s)) {
                case LGLSXP: {
                    if (LOGICAL(s)[0] == NA_LOGICAL) {
                        ss << "NA";
                    } else {
                        ss << (LOGICAL(s)[0] ? "TRUE" : "FALSE");
                    }
                    break;
                }
                case INTSXP: {
                    if (INTEGER(s)[0] == NA_INTEGER) {
                        ss << "NA";
                    } else {
                        ss << INTEGER(s)[0] << "L";
                    }
                    break;
                }
                case REALSXP: {
                    if (ISNA(REAL(s)[0])) {
                        ss << "NA";
                    } else {
                        ss << REAL(s)[0];
                    }
                    break;
                }
                case STRSXP: {
                    ss << dumpSexp(STRING_PTR(s)[0], length);
                    break;
                }
                case RAWSXP: {
                    ss << std::hex << std::setw(2) << RAW(s)[0] << std::dec;
                    break;
                }
                }
            } else {

                ss << "<" << sexptype2char(TYPEOF(s)) << " [1]";

                size_t out = 9;
                for (size_t i = 0; i < n && out < limit; ++i) {
                    ss << " ";
                    out++;
                    switch (TYPEOF(s)) {
                    case LGLSXP: {
                        if (LOGICAL(s)[i] == NA_LOGICAL) {
                            ss << "NA";
                            out += 2;
                        } else {
                            ss << LOGICAL(s)[i];
                            out++;
                        }
                        break;
                    }
                    case INTSXP: {
                        if (INTEGER(s)[i] == NA_INTEGER) {
                            ss << "NA";
                            out += 2;
                        } else {
                            ss << INTEGER(s)[i];
                            out++;
                        }
                        break;
                    }
                    case REALSXP: {
                        if (ISNA(REAL(s)[i])) {
                            ss << "NA";
                            out += 2;
                        } else {
                            ss << REAL(s)[i];
                            out++;
                        }
                        break;
                    }
                    case CPLXSXP: {
                        if ((ISNA(COMPLEX(s)[i].r) || ISNA(COMPLEX(s)[i].i))) {
                            ss << "NA";
                            out += 2;
                        } else {
                            ss << "(" << COMPLEX(s)[i].r << "|"
                               << COMPLEX(s)[i].i << "i)";
                            out += 6;
                        }
                        break;
                    }
                    case STRSXP: {
                        // NA checked for CHARSXP in dumpSexp
                        auto e = dumpSexp(STRING_PTR(s)[i], length);
                        ss << e;
                        out += e.length();
                        break;
                    }
                    case VECSXP: {
                        auto e = dumpSexp(VECTOR_PTR(s)[i], length);
                        ss << e;
                        out += e.length();
                        break;
                    }
                    case RAWSXP: {
                        ss << "0x" << std::hex << std::setw(2) << RAW(s)[i]
                           << std::dec;
                        out++;
                        break;
                    }
                    }
                }
                ss << ">";
            }

        } else {
            switch (TYPEOF(s)) {
            case LGLSXP:
                ss << "logical(0)";
                break;
            case INTSXP:
                ss << "integer(0)";
                break;
            case REALSXP:
                ss << "numeric(0)";
                break;
            case CPLXSXP:
                ss << "complex(0)";
                break;
            case STRSXP:
                ss << "character(0)";
                break;
            case VECSXP:
                ss << "list()";
                break;
            case RAWSXP:
                ss << "raw(0)";
                break;
            }
        }

    } else {
        ss << "<" << sexptype2char(TYPEOF(s)) << " |" << unsafe << ">";
    }

    return ss.str();
}

std::string Print::dumpEXTERNALSXP(SEXP s, size_t length) {
    std::stringstream ss;
    ss << "<";
    if (auto p = Code::check(s)) {
        switch (p->kind) {
        case Code::Kind::Bytecode:
            ss << "bc ";
            break;
        case Code::Kind::Native:
            ss << "n ";
            break;
        case Code::Kind::Deserializing:
            ss << "ds ";
            break;
        }
        ss << "(rir::Code*)" << p;
        if (p->pendingCompilation())
            ss << " nc=pending";
        else if (p->isCompiled())
            ss << " nc=" << p->nativeCode();
    } else if (auto p = Function::check(s)) {
        ss << "(rir::Function*)" << p;
    } else if (auto p = DispatchTable::check(s)) {
        ss << "(rir::DispatchTable*)" << p;
    } else if (auto p = ArglistOrder::check(s)) {
        ss << "(rir::ArglistOrder*)" << p;
    } else if (auto p = LazyArglist::check(s)) {
        ss << "(rir::LazyArglist*)" << p;
    } else if (auto p = LazyEnvironment::check(s)) {
        ss << "(rir::LazyEnvironment*)" << p;
    } else if (auto p = PirTypeFeedback::check(s)) {
        ss << "(rir::PirTypeFeedback*)" << p;
    } else {
        assert(false && "missing RirRuntimeObject printing");
    }
    ss << ">";
    return ss.str();
}

std::string Print::unsafeTags(SEXP s) {
    std::stringstream ss;
    if (ATTRIB(s) != R_NilValue)
        ss << "a|";
    if (OBJECT(s))
        ss << "o|";
    if (IS_S4_OBJECT(s))
        ss << "s4|";
    if (ALTREP(s))
        ss << "alt|";
    return ss.str();
}

std::string Print::dumpSexp(SEXP s, size_t length) {
    std::stringstream ss;

    switch (TYPEOF(s)) {

    case NILSXP: {
        ss << "NULL";
        break;
    }

    case SYMSXP: {
        // TODO: Maybe print active bindings differently?
        if (s == R_UnboundValue)
            ss << "R_UnboundValue";
        else if (s == R_MissingArg)
            ss << "R_MissingArg";
        else if (s == R_RestartToken)
            ss << "R_RestartToken";
        else if (s == symbol::expandDotsTrigger)
            ss << "RIR_ExpandDotsTrigger";
        else
            ss << CHAR(PRINTNAME(s));
        break;
    }

    case LISTSXP: {
        ss << dumpLISTSXP(s, length, length);
        break;
    }

    case CLOSXP: {
        ss << dumpCLOSXP(s, length);
        break;
    }

    case ENVSXP: {
        if (s == R_GlobalEnv)
            ss << "R_GlobalEnv";
        else if (s == R_EmptyEnv)
            ss << "R_EmptyEnv";
        else if (s == R_BaseEnv)
            ss << "R_BaseEnv";
        else if (s == R_BaseNamespace)
            ss << "R_BaseNamespace";
        else {
            ss << "<" << sexptype2char(TYPEOF(s));
            auto unsafe = unsafeTags(s);
            if (unsafe.length())
                ss << " |" << unsafe;
            ss << " " << s << ">";
        }
        break;
    }

    case PROMSXP: {
        ss << dumpPROMSXP(s, length);
        break;
    }

    case LANGSXP: {
        ss << dumpLANGSXP(s, length);
        break;
    }

    case SPECIALSXP:
    case BUILTINSXP: {
        ss << "<" + sexptype2char(TYPEOF(s)) << " " << getBuiltinName(s) << ">";
        break;
    }

    case CHARSXP: {
        if (s == NA_STRING)
            ss << "NA";
        else
            ss << '"' << CHAR(s) << '"';
        break;
    }

    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case RAWSXP: {
        ss << dumpVector(s, length, length);
        break;
    }

    case EXTERNALSXP: {
        ss << dumpEXTERNALSXP(s, length);
        break;
    }

    case DOTSXP:
    case ANYSXP:
    case EXPRSXP:
    case BCODESXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case S4SXP:
    case NEWSXP:
    case FREESXP:
    default: {
        ss << "<" << sexptype2char(TYPEOF(s)) << " " << s << ">";
        break;
    }
    }

    return trim(ss.str(), length);
}

} // namespace rir
