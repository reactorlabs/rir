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

namespace {

const char* sexptype2char(SEXPTYPE type) {
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

} // namespace

std::string Print::dumpSexp(SEXP s, size_t length) {

    if (s == R_NilValue)
        return "R_NilValue";
    if (s == R_UnboundValue)
        return "R_UnboundValue";
    if (s == R_MissingArg)
        return "R_MissingArg";
    if (s == R_RestartToken)
        return "R_RestartToken";
    if (s == symbol::expandDotsTrigger)
        return "RIR_ExpandDotsTrigger";
    if (s == R_GlobalEnv)
        return "R_GlobalEnv";
    if (s == R_EmptyEnv)
        return "R_EmptyEnv";
    if (s == R_BaseEnv)
        return "R_BaseEnv";
    if (s == R_BaseNamespace)
        return "BaseNamespace";
    if (TYPEOF(s) == SYMSXP)
        return CHAR(PRINTNAME(s));
    if (TYPEOF(s) == CHARSXP)
        return std::string("\"") + CHAR(s) + "\"";
    if (TYPEOF(s) == EXTERNALSXP) {
        std::stringstream ss;
        ss << "<";
        if (auto p = Code::check(s)) {
            ss << "(rir::Code*)" << p;
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

    std::stringstream ss;
    ss << "<" << sexptype2char(TYPEOF(s));

    bool unsafe = false;
    if (ATTRIB(s) != R_NilValue) {
        ss << "|a";
        unsafe = true;
    }
    if (OBJECT(s)) {
        ss << "|o";
        unsafe = true;
    }
    if (IS_S4_OBJECT(s)) {
        ss << "|s4";
        unsafe = true;
    }
    if (ALTREP(s)) {
        ss << "|alt";
        unsafe = true;
    }

    switch (TYPEOF(s)) {
    case PROMSXP: {
        if (PRVALUE(s) != R_UnboundValue) {
            auto e = dumpSexp(PRVALUE(s));
            ss << " val=" << e;
        }
        auto e = dumpSexp(PRCODE(s));
        ss << " " << e;
        // No PRENV(s) because we don't want to materialize if lazy
        e = dumpSexp(s->u.promsxp.env);
        ss << " env=" << e;
        break;
    }
    case CLOSXP: {
        auto e = dumpSexp(FORMALS(s));
        ss << " (" << e << ")";
        e = dumpSexp(BODY(s));
        ss << " " << e;
        e = dumpSexp(CLOENV(s));
        ss << " env=" << e;
        break;
    }
    case SPECIALSXP:
    case BUILTINSXP: {
        ss << " " << getBuiltinName(getBuiltinNr(s));
        break;
    }
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case RAWSXP: {
        size_t n = unsafe ? 0 : STDVEC_LENGTH(s);
        for (size_t i = 0, out = 0; i < n && out < length; ++i) {
            switch (TYPEOF(s)) {
            case LGLSXP:
                ss << " " << LOGICAL(s)[i];
                out += 2;
                break;
            case INTSXP:
                ss << " " << INTEGER(s)[i];
                out += 2;
                break;
            case REALSXP:
                ss << " " << REAL(s)[i];
                out += 2;
                break;
            case CPLXSXP:
                ss << " (" << COMPLEX(s)[i].r << "|" << COMPLEX(s)[i].i << "i"
                   << ")";
                out += 7;
                break;
            case STRSXP: {
                auto e = dumpSexp(STRING_PTR(s)[i]);
                ss << " " << e;
                out += 1 + e.length();
                break;
            }
            case VECSXP: {
                auto e = dumpSexp(VECTOR_PTR(s)[i]);
                ss << " " << e;
                out += 1 + e.length();
                break;
            }
            case RAWSXP:
                ss << " " << std::hex << RAW(s)[i] << std::dec;
                out += 2;
                break;
            }
        }
        break;
    }
    case LANGSXP:
    case LISTSXP: {
        size_t out = 0;
        while (s && s != R_NilValue && out < length) {
            if (TAG(s) != R_NilValue) {
                auto e = dumpSexp(TAG(s));
                ss << " " << e << " =";
                out += 5 + e.length();
            }
            auto e = dumpSexp(CAR(s));
            ss << " " << e;
            out += 1 + e.length();
            s = CDR(s);
        }
        break;
    }
    case ENVSXP:
    case DOTSXP:
    case EXPRSXP:
    case ANYSXP:
    case BCODESXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case S4SXP:
    case NEWSXP:
    case FREESXP: {
        ss << " " << s;
        break;
    }
    }

    ss << ">";

    auto res = ss.str();
    if (res.length() <= length)
        return res;
    else
        return res.substr(0, length - 4) + "...>";
}

} // namespace rir
