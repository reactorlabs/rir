#include "R/Funtab.h"
#include "R/Symbols.h"
#include "runtime/ArglistOrder.h"
#include "runtime/Function.h"

namespace rir {

inline static void serializeAstVector(R_outpstream_t out, SEXP s, void (*serializeElem)(R_outpstream_t, SEXP, int)) {
    assert(ATTRIB(s) == R_NilValue && "unexpected attributes in AST");
    assert(!OBJECT(s) && "unexpected object in AST");
    assert(!IS_S4_OBJECT(s) && "unexpected S4 object in AST");
    assert(!ALTREP(s) && "unexpected altrep in AST");
    size_t length = STDVEC_LENGTH(s);
    for (size_t i = 0; i < length; ++i) {
        serializeElem(out, s, i);
    }
}

void serializeAst(R_outpstream_t out, SEXP s) {
    OutInteger(out, TYPEOF(s));
    switch (TYPEOF(s)) {
    case NILSXP: {
        break;
    }

    case SYMSXP: {
        if (s == R_UnboundValue) {
            OutInteger(out, 0);
        } else if (s == R_MissingArg) {
            OutInteger(out, 1);
        } else if (s == R_RestartToken) {
            OutInteger(out, 2);
        } else if (s == symbol::expandDotsTrigger) {
            assert(false && "unexpected expandDotsTrigger in AST");
        } else {
            OutInteger(out, 3);
            const char* name = CHAR(PRINTNAME(s));
            OutChar(out, strlen(name));
            OutBytes(out, (const void*)name, strlen(name));
        }
        break;
    }

    case LISTSXP: {
        OutInteger(out, Rf_length(s));
        for (SEXP cur = s; cur != R_NilValue; cur = CDR(cur)) {
            serializeAst(out, CAR(cur));
        }
        break;
    }

    case CLOSXP: {
        assert(false && "unexpected CLOSXP in AST");
    }

    case ENVSXP: {
        assert(false && "unexpected ENVSXP in AST");
    }

    case PROMSXP: {
        assert(false && "unexpected PROMSXP in AST");
    }

    case LANGSXP: {
        OutInteger(out, Rf_length(s));
        for (SEXP cur = s; cur != R_NilValue; cur = CDR(cur)) {
            serializeAst(out, CAR(cur));
        }
        break;
    }

    case SPECIALSXP:
    case BUILTINSXP: {
        OutInteger(out, getBuiltinNr(s));
        break;
    }

    case CHARSXP: {
        if (s == NA_STRING) {
            OutInteger(out, 0);
        } else {
            OutInteger(out, 1);
            const char* chr = CHAR(s);
            OutChar(out, strlen(chr));
            OutBytes(out, (const void*)chr, strlen(chr));
        }
        break;
    }

    case LGLSXP: {
        serializeAstVector(out, s, [](R_outpstream_t out, SEXP s, int i) {
            OutInteger(out, LOGICAL(s)[i]);
        });
        break;
    }

    case INTSXP: {
        serializeAstVector(out, s, [](R_outpstream_t out, SEXP s, int i) {
            OutInteger(out, INTEGER(s)[i]);
        });
        break;
    }

    case REALSXP: {
        serializeAstVector(out, s, [](R_outpstream_t out, SEXP s, int i) {
            OutReal(out, REAL(s)[i]);
        });
        break;
    }

    case CPLXSXP: {
        serializeAstVector(out, s, [](R_outpstream_t out, SEXP s, int i) {
            OutComplex(out, COMPLEX(s)[i]);
        });
        break;
    }

    case STRSXP: {
        serializeAstVector(out, s, [](R_outpstream_t out, SEXP s, int i) {
            const char* chr = CHAR(STRING_ELT(s, i));
            OutChar(out, strlen(chr));
            OutBytes(out, (const void*)chr, strlen(chr));
        });
        break;
    }

    case VECSXP: {
        serializeAstVector(out, s, [](R_outpstream_t out, SEXP s, int i) {
            serializeAst(out, VECTOR_ELT(s, i));
        });
        break;
    }

    case RAWSXP: {
        serializeAstVector(out, s, [](R_outpstream_t out, SEXP s, int i) {
            OutChar(out, RAW(s)[i]);
        });
        break;
    }

    case EXTERNALSXP: {
        assert(false && "unexpected RIR object in AST");
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
        assert(false && "unexpected type in AST");
    }
    }
}

} // namespace rir
