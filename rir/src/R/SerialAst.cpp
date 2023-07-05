#include "SerialAst.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include <unordered_map>

namespace rir {

// Assumes all symbols are never freed (currently yes because they're in a pool,
// and it makes sense since they're all AST nodes)
static std::unordered_map<SEXP, UUID> hashCache;

inline static void serializeAstVector(UUIDHasher& hasher, SEXP s, void (*serializeElem)(UUIDHasher&, SEXP, int)) {
    // assert(ATTRIB(s) == R_NilValue && "unexpected attributes in AST");
    assert(!OBJECT(s) && "unexpected object in AST");
    assert(!IS_S4_OBJECT(s) && "unexpected S4 object in AST");
    assert(!ALTREP(s) && "unexpected altrep in AST");
    size_t length = STDVEC_LENGTH(s);
    for (size_t i = 0; i < length; ++i) {
        serializeElem(hasher, s, i);
    }
}

void serializeAst(UUIDHasher& hasher, SEXP s) {
    hasher.hashBytesOf<int>(TYPEOF(s));
    switch (TYPEOF(s)) {
    case NILSXP: {
        break;
    }

    case SYMSXP: {
        if (s == R_UnboundValue) {
            hasher.hashBytesOf<int>(0);
        } else if (s == R_MissingArg) {
            hasher.hashBytesOf<int>(1);
        } else if (s == R_RestartToken) {
            hasher.hashBytesOf<int>(2);
        } else if (s == symbol::expandDotsTrigger) {
            hasher.hashBytesOf<int>(3);
        } else {
            hasher.hashBytesOf<int>(4);
            const char* name = CHAR(PRINTNAME(s));
            hasher.hashBytesOf<size_t>(strlen(name));
            hasher.hashBytes((const void*)name, strlen(name));
        }
        break;
    }

    case LISTSXP: {
        hasher.hashBytesOf<int>(Rf_length(s));
        for (SEXP cur = s; cur != R_NilValue; cur = CDR(cur)) {
            serializeAst(hasher, CAR(cur));
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
        hasher.hashBytesOf<int>(Rf_length(s));
        for (SEXP cur = s; cur != R_NilValue; cur = CDR(cur)) {
            serializeAst(hasher, CAR(cur));
        }
        break;
    }

    case SPECIALSXP:
    case BUILTINSXP: {
        hasher.hashBytesOf<int>(getBuiltinNr(s));
        break;
    }

    case CHARSXP: {
        if (s == NA_STRING) {
            hasher.hashBytesOf<int>(0);
        } else {
            hasher.hashBytesOf<int>(1);
            const char* chr = CHAR(s);
            hasher.hashBytesOf<size_t>(strlen(chr));
            hasher.hashBytes((const void*)chr, strlen(chr));
        }
        break;
    }

    case LGLSXP: {
        serializeAstVector(hasher, s, [](UUIDHasher& hasher, SEXP s, int i) {
            hasher.hashBytesOf<int>(LOGICAL(s)[i]);
        });
        break;
    }

    case INTSXP: {
        serializeAstVector(hasher, s, [](UUIDHasher& hasher, SEXP s, int i) {
            hasher.hashBytesOf<int>(INTEGER(s)[i]);
        });
        break;
    }

    case REALSXP: {
        serializeAstVector(hasher, s, [](UUIDHasher& hasher, SEXP s, int i) {
            hasher.hashBytesOf<double>(REAL(s)[i]);
        });
        break;
    }

    case CPLXSXP: {
        serializeAstVector(hasher, s, [](UUIDHasher& hasher, SEXP s, int i) {
            hasher.hashBytesOf<Rcomplex>(COMPLEX(s)[i]);
        });
        break;
    }

    case STRSXP: {
        serializeAstVector(hasher, s, [](UUIDHasher& hasher, SEXP s, int i) {
            const char* chr = CHAR(STRING_ELT(s, i));
            hasher.hashBytesOf<size_t>(strlen(chr));
            hasher.hashBytes((const void*)chr, strlen(chr));
        });
        break;
    }

    case VECSXP: {
        serializeAstVector(hasher, s, [](UUIDHasher& hasher, SEXP s, int i) {
            serializeAst(hasher, VECTOR_ELT(s, i));
        });
        break;
    }

    case RAWSXP: {
        serializeAstVector(hasher, s, [](UUIDHasher& hasher, SEXP s, int i) {
            hasher.hashBytesOf<Rbyte>(RAW(s)[i]);
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

UUID serializeAst(SEXP s) {
    if (hashCache.count(s)) {
        return hashCache[s];
    }
    UUIDHasher hasher;
    serializeAst(hasher, s);
    auto uuid = hasher.finalize();
    hashCache[s] = uuid;
    return uuid;
}


} // namespace rir
