//
// Created by Jakob Hain on 7/21/23.
//

#include "doHash.h"
#include "R/Funtab.h"
#include "R/Protect.h"
#include "compiler/parameter.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/measuring.h"
#include <iostream>
#include <stack>

namespace rir {

using HashRefTable = std::unordered_map<SEXP, unsigned>;

/// "TYPEOF" for special cases, different than any normal SEXP TYPEOF, to ensure
/// they are hashed differently. This is similar to what serialize.c does.
///
/// This has the same size as TYPEOF (unsigned)
enum class SpecialType : SEXPTYPE {
    Global = 0x10000000,
    Ref = 0x10000001,
    Altrep = 0x10000002,
    AttrLangSexp = 0x10000003,
    AttrListSexp = 0x10000004,
    BcRef = 0x10000005,
};

static std::unordered_map<SEXP, unsigned> globals = []{
    std::vector<SEXP> vector {
        R_GlobalEnv, R_BaseEnv, R_BaseNamespace, R_TrueValue, R_NilValue,
        R_FalseValue, R_UnboundValue, R_MissingArg, R_RestartToken,
        R_LogicalNAValue, R_EmptyEnv, R_DimSymbol, R_DotsSymbol,
        R_NamesSymbol, NA_STRING
    };
    std::unordered_map<SEXP, unsigned> map;
    for (auto g : vector) {
        map[g] = map.size();
    }
    return map;
}();

static bool canSelfReference(SEXPTYPE type) {
    switch (type) {
    case SYMSXP:
    case ENVSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case BCODESXP:
    case EXTERNALSXP:
        return true;
    case NILSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case DOTSXP:
    case ANYSXP:
    case RAWSXP:
        return false;
    default:
        assert(false && "canSelfReference: unhandled type");
    }
}

/*
 * From serialize.c
 * Type/Flag Packing and Unpacking
 *
 * To reduce space consumption for serializing code (lots of list
 * structure) the type (at most 8 bits), several single bit flags,
 * and the sxpinfo gp field (LEVELS, 16 bits) are packed into a single
 * integer.  The integer is signed, so this shouldn't be pushed too
 * far.  It assumes at least 28 bits, but that should be no problem.
 */

#define IS_OBJECT_BIT_MASK (1 << 8)
#define HAS_ATTR_BIT_MASK (1 << 9)
#define HAS_TAG_BIT_MASK (1 << 10)
#define ENCODE_LEVELS(v) ((v) << 12)

static unsigned packFlags(SEXPTYPE type, int levs, int isobj, int hasattr, int hastag)
{
    unsigned val;
    val = type | ENCODE_LEVELS(levs);
    if (isobj) val |= IS_OBJECT_BIT_MASK;
    if (hasattr) val |= HAS_ATTR_BIT_MASK;
    if (hastag) val |= HAS_TAG_BIT_MASK;
    return val;
}

// Will hash sexp if it's an instance of CLS
template <typename CLS>
static inline bool tryHash(SEXP sexp, Hasher& hasher) {
    if (CLS* b = CLS::check(sexp)) {
        hasher.hashBytesOf<unsigned>(b->info.magic);
        b->hash(hasher);
        return true;
    } else {
        return false;
    }
}

static inline void hashRir(SEXP sexp, Hasher& hasher) {
    if (!tryHash<DispatchTable>(sexp, hasher) &&
        !tryHash<Code>(sexp, hasher) &&
        !tryHash<Function>(sexp, hasher)) {
        std::cerr << "couldn't deserialize EXTERNALSXP: ";
        Rf_PrintValue(sexp);
        assert(false);
    }
}

static void hashBcLang1(SEXP sexp, Hasher& hasher, HashRefTable& bcRefs, std::queue<SEXP>& bcWorklist) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "doHash.cpp: hashBcLang1", sexp, [&]{
        int type = TYPEOF(sexp);
        if (type == LANGSXP || type == LISTSXP) {
            if (bcRefs.count(sexp)) {
                hasher.hashBytesOf<SpecialType>(SpecialType::BcRef);
                hasher.hashBytesOf<unsigned>(bcRefs.at(sexp));
                return;
            } else {
                bcRefs[sexp] = bcRefs.size();
            }

            auto attr = ATTRIB(sexp);
            if (attr != R_NilValue) {
                switch (type) {
                case LANGSXP:
                    type = (SEXPTYPE)SpecialType::AttrLangSexp;
                    break;
                case LISTSXP:
                    type = (SEXPTYPE)SpecialType::AttrListSexp;
                    break;
                default:
                    assert(false);
                }
                hasher.hashBytesOf<SEXPTYPE>(type);
                hasher.hash(attr);
            }
            hasher.hash(TAG(sexp));
            bcWorklist.push(CAR(sexp));
            bcWorklist.push(CDR(sexp));
        } else {
            hasher.hash(sexp);
        }
    });
}

static void hashBcLang(SEXP sexp, Hasher& hasher, HashRefTable& bcRefs) {
    std::queue<SEXP> bcWorklist;
    bcWorklist.push(sexp);
    if (!bcWorklist.empty()) {
        sexp = bcWorklist.front();
        bcWorklist.pop();

        hashBcLang1(sexp, hasher, bcRefs, bcWorklist);
    }
}

static void hashBc1(SEXP sexp, Hasher& hasher, HashRefTable& bcRefs, std::queue<SEXP>& bcWorklist, Protect& p) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "doHash.cpp: hashBc1", sexp, [&]{
        SEXP code = p(R_bcDecode(BCODE_CODE(sexp)));
        hasher.hash(code);
        auto consts = BCODE_CONSTS(sexp);
        auto n = LENGTH(consts);
        hasher.hashBytesOf<int>(n);
        for (auto i = 0; i < n; i++) {
            auto c = VECTOR_ELT(consts, i);
            auto type = TYPEOF(c);
            switch (type) {
            case BCODESXP:
                hasher.hashBytesOf<SEXPTYPE>(type);
                bcWorklist.push(c);
                break;
            case LANGSXP:
            case LISTSXP:
                hashBcLang(c, hasher, bcRefs);
                break;
            default:
                hasher.hashBytesOf<SEXPTYPE>(type);
                hasher.hash(c);
                break;
            }
        }
    });
}

static void hashBc(SEXP sexp, Hasher& hasher, HashRefTable& bcRefs) {
    Protect p;
    std::queue<SEXP> bcWorklist;
    bcWorklist.push(sexp);
    while (!bcWorklist.empty()) {
        sexp = bcWorklist.front();
        bcWorklist.pop();

        hashBc1(sexp, hasher, bcRefs, bcWorklist, p);
    }
}

static void hashSexp(SEXP sexp, Hasher& hasher, HashRefTable& refs) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "doHash.cpp: hashSexp", sexp, [&]{
        auto type = TYPEOF(sexp);

        if (ALTREP(sexp)) {
            auto info = ALTREP_SERIALIZED_CLASS(sexp);
            auto state = ALTREP_SERIALIZED_STATE(sexp);
            auto attrib = ATTRIB(sexp);
            if (info != nullptr && state != nullptr) {
                auto flags = packFlags((SEXPTYPE)SpecialType::Altrep,
                                       LEVELS(sexp), OBJECT(sexp), 0, 0);
                PROTECT(state);
                PROTECT(info);
                hasher.hashBytesOf<unsigned>(flags);
                hasher.hash(info);
                hasher.hash(state);
                hasher.hash(attrib);
                UNPROTECT(2); /* state, info */
                return;
            }
            /* else fall through to standard processing */
        } else if (globals.count(sexp)) {
            hasher.hashBytesOf<SpecialType>(SpecialType::Global);
            hasher.hashBytesOf<unsigned>(globals[sexp]);
            return;
        } else if (canSelfReference(type)) {
            if (refs.count(sexp)) {
                hasher.hashBytesOf<SpecialType>(SpecialType::Ref);
                hasher.hashBytesOf<unsigned>(refs[sexp]);
                return;
            } else {
                refs[sexp] = refs.size();
            }
        }
        hasher.hashBytesOf<SEXPTYPE>(type);

        bool hasTag;
        switch (type) {
        case LISTSXP:
        case LANGSXP:
        case PROMSXP:
        case DOTSXP:
            hasTag = TAG(sexp) != R_NilValue;
            break;
        case CLOSXP:
            hasTag = TRUE;
            break;
        default:
            hasTag = FALSE;
            break;
        }
        // With the CHARSXP cache chains maintained through the ATTRIB
        // field the content of that field must not be serialized, so
        // we treat it as not there.
        auto hasAttr = (type != CHARSXP && ATTRIB(sexp) != R_NilValue);
        auto flags = packFlags(type, LEVELS(sexp), OBJECT(sexp), hasAttr, hasTag);
        hasher.hashBytesOf<unsigned>(flags);
        hasher.hashBytesOf<bool>(hasAttr);
        if (hasAttr) {
            hasher.hash(ATTRIB(sexp));
        }

        switch (type) {
        case NILSXP:
            break;
        case SYMSXP:
            hasher.hash(PRINTNAME(sexp));
            break;
        case LISTSXP:
        case LANGSXP:
        case PROMSXP:
        case DOTSXP:
            if (hasTag) {
                hasher.hash(TAG(sexp));
            }
            if (BNDCELL_TAG(sexp)) {
                assert(false && "TODO R_expand_binding_value isn't public");
            }
            hasher.hash(CAR(sexp));
            // ???: use goto tailcall like R for perf boost?
            hasher.hash(CDR(sexp));
            break;
        case CLOSXP:
            hasher.hash(CLOENV(sexp));
            hasher.hash(FORMALS(sexp));
            // ???: use goto tailcall like R for perf boost?
            hasher.hash(BODY(sexp));
            break;
        case EXTPTRSXP:
            hasher.hash(EXTPTR_PROT(sexp));
            hasher.hash(EXTPTR_TAG(sexp));
            break;
        case WEAKREFSXP:
            // Currently we don't hash environment data because it's mutable
        case ENVSXP:
            break;
        case SPECIALSXP:
        case BUILTINSXP:
            hasher.hashBytesOf<int>(getBuiltinNr(sexp));
            break;
        case CHARSXP: {
            auto n = LENGTH(sexp);
            hasher.hashBytesOf<unsigned>(n);
            hasher.hashBytes(CHAR(sexp), n * sizeof(char));
            break;
        }
        case LGLSXP:
        case INTSXP: {
            auto n = XLENGTH(sexp);
            hasher.hashBytesOf<unsigned>(n);
            hasher.hashBytes(INTEGER(sexp), n * sizeof(int));
            break;
        }
        case REALSXP: {
            auto n = XLENGTH(sexp);
            hasher.hashBytesOf<unsigned>(n);
            hasher.hashBytes(REAL(sexp), n * sizeof(double));
            break;
        }
        case CPLXSXP: {
            auto n = XLENGTH(sexp);
            hasher.hashBytesOf<unsigned>(n);
            hasher.hashBytes(COMPLEX(sexp), n * sizeof(Rcomplex));
            break;
        }
        case RAWSXP: {
            auto n = XLENGTH(sexp);
            hasher.hashBytesOf<unsigned>(n);
            hasher.hashBytes(RAW(sexp), n * sizeof(Rbyte));
            break;
        }
        case STRSXP: {
            auto n = XLENGTH(sexp);
            hasher.hashBytesOf<unsigned>(n);
            for (int i = 0; i < n; ++i) {
                hasher.hash(STRING_ELT(sexp, i));
            }
            break;
        }
        case VECSXP:
        case EXPRSXP: {
            auto n = XLENGTH(sexp);
            hasher.hashBytesOf<unsigned>(n);
            for (int i = 0; i < n; ++i) {
                hasher.hash(VECTOR_ELT(sexp, i));
            }
            break;
        }
        case S4SXP:
            // Only attributes (i.e., slots) count
            break;
        case BCODESXP: {
            HashRefTable bcRefs;
            hashBc(sexp, hasher, bcRefs);
            break;
        }
        case EXTERNALSXP:
            hashRir(sexp, hasher);
            break;
        default:
            Rf_error("hashSexp: unknown type %i", type);
        }
    });
}

void hashRoot(SEXP root, UUID::Hasher& uuidHasher) {
    HashRefTable refs;
    std::queue<SEXP> worklist;
    worklist.push(root);
    Hasher hasher(uuidHasher, worklist);

    while (!worklist.empty()) {
        auto sexp = worklist.front();
        worklist.pop();

        hashSexp(sexp, hasher, refs);
    }
}

} // namespace rir