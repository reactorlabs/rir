//
// Created by Jakob Hain on 7/21/23.
//

#include "hashRootOld.h"
#include "R/Funtab.h"
#include "R/disableGc.h"
#include "compiler/parameter.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "serializeHash/globals.h"
#include "serializeHash/hash/hashAst.h"
#include "serializeHash/hash/hashRoot_getConnected_common.h"
#include "utils/Pool.h"
#include "utils/measuring.h"
#include <iostream>

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
    case LISTSXP:
    case CLOSXP:
    case PROMSXP:
    case LANGSXP:
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
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
    case S4SXP:
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
static inline bool tryHash(SEXP sexp, HasherOld& hasher) {
    if (CLS* b = CLS::check(sexp)) {
        hasher.hashBytesOf<unsigned>(b->info.magic);
        b->hash(hasher);
        return true;
    } else {
        return false;
    }
}

static inline void hashRir(SEXP sexp, HasherOld& hasher) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashRir", sexp, [&]{
        if (!tryHash<DispatchTable>(sexp, hasher) &&
            !tryHash<Function>(sexp, hasher) &&
            !tryHash<Code>(sexp, hasher) &&
            !tryHash<ArglistOrder>(sexp, hasher) &&
            !tryHash<LazyArglist>(sexp, hasher) &&
            !tryHash<LazyEnvironment>(sexp, hasher) &&
            !tryHash<PirTypeFeedback>(sexp, hasher)) {
            std::cerr << "couldn't hash EXTERNALSXP: ";
            Rf_PrintValue(sexp);
            assert(false);
        }
    });
}

static void hashBcLang1(SEXP sexp, HasherOld& hasher, HashRefTable& bcRefs,
                        std::queue<SEXP>& bcLangWorklist) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashBcLang1", sexp, [&]{
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
            bcLangWorklist.push(CAR(sexp));
            bcLangWorklist.push(CDR(sexp));
        } else {
            hasher.hash(sexp);
        }
    });
}

static void hashBcLang(SEXP sexp, HasherOld& hasher, HashRefTable& bcRefs) {
    std::queue<SEXP> bcLangWorklist;
    bcLangWorklist.push(sexp);
    while (!bcLangWorklist.empty()) {
        sexp = bcLangWorklist.front();
        bcLangWorklist.pop();

        hashBcLang1(sexp, hasher, bcRefs, bcLangWorklist);
    }
}

static void hashBc1(SEXP sexp, HasherOld& hasher, HashRefTable& bcRefs, std::queue<SEXP>& bcWorklist) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashBc1", sexp, [&]{
        SEXP code = R_bcDecode(BCODE_CODE(sexp));
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

static void hashBc(SEXP sexp, HasherOld& hasher, HashRefTable& bcRefs) {
    std::queue<SEXP> bcWorklist;
    bcWorklist.push(sexp);
    while (!bcWorklist.empty()) {
        sexp = bcWorklist.front();
        bcWorklist.pop();

        hashBc1(sexp, hasher, bcRefs, bcWorklist);
    }
}

static void hashChild(SEXP sexp, HasherOld& hasher, HashRefTable& refs) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild", sexp, [&]{
        auto type = TYPEOF(sexp);

        if (ALTREP(sexp)) {
            auto info = ALTREP_SERIALIZED_CLASS(sexp);
            auto state = ALTREP_SERIALIZED_STATE(sexp);
            auto attrib = ATTRIB(sexp);
            if (info != nullptr && state != nullptr) {
                Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild altrep", sexp, [&]{
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
                });
            }
            /* else fall through to standard processing */
        } else if (global2Index.count(sexp)) {
            hasher.hashBytesOf<SpecialType>(SpecialType::Global);
            hasher.hashBytesOf<unsigned>(global2Index.at(sexp));
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

        bool hasTag_ = hasTag(sexp);
        // With the CHARSXP cache chains maintained through the ATTRIB
        // field the content of that field must not be serialized, so
        // we treat it as not there.
        auto hasAttr = (type != CHARSXP && ATTRIB(sexp) != R_NilValue);
        auto flags = packFlags(type, LEVELS(sexp), OBJECT(sexp), hasAttr, hasTag_);
        hasher.hashBytesOf<unsigned>(flags);
        hasher.hashBytesOf<bool>(hasAttr);
        if (hasAttr) {
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild attrib", sexp, [&]{
                hasher.hash(ATTRIB(sexp));
            });
        }

        switch (type) {
        case NILSXP:
            break;
        case SYMSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild symbol", sexp, [&]{
                hasher.hash(PRINTNAME(sexp));
            });
            break;
        case LISTSXP:
        case LANGSXP:
        case PROMSXP:
        case DOTSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild tag", sexp, [&]{
                if (hasTag_) {
                    hasher.hash(TAG(sexp));
                }
            });
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild list elem", sexp, [&]{
                if (BNDCELL_TAG(sexp)) {
                    assert(false && "TODO R_expand_binding_value isn't public");
                }
                hasher.hash(CAR(sexp));
            });
            // ???: use goto tailcall like R for perf boost?
            hasher.hash(CDR(sexp));
            break;
        case CLOSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild closure sans body", sexp, [&]{
                hasher.hash(CLOENV(sexp));
                hasher.hash(FORMALS(sexp));
            });
            // ???: use goto tailcall like R for perf boost?
            hasher.hash(BODY(sexp));
            break;
        case EXTPTRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild external pointer", sexp, [&]{
                hasher.hash(EXTPTR_PROT(sexp));
                hasher.hash(EXTPTR_TAG(sexp));
            });
            break;
        case WEAKREFSXP:
            // Currently we don't hash environment data because it's mutable
        case ENVSXP:
            break;
        case SPECIALSXP:
        case BUILTINSXP:
            hasher.hashBytesOf<int>(getBuiltinNr(sexp));
            break;
        case CHARSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild char vector", sexp, [&]{
                auto n = LENGTH(sexp);
                hasher.hashBytesOf<unsigned>(n);
                hasher.hashBytes(CHAR(sexp), n * sizeof(char));
            });
            break;
        case LGLSXP:
        case INTSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild int vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                hasher.hashBytesOf<unsigned>(n);
                hasher.hashBytes(INTEGER(sexp), n * sizeof(int));
            });
            break;
        case REALSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild real vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                hasher.hashBytesOf<unsigned>(n);
                hasher.hashBytes(REAL(sexp), n * sizeof(double));
            });
            break;
        case CPLXSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild complex number vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                hasher.hashBytesOf<unsigned>(n);
                hasher.hashBytes(COMPLEX(sexp), n * sizeof(Rcomplex));
            });
            break;
        case RAWSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild byte vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                hasher.hashBytesOf<unsigned>(n);
                hasher.hashBytes(RAW(sexp), n * sizeof(Rbyte));
            });
            break;
        case STRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild string vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                hasher.hashBytesOf<unsigned>(n);
                for (int i = 0; i < n; ++i) {
                    hasher.hash(STRING_ELT(sexp, i));
                }
            });
            break;
        case VECSXP:
        case EXPRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot.cpp: hashChild expression or vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                hasher.hashBytesOf<unsigned>(n);
                for (int i = 0; i < n; ++i) {
                    hasher.hash(VECTOR_ELT(sexp, i));
                }
            });
            break;
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
            Rf_error("hashChild: unknown type %i", type);
        }
    });
}

void HasherOld::hashConstant(unsigned idx) {
    hash(Pool::get(idx));
}

void HasherOld::hashSrc(unsigned idx) {
    hash(src_pool_at(idx), true);
}

UUID hashRootOld(SEXP root) {
    UUID result;
    disableInterpreter([&]{
        disableGc([&]{
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashRoot", root, [&]{
                UUID::Hasher uuidHasher;
                HasherOld::Worklist worklist;
                HashRefTable refs;
                worklist.push({root, false});
                HasherOld hasher(uuidHasher, worklist);

                while (!worklist.empty()) {
                    auto& elem = worklist.front();
                    auto sexp = elem.sexp;
                    auto isAst = elem.isAst;
                    worklist.pop();

                    if (isAst) {
                        auto uuid = hashAst(sexp);
                        hasher.hashBytesOf(uuid);
                    } else {
                        hashChild(sexp, hasher, refs);
                    }
                }
                result = uuidHasher.finalize();
            });
        });
    });
    return result;
}

} // namespace rir