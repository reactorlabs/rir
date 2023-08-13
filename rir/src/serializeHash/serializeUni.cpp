
//
// Created by Jakob Hain on 8/9/23.
//

#include "serializeUni.h"
#include "R/Funtab.h"
#include "compiler/parameter.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "serializeHash/hash/hashRoot_getConnected_common.h"
#include "utils/Pool.h"
#include "utils/measuring.h"
#include <algorithm>

namespace rir {

/// All flags are set. Flags are only unset in children.
SerialFlags SerialFlags::Inherit(EnumSet<SerialFlag>::Any());
/// AST, not guaranteed RIR, hashed, in source, not in feedback
SerialFlags SerialFlags::Ast(SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource);
/// Not an SEXP, not hashed, in source, not in feedback
SerialFlags SerialFlags::DtContext(SerialFlag::InSource);
/// Not an AST, guaranteed rir, hashed, in source, in feedback
SerialFlags SerialFlags::DtBaseline(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource, SerialFlag::InFeedback);
/// Not an AST, guaranteed RIR, not hashed, not in feedback, not in source
SerialFlags SerialFlags::DtOptimized(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp);
/// Not an AST, guaranteed rir, hashed, in source, in feedback
SerialFlags SerialFlags::FunBody(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource, SerialFlag::InFeedback);
/// Not an AST, guaranteed rir, hashed, in source, in feedback
SerialFlags SerialFlags::FunDefaultArg(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource, SerialFlag::InFeedback);
/// Not an SEXP, not hashed, not in source, in feedback
SerialFlags SerialFlags::FunStats(SerialFlag::InFeedback);
/// Not an SEXP, hashed, in source, not in feedback
SerialFlags SerialFlags::FunMiscBytes(SerialFlag::Hashed, SerialFlag::InSource);
/// Not an AST, guaranteed rir, hashed, not in source, not in feedback
SerialFlags SerialFlags::CodeOuterFun(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed);
/// Not an AST, guaranteed rir, hashed, in source, not in feedback
SerialFlags SerialFlags::CodeArglistOrder(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource);
/// Child promise in extra pool
///
/// Not an AST, guaranteed rir, hashed, in source, in feedback
SerialFlags SerialFlags::CodePromise(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource, SerialFlag::InFeedback);
/// Data is part of a record_ bytecode. SEXP is a recorded call in extra pool.
///
/// Not an AST, not guaranteed rir, not hashed, not in source, in feedback
SerialFlags SerialFlags::CodeFeedback(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::InFeedback);
/// Unclassified SEXP in extra pool: original bytecode, any pool entry in
/// native code.
///
/// Not an AST, not guaranteed rir, hashed, not in source, not in feedback
SerialFlags SerialFlags::CodePoolUnknown(SerialFlag::MaybeNotAst, SerialFlag::MaybeSexp, SerialFlag::Hashed);
/// Code kind (i.e. whether the code is native) and native code.
///
/// Not an SEXP, hashed, not in source, not in feedback
SerialFlags SerialFlags::CodeNative(SerialFlag::Hashed);
/// AST, not guaranteed rir, hashed, in source, not in feedback
SerialFlags SerialFlags::CodeAst(SerialFlag::MaybeSexp, SerialFlag::Hashed, SerialFlag::InSource);
/// Not an SEXP, hashed, in source, not in feedback
SerialFlags SerialFlags::CodeMisc(SerialFlag::MaybeSexp, SerialFlag::MaybeNotAst, SerialFlag::Hashed, SerialFlag::InSource);

void AbstractSerializer::writeConst(unsigned idx, SerialFlags flags) {
    write(Pool::get(idx), flags);
}

void AbstractSerializer::writeSrc(unsigned idx, SerialFlags flags) {
    write(src_pool_at(idx), flags);
}

unsigned AbstractDeserializer::readConst(SerialFlags flags) {
    return Pool::insert(read(flags));
}

unsigned AbstractDeserializer::readSrc(SerialFlags flags) {
    return src_pool_add(read(flags));
}

/// "TYPEOF" for special cases, different than any normal SEXP TYPEOF, to ensure
/// they are hashed differently. This is similar to what serialize.c does.
///
/// This has the same size as TYPEOF (unsigned)
enum class SpecialType : SEXPTYPE {
    // Starts at 128, assuming regular SEXPTYPEs only go up to 127, and we
    // remove bytes after 255
    Global = 128,
    Ref = 129,
    Altrep = 130,
    // Only used in writeBc and readBc (when reading and writing bytecode)
    BcRef = 131
};

enum class EnvType {
    Package,
    Namespace,
    Regular
};

/// Reverse mapping of SEXP to global index
static std::unordered_map<SEXP, unsigned> globalsMap = []{
    std::unordered_map<SEXP, unsigned> map;
    for (auto g : globals) {
        map[g] = map.size();
    }
    return map;
}();

/// These SEXPs are added to the ref table the first time they are serialized or
/// deserialized, and serialized as / deserialized from refs subsequent times.
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

static char lastname[8192] = "<unknown>";
/// Similar to R_FindNamespace1 (tbh the code in serialize.c is very hacky and
/// I'm not 100% sure I'm following it correctly)
static SEXP findNamespace(SEXP info) {
    PROTECT(info);
    auto where = Rf_ScalarString(Rf_mkChar(lastname));
    PROTECT(where);
    auto s_getNamespace = Rf_install("..getNamespace");
    PROTECT(s_getNamespace);
    auto expr = Rf_lcons(s_getNamespace, Rf_lcons(info, Rf_lcons(where, R_NilValue)));
    PROTECT(expr);
    auto val = Rf_eval(expr, R_GlobalEnv);
    UNPROTECT(4);
    return val;
}


/*
 * From serialize.c
 * Type/Flag Packing and Unpacking
 *
 * To reduce space consumption for serializing code (lots of list
 * structure) the type (at most 8 bits), several single bit flags,
 * and the sxpinfo gp field (LEVELS, 16 bits) are packed into a single
 * integer. The integer is signed, so this shouldn't be pushed too
 * far. It assumes at least 28 bits, but that should be no problem.
 */

#define IS_OBJECT_BIT_MASK (1 << 8)
#define HAS_ATTR_BIT_MASK (1 << 9)
#define HAS_TAG_BIT_MASK (1 << 10)
#define ENCODE_LEVELS(v) ((v) << 12)
#define DECODE_LEVELS(v) ((v) >> 12)
#define DECODE_TYPE(v) ((v) & 255)
#define CACHED_MASK (1<<5)
#define HASHASH_MASK 1

static unsigned packFlags(SEXPTYPE type, int levs, bool isobj, bool hasattr,
                          bool hastag) {
    unsigned val;
    if (type == CHARSXP) levs &= (~(CACHED_MASK | HASHASH_MASK));
    val = type | ENCODE_LEVELS(levs);
    if (isobj) val |= IS_OBJECT_BIT_MASK;
    if (hasattr) val |= HAS_ATTR_BIT_MASK;
    if (hastag) val |= HAS_TAG_BIT_MASK;
    return val;
}


static void unpackFlags(unsigned flags, SEXPTYPE& ptype, int& plevs,
                        bool& pisobj, bool& phasattr, bool& phastag) {
    ptype = DECODE_TYPE(flags);
    plevs = DECODE_LEVELS(flags);
    pisobj = !!(flags & IS_OBJECT_BIT_MASK);
    phasattr = !!(flags & HAS_ATTR_BIT_MASK);
    phastag = !!(flags & HAS_TAG_BIT_MASK);
}

/// More code from R
void R_expand_binding_value(SEXP b) {
#if BOXED_BINDING_CELLS
    SET_BNDCELL_TAG(b, 0);
#else
    int typetag = BNDCELL_TAG(b);
    if (typetag) {
        union {
            SEXP sxpval;
            double dval;
            int ival;
        } vv;
        SEXP val;
        vv.sxpval = CAR0(b);
        switch (typetag) {
        case REALSXP:
            PROTECT(b);
            val = ScalarReal(vv.dval);
            SET_BNDCELL(b, val);
            INCREMENT_NAMED(val);
            UNPROTECT(1);
            break;
        case INTSXP:
            PROTECT(b);
            val = ScalarInteger(vv.ival);
            SET_BNDCELL(b, val);
            INCREMENT_NAMED(val);
            UNPROTECT(1);
            break;
        case LGLSXP:
            PROTECT(b);
            val = ScalarLogical(vv.ival);
            SET_BNDCELL(b, val);
            INCREMENT_NAMED(val);
            UNPROTECT(1);
            break;
        }
    }
#endif
}

// Will serialize s if it's an instance of CLS
template <typename CLS>
static bool tryWrite(AbstractSerializer& serializer, SEXP s) {
    if (CLS* b = CLS::check(s)) {
        serializer.writeBytesOf(b->info.magic);
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: writeRir", s, [&]{
            b->serialize(serializer);
        });
        return true;
    } else {
        return false;
    }
}

static void writeRir(AbstractSerializer& serializer, SEXP s) {
    if (!tryWrite<DispatchTable>(serializer, s) &&
        !tryWrite<Code>(serializer, s) &&
        !tryWrite<Function>(serializer, s) &&
        !tryWrite<ArglistOrder>(serializer, s) &&
        !tryWrite<LazyArglist>(serializer, s) &&
        !tryWrite<LazyEnvironment>(serializer, s) &&
        !tryWrite<PirTypeFeedback>(serializer, s)) {
        std::cerr << "couldn't serialize EXTERNALSXP: ";
        Rf_PrintValue(s);
        assert(false);
    }
}

static SEXP readRir(AbstractDeserializer& deserializer) {
    return Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: readRir", [&]{
        auto magic = deserializer.readBytesOf<unsigned>();
        switch (magic) {
        case DISPATCH_TABLE_MAGIC:
            return DispatchTable::deserialize(deserializer)->container();
        case CODE_MAGIC:
            return Code::deserialize(deserializer)->container();
        case FUNCTION_MAGIC:
            return Function::deserialize(deserializer)->container();
        case ARGLIST_ORDER_MAGIC:
            return ArglistOrder::deserialize(deserializer)->container();
        case LAZY_ARGS_MAGIC:
            return LazyArglist::deserialize(deserializer)->container();
        case LAZY_ENVIRONMENT_MAGIC:
            return LazyEnvironment::deserialize(deserializer)->container();
        case PIR_TYPE_FEEDBACK_MAGIC:
            return PirTypeFeedback::deserialize(deserializer)->container();
        default:
            std::cerr << "unhandled RIR object magic: 0x" << std::hex << magic
                      << "\n";
            assert(false);
        }
    });
}

static void writeBcLang(AbstractSerializer& serializer, SerializedRefs& bcRefs,
                        SEXP sexp) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: writeBcLang1", sexp, [&]{
        int type = TYPEOF(sexp);
        if (type == LANGSXP || type == LISTSXP) {
            if (bcRefs.count(sexp)) {
                serializer.writeBytesOf(SpecialType::BcRef);
                serializer.writeBytesOf((unsigned)bcRefs.at(sexp));
                return;
            } else {
                serializer.writeBytesOf(type);
                bcRefs[sexp] = bcRefs.size();
            }

            auto attr = ATTRIB(sexp);
            serializer.writeBytesOf(attr != R_NilValue);
            if (attr != R_NilValue) {
                serializer.write(attr);
            }
            serializer.write(TAG(sexp));
            writeBcLang(serializer, bcRefs, CAR(sexp));
            writeBcLang(serializer, bcRefs, CDR(sexp));
        } else {
            serializer.writeBytesOf(type);
            serializer.write(sexp);
        }
    });
}

static SEXP readBcLang(AbstractDeserializer& deserializer,
                       SEXPTYPE type,
                       DeserializedRefs& bcRefs) {
    return Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: readBcLang1", [&]{
        switch (type) {
        case (SEXPTYPE)SpecialType::BcRef:
            return bcRefs.at(deserializer.readBytesOf<unsigned>());
        case LISTSXP:
        case LANGSXP: {
            auto result = Rf_allocSExp(type);
            PROTECT(result);
            bcRefs.push_back(result);
            if (deserializer.readBytesOf<bool>()) {
                SET_ATTRIB(result, deserializer.read());
            }
            SET_TAG(result, deserializer.read());
            SETCAR(result, readBcLang(deserializer, deserializer.readBytesOf<SEXPTYPE>(), bcRefs));
            SETCDR(result, readBcLang(deserializer, deserializer.readBytesOf<SEXPTYPE>(), bcRefs));
            UNPROTECT(1);
            return result;
        }
        default:
            return deserializer.read();
        }
    });
}

static void writeBc(AbstractSerializer& serializer, SerializedRefs& bcRefs,
                    SEXP sexp) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: writeBc1", sexp, [&]{
        SEXP code = R_bcDecode(BCODE_CODE(sexp));
        serializer.write(code);
        auto consts = BCODE_CONSTS(sexp);
        auto n = LENGTH(consts);
        serializer.writeBytesOf(n);
        for (auto i = 0; i < n; i++) {
            auto c = VECTOR_ELT(consts, i);
            auto type = TYPEOF(c);
            switch (type) {
            case BCODESXP:
                serializer.writeBytesOf(type);
                writeBc(serializer, bcRefs, c);
                break;
            case LANGSXP:
            case LISTSXP:
                writeBcLang(serializer, bcRefs, c);
                break;
            default:
                serializer.writeBytesOf(type);
                serializer.write(c);
                break;
            }
        }
    });
}

static SEXP readBc(AbstractDeserializer& deserializer, DeserializedRefs* refs,
                   DeserializedRefs& bcRefs) {
    return Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: readBc1", [&]{
        auto result = Rf_allocSExp(BCODESXP);
        if (refs) {
            refs->push_back(result);
        }
        PROTECT(result);
        auto bytes = deserializer.read();
        PROTECT(bytes);
        SETCAR(result, R_bcEncode(bytes));
        auto n = deserializer.readBytesOf<R_len_t>();
        auto consts = Rf_allocVector(VECSXP, n);
        PROTECT(consts);
        for (auto i = 0; i < n; i++) {
            auto type = deserializer.readBytesOf<SEXPTYPE>();
            SEXP elem;
            switch (type) {
            case BCODESXP:
                // Don't add this element to refs
                elem = readBc(deserializer, nullptr, bcRefs);
                break;
            case (SEXPTYPE)SpecialType::BcRef:
                elem = bcRefs.at(deserializer.readBytesOf<unsigned>());
                break;
            case LISTSXP:
            case LANGSXP:
                elem = readBcLang(deserializer, type, bcRefs);
                break;
            default:
                elem = deserializer.read();
                break;
            }
            SET_VECTOR_ELT(consts, i, elem);
        }
        SETCDR(result, consts);
        SET_TAG(bytes, R_NilValue);
        R_registerBC(bytes, result);
        UNPROTECT(3);
        return result;
    });
}

void AbstractSerializer::writeInline(SEXP sexp) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline", sexp, [&]{
        auto refs = this->refs();

        SEXPTYPE type;
        if (ALTREP(sexp) && ALTREP_SERIALIZED_CLASS(sexp) && ALTREP_SERIALIZED_STATE(sexp)) {
            type = (SEXPTYPE)SpecialType::Altrep;
        } else if (globalsMap.count(sexp)) {
            type = (SEXPTYPE)SpecialType::Global;
        } else if (canSelfReference(TYPEOF(sexp)) && refs && refs->count(sexp)) {
            type = (SEXPTYPE)SpecialType::Ref;
        } else {
            type = TYPEOF(sexp);
        }

        bool hasTag_ = type != (SEXPTYPE)SpecialType::Global &&
                       type != (SEXPTYPE)SpecialType::Ref &&
                       type != (SEXPTYPE)SpecialType::Altrep && hasTag(sexp);
        // With the CHARSXP cache chains maintained through the ATTRIB
        // field the content of that field must not be serialized, so
        // we treat it as not there.
        auto hasAttr = type != (SEXPTYPE)SpecialType::Global &&
                       type != (SEXPTYPE)SpecialType::Ref &&
                       type != CHARSXP &&
                       (type == (SEXPTYPE)SpecialType::Altrep ||
                            ATTRIB(sexp) != R_NilValue);
        auto rFlags = packFlags(type, LEVELS(sexp), OBJECT(sexp), hasAttr, hasTag_);
        writeBytesOf(rFlags);

        // Write attrs and tag at the beginning if we (maybe) tail call, at the
        // end if we self-reference, and otherwise at the end (otherwise doesn't
        // matter as long as we read at the same position)
        auto writeAttr = [&]{
            if (hasAttr) {
                Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline attribute", sexp, [&]{
                    write(ATTRIB(sexp));
                });
            }
        };
        auto writeTag = [&]{
            if (hasTag_) {
                Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline tag", sexp, [&]{
                    write(TAG(sexp));
                });
            }
        };

        if (type == TYPEOF(sexp) && canSelfReference(type) && refs &&
            !refs->count(sexp)) {
            (*refs)[sexp] = refs->size();
        }

        switch (type) {
        case (SEXPTYPE)SpecialType::Altrep:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline altrep", sexp, [&]{
                auto info = ALTREP_SERIALIZED_CLASS(sexp);
                auto state = ALTREP_SERIALIZED_STATE(sexp);
                PROTECT(info);
                PROTECT(state);
                write(info);
                write(state);
                UNPROTECT(2);
                writeAttr();
                // No tag
            });
            break;
        case (SEXPTYPE)SpecialType::Global:
            writeBytesOf(globalsMap.at(sexp));
            // Attr and tag already present
            break;
        case (SEXPTYPE)SpecialType::Ref:
            writeBytesOf((unsigned)refs->at(sexp));
            // Attr and tag already present
            break;
        case NILSXP:
            // No attr or tag
            break;
        case SYMSXP:
            writeInline(PRINTNAME(sexp));
            writeAttr();
            // No tag
            break;
        case LISTSXP:
        case LANGSXP:
        case PROMSXP:
        case DOTSXP:
            writeAttr();
            writeTag();
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline list elem", sexp, [&]{
                if (BNDCELL_TAG(sexp)) {
                    R_expand_binding_value(sexp);
                }
                write(CAR(sexp));
            });
            writeInline(CDR(sexp));
            break;
        case CLOSXP:
            writeAttr();
            writeTag();
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline closure sans body", sexp, [&]{
                write(CLOENV(sexp));
                write(FORMALS(sexp));
            });
            writeInline(BODY(sexp));
            break;
        case EXTPTRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline external pointer", sexp, [&]{
                write(EXTPTR_PROT(sexp));
                write(EXTPTR_TAG(sexp));
            });
            writeAttr();
            // No tag
            break;
        case WEAKREFSXP:
            // Only exists as a reference
            writeAttr();
            // No tag
            break;
        case ENVSXP:
            // TODO: Don't hash (don't write when hashing)
            if (R_IsPackageEnv(sexp)) {
                writeBytesOf(EnvType::Package);
                writeInline(PROTECT(R_PackageEnvName(sexp)));
                UNPROTECT(1);
            } else if (R_IsNamespaceEnv(sexp)) {
                writeBytesOf(EnvType::Namespace);
                writeInline(PROTECT(R_NamespaceEnvSpec(sexp)));
                UNPROTECT(1);
            } else {
                writeBytesOf(EnvType::Regular);
                writeBytesOf((bool)R_EnvironmentIsLocked(sexp));
                write(ENCLOS(sexp));
                write(FRAME(sexp));
                write(HASHTAB(sexp));
            }
            writeAttr();
            // No tag
            break;
        case SPECIALSXP:
        case BUILTINSXP:
            writeBytesOf(getBuiltinNr(sexp));
            writeAttr();
            // No tag
            break;
        case CHARSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline char vector", sexp, [&]{
                if (sexp == NA_STRING) {
                    writeBytesOf<R_len_t>(-1);
                } else {
                    auto n = LENGTH(sexp);
                    writeBytesOf<R_len_t>(n);
                    writeBytes(CHAR(sexp), n * sizeof(char));
                }
            });
            writeAttr();
            // No tag
            break;
        case LGLSXP:
        case INTSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline int vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n);
                writeBytes(INTEGER(sexp), n * sizeof(int));
            });
            writeAttr();
            // No tag
            break;
        case REALSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline real vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n);
                writeBytes(REAL(sexp), n * sizeof(double));
            });
            writeAttr();
            // No tag
            break;
        case CPLXSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline complex number vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n);
                writeBytes(COMPLEX(sexp), n * sizeof(Rcomplex));
            });
            writeAttr();
            // No tag
            break;
        case RAWSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline byte vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n);
                writeBytes(RAW(sexp), n * sizeof(Rbyte));
            });
            writeAttr();
            // No tag
            break;
        case STRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline string vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n);
                for (int i = 0; i < n; i++) {
                    write(STRING_ELT(sexp, i));
                }
            });
            writeAttr();
            // No tag
            break;
        case VECSXP:
        case EXPRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline expression or vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n);
                for (int i = 0; i < n; i++) {
                    write(VECTOR_ELT(sexp, i));
                }
            });
            writeAttr();
            // No tag
            break;
        case S4SXP:
            // Only attributes (i.e., slots) count
            writeAttr();
            // No tag
            break;
        case BCODESXP: {
            SerializedRefs bcRefs;
            writeBc(*this, bcRefs, sexp);
            writeAttr();
            // No tag
            break;
        }
        case EXTERNALSXP:
            writeRir(*this, sexp);
            writeAttr();
            // No tag
            break;
        default:
            Rf_error("hashChild: unknown type %i", type);
        }
    });
}

SEXP AbstractDeserializer::readInline() {
    return Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline", [&]{
        auto refs = this->refs();

        auto rFlags = readBytesOf<unsigned>();
        SEXPTYPE type;
        int levels;
        bool object, hasAttr, hasTag_;
        unpackFlags(rFlags, type, levels, object, hasAttr, hasTag_);

        // Read attrs and tag at the beginning if we (maybe) tail call, at the
        // end if we self-reference, and otherwise at the end (otherwise doesn't
        // matter as long as we wrote at the same position)
        SEXP attrib = nullptr;
        SEXP tag = nullptr;
        auto readAttr = [&]{
            if (hasAttr) {
                attrib = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline attribute", [&]{
                    return read();
                });
                PROTECT(attrib);
            }
        };
        auto readTag = [&]{
            if (hasTag_) {
                tag = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline tag", [&]{
                    return read();
                });
                PROTECT(tag);
            }
        };

        SEXP result;
        switch (type) {
        case (SEXPTYPE)SpecialType::Altrep: {
            auto info = PROTECT(read());
            auto state = PROTECT(read());
            readAttr();
            // No tag
            result = ALTREP_UNSERIALIZE_EX(info, state, attrib, object, levels);
            UNPROTECT(2);
            break;
        }
        case (SEXPTYPE)SpecialType::Global:
            result = globals[readBytesOf<unsigned>()];
            // Attr and tag already present
            break;
        case (SEXPTYPE)SpecialType::Ref:
            result = refs->at(readBytesOf<unsigned>());
            // Attr and tag already present
            break;
        case NILSXP:
            result = R_NilValue;
            // No attr or tag
            break;
        case SYMSXP:
            result = Rf_installTrChar(readInline());
            // Symbols have read refs (same symbol can be serialized and
            // we want it to point to the same SEXP when deserializing)
            if (refs) {
                refs->push_back(result);
            }
            readAttr();
            // No tag
            break;
        case LISTSXP:
        case LANGSXP:
        case PROMSXP:
        case DOTSXP:
            readAttr();
            readTag();
            result = Rf_allocSExp(type);
            PROTECT(result);
            if (tag && Rf_isSymbol(tag)) {
                snprintf(lastname, 8192, "%s", CHAR(PRINTNAME(tag)));
            }
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline list elem", result, [&]{
                SETCAR(result, read());
            });
            SETCDR(result, readInline());
            if (type == CLOSXP && CLOENV(result) == R_NilValue) {
                SET_CLOENV(result, R_BaseEnv);
            }
            if (type == PROMSXP && PRENV(result) == R_NilValue) {
                SET_PRENV(result, R_BaseEnv);
            }
            snprintf(lastname, 8192, "<unknown>");
            UNPROTECT(1);
            break;
        case CLOSXP:
            readAttr();
            readTag();
            result = Rf_allocSExp(type);
            PROTECT(result);
            Measuring::timeEventIf(
                pir::Parameter::PIR_MEASURE_SERIALIZATION,
                "serializeUni.cpp: AbstractDeserializer::readInline closure sans body", result,
                [&] {
                    SET_CLOENV(result, read());
                    SET_FORMALS(result, read());
                });
            SET_BODY(result, readInline());
            UNPROTECT(1);
            break;
        case EXTPTRSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline external pointer", [&]{
                auto result = Rf_allocSExp(type);
                PROTECT(result);
                if (refs) {
                    refs->push_back(result);
                }
                R_SetExternalPtrAddr(result, nullptr);
                R_SetExternalPtrProtected(result, read());
                R_SetExternalPtrTag(result, read());
                UNPROTECT(1);
                return result;
            });
            readAttr();
            // No tag
            break;
        case WEAKREFSXP:
            result = R_MakeWeakRef(R_NilValue, R_NilValue, R_NilValue, FALSE);
            if (refs) {
                refs->push_back(result);
            }
            readAttr();
            // No tag
            break;
        case ENVSXP:
            switch (readBytesOf<EnvType>()) {
            case EnvType::Package: {
                auto name = readInline();
                PROTECT(name);
                result = R_FindPackageEnv(name);
                if (refs) {
                    refs->push_back(result);
                }
                UNPROTECT(1);
                break;
            }
            case EnvType::Namespace: {
                auto name = readInline();
                PROTECT(name);
                result = findNamespace(name);
                if (refs) {
                    refs->push_back(result);
                }
                UNPROTECT(1);
                break;
            }
            case EnvType::Regular: {
                auto isLocked = readBytesOf<bool>();
                result = Rf_allocSExp(type);
                PROTECT(result);
                if (refs) {
                    refs->push_back(result);
                }

                SET_ENCLOS(result, read());
                SET_FRAME(result, read());
                SET_HASHTAB(result, read());

                R_RestoreHashCount(result);
                if (isLocked) {
                    R_LockEnvironment(result, FALSE);
                }
                if (ENCLOS(result) == R_NilValue) {
                    SET_ENCLOS(result, R_BaseEnv);
                }
                UNPROTECT(1);
                break;
            }
            }
            readAttr();
            // No tag
            break;
        case SPECIALSXP:
        case BUILTINSXP:
            result = getBuiltinOrSpecialFun(readBytesOf<int>());
            readAttr();
            // No tag
            break;
        case CHARSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline char vector", [&]{
                auto length = readBytesOf<R_len_t>();
                if (length == -1) {
                    return NA_STRING;
                } else if (length < 8192) {
                    // Store data on stack
                    // R doesn't allow allocVector because it interns strings
                    char data[8192];
                    readBytes(data, length);
                    data[length] = '\0';
                    return Rf_mkCharLenCE(data, length, CE_NATIVE);
                } else {
                    // Too large, store data on heap
                    // R doesn't allow allocVector(CHARSXP) because it interns
                    // strings
                    char* data = (char*)malloc(length + 1);
                    readBytes(data, length);
                    data[length] = '\0';
                    auto result = Rf_mkCharLenCE(data, length, CE_NATIVE);
                    free(data);
                    return result;
                }
            });
            readAttr();
            // No tag
            break;
        case LGLSXP:
        case INTSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline int vector", [&]{
                auto length = readBytesOf<R_xlen_t>();
                auto sexp = Rf_allocVector(type, length);
                readBytes((void*)INTEGER(sexp), length * sizeof(int));
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case REALSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline real vector", [&]{
                auto length = readBytesOf<R_xlen_t>();
                auto sexp = Rf_allocVector(type, length);
                readBytes((void*)REAL(sexp), length * sizeof(double));
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case CPLXSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline complex number vector sexp", [&]{
                auto length = readBytesOf<R_xlen_t>();
                auto sexp = Rf_allocVector(type, length);
                readBytes((void*)COMPLEX(sexp), length * sizeof(Rcomplex));
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case RAWSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline byte vector", [&]{
                auto length = readBytesOf<R_xlen_t>();
                auto sexp = Rf_allocVector(type, length);
                readBytes((void*)RAW(sexp), length * sizeof(Rbyte));
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case STRSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline string vector", [&]{
                auto length = readBytesOf<R_xlen_t>();
                auto sexp = Rf_allocVector(type, length);
                PROTECT(sexp);
                for (int i = 0; i < length; i++) {
                    SET_STRING_ELT(sexp, i, read());
                }
                UNPROTECT(1);
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case VECSXP:
        case EXPRSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline expression or vector", [&]{
                auto length = readBytesOf<R_xlen_t>();
                auto sexp = Rf_allocVector(type, length);
                PROTECT(sexp);
                for (int i = 0; i < length; i++) {
                    SET_VECTOR_ELT(sexp, i, read());
                }
                UNPROTECT(1);
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case S4SXP:
            // Only attributes (i.e., slots) count
            result = Rf_allocSExp(type);
            readAttr();
            // No tag
            break;
        case BCODESXP: {
            DeserializedRefs bcRefs;
            result = readBc(*this, refs, bcRefs);
            readAttr();
            // No tag
            break;
        }
        case EXTERNALSXP:
            result = readRir(*this);
            readAttr();
            // No tag
            break;
        default:
            Rf_error("hashChild: unknown type %i", type);
        }

        PROTECT(result);
        if (type != CHARSXP) {
            SETLEVELS(result, levels);
        }
        SET_OBJECT(result, object);
        if (attrib) {
            SET_ATTRIB(result, attrib);
            if (TYPEOF(result) == ENVSXP &&
                Rf_getAttrib(result, R_ClassSymbol) != R_NilValue) {
                // TODO: This is what R's serialization does, it it needed for RIR's serialization
                // We don't write out the object bit for environments, so
                // reconstruct it here if needed
                SET_OBJECT(result, TRUE);
            }
        }
        if (tag) {
            SET_TAG(result, tag);
        }
        if (attrib) {
            UNPROTECT(1);
        }
        if (tag) {
            UNPROTECT(1);
        }
        UNPROTECT(1);

        assert(
            (type == (SEXPTYPE)SpecialType::Altrep ||
             type == (SEXPTYPE)SpecialType::Global ||
             type == (SEXPTYPE)SpecialType::Ref || type == TYPEOF(result)) &&
            "sanity check failed: result deserialized into a different type"
        );
        SLOWASSERT(
            (type == (SEXPTYPE)SpecialType::Altrep ||
             type == (SEXPTYPE)SpecialType::Global ||
             type == (SEXPTYPE)SpecialType::Ref || !canSelfReference(type) ||
             !refs ||
             std::find(refs->begin(), refs->end(), result) != refs->end()) &&
            "sanity check failed: type can self reference but wasn't inserted "
            "into ref table"
        );

        return result;
    });
}

} // namespace rir
