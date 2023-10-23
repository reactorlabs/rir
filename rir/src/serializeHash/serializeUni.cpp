
//
// Created by Jakob Hain on 8/9/23.
//

#include "serializeUni.h"
#include "R/Funtab.h"
#include "compiler/parameter.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "runtime/PoolStub.h"
#include "serializeHash/globals.h"
#include "serializeHash/hash/hashRoot_getConnected_common.h"
#include "serializeHash/serialize/rPackFlags.h"
#include "utils/Pool.h"
#include "utils/measuring.h"
#include <algorithm>
#include <ostream>

namespace rir {

unsigned SerialFlags::nextId = 0;

// Inlay hints are needed to understand the below code
#define V(name)                                                                \
const SerialFlags SerialFlags::name(true, true, true, true, true, true, true);
LIST_OF_INHERIT_SERIAL_FLAGS(V)
#undef V
const SerialFlags SerialFlags::Ast(
    true,
    false,
    true,
    true,
    true,
    false,
    true);
const SerialFlags SerialFlags::DtContext(
    false,
    true,
    true,
    false,
    true,
    false,
    true);
const SerialFlags SerialFlags::DtBaseline(
    true,
    true,
    true,
    true,
    true,
    true,
    true);
const SerialFlags SerialFlags::DtOptimized(
    false,
    true,
    true,
    true,
    false,
    false,
    true);
const SerialFlags SerialFlags::FunBody(
    true,
    true,
    true,
    true,
    true,
    true,
    true);
const SerialFlags SerialFlags::FunDefaultArg(
    true,
    true,
    true,
    true,
    true,
    true,
    true);
const SerialFlags SerialFlags::FunFeedback(
    false,
    true,
    true,
    true,
    false,
    true,
    true);
const SerialFlags SerialFlags::FunStats(
    false,
    true,
    true,
    false,
    false,
    false,
    true);
const SerialFlags SerialFlags::FunMiscBytes(
    true,
    true,
    true,
    false,
    true,
    false,
    true);
const SerialFlags SerialFlags::CodeArglistOrder(
    true,
    true,
    true,
    true,
    true,
    false,
    true);
const SerialFlags SerialFlags::CodeOuterFun(
    true,
    true,
    true,
    true,
    true,
    false,
    true);
const SerialFlags SerialFlags::CodePromise(
    true,
    true,
    true,
    true,
    true,
    true,
    true);
// The values should be the same as FunFeedback's, however the is different
const SerialFlags SerialFlags::CodeFeedback(
    false,
    true,
    true,
    true,
    false,
    true,
    true);
const SerialFlags SerialFlags::CodePoolUnknown(
    true,
    true,
    true,
    true,
    true,
    false,
    true);
const SerialFlags SerialFlags::CodeNative(
    false,
    true,
    true,
    false,
    true,
    false,
    true);
const SerialFlags SerialFlags::CodeAst(
    true,
    false,
    true,
    true,
    true,
    false,
    true);
const SerialFlags SerialFlags::CodeMisc(
    true,
    true,
    true,
    true,
    true,
    false,
    true);
const SerialFlags SerialFlags::EnvLock(
    false,
    true,
    true,
    true,
    true,
    true,
    false);
const SerialFlags SerialFlags::EnvMisc(
    false,
    true,
    true,
    true,
    true,
    true,
    true);
const SerialFlags SerialFlags::_Unused(
    false,
    false,
    false,
    false,
    false,
    false,
    false);

static std::vector<SerialFlags> ById_{
#define V(name) SerialFlags::name,
    LIST_OF_SERIAL_FLAGS(V)
#undef V
    SerialFlags::_Unused};

const std::vector<SerialFlags>& SerialFlags::ById = ById_;

const SerialFlags& SerialFlags::parse(const std::string& name) {
#define V(name_)                                                               \
    if (name == #name_)                                                        \
        return SerialFlags::name_;
    LIST_OF_SERIAL_FLAGS(V)
#undef V
    std::cerr << "unknown serial flag: " << name << "\n";
    assert(false && "unknown serial flag, can't parse");
}

std::ostream& operator<<(std::ostream& out, const SerialFlags& f) {
#define V(name)                                                                \
        if (SerialFlags::name.id_ == f.id_) {                                  \
            out << #name;                                                      \
            return out;                                                        \
        }
    LIST_OF_SERIAL_FLAGS(V)
#undef V
    assert(false && "Serial flag is not one of the defined globals, corrupt?");
}

void AbstractSerializer::writeConst(unsigned idx, const SerialFlags& flags) {
    write(Pool::get(idx), flags);
}

void AbstractSerializer::writeSrc(unsigned idx, const SerialFlags& flags) {
    write(src_pool_at(idx), flags);
}

unsigned AbstractDeserializer::readConst(const SerialFlags& flags) {
    return Pool::insert(read(flags));
}

unsigned AbstractDeserializer::readSrc(const SerialFlags& flags) {
    return src_pool_add(read(flags));
}

/// These SEXPs are added to the ref table the first time they are serialized or
/// deserialized, and serialized as / deserialized from refs subsequent times.
static bool canSelfReference(SEXP sexp) {
    switch (TYPEOF(sexp)) {
    case SYMSXP:
    case ENVSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case BCODESXP:
        return true;
    case EXTERNALSXP:
        return !TypeFeedback::check(sexp) && !ArglistOrder::check(sexp) && !PoolStub::check(sexp);
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

/// Code from R
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
        serializer.writeBytesOf(b->info.magic, SerialFlags::RirMagic);
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
        !tryWrite<PirTypeFeedback>(serializer, s) &&
        !tryWrite<TypeFeedback>(serializer, s) &&
        !tryWrite<PoolStub>(serializer, s)) {
        std::cerr << "couldn't serialize EXTERNALSXP: ";
        Rf_PrintValue(s);
        assert(false);
    }
}

static SEXP readRir(AbstractDeserializer& deserializer) {
    return Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: readRir", [&]{
        auto magic = deserializer.readBytesOf<unsigned>(SerialFlags::RirMagic);
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
        case TYPEFEEDBACK_MAGIC:
            return TypeFeedback::deserialize(deserializer)->container();
        case POOL_STUB_MAGIC:
            return PoolStub::deserialize(deserializer)->container();
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
        serializer.write(code, SerialFlags::RBytecodeCode);
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
        auto bytes = deserializer.read(SerialFlags::RBytecodeCode);
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

static void writeString(AbstractSerializer& serializer, SEXP sexp,
                        const SerialFlags& flags) {
    assert(TYPEOF(sexp) == CHARSXP);
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline char vector", sexp, [&]{
        if (sexp == NA_STRING) {
            serializer.writeBytesOf<R_len_t>(-1, SerialFlags::StringLength);
        } else {
            auto n = LENGTH(sexp);
            serializer.writeBytesOf<R_len_t>(n, SerialFlags::StringLength);
            serializer.writeBytes(CHAR(sexp), n * sizeof(char), flags);
        }
    });
}

static SEXP readString(AbstractDeserializer& deserializer,
                       const SerialFlags& flags) {
    return Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline char vector", [&]{
        auto length = deserializer.readBytesOf<R_len_t>(SerialFlags::StringLength);
        if (length == -1) {
            return NA_STRING;
        } else if (length < 8192) {
            // Store data on stack
            // R doesn't allow allocVector(SEXP) because it interns
            // strings
            char data[8192];
            deserializer.readBytes(data, length, flags);
            data[length] = '\0';
            return Rf_mkCharLenCE(data, length, CE_NATIVE);
        } else {
            // Too large, store data on heap
            // R doesn't allow allocVector(CHARSXP) because it interns
            // strings
            char* data = (char*)malloc(length + 1);
            deserializer.readBytes(data, length, flags);
            data[length] = '\0';
            auto result = Rf_mkCharLenCE(data, length, CE_NATIVE);
            free(data);
            return result;
        }
    });
}

void AbstractSerializer::writeInline(SEXP sexp) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline", sexp, [&]{
        auto refs = this->refs();

        SEXPTYPE type;
        if (ALTREP(sexp) && ALTREP_SERIALIZED_CLASS(sexp) && ALTREP_SERIALIZED_STATE(sexp)) {
            type = (SEXPTYPE)SpecialType::Altrep;
        } else if (global2Index.count(sexp)) {
            type = (SEXPTYPE)SpecialType::Global;
        } else if (canSelfReference(sexp) && refs && refs->count(sexp)) {
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
        writeBytesOf(rFlags, SerialFlags::RFlags);

        // Write attrs and tag at the beginning if we (maybe) tail call, at the
        // end if we self-reference, and otherwise at the end (otherwise doesn't
        // matter as long as we read at the same position)
        auto writeAttr = [&]{
            if (hasAttr) {
                Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline attribute", sexp, [&]{
                    write(ATTRIB(sexp), SerialFlags::RAttrib);
                });
            }
        };
        auto writeTag = [&]{
            if (hasTag_) {
                Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline tag", sexp, [&]{
                    write(TAG(sexp), SerialFlags::RTag);
                });
            }
        };

        if (type == TYPEOF(sexp) && canSelfReference(sexp) && refs &&
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
                write(info, SerialFlags::AltrepInfo);
                write(state, SerialFlags::AltrepState);
                UNPROTECT(2);
                writeAttr();
                // No tag
            });
            break;
        case (SEXPTYPE)SpecialType::Global:
            writeBytesOf(global2Index.at(sexp), SerialFlags::GlobalId);
            // Attr and tag already present
            break;
        case (SEXPTYPE)SpecialType::Ref:
            // If you get an out-of-range here, a RIR object is probably either
            // not adding its ref, or the rir object should be excluded from
            // `canSelfReference` (and probably also `UUIDPool::internable`)
            writeBytesOf((unsigned)refs->at(sexp), SerialFlags::RefId);
            // Attr and tag already present
            break;
        case NILSXP:
            // No attr or tag
            break;
        case SYMSXP: {
            auto name = PRINTNAME(sexp);
            assert(LENGTH(name) > 0 &&
                   "Empty symbol name, sexp should be a global");
            writeString(*this, name, SerialFlags::SymbolName);
            writeAttr();
            // No tag
            break;
        }
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
                write(CAR(sexp), SerialFlags::Car);
            });
            writeInline(CDR(sexp));
            break;
        case CLOSXP:
            writeAttr();
            writeTag();
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline closure sans body", sexp, [&]{
                write(CLOENV(sexp), SerialFlags::ClosureEnv);
                write(FORMALS(sexp), SerialFlags::ClosureFormals);
            });
            writeInline(BODY(sexp));
            break;
        case EXTPTRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline external pointer", sexp, [&]{
                write(EXTPTR_PROT(sexp), SerialFlags::ExternalPtrProtection);
                write(EXTPTR_TAG(sexp), SerialFlags::ExternalPtrTag);
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
            if (R_IsPackageEnv(sexp)) {
                writeBytesOf(EnvType::Package, SerialFlags::EnvType);
                writeInline(PROTECT(R_PackageEnvName(sexp)));
                UNPROTECT(1);
            } else if (R_IsNamespaceEnv(sexp)) {
                writeBytesOf(EnvType::Namespace, SerialFlags::EnvType);
                writeInline(PROTECT(R_NamespaceEnvSpec(sexp)));
                UNPROTECT(1);
            } else {
                writeBytesOf(EnvType::Regular, SerialFlags::EnvType);
                writeBytesOf((bool)R_EnvironmentIsLocked(sexp), SerialFlags::EnvLock);
                write(ENCLOS(sexp), SerialFlags::EnvMisc);
                write(FRAME(sexp), SerialFlags::EnvMisc);
                write(HASHTAB(sexp), SerialFlags::EnvMisc);
            }
            writeAttr();
            // No tag
            break;
        case SPECIALSXP:
        case BUILTINSXP:
            writeBytesOf(getBuiltinNr(sexp), SerialFlags::BuiltinNr);
            writeAttr();
            // No tag
            break;
        case CHARSXP:
            writeString(*this, sexp, SerialFlags::String);
            writeAttr();
            // No tag
            break;
        case LGLSXP:
        case INTSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline int vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n, SerialFlags::VectorLength);
                writeBytes(INTEGER(sexp), n * sizeof(int), SerialFlags::VectorElt);
            });
            writeAttr();
            // No tag
            break;
        case REALSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline real vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n, SerialFlags::VectorLength);
                writeBytes(REAL(sexp), n * sizeof(double), SerialFlags::VectorElt);
            });
            writeAttr();
            // No tag
            break;
        case CPLXSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline complex number vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n, SerialFlags::VectorLength);
                writeBytes(COMPLEX(sexp), n * sizeof(Rcomplex), SerialFlags::VectorElt);
            });
            writeAttr();
            // No tag
            break;
        case RAWSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline byte vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n, SerialFlags::VectorLength);
                writeBytes(RAW(sexp), n * sizeof(Rbyte), SerialFlags::VectorElt);
            });
            writeAttr();
            // No tag
            break;
        case STRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline string vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n, SerialFlags::VectorLength);
                for (int i = 0; i < n; i++) {
                    write(STRING_ELT(sexp, i), SerialFlags::VectorElt);
                }
            });
            writeAttr();
            // No tag
            break;
        case VECSXP:
        case EXPRSXP:
            Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractSerializer::writeInline expression or vector", sexp, [&]{
                auto n = XLENGTH(sexp);
                writeBytesOf<R_xlen_t>(n, SerialFlags::VectorLength);
                for (int i = 0; i < n; i++) {
                    write(VECTOR_ELT(sexp, i), SerialFlags::VectorElt);
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

        auto rFlags = readBytesOf<unsigned>(SerialFlags::RFlags);
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
                    return read(SerialFlags::RAttrib);
                });
                PROTECT(attrib);
            }
        };
        auto readTag = [&]{
            if (hasTag_) {
                tag = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline tag", [&]{
                    return read(SerialFlags::RTag);
                });
                PROTECT(tag);
            }
        };

        SEXP result;
        switch (type) {
        case (SEXPTYPE)SpecialType::Altrep: {
            auto info = PROTECT(read(SerialFlags::AltrepInfo));
            auto state = PROTECT(read(SerialFlags::AltrepState));
            readAttr();
            // No tag
            result = ALTREP_UNSERIALIZE_EX(info, state, attrib, object, levels);
            UNPROTECT(2);
            break;
        }
        case (SEXPTYPE)SpecialType::Global:
            result = globals[readBytesOf<unsigned>(SerialFlags::GlobalId)];
            // Attr and tag already present
            break;
        case (SEXPTYPE)SpecialType::Ref:
            result = refs->at(readBytesOf<unsigned>(SerialFlags::RefId));
            // Attr and tag already present
            break;
        case NILSXP:
            result = R_NilValue;
            // No attr or tag
            break;
        case SYMSXP: {
            auto name = readString(*this, SerialFlags::SymbolName);
            result = Rf_installTrChar(name);
            // Symbols have read refs (same symbol can be serialized and
            // we want it to point to the same SEXP when deserializing)
            if (refs) {
                refs->push_back(result);
            }
            readAttr();
            // No tag
            break;
        }
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
                SETCAR(result, read(SerialFlags::Car));
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
                    SET_CLOENV(result, read(SerialFlags::ClosureEnv));
                    SET_FORMALS(result, read(SerialFlags::ClosureFormals));
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
                R_SetExternalPtrProtected(result, read(SerialFlags::ExternalPtrProtection));
                R_SetExternalPtrTag(result, read(SerialFlags::ExternalPtrTag));
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
            switch (readBytesOf<EnvType>(SerialFlags::EnvType)) {
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
                auto isLocked = readBytesOf<bool>(SerialFlags::EnvLock);
                result = Rf_allocSExp(type);
                PROTECT(result);
                if (refs) {
                    refs->push_back(result);
                }

                if (willRead(SerialFlags::EnvMisc)) {
                    SET_ENCLOS(result, read(SerialFlags::EnvMisc));
                    SET_FRAME(result, read(SerialFlags::EnvMisc));
                    SET_HASHTAB(result, read(SerialFlags::EnvMisc));
                } else {
                    SET_ENCLOS(result, R_NilValue);
                    SET_FRAME(result, R_NilValue);
                    SET_HASHTAB(result, R_NilValue);
                }

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
            result = getBuiltinOrSpecialFun(readBytesOf<int>(SerialFlags::BuiltinNr));
            readAttr();
            // No tag
            break;
        case CHARSXP:
            result = readString(*this, SerialFlags::String);
            readAttr();
            // No tag
            break;
        case LGLSXP:
        case INTSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline int vector", [&]{
                auto length = readBytesOf<R_xlen_t>(SerialFlags::VectorLength);
                auto sexp = Rf_allocVector(type, length);
                readBytes((void*)INTEGER(sexp), length * sizeof(int), SerialFlags::VectorElt);
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case REALSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline real vector", [&]{
                auto length = readBytesOf<R_xlen_t>(SerialFlags::VectorLength);
                auto sexp = Rf_allocVector(type, length);
                readBytes((void*)REAL(sexp), length * sizeof(double), SerialFlags::VectorElt);
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case CPLXSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline complex number vector sexp", [&]{
                auto length = readBytesOf<R_xlen_t>(SerialFlags::VectorLength);
                auto sexp = Rf_allocVector(type, length);
                readBytes((void*)COMPLEX(sexp), length * sizeof(Rcomplex), SerialFlags::VectorElt);
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case RAWSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline byte vector", [&]{
                auto length = readBytesOf<R_xlen_t>(SerialFlags::VectorLength);
                auto sexp = Rf_allocVector(type, length);
                readBytes((void*)RAW(sexp), length * sizeof(Rbyte), SerialFlags::VectorElt);
                return sexp;
            });
            readAttr();
            // No tag
            break;
        case STRSXP:
            result = Measuring::timeEventIf3(pir::Parameter::PIR_MEASURE_SERIALIZATION, "serializeUni.cpp: AbstractDeserializer::readInline string vector", [&]{
                auto length = readBytesOf<R_xlen_t>(SerialFlags::VectorLength);
                auto sexp = Rf_allocVector(type, length);
                PROTECT(sexp);
                for (int i = 0; i < length; i++) {
                    SET_STRING_ELT(sexp, i, read(SerialFlags::VectorElt));
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
                auto length = readBytesOf<R_xlen_t>(SerialFlags::VectorLength);
                auto sexp = Rf_allocVector(type, length);
                PROTECT(sexp);
                for (int i = 0; i < length; i++) {
                    SET_VECTOR_ELT(sexp, i, read(SerialFlags::VectorElt));
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
             type == (SEXPTYPE)SpecialType::Ref || !canSelfReference(result) ||
             !refs ||
             std::find(refs->begin(), refs->end(), result) != refs->end()) &&
            "sanity check failed: type can self reference but wasn't inserted "
            "into ref table"
        );

        return result;
    });
}

} // namespace rir
