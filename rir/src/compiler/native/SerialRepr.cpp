//
// Created by Jakob Hain on 6/24/23.
//

#include "SerialRepr.h"
#include "R/Funtab.h"
#include "compiler/native/lower_function_llvm.h"
#include "compiler/native/types_llvm.h"
#include "hash/UUIDPool.h"
#include "utils/ByteBuffer.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>

namespace rir {
namespace pir {

// Some of these would serialize fine regardless, thanks to
// serialize.c:SaveSpecialHook
static std::unordered_map<std::string, SEXP> globals = {
    {"R_GlobalEnv", R_GlobalEnv},
    {"R_BaseEnv", R_BaseEnv},
    {"R_BaseNamespace", R_BaseNamespace},
    {"R_TrueValue", R_TrueValue},
    {"R_NilValue", R_NilValue},
    {"R_FalseValue", R_FalseValue},
    {"R_UnboundValue", R_UnboundValue},
    {"R_MissingArg", R_MissingArg},
    {"R_RestartToken", R_RestartToken},
    {"R_LogicalNAValue", R_LogicalNAValue},
    {"R_EmptyEnv", R_EmptyEnv},
    {"R_DimSymbol", R_DimSymbol},
    {"R_DotsSymbol", R_DotsSymbol},
    {"R_NamesSymbol", R_NamesSymbol},
};

static std::unordered_map<SEXP, std::string> globalsRev = []{
    std::unordered_map<SEXP, std::string> res;
    for (auto& e : globals) {
        res[e.second] = e.first;
    }
    return res;
}();

llvm::MDNode* SerialRepr::SEXP::metadata(llvm::LLVMContext& ctx) const {
    if (globalsRev.count(what)) {
        return llvm::MDTuple::get(
            ctx,
            {llvm::MDString::get(ctx, "Global"),
             llvm::MDString::get(ctx, globalsRev.at(what))});
    } else if (TYPEOF(what) == BUILTINSXP || TYPEOF(what) == SPECIALSXP) {
        return llvm::MDTuple::get(
            ctx,
            {llvm::MDString::get(ctx, "Builtin"),
             llvm::MDString::get(ctx, getBuiltinName(what))});
    }
    ByteBuffer buf;
    UUIDPool::intern(what, true, false);
    UUIDPool::writeItem(what, buf, true);
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "SEXP"),
         llvm::MDString::get(
             ctx,
             llvm::StringRef((const char*)buf.data(), buf.size()))});
}

llvm::MDNode* SerialRepr::String::metadata(llvm::LLVMContext& ctx) const {
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "String"),
         llvm::MDString::get(ctx, str)});
}

llvm::MDNode* SerialRepr::Code::metadata(llvm::LLVMContext& ctx) const {
    ByteBuffer buf;
    auto sexp = code->container();
    UUIDPool::intern(sexp, true, false);
    UUIDPool::writeItem(sexp, buf, true);
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "Code"),
         llvm::MDString::get(
             ctx,
             llvm::StringRef((const char*)buf.data(), buf.size()))});
}

llvm::MDNode* SerialRepr::DeoptMetadata::metadata(llvm::LLVMContext& ctx) const {
    ByteBuffer buf;
    m->internRecursive();
    m->serialize(buf);
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "DeoptMetadata"),
         llvm::MDString::get(
             ctx,
             llvm::StringRef((const char*)buf.data(), buf.size()))});
}

llvm::MDNode* SerialRepr::OpaqueTrue::metadata(llvm::LLVMContext& ctx) const {
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "OpaqueTrue")});
}

llvm::MDNode* SerialRepr::R_Visible::metadata(llvm::LLVMContext& ctx) const {
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "R_Visible")});
}

llvm::MDNode* SerialRepr::R_BCNodeStackTop::metadata(llvm::LLVMContext& ctx) const {
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "R_BCNodeStackTop")});
}

llvm::MDNode* SerialRepr::R_GlobalContext::metadata(llvm::LLVMContext& ctx) const {
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "R_GlobalContext")});
}

llvm::MDNode* SerialRepr::R_ReturnedValue::metadata(llvm::LLVMContext& ctx) const {
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "R_ReturnedValue")});
}

llvm::MDNode* SerialRepr::functionMetadata(llvm::LLVMContext& ctx,
                                           const char* llvmValueName,
                                           int builtinId) {
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, llvmValueName),
         llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(
             llvm::Type::getInt32Ty(ctx), builtinId))});
}

llvm::MDNode* SerialRepr::srcIdxMetadata(llvm::LLVMContext& ctx, Immediate i) {
    // Source pool should never have global SEXPs, except R_NilValue which is
    // trivial to serialize (specifically, we care about having no global envs)
    auto what = src_pool_at(i);
    ByteBuffer buf;
    UUIDPool::intern(what, true, false);
    UUIDPool::writeItem(what, buf, true);
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(
            ctx,
            llvm::StringRef((const char*)buf.data(), buf.size()))});
}

llvm::MDNode* SerialRepr::poolIdxMetadata(llvm::LLVMContext& ctx, BC::PoolIdx i) {
    // We assume the constant pool as used here has no global environments or
    // other tricky exprs, if it does we need to abstract SEXP::metadata...
    auto what = Pool::get(i);
    ByteBuffer buf;
    UUIDPool::intern(what, true, false);
    UUIDPool::writeItem(what, buf, true);
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(
            ctx,
            llvm::StringRef((const char*)buf.data(), buf.size()))});
}

llvm::MDNode* SerialRepr::namesMetadata(llvm::LLVMContext& ctx,
                                        const std::vector<BC::PoolIdx>& names) {
    std::vector<llvm::Metadata*> args;
    args.reserve(names.size());
    for (auto i : names) {
        auto sexp = Pool::get(i);
        switch (TYPEOF(sexp)) {
        case SYMSXP:
            args.push_back(llvm::MDString::get(ctx, CHAR(PRINTNAME(sexp))));
            break;
        case LISTSXP:
            if (TYPEOF(CAR(sexp)) != SYMSXP || CDR(sexp) != R_NilValue) {
                std::cerr << "List name is expected to be CONS(actual_name, R_NilValue)\n";
                Rf_PrintValue(sexp);
                assert(false);
            }
            args.push_back(llvm::MDTuple::get(ctx, {llvm::MDString::get(ctx, CHAR(PRINTNAME(CAR(sexp))))}));
            break;
        case NILSXP:
            args.push_back(llvm::MDTuple::get(ctx, {}));
            break;
        // TODO: Do we need INTSXP?
        case INTSXP:
            args.push_back(llvm::ConstantAsMetadata::get(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx),
                                       INTEGER(sexp)[0])));
            break;
        default:
            std::cerr << "Unhandled name type: " << TYPEOF(sexp) << "\n";
            Rf_PrintValue(sexp);
            assert(false);
        }
    }
    return llvm::MDTuple::get(ctx, args);
}

static void* getMetadataPtr_Global(const llvm::MDNode& meta,
                                   __attribute__((unused)) rir::Code* outer) {
    auto name = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    return (void*)globals.at(name.str());
}

static void* getMetadataPtr_Builtin(const llvm::MDNode& meta,
                                    __attribute__((unused)) rir::Code* outer) {
    auto name = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    return (void*)getBuiltinFun(name.str().c_str());
}

static void* getMetadataPtr_SEXP(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    if (outer) {
        outer->addExtraPoolEntry(sexp);
    }
    return (void*)sexp;
}

static void* getMetadataPtr_String(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    auto dataSexp = Rf_install(data.str().c_str());
    if (outer) {
        outer->addExtraPoolEntry(dataSexp);
    }
    return (void*)CHAR(PRINTNAME(dataSexp));
}

static void* getMetadataPtr_Code(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    if (outer) {
        outer->addExtraPoolEntry(sexp);
    }
    assert(TYPEOF(sexp) == EXTERNALSXP && "deserialized Code SEXP is not actually an EXTERNALSXP");
    return (void*)rir::Code::unpack(sexp);
}

static void* getMetadataPtr_DeoptMetadata(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto m = DeoptMetadata::deserialize(buffer);
    assert(m->numFrames < 65536 && "deserialized obviously corrupt DeoptMetadata");
    if (outer) {
        // TODO remove: testing why DeoptMetadata gets GCd
        R_PreserveObject(m->container());
        for (int i = 0; i < (int)m->numFrames; i++) {
            R_PreserveObject(m->frames[i].code->container());
        }
        m->gcAttach(outer);
    }
    return (void*)m;
}

static void*
getMetadataPtr_OpaqueTrue(__attribute__((unused)) const llvm::MDNode& meta,
                          __attribute__((unused)) rir::Code* outer) {
    return (void*)OpaqueTrue::instance();
}

static void*
getMetadataPtr_R_Visible(__attribute__((unused)) const llvm::MDNode& meta,
                         __attribute__((unused)) rir::Code* outer) {
    return (void*)&R_Visible;
}

static void*
getMetadataPtr_R_BCNodeStackTop(__attribute__((unused)) const llvm::MDNode& meta,
                                __attribute__((unused)) rir::Code* outer) {
    return (void*)&R_BCNodeStackTop;
}

static void*
getMetadataPtr_R_GlobalContext(__attribute__((unused)) const llvm::MDNode& meta,
                               __attribute__((unused)) rir::Code* outer) {
    return (void*)&R_GlobalContext;
}

static void*
getMetadataPtr_R_ReturnedValue(__attribute__((unused)) const llvm::MDNode& meta,
                               __attribute__((unused)) rir::Code* outer) {
    return (void*)&R_ReturnedValue;
}

typedef void* (*GetMetadataPtr)(const llvm::MDNode& meta, rir::Code* outer);
static std::unordered_map<std::string, GetMetadataPtr> getMetadataPtr{
    {"Global", getMetadataPtr_Global},
    {"Builtin", getMetadataPtr_Builtin},
    {"SEXP", getMetadataPtr_SEXP},
    {"String", getMetadataPtr_String},
    {"Code", getMetadataPtr_Code},
    {"DeoptMetadata", getMetadataPtr_DeoptMetadata},
    {"OpaqueTrue", getMetadataPtr_OpaqueTrue},
    {"R_Visible", getMetadataPtr_R_Visible},
    {"R_BCNodeStackTop", getMetadataPtr_R_BCNodeStackTop},
    {"R_GlobalContext", getMetadataPtr_R_GlobalContext},
    {"R_ReturnedValue", getMetadataPtr_R_ReturnedValue}
};

static llvm::Value* patchPointerMetadata(llvm::Module& mod,
                                         llvm::GlobalVariable& inst,
                                         llvm::MDNode* ptrMeta, rir::Code* outer) {
    auto type = ((llvm::MDString&)*ptrMeta->getOperand(0)).getString();
    auto llvmType = inst.getValueType();
    auto isConstant = inst.isConstant();
    auto ptr = getMetadataPtr[type.str()](*ptrMeta, outer);
    return LowerFunctionLLVM::convertToPointer(mod, ptr, llvmType, isConstant, ptrMeta);
}

static llvm::Value* patchSrcIdxMetadata(llvm::Module& mod,
                                        llvm::MDNode* srcIdxMeta) {
    auto data = ((llvm::MDString*)srcIdxMeta->getOperand(0).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    // TODO: Reuse index if it's already in the source pool
    //  (and maybe merge and refactor pools)
    auto i = src_pool_add(sexp);
    return LowerFunctionLLVM::llvmSrcIdx(mod, i);
}

static llvm::Value* patchPoolIdxMetadata(llvm::Module& mod,
                                         llvm::MDNode* poolIdxMeta) {
    auto data = ((llvm::MDString*)poolIdxMeta->getOperand(0).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    // TODO: Reuse index if it's already in the constant pool
    //  (and maybe merge and refactor pools)
    auto i = Pool::insert(sexp);
    return LowerFunctionLLVM::llvmPoolIdx(mod, i);
}

static llvm::Value* patchNamesMetadata(llvm::Module& mod,
                                       llvm::MDNode* namesMeta) {
    std::vector<BC::PoolIdx> names;
    for (auto& nameOperand : namesMeta->operands()) {
        auto nameNode = nameOperand.get();
        auto nameTuple = llvm::dyn_cast_or_null<llvm::MDTuple>(nameNode);
        auto nameStr =
            llvm::dyn_cast_or_null<llvm::MDString>(nameNode);
        auto nameInt =
            llvm::dyn_cast_or_null<llvm::ValueAsMetadata>(nameNode);
        if (nameTuple) {
            switch (nameTuple->getNumOperands()) {
            case 0: {
                // We should probably ensure that we only have one R_NilValue in
                // the pool...
                names.push_back(Pool::insert(R_NilValue));
                break;
            }
            case 1: {
                // This is a "cons name" AKA CONS_NR(actualName, R_NilValue). These are used to distinguish missing values.
                nameNode = nameTuple->getOperand(0).get();
                nameStr = llvm::dyn_cast<llvm::MDString>(nameNode);
                auto sexp = CONS_NR(
                    Rf_install(nameStr->getString().str().c_str()), R_NilValue);
                // Presumably Rf_install interns, but we inserting a lot of redundant names in the pool. Does it make sense to have a hashmap of inserted SEXPs?
                names.push_back(Pool::insert(sexp));
                break;
            }
            default:
                assert(false && "Unexpected name operand tuple size");
            }
        } else if (nameStr) {
            auto sexp = Rf_install(nameStr->getString().str().c_str());
            // Presumably Rf_install interns, but we inserting a lot of redundant
            // names in the pool. Does it make sense to have a hashmap of inserted
            // SEXPs?
            names.push_back(Pool::insert(sexp));
        } else if (nameInt) {
            auto value = (int)((llvm::ConstantInt*)nameInt->getValue())->getZExtValue();
            // Pool::getInt does intern
            names.push_back(Pool::getInt(value));
        } else {
            assert(false && "Unexpected name operand type");
        }
    }

    return LowerFunctionLLVM::llvmNames(mod, names);
}

static void patchGlobalMetadatas(llvm::Module& mod, rir::Code* outer) {
    // Need to store globals first, because otherwise we'll replace already-
    // added values and cause an infinite loop. We also defer replacements
    // although that probably isn't necessary
    std::vector<llvm::GlobalVariable*> oldGlobals;
    for (auto& global : mod.globals()) {
        oldGlobals.push_back(&global);
    }
    std::vector<std::pair<llvm::GlobalVariable*, llvm::Value*>> replacements;
    for (auto& global : oldGlobals) {
        auto ptrMeta = global->getMetadata(SerialRepr::POINTER_METADATA_NAME);
        auto srcIdxMeta = global->getMetadata(SerialRepr::SRC_IDX_METADATA_NAME);
        auto poolIdxMeta = global->getMetadata(SerialRepr::POOL_IDX_METADATA_NAME);
        auto namesMeta = global->getMetadata(SerialRepr::NAMES_METADATA_NAME);

        llvm::Value* replacement = nullptr;
        if (ptrMeta) {
            replacement = patchPointerMetadata(mod, *global, ptrMeta, outer);
        }
        if (srcIdxMeta) {
            assert(!replacement);
            replacement = patchSrcIdxMetadata(mod, srcIdxMeta);
        }
        if (poolIdxMeta) {
            assert(!replacement);
            replacement = patchPoolIdxMetadata(mod, poolIdxMeta);
        }
        if (namesMeta) {
            assert(!replacement);
            replacement = patchNamesMetadata(mod, namesMeta);
        }

        if (replacement) {
            replacements.emplace_back(global, replacement);
        }
    }
    for (auto& replacement : replacements) {
        replacement.first->replaceAllUsesWith(replacement.second);
    }
}

static void patchFunctionMetadata(llvm::Module& mod,
                                  const llvm::MDNode* operand) {
    auto& meta = *(const llvm::MDTuple*)operand;
    auto llvmValueName = ((llvm::MDString*)meta.getOperand(0).get())->getString();
    auto builtinId = (int)((llvm::ConstantInt*)((llvm::ConstantAsMetadata*)meta.getOperand(1).get())->getValue())->getZExtValue();
    auto llvmValue = mod.getNamedValue(llvmValueName);

    auto builtin = getBuiltin(getBuiltinFun(builtinId));
    auto replacement = LowerFunctionLLVM::convertToFunction(
        mod, (void*)builtin, t::builtinFunction, builtinId);

    // I don't know why the types are different, but they shouldn't be
    // (every builtin has the same type, but the same types in the old module
    //  are different from those of the new one. Maybe that will be an issue
    //  later on...)
    replacement.getCallee()->mutateType(llvmValue->getType());
    llvmValue->replaceAllUsesWith(replacement.getCallee());
}

static void patchFunctionMetadatas(llvm::Module& mod) {
    auto meta = mod.getNamedMetadata(pir::SerialRepr::FUNCTION_METADATA_NAME);
    if (!meta) {
        return;
    }
    for (auto operand : meta->operands()) {
        patchFunctionMetadata(mod, operand);
    }
}

void SerialRepr::patch(llvm::Module& mod, rir::Code* outer) {
    patchGlobalMetadatas(mod, outer);
    patchFunctionMetadatas(mod);
}

} // namespace pir
} // namespace rir