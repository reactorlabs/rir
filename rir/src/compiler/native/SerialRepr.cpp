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

static std::unordered_map<std::string, SEXP> globals = {
    {"R_GlobalEnv", R_GlobalEnv},
    {"R_BaseEnv", R_BaseEnv},
    {"R_BaseNamespace", R_BaseNamespace},
    {"R_TrueValue", R_TrueValue},
    {"R_NilValue", R_NilValue},
    {"R_FalseValue", R_FalseValue},
    {"R_UnboundValue", R_UnboundValue},
    {"R_MissingArg", R_MissingArg},
    {"R_LogicalNAValue", R_LogicalNAValue},
    {"R_EmptyEnv", R_EmptyEnv},
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

llvm::MDNode* SerialRepr::namesMetadata(llvm::LLVMContext& ctx,
                                        const std::vector<BC::PoolIdx>& names) {
    std::vector<llvm::Metadata*> args;
    args.reserve(names.size());
    for (auto i : names) {
        auto sexp = Pool::get(i);
        assert(TYPEOF(sexp) == SYMSXP);
        auto name = CHAR(PRINTNAME(sexp));
        args.push_back(llvm::MDString::get(ctx, name));
    }
    return llvm::MDTuple::get(ctx, args);
}

static void* getMetadataPtr_Global(const llvm::MDNode& meta) {
    auto name = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    return (void*)globals.at(name.str());
}

static void* getMetadataPtr_Builtin(const llvm::MDNode& meta) {
    auto name = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    return (void*)getBuiltinFun(name.str().c_str());
}

static void* getMetadataPtr_SEXP(const llvm::MDNode& meta) {
    auto data = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    // TODO: Don't permanently preserve SEXP, instead attach it to the Code
    //  object so that it gets freed when the Code object is freed
    R_PreserveObject(sexp);
    return (void*)sexp;
}

static void* getMetadataPtr_String(const llvm::MDNode& meta) {
    auto data = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    // TODO: This will also need to be gc-attached to the Code object
    return (void*)new std::string(data);
}

static void* getMetadataPtr_Code(const llvm::MDNode& meta) {
    auto data = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    // TODO: This will also need to be gc-attached to the Code object
    R_PreserveObject(sexp);
    return (void*)rir::Code::unpack(sexp);
}

static void* getMetadataPtr_DeoptMetadata(const llvm::MDNode& meta) {
    auto data = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto m = DeoptMetadata::deserialize(buffer);
    // TODO: This will also need to be gc-attached to the Code object
    m->preserveSexps();
    return (void*)m;
}

static void* getMetadataPtr_OpaqueTrue(__attribute__((unused)) const llvm::MDNode& meta) {
    return (void*)OpaqueTrue::instance();
}

static void* getMetadataPtr_R_Visible(__attribute__((unused)) const llvm::MDNode& meta) {
    return (void*)&R_Visible;
}

static void* getMetadataPtr_R_BCNodeStackTop(__attribute__((unused)) const llvm::MDNode& meta) {
    return (void*)&R_BCNodeStackTop;
}

static void* getMetadataPtr_R_GlobalContext(__attribute__((unused)) const llvm::MDNode& meta) {
    return (void*)&R_GlobalContext;
}

static void* getMetadataPtr_R_ReturnedValue(__attribute__((unused)) const llvm::MDNode& meta) {
    return (void*)&R_ReturnedValue;
}

typedef void* (*GetMetadataPtr)(const llvm::MDNode& meta);
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

static void patchPointerMetadata(llvm::Module& mod,
                                 llvm::GlobalVariable& inst,
                                 llvm::MDNode* ptrMeta) {
    auto type = ((llvm::MDString&)*ptrMeta->getOperand(0)).getString();
    auto llvmType = inst.getType();
    auto isConstant = inst.isConstant();
    auto ptr = getMetadataPtr[type.str()](*ptrMeta);
    auto replacement = LowerFunctionLLVM::convertToPointer(mod, ptr, llvmType, isConstant, ptrMeta);
    inst.replaceAllUsesWith(replacement);
}

static void patchNamesMetadata(llvm::Module& mod,
                               llvm::GlobalVariable& inst,
                               llvm::MDNode* namesMeta) {
    std::vector<BC::PoolIdx> names;
    for (auto& nameOperand : namesMeta->operands()) {
        auto name = ((const llvm::MDString&)nameOperand).getString();
        auto sexp = Rf_install(name.str().c_str());
        // Presumably Rf_install interns, but we inserting a lot of redundant
        // names in the pool. Does it make sense to have a hashmap of inserted
        // SEXPs?
        names.push_back(Pool::insert(sexp));
    }

    auto replacement = LowerFunctionLLVM::llvmNames(mod, names);
    inst.replaceAllUsesWith(replacement);
}

static void patchGlobalMetadatas(llvm::Module& mod) {
    for (auto& global : mod.globals()) {
        auto ptrMeta = global.getMetadata(SerialRepr::POINTER_METADATA_NAME);
        if (ptrMeta) {
            patchPointerMetadata(mod, global, ptrMeta);
        }
        auto namesMeta = global.getMetadata(SerialRepr::NAMES_METADATA_NAME);
        if (namesMeta) {
            patchNamesMetadata(mod, global, namesMeta);
        }
    }
}

static void patchFunctionMetadata(llvm::Module& mod,
                                      const llvm::MDNode* operand) {
    auto& meta = *(const llvm::MDTuple*)operand;
    auto llvmValueName = ((const llvm::MDString&)*meta.getOperand(0)).getString();
    auto builtinId = (int)((const llvm::ConstantInt&)*meta.getOperand(1)).getZExtValue();
    auto llvmValue = mod.getNamedValue(llvmValueName);

    auto builtin = getBuiltinFun(builtinId);
    auto replacement = LowerFunctionLLVM::convertToFunction(
        mod, builtin, t::builtinFunction, builtinId);

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

void SerialRepr::patch(llvm::Module& mod) {
    patchGlobalMetadatas(mod);
    patchFunctionMetadatas(mod);
}

} // namespace pir
} // namespace rir