//
// Created by Jakob Hain on 6/24/23.
//

#include "SerialRepr.h"
#include "R/Funtab.h"
#include "compiler/native/lower_function_llvm.h"
#include "compiler/native/types_llvm.h"
#include "hash/UUIDPool.h"
#include "interpreter/serialize.h"
#include "utils/ByteBuffer.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>

namespace rir {
namespace pir {

llvm::MDNode* SerialRepr::SEXP::metadata(llvm::LLVMContext& ctx) const {
    ByteBuffer buf;
    UUIDPool::intern(what, true, false);
    serialize(what, buf, true);
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
    serialize(sexp, buf, true);
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

static void* getMetadataPtr_SEXP(const llvm::MDNode& meta) {
    auto data = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    return (void*)deserialize(buffer, true);
}

static void* getMetadataPtr_String(const llvm::MDNode& meta) {
    auto data = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    // TODO: May need this to be a const char and then leak, or call c_str and
    //     it somehow doesn't leak or get freed early?
    return (void*)new std::string(data);
}

static void* getMetadataPtr_Code(const llvm::MDNode& meta) {
    auto data = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = deserialize(buffer, true);
    return (void*)rir::Code::unpack(sexp);
}

static void* getMetadataPtr_DeoptMetadata(const llvm::MDNode& meta) {
    auto data = ((const llvm::MDString&)*meta.getOperand(1)).getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    return (void*)DeoptMetadata::deserialize(buffer);
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

static void patchInstructionMetadata(llvm::Module& mod) {
    for (auto& fun : mod.functions()) {
        for (auto& bb : fun) {
            for (auto& inst : bb) {
                auto ptrMeta = inst.getMetadata(SerialRepr::POINTER_METADATA_NAME);
                if (ptrMeta) {
                    patchPointerMetadata(mod, (llvm::GlobalVariable&)inst, ptrMeta);
                }
            }
        }
    }
}

static void patchWithFunctionMetadata1(llvm::Module& mod,
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

static void patchFunctionMetadata(llvm::Module& mod) {
    auto meta = mod.getNamedMetadata(pir::SerialRepr::FUNCTION_METADATA_NAME);
    if (!meta) {
        return;
    }
    for (auto operand : meta->operands()) {
        patchWithFunctionMetadata1(mod, operand);
    }
}

void SerialRepr::patch(llvm::Module& mod) {
    patchInstructionMetadata(mod);
    patchFunctionMetadata(mod);
}

} // namespace pir
} // namespace rir