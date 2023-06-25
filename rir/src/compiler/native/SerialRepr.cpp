//
// Created by Jakob Hain on 6/24/23.
//

#include "SerialRepr.h"
#include "api.h"
#include "utils/ByteBuffer.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Metadata.h>

namespace rir {
namespace pir {

llvm::MDNode* SerialRepr::SEXP::metadata(llvm::LLVMContext& ctx) const {
    ByteBuffer buf;
    serialize(what, buf);
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "DeoptMetadata"),
         llvm::MDString::get(
             ctx,
             llvm::StringRef((const char*)buf.data(), buf.size()))});
}

llvm::MDNode* SerialRepr::String::metadata(llvm::LLVMContext& ctx) const {
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "DeoptMetadata"),
         llvm::MDString::get(ctx, str)});
}

llvm::MDNode* SerialRepr::DeoptMetadata::metadata(llvm::LLVMContext& ctx) const {
    ByteBuffer buf;
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

llvm::MDNode* SerialRepr::functionMetadata(llvm::LLVMContext& ctx,
                                           const char* llvmValueName,
                                           int builtinId) {
    return llvm::MDTuple::get(
            ctx,
            {llvm::MDString::get(ctx, "Function"),
                   llvm::MDString::get(ctx, llvmValueName),
         llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(
             llvm::Type::getInt32Ty(ctx), builtinId))});
}

} // namespace pir
} // namespace rir