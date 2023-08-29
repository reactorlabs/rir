//
// Created by Jakob Hain on 6/24/23.
//

#include "SerialRepr.h"
#include "R/Funtab.h"
#include "compiler/native/lower_function_llvm.h"
#include "compiler/native/types_llvm.h"
#include "serializeHash/globals.h"
#include "serializeHash/hash/UUIDPool.h"
#include "utils/ByteBuffer.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>

namespace rir {
namespace pir {


llvm::MDNode* SerialRepr::SEXP::metadata(llvm::LLVMContext& ctx) const {
    // Some of these would serialize fine regardless, thanks to
    // serialize.c:SaveSpecialHook
    // Also, hashing handles all globals and builtins already, and serialization
    // will once we migrate from R's serializer to RIR's
    if (global2CppId.count(what)) {
        return llvm::MDTuple::get(
            ctx,
            {llvm::MDString::get(ctx, "Global"),
             llvm::MDString::get(ctx, global2CppId.at(what))});
    } else if (TYPEOF(what) == BUILTINSXP || TYPEOF(what) == SPECIALSXP) {
        return llvm::MDTuple::get(
            ctx,
            {llvm::MDString::get(ctx, "Builtin"),
             llvm::MDString::get(ctx, getBuiltinName(what))});
    }
    ByteBuffer buf;
    UUIDPool::intern(what, true, false);
    UUIDPool::writeItem(what, false, buf, true);
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

llvm::MDNode* SerialRepr::Function::metadata(llvm::LLVMContext& ctx) const {
    ByteBuffer buf;
    auto sexp = function->container();
    UUIDPool::intern(sexp, true, false);
    UUIDPool::writeItem(sexp, false, buf, true);
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "Function"),
         llvm::MDString::get(
             ctx,
             llvm::StringRef((const char*)buf.data(), buf.size()))});
}

llvm::MDNode* SerialRepr::TypeFeedback::metadata(llvm::LLVMContext& ctx) const {
    ByteBuffer buf;
    auto sexp = typeFeedback->container();
    UUIDPool::intern(sexp, true, false);
    UUIDPool::writeItem(sexp, false, buf, true);
    return llvm::MDTuple::get(
        ctx,
        {llvm::MDString::get(ctx, "TypeFeedback"),
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
    UUIDPool::writeItem(what, false, buf, true);
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
    UUIDPool::writeItem(what, false, buf, true);
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
        if (global2CppId.count(sexp)) {
            args.push_back(
                llvm::MDTuple::get(
                    ctx,
                    {llvm::MDString::get(ctx, "Global"),
                     llvm::MDString::get(ctx, global2CppId.at(sexp))}));
        } else {
            ByteBuffer buf;
            UUIDPool::intern(sexp, true, false);
            UUIDPool::writeItem(sexp, false, buf, true);
            args.push_back(
                llvm::MDTuple::get(
                    ctx,
                    {llvm::MDString::get(ctx, "SEXP"),
                        llvm::MDString::get(ctx,
                            llvm::StringRef((const char*)buf.data(), buf.size()))}));
        }
    }
    return llvm::MDTuple::get(ctx, args);
}

static void* getMetadataPtr_Global(const llvm::MDNode& meta,
                                   __attribute__((unused)) rir::Code* outer) {
    auto name = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    return (void*)cppId2Global.at(name.str());
}

static void* getMetadataPtr_Builtin(const llvm::MDNode& meta,
                                    __attribute__((unused)) rir::Code* outer) {
    auto name = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    return (void*)getBuiltinOrSpecialFun(name.str().c_str());
}

static void* getMetadataPtr_SEXP(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    if (outer) {
        // TODO: why is gcAttach not enough?
        R_PreserveObject(sexp);
        outer->addExtraPoolEntry(sexp);
    }
    return (void*)sexp;
}

static void* getMetadataPtr_String(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    auto dataSexp = Rf_install(data.str().c_str());
    if (outer) {
        // TODO: why is gcAttach not enough?
        R_PreserveObject(dataSexp);
        outer->addExtraPoolEntry(dataSexp);
    }
    return (void*)CHAR(PRINTNAME(dataSexp));
}

static void* getMetadataPtr_Function(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    if (outer) {
        // TODO: why is gcAttach not enough?
        R_PreserveObject(sexp);
        outer->addExtraPoolEntry(sexp);
    }
    assert(TYPEOF(sexp) == EXTERNALSXP &&
           "deserialized Function SEXP is not actually an EXTERNALSXP");
    assert(rir::Function::check(sexp) &&
           "deserialized Function SEXP is not actually a Function");
    return (void*)rir::Function::unpack(sexp);
}

static void* getMetadataPtr_TypeFeedback(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);
    if (outer) {
        // TODO: why is gcAttach not enough?
        R_PreserveObject(sexp);
        outer->addExtraPoolEntry(sexp);
    }
    assert(TYPEOF(sexp) == EXTERNALSXP &&
           "deserialized TypeFeedback SEXP is not actually an EXTERNALSXP");
    assert(rir::TypeFeedback::check(sexp) &&
           "deserialized TypeFeedback SEXP is not actually a TypeFeedback");
    return (void*)rir::TypeFeedback::unpack(sexp);
}

static void* getMetadataPtr_DeoptMetadata(const llvm::MDNode& meta, rir::Code* outer) {
    auto data = ((llvm::MDString*)meta.getOperand(1).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto m = DeoptMetadata::deserialize(buffer);
    assert(m->numFrames < 65536 &&
           "deserialized obviously corrupt DeoptMetadata");
    if (outer) {
        // TODO: why is gcAttach not enough?
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
    {"Function", getMetadataPtr_Function},
    {"TypeFeedback", getMetadataPtr_TypeFeedback},
    {"DeoptMetadata", getMetadataPtr_DeoptMetadata},
    {"OpaqueTrue", getMetadataPtr_OpaqueTrue},
    {"R_Visible", getMetadataPtr_R_Visible},
    {"R_BCNodeStackTop", getMetadataPtr_R_BCNodeStackTop},
    {"R_GlobalContext", getMetadataPtr_R_GlobalContext},
    {"R_ReturnedValue", getMetadataPtr_R_ReturnedValue}
};

static void patchPointerMetadata(llvm::GlobalVariable& inst,
                                 llvm::MDNode* ptrMeta, rir::Code* outer) {
    auto type = ((llvm::MDString&)*ptrMeta->getOperand(0)).getString();
    auto ptr = getMetadataPtr[type.str()](*ptrMeta, outer);

    char name[21];
    sprintf(name, "ept_%lx", (uintptr_t)ptr);
    inst.setName(name);
}

static void patchSrcIdxMetadata(llvm::GlobalVariable& inst,
                                llvm::MDNode* srcIdxMeta) {
    auto data = ((llvm::MDString*)srcIdxMeta->getOperand(0).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);

    // TODO: Reuse index if it's already in the source pool
    //  (and maybe merge and refactor pools)
    char name[13];
    sprintf(name, "src_%08x", (uint32_t)src_pool_add(sexp));
    inst.setName(name);
}

static void patchPoolIdxMetadata(llvm::GlobalVariable& inst,
                                 llvm::MDNode* poolIdxMeta) {
    auto data = ((llvm::MDString*)poolIdxMeta->getOperand(0).get())->getString();
    ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
    auto sexp = UUIDPool::readItem(buffer, true);

    // TODO: Reuse index if it's already in the constant pool
    //  (and maybe merge and refactor pools)
    char name[12];
    sprintf(name, "cp_%08x", (uint32_t)Pool::insert(sexp));
    inst.setName(name);
}

static void patchNamesMetadata(llvm::GlobalVariable& inst,
                               llvm::MDNode* namesMeta) {
    std::stringstream llvmName;
    llvmName << "names";
    if (namesMeta->getNumOperands() == 0) {
        // Special case so that the empty vector name still starts with "names_"
        llvmName << "_";
    } else for (auto& nameOperand : namesMeta->operands()) {
        auto nameMetadata = (llvm::MDTuple*)nameOperand.get();
        auto type = ((llvm::MDString*)(nameMetadata->getOperand(0)).get())->getString();
        auto data = ((llvm::MDString*)(nameMetadata->getOperand(1)).get())->getString();
        SEXP sexp;
        if (type.equals("Global")) {
            assert(cppId2Global.count(data.str()) && "Invalid global");
            sexp = cppId2Global.at(data.str());
        } else if (type.equals("SEXP")) {
            ByteBuffer buffer((uint8_t*)data.data(), (uint32_t)data.size());
            sexp = UUIDPool::readItem(buffer, true);
        } else {
            assert(false && "Invalid name type (not \"Global\" or \"SEXP\")");
        }
        // TODO: Reuse index if it's already in the constant pool
        //  (and maybe merge and refactor pools)
        BC::PoolIdx nextName = Pool::insert(sexp);
        llvmName << "_" << std::hex << nextName;
    }

    inst.setName(llvmName.str());
}

static void patchGlobalMetadatas(llvm::Module& mod, rir::Code* outer) {
    // Need to store globals first, because otherwise we'll replace already-
    // added values and cause an infinite loop. We also defer replacements
    // although that probably isn't necessary
    for (auto& global : mod.globals()) {
        auto ptrMeta = global.getMetadata(SerialRepr::POINTER_METADATA_NAME);
        auto srcIdxMeta = global.getMetadata(SerialRepr::SRC_IDX_METADATA_NAME);
        auto poolIdxMeta = global.getMetadata(SerialRepr::POOL_IDX_METADATA_NAME);
        auto namesMeta = global.getMetadata(SerialRepr::NAMES_METADATA_NAME);

        bool replaced = false;
        if (ptrMeta) {
            patchPointerMetadata(global, ptrMeta, outer);
            replaced = true;
        }
        if (srcIdxMeta) {
            assert(!replaced);
            patchSrcIdxMetadata(global, srcIdxMeta);
            replaced = true;
        }
        if (poolIdxMeta) {
            assert(!replaced);
            patchPoolIdxMetadata(global, poolIdxMeta);
            replaced = true;
        }
        if (namesMeta) {
            assert(!replaced);
            patchNamesMetadata(global, namesMeta);
            // replaced = true;
        }
    }
}

static llvm::MDNode* patchFunctionMetadata(llvm::Module& mod,
                                           const llvm::MDNode* operand) {
    auto& meta = *(const llvm::MDTuple*)operand;
    auto llvmValueName = ((llvm::MDString*)meta.getOperand(0).get())->getString();
    auto llvmValue = mod.getNamedValue(llvmValueName);
    auto builtinId = (int)((llvm::ConstantInt*)((llvm::ConstantAsMetadata*)meta.getOperand(1).get())->getValue())->getZExtValue();
    auto builtin = getBuiltin(getBuiltinFun(builtinId));
    if (!llvmValue) {
        return nullptr;
    }

    char name[21];
    sprintf(name, "efn_%lx", (uintptr_t)builtin);
    llvmValue->setName(name);

    return SerialRepr::functionMetadata(llvmValue->getContext(), name, builtinId);
}

static void patchFunctionMetadatas(llvm::Module& mod) {
    auto meta = mod.getNamedMetadata(pir::SerialRepr::FUNCTION_METADATA_NAME);
    if (!meta) {
        return;
    }
    std::vector<llvm::MDNode*> newOperands;
    for (auto operand : meta->operands()) {
        auto newOperand = patchFunctionMetadata(mod, operand);
        if (newOperand) {
            newOperands.push_back(newOperand);
        }
    }
    meta->clearOperands();
    for (auto newOperand : newOperands) {
        meta->addOperand(newOperand);
    }
}

void SerialRepr::patch(llvm::Module& mod, rir::Code* outer) {
    patchGlobalMetadatas(mod, outer);
    patchFunctionMetadatas(mod);
}

} // namespace pir
} // namespace rir