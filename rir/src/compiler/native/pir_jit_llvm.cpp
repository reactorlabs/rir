#include "pir_jit_llvm.h"
#include "compiler/native/builtins.h"
#include "compiler/native/lower_function_llvm.h"
#include "compiler/native/pass_schedule_llvm.h"
#include "compiler/native/types_llvm.h"

#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/Mangling.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"

namespace rir {
namespace pir {

size_t PirJitLLVM::nModules = 1;
bool PirJitLLVM::initialized = false;

namespace {

llvm::ExitOnError ExitOnErr;
std::unique_ptr<llvm::orc::LLJIT> JIT;
llvm::orc::ThreadSafeContext TSC;

} // namespace

void PirJitLLVM::finalizeAndFixup() {
    // TODO: maybe later have TSM from the start and use locking
    //       to allow concurrent compilation?
    auto TSM = llvm::orc::ThreadSafeModule(std::move(M), TSC);
    ExitOnErr(JIT->addIRModule(std::move(TSM)));
    for (auto& fix : jitFixup) {
        auto symbol = ExitOnErr(JIT->lookup(fix.second.second));
        void* native = (void*)symbol.getAddress();
        fix.second.first->nativeCode = (NativeCode)native;
    }
}

void PirJitLLVM::compile(
    rir::Code* target, Code* code, const PromMap& promMap,
    const NeedsRefcountAdjustment& refcount,
    const std::unordered_set<Instruction*>& needsLdVarForUpdate,
    LogStream& log) {

    if (!M.get()) {
        M = std::make_unique<llvm::Module>("", *TSC.getContext());
    }

    std::string mangledName = JIT->mangle(makeName(code));

    LowerFunctionLLVM funCompiler(
        mangledName, code, promMap, refcount, needsLdVarForUpdate,
        // getModule
        [&]() -> llvm::Module& { return *M; },
        // getFunction
        [&](Code* c) -> llvm::Function* {
            auto r = funs.find(c);
            if (r != funs.end())
                return r->second;
            return nullptr;
        },
        // getBuiltin
        [&](const rir::pir::NativeBuiltin& b) -> llvm::Function* {
            auto l = builtins.find(b.name);
            if (l != builtins.end()) {
                assert(l->second.second == b.fun);
                return l->second.first;
            }

            assert(b.llvmSignature);
            auto f = llvm::Function::Create(
                b.llvmSignature, llvm::Function::ExternalLinkage, b.name, *M);
            for (auto a : b.attrs)
                f->addFnAttr(a);

            builtins[b.name] = {f, b.fun};
            return f;
        },
        // declare
        [&](Code* c, const std::string& name, llvm::FunctionType* signature) {
            assert(!funs.count(c));
            auto f = llvm::Function::Create(
                signature, llvm::Function::ExternalLinkage, name, *M);
            funs[c] = f;
            return f;
        },
        log);
    funCompiler.compile();
    if (funCompiler.pirTypeFeedback)
        target->pirTypeFeedback(funCompiler.pirTypeFeedback);
    if (funCompiler.hasArgReordering())
        target->arglistOrder(ArglistOrder::New(funCompiler.getArgReordering()));
    llvm::verifyFunction(*funCompiler.fun);
    assert(jitFixup.count(code) == 0);
    // can we use llvm::StringRefs?
    jitFixup.emplace(code,
                     std::make_pair(target, funCompiler.fun->getName().str()));
}

llvm::LLVMContext& PirJitLLVM::getContext() { return *TSC.getContext(); }

void PirJitLLVM::initializeLLVM() {
    if (initialized)
        return;

    // Initialize LLVM
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    ExitOnErr.setBanner("PIR LLVM error: ");

    // Set some TargetMachine options
    auto JTMB = ExitOnErr(llvm::orc::JITTargetMachineBuilder::detectHost());
    // JTMB.setCodeModel(llvm::CodeModel::Small);
    // JTMB.setCodeGenOptLevel(llvm::CodeGenOpt::Default);
    JTMB.getOptions().EnableMachineOutliner = true;
    JTMB.getOptions().EnableFastISel = true;

    // Create an LLJIT instance with custom TargetMachine builder and
    // ObjectLinkingLayer
    assert(!JIT.get());
    JIT = ExitOnErr(
        llvm::orc::LLJITBuilder()
            .setJITTargetMachineBuilder(std::move(JTMB))
            // .setObjectLinkingLayerCreator([&](llvm::orc::ExecutionSession&
            // ES,
            //                                   const llvm::Triple& TT) {
            //     return std::make_unique<llvm::ObjectLinkingLayer>(
            //         ES,
            //         std::make_unique<llvm::jitlink::InProcessMemoryManager>());
            // })
            .create());

    // Create one global ThreadSafeContext
    assert(!TSC.getContext());
    TSC = std::make_unique<llvm::LLVMContext>();

    // Set what passes to run
    JIT->getIRTransformLayer().setTransform(*PassScheduleLLVM::instance());

    // Initialize a JITDylib for builtins - these are implemented in C++ and
    // compiled when building Ř, we need to define symbols for them and
    // initialize these to the static addresses of each builtin; they are in
    // a separate dylib because they are shared by all the modules in the
    // main dylib
    auto& builtins = ExitOnErr(JIT->createJITDylib("builtins"));
    JIT->getMainJITDylib().addToLinkOrder(builtins);

    // Build a map of builtin names to the builtins' addresses and populate the
    // builtins dylib. Add record for "memset" explicitly otherwise it's not
    // found.
    llvm::orc::SymbolMap builtinSymbols(
        static_cast<size_t>(NativeBuiltins::Id::NUM_BUILTINS) + 1);
    builtinSymbols[JIT->mangleAndIntern("memset")] = llvm::JITEvaluatedSymbol(
        llvm::pointerToJITTargetAddress(&memset),
        llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable);
    NativeBuiltins::eachBuiltin([&](const NativeBuiltin& blt) {
        auto res = builtinSymbols.try_emplace(
            JIT->mangleAndIntern(blt.name),
            llvm::JITEvaluatedSymbol(llvm::pointerToJITTargetAddress(blt.fun),
                                     llvm::JITSymbolFlags::Exported |
                                         llvm::JITSymbolFlags::Callable));
        assert(res.second && "duplicate builtin?");
    });
    ExitOnErr(builtins.define(llvm::orc::absoluteSymbols(builtinSymbols)));

    // Initialize types specific to PIR
    initializeTypes(*TSC.getContext());

    initialized = true;
}

} // namespace pir
} // namespace rir
