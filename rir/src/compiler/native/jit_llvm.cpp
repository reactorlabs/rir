#include "jit_llvm.h"

#include "types_llvm.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/IndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Vectorize.h"

// analysis passes
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Vectorize.h>

#include <llvm/Support/SmallVectorMemoryBuffer.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Bitcode/BitcodeWriterPass.h>

#include "llvm/Object/ArchiveWriter.h"
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/LegacyPassManagers.h>
#include <llvm/Transforms/Utils/Cloning.h>

namespace {

using namespace llvm;
using namespace llvm::orc;

LLVMContext& C = rir::pir::JitLLVM::C;

class JitLLVMImplementation {
  private:
    ExecutionSession ES;
    std::shared_ptr<SymbolResolver> Resolver;
    std::unique_ptr<TargetMachine> TM;
    DataLayout DL;
    LegacyRTDyldObjectLinkingLayer ObjectLayer;
    LegacyIRCompileLayer<decltype(ObjectLayer), SimpleCompiler> CompileLayer;

    using OptimizeFunction = std::function<std::unique_ptr<llvm::Module>(
        std::unique_ptr<llvm::Module>)>;

    LegacyIRTransformLayer<decltype(CompileLayer), OptimizeFunction>
        OptimizeLayer;
    MangleAndInterner Mangle;

    orc::VModuleKey moduleKey;

  public:
    llvm::Module* module = nullptr;
    JitLLVMImplementation()
        : Resolver(createLegacyLookupResolver(
              ES,
              [this](const std::string& Name) {
                  return findMangledSymbol(Name);
              },
              [](Error Err) {
                  cantFail(std::move(Err), "lookupFlags failed");
              })),
          TM(EngineBuilder().selectTarget()), DL(TM->createDataLayout()),
          ObjectLayer(ES,
                      [this](VModuleKey K) {
                          return LegacyRTDyldObjectLinkingLayer::Resources{
                              std::make_shared<SectionMemoryManager>(),
                              Resolver};
                      }),
          CompileLayer(ObjectLayer, SimpleCompiler(*TM)),
          OptimizeLayer(CompileLayer,
                        [this](std::unique_ptr<llvm::Module> M) {
                            return optimizeModule(std::move(M));
                        }),
          Mangle(ES, this->DL) {
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
    }

    void createModule() {
        module = new llvm::Module("", C);
        module->setDataLayout(TM->createDataLayout());
        moduleKey = -1;
    }

    llvm::Function* declareFunction(const std::string& name,
                                    llvm::FunctionType* signature) {
        return Function::Create(signature, Function::ExternalLinkage, name,
                                JitLLVMImplementation::instance().module);
    }

    void* tryCompile(llvm::Function* fun) {
        auto name = fun->getName().str();

        verifyFunction(*fun);
        moduleKey = ES.allocateVModule();
        cantFail(OptimizeLayer.addModule(
            moduleKey, std::unique_ptr<llvm::Module>(module)));
        module = nullptr;
        auto res = findSymbol(name);
        auto adr = res.getAddress();
        // cantFail(OptimizeLayer.removeModule(K));
        if (adr) {
            assert(*adr);
            return (void*)*adr;
        }
        return nullptr;
    }

    static JitLLVMImplementation& instance() {
        static std::unique_ptr<JitLLVMImplementation> singleton;
        if (!singleton) {
            InitializeNativeTarget();
            InitializeNativeTargetAsmPrinter();
            InitializeNativeTargetAsmParser();
            rir::pir::initializeTypes(C);
            singleton.reset(new JitLLVMImplementation());
        }
        return *singleton;
    }

    std::string mangle(const std::string& Name) {
        std::string MangledName;
        {
            raw_string_ostream MangledNameStream(MangledName);
            Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
        }
        return MangledName;
    }

    JITSymbol findSymbol(const std::string& Name) {
        return findMangledSymbol(mangle(Name));
    }

  private:
    JITSymbol findMangledSymbol(const std::string& Name) {
#ifdef _WIN32
        // The symbol lookup of ObjectLinkingLayer uses the
        // SymbolRef::SF_Exported flag to decide whether a symbol will be
        // visible or not, when we call IRCompileLayer::findSymbolIn with
        // ExportedSymbolsOnly set to true.
        //
        // But for Windows COFF objects, this flag is currently never set.
        // For a potential solution see: https://reviews.llvm.org/rL258665
        // For now, we allow non-exported symbols on Windows as a workaround.
        const bool ExportedSymbolsOnly = false;
#else
        const bool ExportedSymbolsOnly = true;
#endif

        // Search modules in reverse order: from last added to first added.
        // This is the opposite of the usual search order for dlsym, but makes
        // more sense in a REPL where we want to bind to the newest available
        // definition.
        if (moduleKey != (unsigned long)-1)
            if (auto Sym = CompileLayer.findSymbolIn(moduleKey, Name,
                                                     ExportedSymbolsOnly))
                return Sym;

        // If we can't find the symbol in the JIT, try looking in the host
        // process.
        if (auto SymAddr = RTDyldMemoryManager::getSymbolAddressInProcess(Name))
            return JITSymbol(SymAddr, JITSymbolFlags::Exported);

#ifdef _WIN32
        // For Windows retry without "_" at beginning, as RTDyldMemoryManager
        // uses GetProcAddress and standard libraries like msvcrt.dll use names
        // with and without "_" (for example "_itoa" but "sin").
        if (Name.length() > 2 && Name[0] == '_')
            if (auto SymAddr = RTDyldMemoryManager::getSymbolAddressInProcess(
                    Name.substr(1)))
                return JITSymbol(SymAddr, JITSymbolFlags::Exported);
#endif

        return nullptr;
    }

    std::unique_ptr<llvm::Module>
    optimizeModule(std::unique_ptr<llvm::Module> M) {
        // Create a function pass manager.
        auto PM = llvm::make_unique<legacy::FunctionPassManager>(M.get());

        PM->add(createPromoteMemoryToRegisterPass());

        PM->add(createScopedNoAliasAAWrapperPass());
        PM->add(createTypeBasedAAWrapperPass());
        PM->add(createBasicAAWrapperPass());

        // list of passes from vmkit
        PM->add(createCFGSimplificationPass()); // Clean up disgusting code
        PM->add(createDeadCodeEliminationPass());
        PM->add(createSROAPass()); // Kill useless allocas

        PM->add(createMemCpyOptPass());

        // Running `memcpyopt` between this and `sroa` seems to give `sroa` a
        // hard time merging the `alloca` for the unboxed data and the `alloca`
        // created by the `alloc_opt` pass.
        PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
        // Now that SROA has cleaned up for front-end mess, a lot of control
        // flow should be more evident - try to clean it up.
        PM->add(createCFGSimplificationPass());    // Merge & remove BBs
        PM->add(createSROAPass());                 // Break up aggregate allocas
        PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
        PM->add(createJumpThreadingPass());        // Thread jumps.
        PM->add(createInstructionCombiningPass()); // Combine silly seq's

        PM->add(createCFGSimplificationPass()); // Merge & remove BBs
        PM->add(createReassociatePass());       // Reassociate expressions

        PM->add(createEarlyCSEPass()); //// ****

        // Load forwarding above can expose allocations that aren't actually
        // used remove those before optimizing loops.
        PM->add(createLoopIdiomPass());  //// ****
        PM->add(createLoopRotatePass()); // Rotate loops.
        // LoopRotate strips metadata from terminator, so run LowerSIMD
        // afterwards
        PM->add(createLICMPass());         // Hoist loop invariants
        PM->add(createLoopUnswitchPass()); // Unswitch loops.
        // Subsequent passes not stripping metadata from terminator
        PM->add(createInstructionCombiningPass());
        PM->add(createIndVarSimplifyPass());     // Canonicalize indvars
        PM->add(createLoopDeletionPass());       // Delete dead loops
        PM->add(createSimpleLoopUnrollPass());   // Unroll small loops
        PM->add(createLoopStrengthReducePass()); // (jwb added)

        // Re-run SROA after loop-unrolling (useful for small loops that
        // operate, over the structure of an aggregate)
        PM->add(createSROAPass()); // Break up aggregate allocas
        PM->add(
            createInstructionCombiningPass()); // Clean up after the unroller
        PM->add(createGVNPass());              // Remove redundancies
        PM->add(createMemCpyOptPass());        // Remove memcpy / form memset
        PM->add(createSCCPPass());             // Constant prop with SCCP

        // Run instcombine after redundancy elimination to exploit opportunities
        // opened up by them.
        PM->add(createSinkingPass()); ////////////// ****
        PM->add(createInstructionCombiningPass());
        PM->add(createJumpThreadingPass());        // Thread jumps
        PM->add(createDeadStoreEliminationPass()); // Delete dead stores

        // see if all of the constant folding has exposed more loops
        // to simplification and deletion
        // this helps significantly with cleaning up iteration
        PM->add(createCFGSimplificationPass()); // Merge & remove BBs
        PM->add(createLoopIdiomPass());
        PM->add(createLoopDeletionPass());  // Delete dead loops
        PM->add(createJumpThreadingPass()); // Thread jumps
        PM->add(createSLPVectorizerPass());
        PM->add(createAggressiveDCEPass()); // Delete dead instructions
        PM->add(createInstructionCombiningPass());
        PM->add(createLoopVectorizePass());
        PM->add(createInstructionCombiningPass());

        PM->doInitialization();

        // Run the optimizations over all functions in the module being added to
        // the JIT.
        for (auto& F : *M) {
            PM->run(F);
            verifyFunction(F);
        }

        return M;
    }
};
} // namespace

namespace rir {
namespace pir {

LLVMContext JitLLVM::C;

std::string JitLLVM::mangle(const std::string& name) {
    return JitLLVMImplementation::instance().mangle(name);
}
void JitLLVM::createModule() {
    JitLLVMImplementation::instance().createModule();
}

void* JitLLVM::tryCompile(llvm::Function* fun) {
    JitLLVMImplementation::instance();
    return JitLLVMImplementation::instance().tryCompile(fun);
}

llvm::Function* JitLLVM::declare(const std::string& name,
                                 llvm::FunctionType* signature) {
    return JitLLVMImplementation::instance().declareFunction(name, signature);
}

llvm::Value* JitLLVM::getFunctionDeclaration(const std::string& name,
                                             llvm::FunctionType* signature,
                                             llvm::IRBuilder<>& builder) {
    auto sym = JitLLVMImplementation::instance().findSymbol(name).getAddress();
    if (!sym) {
        return nullptr;
    }
    llvm::Type* tp = PointerType::get(signature, 0);
    auto ptr = llvm::ConstantInt::get(C, APInt(64, (uintptr_t)*sym));
    return builder.CreateIntToPtr(ptr, tp);
}

llvm::Module& JitLLVM::module() {
    return *JitLLVMImplementation::instance().module;
}

} // namespace pir
} // namespace rir
