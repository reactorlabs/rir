#include "jit_llvm.h"

#include "compiler/parameter.h"
#include "types_llvm.h"

#include <llvm/ADT/STLExtras.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/CFLAndersAliasAnalysis.h>
#include <llvm/Analysis/CFLSteensAliasAnalysis.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/IndirectionUtils.h>
#include <llvm/ExecutionEngine/Orc/LambdaResolver.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Mangler.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/InductiveRangeCheckElimination.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/Vectorize.h>

// analysis passes
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/InstSimplifyPass.h>
#include <llvm/Transforms/Vectorize.h>

#include <llvm/Transforms/IPO.h>
#include <unordered_map>

#include <iostream>

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
    std::unordered_map<std::string, std::pair<llvm::Function*, void*>>
        builtins_;

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
              [this](const llvm::StringRef& Name) {
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
        TM->setMachineOutliner(true);
        TM->setFastISel(true);
    }

    std::unordered_map<rir::pir::ClosureVersion*, llvm::Function*> funs;
    void createModule() {
        module = new llvm::Module("", C);
        module->setDataLayout(TM->createDataLayout());
        moduleKey = -1;
        funs.clear();
        builtins_.clear();
    }

    llvm::Function* declareFunction(rir::pir::ClosureVersion* v,
                                    const std::string& name,
                                    llvm::FunctionType* signature) {
        assert(!funs.count(v));
        auto f = Function::Create(signature, Function::ExternalLinkage, name,
                                  JitLLVMImplementation::instance().module);
        funs[v] = f;
        return f;
    }

    llvm::Function* getBuiltin(const rir::pir::NativeBuiltin& b) {
        auto l = builtins_.find(b.name);
        if (l != builtins_.end()) {
            assert(l->second.second == b.fun);
            return l->second.first;
        }

        assert(b.llvmSignature);
        auto f =
            Function::Create(b.llvmSignature, Function::ExternalLinkage, b.name,
                             JitLLVMImplementation::instance().module);
        for (auto a : b.attrs)
            f->addFnAttr(a);

        builtins_[b.name] = {f, b.fun};
        return f;
    }

    llvm::Function* getFunction(rir::pir::ClosureVersion* v) {
        auto r = funs.find(v);
        if (r != funs.end())
            return r->second;
        return nullptr;
    }

    void* compile(llvm::Function* fun) {
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
    JITSymbol findMangledSymbol(const llvm::StringRef& Name) {
        auto l = builtins_.find(Name.str());
        if (l != builtins_.end()) {
            return JITSymbol((uintptr_t)l->second.second,
                             JITSymbolFlags::Exported |
                                 JITSymbolFlags::Callable);
        }
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
            if (auto Sym = CompileLayer.findSymbolIn(moduleKey, Name.str(),
                                                     ExportedSymbolsOnly))
                return Sym;

        // If we can't find the symbol in the JIT, try looking in the host
        // process.
        if (auto SymAddr =
                RTDyldMemoryManager::getSymbolAddressInProcess(Name.str()))
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
    optimizeModule(std::unique_ptr<llvm::Module> M);
};

static void pirPassSchedule(const PassManagerBuilder& b,
                            legacy::PassManagerBase& PM_) {
    legacy::PassManagerBase* PM = &PM_;

    // See
    // https://github.com/JuliaLang/julia/blob/235784a49b6ed8ab5677f42887e08c84fdc12c5c/src/aotcompile.cpp#L607
    // for inspiration

    PM->add(createEntryExitInstrumenterPass());

    if (rir::pir::Parameter::PIR_LLVM_OPT_LEVEL > 1) {
        PM->add(createCFLSteensAAWrapperPass());
        PM->add(createTypeBasedAAWrapperPass());
        PM->add(createScopedNoAliasAAWrapperPass());
    } else {
        PM->add(createBasicAAWrapperPass());
    }

    PM->add(createCFGSimplificationPass());
    PM->add(createSROAPass());
    if (rir::pir::Parameter::PIR_LLVM_OPT_LEVEL > 0) {
        PM->add(createConstantPropagationPass());
        PM->add(createPromoteMemoryToRegisterPass());
    }
    PM->add(createEarlyCSEPass());
    PM->add(createLowerExpectIntrinsicPass());

    PM->add(createDeadInstEliminationPass());
    PM->add(createDeadCodeEliminationPass());
    PM->add(createInstructionCombiningPass());
    PM->add(createCFGSimplificationPass());

    if (rir::pir::Parameter::PIR_LLVM_OPT_LEVEL < 2)
        return;

    PM->add(createSROAPass());
    PM->add(createInstSimplifyLegacyPass());
    PM->add(createAggressiveDCEPass());
    PM->add(createBitTrackingDCEPass());
    PM->add(createJumpThreadingPass());

    PM->add(createReassociatePass());
    PM->add(createEarlyCSEPass());
    PM->add(createMergedLoadStoreMotionPass());

    // Load forwarding above can expose allocations that aren't actually used
    // remove those before optimizing loops.
    PM->add(createLoopRotatePass());
    PM->add(createLoopIdiomPass());

    // LoopRotate strips metadata from terminator, so run LowerSIMD afterwards
    PM->add(createLICMPass());
    PM->add(createLoopUnswitchPass());
    PM->add(createInductiveRangeCheckEliminationPass());
    PM->add(createLICMPass());
    // Subsequent passes not stripping metadata from terminator
    PM->add(createInstSimplifyLegacyPass());
    PM->add(createIndVarSimplifyPass());
    PM->add(createLoopDeletionPass());
    PM->add(createSimpleLoopUnrollPass());

    // Re-run SROA after loop-unrolling (useful for small loops that operate,
    // over the structure of an aggregate)
    PM->add(createSROAPass());
    // might not be necessary:
    PM->add(createInstSimplifyLegacyPass());

    PM->add(createGVNPass());
    PM->add(createMemCpyOptPass());
    PM->add(createSCCPPass());
    PM->add(createSinkingPass());

    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    // This needs to be InstCombine instead of InstSimplify to allow
    // loops over Union-typed arrays to vectorize.
    PM->add(createInstructionCombiningPass());
    PM->add(createJumpThreadingPass());
    PM->add(createDeadStoreEliminationPass());

    // see if all of the constant folding has exposed more loops
    // to simplification and deletion
    // this helps significantly with cleaning up iteration
    PM->add(createCFGSimplificationPass());
    PM->add(createLoopDeletionPass());
    PM->add(createInstructionCombiningPass());
    PM->add(createLoopVectorizePass());
    PM->add(createLoopLoadEliminationPass());
    PM->add(createCFGSimplificationPass());
    PM->add(createSLPVectorizerPass());
    PM->add(createVectorCombinePass());

    PM->add(createSpeculativeExecutionIfHasBranchDivergencePass());
    PM->add(createAggressiveDCEPass());

    PM->add(createDivRemPairsPass());
}

// Pass to run after hotCold splitting:
// We use cold functions for bailout branches, thus we set the optnone attribute
// on cold functions and uses the coldcc calling convention to call them.
struct NooptCold : public ModulePass {
    static char ID;
    NooptCold() : ModulePass(ID) {}

    bool runOnModule(Module& module) override {
        bool changed = false;
        for (auto& fun : module) {
            if (fun.hasName()) {
                if (fun.getName().contains(".cold.")) {
                    if (!fun.hasFnAttribute(Attribute::OptimizeNone)) {
                        fun.addAttribute(AttributeList::FunctionIndex,
                                         Attribute::OptimizeNone);
                        fun.setCallingConv(CallingConv::Cold);
                        changed = true;
                    }
                }
            }
        }
        for (auto& fun : module) {
            for (auto& bb : fun) {
                for (auto& instr : bb) {
                    if (CallInst* call = llvm::dyn_cast<CallInst>(&instr)) {
                        if (call->getCallingConv() != CallingConv::Cold) {
                            if (auto callee = call->getCalledFunction()) {
                                if (callee->getCallingConv() ==
                                    CallingConv::Cold) {
                                    call->setCallingConv(CallingConv::Cold);
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }
        return changed;
    }
};

char NooptCold::ID = 0;
static RegisterPass<NooptCold> X("noopt-cold", "Don't optimize cold functions",
                                 false /* Only looks at CFG */,
                                 false /* Analysis Pass */);

std::unique_ptr<llvm::Module>
JitLLVMImplementation::optimizeModule(std::unique_ptr<llvm::Module> M) {

    M->setTargetTriple(TM->getTargetTriple().str());
    M->setDataLayout(TM->createDataLayout());

    llvm::legacy::PassManager MPM;
    auto PM = std::make_unique<legacy::FunctionPassManager>(M.get());

    {
        llvm::PassManagerBuilder builder;

        builder.OptLevel = 0;
        builder.SizeLevel = 0;
        builder.Inliner = llvm::createFunctionInliningPass();
        TM->adjustPassManager(builder);

        // Start with some custom passes tailored to our backend
        builder.addExtension(PassManagerBuilder::EP_EarlyAsPossible,
                             pirPassSchedule);

        PM->add(
            new TargetLibraryInfoWrapperPass(Triple(TM->getTargetTriple())));
        PM->add(
            createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));

        builder.populateFunctionPassManager(*PM);
        MPM.add(createHotColdSplittingPass());
        MPM.add(new NooptCold());
        builder.populateModulePassManager(MPM);
        builder.populateLTOPassManager(MPM);
    }

    PM->doInitialization();
    for (auto& F : *M) {
        PM->run(F);
#ifdef ENABLE_SLOWASSERT
            verifyFunction(F);
#endif
        }
        PM->doFinalization();

        MPM.run(*M);

        return M;
    }

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

void* JitLLVM::compile(llvm::Function* fun) {
    return JitLLVMImplementation::instance().compile(fun);
}

llvm::Function* JitLLVM::get(ClosureVersion* v) {
    return JitLLVMImplementation::instance().getFunction(v);
}

llvm::Function* JitLLVM::declare(ClosureVersion* v, const std::string& name,
                                 llvm::FunctionType* signature) {
    return JitLLVMImplementation::instance().declareFunction(v, name,
                                                             signature);
}

llvm::Function* JitLLVM::getBuiltin(const NativeBuiltin& b) {
    return JitLLVMImplementation::instance().getBuiltin(b);
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

unsigned Parameter::PIR_LLVM_OPT_LEVEL =
    getenv("PIR_LLVM_OPT_LEVEL") ? atoi(getenv("PIR_LLVM_OPT_LEVEL")) : 2;

} // namespace pir
} // namespace rir
