#include "JITCompileLayer.h"

#include "GCPassApi.h"

#include "ICCompiler.h"
#include "JITMemoryManager.h"
#include "JITSymbolResolver.h"
#include "StackMap.h"
#include "CodeCache.h"
#include "Instrumentation.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"

#include "llvm/Support/DynamicLibrary.h"

#include "ir/Analysis/VariableAnalysis.h"
#include "ir/Analysis/TypeAndShape.h"
#include "ir/Analysis/ScalarsTracking.h"
#include "ir/Optimization/ConstantLoad.h"
#include "ir/Optimization/Scalars.h"
#include "ir/Optimization/BoxingRemoval.h"
#include "ir/Optimization/DeadAllocationRemoval.h"

#include "llvm/IR/IRPrintingPasses.h"

#include "Flags.h"

using namespace llvm;

namespace rjit {

ExecutionEngine* JITCompileLayer::finalize(JITModule* m) {
    // to the function tells DynamicLibrary to load the program, not a library.
    auto mm = new JITMemoryManager();

    // create execution engine and finalize the module
    std::string err;

    TargetOptions opts;
    ExecutionEngine* engine =
        EngineBuilder(std::unique_ptr<Module>(m))
            .setErrorStr(&err)
            .setMCJITMemoryManager(std::unique_ptr<RTDyldMemoryManager>(mm))
            .setEngineKind(EngineKind::JIT)
            .setTargetOptions(opts)
            .create();

    if (!engine) {
        fprintf(stderr, "Could not create ExecutionEngine: %s\n", err.c_str());
        exit(1);
    }

    std::string str;
    llvm::raw_string_ostream rso(str);

    // Make sure we can resolve symbols in the program as well. The zero arg
    legacy::PassManager pm;

    if (Flag::singleton().printIR)
        pm.add(createPrintModulePass(rso));

    pm.add(new analysis::TypeAndShape());
    pm.add(new optimization::Scalars());
    pm.add(new analysis::ScalarsTracking());
    pm.add(new optimization::BoxingRemoval());
    pm.add(new optimization::DeadAllocationRemoval());

    pm.add(new ir::VariableAnalysis());
    pm.add(new ir::ConstantLoadOptimization());

    if (Flag::singleton().printOptIR)
        pm.add(createPrintModulePass(rso));

    pm.add(createTargetTransformInfoWrapperPass(TargetIRAnalysis()));

    PassManagerBuilder PMBuilder;
    PMBuilder.OptLevel = 1;  // Set optimization level to -O0
    PMBuilder.SizeLevel = 1; // so that no additional phases are run.
    PMBuilder.populateModulePassManager(pm);

    pm.add(rjit::createPlaceRJITSafepointsPass());
    pm.add(rjit::createRJITRewriteStatepointsForGCPass());

    pm.run(*m);

    std::cout << rso.str();

    engine->finalizeObject();
    m->finalizeNativeSEXPs(engine);

    // Fill in addresses for cached code
    for (llvm::Function& f : m->getFunctionList()) {
        TypeFeedback::detach(&f);

        if (CodeCache::missingAddress(f.getName())) {
            CodeCache::setAddress(f.getName(),
                                  (uint64_t)engine->getPointerToFunction(&f));
        }
    }

    recordStackmaps(engine, m, mm);

    // patch initial icStubs
    for (auto p : safepoints) {
        auto f = (uintptr_t)engine->getPointerToFunction(std::get<0>(p));
        for (auto i : std::get<1>(p)) {
            if (StackMap::isPatchpoint(i)) {
                std::string name = ICCompiler::stubName(patchpoints.at(i));
                patchIC((void*)CodeCache::getAddress(name), i, (void*)f);
            }
        }
    }

    safepoints.clear();
    patchpoints.clear();

    return engine;
}

void JITCompileLayer::recordStackmaps(ExecutionEngine* engine, Module* m,
                                      JITMemoryManager* mm) {

    // Pass one: collect all stackmap ids and construct a mapping to the
    // correspondig native function addresses
    StackMap::StackmapToFunction sp;
    for (auto p : safepoints) {
        auto f = (uintptr_t)engine->getPointerToFunction(std::get<0>(p));
        for (auto i : std::get<1>(p)) {
            sp[i] = f;
        }
    }

    // Pass two: parse the current stackmap
    if (mm->stackmapAddr()) {
        ArrayRef<uint8_t> sm(mm->stackmapAddr(), mm->stackmapSize());
        StackMap::recordStackmaps(sm, sp, patchpoints);
    }
}

uint64_t JITCompileLayer::getSafepointId(llvm::Function* f) {
    auto n = ++nextStackmapId;
    safepoints[f].push_back(n);
    return n;
}

JITCompileLayer JITCompileLayer::singleton;
}
