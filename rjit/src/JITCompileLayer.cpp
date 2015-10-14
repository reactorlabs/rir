#include "JITCompileLayer.h"

#include "GCPassApi.h"

#include "ICCompiler.h"
#include "JITMemoryManager.h"
#include "JITSymbolResolver.h"
#include "StackMap.h"
#include "CodeCache.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"

#include "llvm/Support/DynamicLibrary.h"

using namespace llvm;

namespace rjit {

ExecutionEngine* JITCompileLayer::getEngine(Module* m) {

    legacy::PassManager pm;

    pm.add(createTargetTransformInfoWrapperPass(TargetIRAnalysis()));

    pm.add(rjit::createPlaceRJITSafepointsPass());

    PassManagerBuilder PMBuilder;
    PMBuilder.OptLevel = 0;  // Set optimization level to -O0
    PMBuilder.SizeLevel = 0; // so that no additional phases are run.
    PMBuilder.populateModulePassManager(pm);

    // TODO: maybe have our own version which is not relocating?
    pm.add(rjit::createRJITRewriteStatepointsForGCPass());
    pm.run(*m);

    // Make sure we can resolve symbols in the program as well. The zero arg
    // to the function tells DynamicLibrary to load the program, not a library.
    auto mm = new JITMemoryManager();

    // create execution engine and finalize the module
    std::string err;
    ExecutionEngine* engine =
        EngineBuilder(std::unique_ptr<Module>(m))
            .setErrorStr(&err)
            .setMCJITMemoryManager(std::unique_ptr<RTDyldMemoryManager>(mm))
            .setEngineKind(EngineKind::JIT)
            .create();

    if (!engine) {
        fprintf(stderr, "Could not create ExecutionEngine: %s\n", err.c_str());
        exit(1);
    }

    engine->finalizeObject();

    // Fill in addresses for cached code
    for (llvm::Function& f : m->getFunctionList()) {
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
