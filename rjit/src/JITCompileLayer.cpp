#include "JITCompileLayer.h"

#include "GCPassApi.h"

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

    recordStackmaps(engine, m, mm);

    for (llvm::Function& f : m->getFunctionList()) {
        if (CodeCache::missingAddress(f.getName())) {
            CodeCache::setAddress(f.getName(),
                                  (uint64_t)engine->getPointerToFunction(&f));
        }
    }

    return engine;
}

void JITCompileLayer::recordStackmaps(ExecutionEngine* engine, Module* m,
                                      JITMemoryManager* mm) {
    // TODO: maybe we could record the statepoints lazily in getFunctionPointer
    std::unordered_map<uint64_t, uintptr_t> fids;
    for (llvm::Function& f : m->getFunctionList()) {
        auto attrs = f.getAttributes();
        uint64_t id = -1;
        Attribute attr_id =
            attrs.getAttribute(AttributeSet::FunctionIndex, "statepoint-id");
        bool has_id = attr_id.isStringAttribute() &&
                      !attr_id.getValueAsString().getAsInteger(10, id);
        if (has_id)
            fids[id] = (uintptr_t)engine->getPointerToFunction(&f);
    }

    if (mm->stackmapAddr()) {
        ArrayRef<uint8_t> sm(mm->stackmapAddr(), mm->stackmapSize());
        StackMap::recordStackmaps(sm, fids);
    }
}
}
