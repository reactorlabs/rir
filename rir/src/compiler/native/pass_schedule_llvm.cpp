#include "compiler/native/pass_schedule_llvm.h"
#include "compiler/parameter.h"

#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFLSteensAliasAnalysis.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/InstSimplifyPass.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Vectorize.h"

namespace rir {
namespace pir {

llvm::Expected<llvm::orc::ThreadSafeModule> PassScheduleLLVM::
operator()(llvm::orc::ThreadSafeModule TSM,
           llvm::orc::MaterializationResponsibility& R) {

    TSM.withModuleDo([&](llvm::Module& M) {

#ifdef ENABLE_SLOWASSERT
        auto verify = [&]() {
            for (auto& F : M) {
                assert(!verifyFunction(F, &llvm::errs()) &&
                       "LLVM Verifier failed.");
            }
        };

        verify();
#endif

        PM->run(M);

#ifdef ENABLE_SLOWASSERT
        verify();

#endif
    });
    return std::move(TSM);
}

PassScheduleLLVM::PassScheduleLLVM() {
    using namespace llvm;

    if (PM.get())
        return;

    PM.reset(new llvm::legacy::PassManager);

    PM->add(createHotColdSplittingPass());

    PM->add(createFunctionInliningPass());

    // See
    // https://github.com/JuliaLang/julia/blob/235784a49b6ed8ab5677f42887e08c84fdc12c5c/src/aotcompile.cpp#L607
    // for inspiration

    PM->add(createEntryExitInstrumenterPass());
    PM->add(createDeadInstEliminationPass());
    PM->add(createCFGSimplificationPass());

    if (rir::pir::Parameter::PIR_LLVM_OPT_LEVEL > 1) {
        PM->add(createCFLSteensAAWrapperPass());
        PM->add(createTypeBasedAAWrapperPass());
        PM->add(createScopedNoAliasAAWrapperPass());
    } else {
        PM->add(createBasicAAWrapperPass());
    }

    PM->add(createSROAPass());
    PM->add(createEarlyCSEPass(true));
    if (rir::pir::Parameter::PIR_LLVM_OPT_LEVEL > 0) {
        PM->add(createPromoteMemoryToRegisterPass());
        PM->add(createConstantPropagationPass());
    }
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
    PM->add(createTailCallEliminationPass());

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

    PM->add(createSpeculativeExecutionIfHasBranchDivergencePass());
    PM->add(createAggressiveDCEPass());

    PM->add(createDivRemPairsPass());
}

std::unique_ptr<llvm::legacy::PassManager> PassScheduleLLVM::PM = nullptr;

unsigned Parameter::PIR_LLVM_OPT_LEVEL =
    getenv("PIR_LLVM_OPT_LEVEL") ? atoi(getenv("PIR_LLVM_OPT_LEVEL")) : 2;

} // namespace pir
} // namespace rir
