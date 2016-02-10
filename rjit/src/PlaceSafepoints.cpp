/*
 * RJit info: this is a copy from llvm 3.7
 * Everything but support for parsable calls is ripped out
 */

//===- PlaceSafepoints.cpp - Place GC Safepoints --------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Place garbage collection safepoints at appropriate locations in the IR. This
// does not make relocation semantics or variable liveness explicit.  That's
// done by RewriteStatepointsForGC.
//
// Terminology:
// - A call is said to be "parseable" if there is a stack map generated for the
// return PC of the call.  A runtime can determine where values listed in the
// deopt arguments and (after RewriteStatepointsForGC) gc arguments are located
// on the stack when the code is suspended inside such a call.  Every parse
// point is represented by a call wrapped in an gc.statepoint intrinsic.
// - A "poll" is an explicit check in the generated code to determine if the
// runtime needs the generated code to cooperate by calling a helper routine
// and thus suspending its execution at a known state. The call to the helper
// routine will be parseable.  The (gc & runtime specific) logic of a poll is
// assumed to be provided in a function of the name "gc.safepoint_poll".
//
// We aim to insert polls such that running code can quickly be brought to a
// well defined state for inspection by the collector.  In the current
// implementation, this is done via the insertion of poll sites at method entry
// and the backedge of most loops.  We try to avoid inserting more polls than
// are neccessary to ensure a finite period between poll sites.  This is not
// because the poll itself is expensive in the generated code; it's not.  Polls
// do tend to impact the optimizer itself in negative ways; we'd like to avoid
// perturbing the optimization of the method as much as we can.
//
// We also need to make most call sites parseable.  The callee might execute a
// poll (or otherwise be inspected by the GC).  If so, the entire stack
// (including the suspended frame of the current method) must be parseable.
//
// This pass will insert:
// - Call parse points ("call safepoints") for any call which may need to
// reach a safepoint during the execution of the callee function.
// - Backedge safepoint polls and entry safepoint polls to ensure that
// executing code reaches a safepoint poll in a finite amount of time.
//
// We do not currently support return statepoints, but adding them would not
// be hard.  They are not required for correctness - entry safepoints are an
// alternative - but some GCs may prefer them.  Patches welcome.
//
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Statepoint.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/Local.h"

#include "GCPassApi.h"
#include "JITGCStrategy.h"
#include "JITCompileLayer.h"
#include "Runtime.h"

#define DEBUG_TYPE "safepoint-placement"
STATISTIC(NumCallSafepoints, "Number of call safepoints inserted");

using namespace llvm;

// Ignore oppurtunities to avoid placing safepoints on backedges, useful for
// validation
static cl::opt<bool> AllBackedges("spp-all-backedges", cl::Hidden,
                                  cl::init(false));

/// If true, do not place backedge safepoints in counted loops.
static cl::opt<bool> SkipCounted("spp-counted", cl::Hidden, cl::init(true));

// If true, split the backedge of a loop when placing the safepoint, otherwise
// split the latch block itself.  Both are useful to support for
// experimentation, but in practice, it looks like splitting the backedge
// optimizes better.
static cl::opt<bool> SplitBackedge("spp-split-backedge", cl::Hidden,
                                   cl::init(false));

// Print tracing output
static cl::opt<bool> TraceLSP("spp-trace", cl::Hidden, cl::init(false));

namespace {
struct PlaceSafepoints : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid

    PlaceSafepoints() : FunctionPass(ID) {
        initializePlaceSafepointsPass(*PassRegistry::getPassRegistry());
    }
    bool runOnFunction(Function& F) override;

    void getAnalysisUsage(AnalysisUsage& AU) const override {
        // We modify the graph wholesale (inlining, block insertion, etc).  We
        // preserve nothing at the moment.  We could potentially preserve dom
        // tree
        // if that was worth doing
    }
};
}

static bool isGCLeafFunction(const CallSite& CS);

static bool needsStatepoint(const CallSite& CS) {
    if (isGCLeafFunction(CS))
        return false;
    if (CS.isCall()) {
        CallInst* call = cast<CallInst>(CS.getInstruction());
        if (call->isInlineAsm())
            return false;
    }
    if (isStatepoint(CS) || isGCRelocate(CS) || isGCResult(CS)) {
        return false;
    }

    // Functions which need to be parsable have to be tagged by either
    // "needs-statepoint" or "ic-stub". Every other call is discarded

    auto attrs = CS.getAttributes();
    if (attrs.getAttribute(AttributeSet::FunctionIndex, "needs-statepoint")
            .isStringAttribute()) {
        // Later phases of this pass can currently only deal with calls, not
        // with invoke.
        assert(CS.isCall());
        return true;
    }

    if (attrs.getAttribute(AttributeSet::FunctionIndex, "ic-stub")
            .isStringAttribute()) {
        assert(CS.isCall());
        return true;
    }

    return false;
}

static Value* ReplaceWithStatepoint(const CallSite& CS, Pass* P);

/// Identify the list of call sites which need to be have parseable state
static void findCallSafepoints(Function& F,
                               std::vector<CallSite>& Found /*rval*/) {
    assert(Found.empty() && "must be empty!");
    for (Instruction& I : inst_range(F)) {
        Instruction* inst = &I;
        if (isa<CallInst>(inst) || isa<InvokeInst>(inst)) {
            CallSite CS(inst);

            // No safepoint needed or wanted
            if (!needsStatepoint(CS)) {
                continue;
            }

            Found.push_back(CS);
        }
    }
}

/// Implement a unique function which doesn't require we sort the input
/// vector.  Doing so has the effect of changing the output of a couple of
/// tests in ways which make them less useful in testing fused safepoints.
template <typename T>
static void unique_unsorted(std::vector<T>& vec) {
    std::set<T> seen;
    std::vector<T> tmp;
    vec.reserve(vec.size());
    std::swap(tmp, vec);
    for (auto V : tmp) {
        if (seen.insert(V).second) {
            vec.push_back(V);
        }
    }
}

static const char* const GCSafepointPollName = "gc.safepoint_poll";

static bool isGCSafepointPoll(Function& F) {
    return F.getName().equals(GCSafepointPollName);
}

/// Returns true if this function should be rewritten to include safepoint
/// polls and parseable call sites.  The main point of this function is to be
/// an extension point for custom logic.
static bool shouldRewriteFunction(Function& F) {
    // TODO: This should check the GCStrategy
    if (F.hasGC()) {
        const char* FunctionGCName = F.getGC();
        return FunctionGCName == rjit::JITStatepointGC::name();
    } else
        return false;
}

bool PlaceSafepoints::runOnFunction(Function& F) {
    if (F.isDeclaration() || F.empty()) {
        // This is a declaration, nothing to do.  Must exit early to avoid crash
        // in
        // dom tree calculation
        return false;
    }

    if (isGCSafepointPoll(F)) {
        // Given we're inlining this inside of safepoint poll insertion, this
        // doesn't make any sense.  Note that we do make any contained calls
        // parseable after we inline a poll.
        return false;
    }

    if (!shouldRewriteFunction(F))
        return false;

    bool modified = false;

    // In various bits below, we rely on the fact that uses are reachable from
    // defs.  When there are basic blocks unreachable from the entry, dominance
    // and reachablity queries return non-sensical results.  Thus, we preprocess
    // the function to ensure these properties hold.
    modified |= removeUnreachableBlocks(F);

    // STEP 1 - Insert the safepoint polling locations.  We do not need to
    // actually insert parse points yet.  That will be done for all polls and
    // calls in a single pass.

    DominatorTree DT;
    DT.recalculate(F);

    std::vector<CallSite> ParsePointNeeded;

    findCallSafepoints(F, ParsePointNeeded);
    NumCallSafepoints += ParsePointNeeded.size();

    // Unique the vectors since we can end up with duplicates if we scan the
    // call
    // site for call safepoints after we add it for entry or backedge.  The
    // only reason we need tracking at all is that some functions might have
    // polls but not call safepoints and thus we might miss marking the runtime
    // calls for the polls. (This is useful in test cases!)
    unique_unsorted(ParsePointNeeded);

    // Any parse point (no matter what source) will be handled here

    // We're about to start modifying the function
    if (!ParsePointNeeded.empty())
        modified = true;

    // Now run through and insert the safepoints, but do _NOT_ update or remove
    // any existing uses.  We have references to live variables that need to
    // survive to the last iteration of this loop.
    std::vector<Value*> Results;
    Results.reserve(ParsePointNeeded.size());
    for (size_t i = 0; i < ParsePointNeeded.size(); i++) {
        CallSite& CS = ParsePointNeeded[i];

        Value* GCResult = ReplaceWithStatepoint(CS, nullptr);
        Results.push_back(GCResult);
    }
    assert(Results.size() == ParsePointNeeded.size());

    // Adjust all users of the old call sites to use the new ones instead
    for (size_t i = 0; i < ParsePointNeeded.size(); i++) {
        CallSite& CS = ParsePointNeeded[i];
        Value* GCResult = Results[i];
        if (GCResult) {
            // Can not RAUW for the invoke gc result in case of phi nodes
            // preset.
            assert(CS.isCall() ||
                   !isa<PHINode>(
                       cast<Instruction>(GCResult)->getParent()->begin()));

            // Replace all uses with the new call
            CS.getInstruction()->replaceAllUsesWith(GCResult);
        }

        // Now that we've handled all uses, remove the original call itself
        // Note: The insert point can't be the deleted instruction!
        CS.getInstruction()->eraseFromParent();
    }
    return modified;
}

char PlaceSafepoints::ID = 0;

FunctionPass* rjit::createPlaceRJITSafepointsPass() {
    return new PlaceSafepoints();
}

INITIALIZE_PASS_BEGIN(PlaceSafepoints, "place-safepoints", "Place Safepoints",
                      false, false)
INITIALIZE_PASS_END(PlaceSafepoints, "place-safepoints", "Place Safepoints",
                    false, false)

static bool isGCLeafFunction(const CallSite& CS) {
    Instruction* inst = CS.getInstruction();
    if (isa<IntrinsicInst>(inst)) {
        // Most LLVM intrinsics are things which can never take a safepoint.
        // As a result, we don't need to have the stack parsable at the
        // callsite.  This is a highly useful optimization since intrinsic
        // calls are fairly prevelent, particularly in debug builds.
        return true;
    }

    // If this function is marked explicitly as a leaf call, we don't need to
    // place a safepoint of it.  In fact, for correctness we *can't* in many
    // cases.  Note: Indirect calls return Null for the called function,
    // these obviously aren't runtime functions with attributes
    // TODO: Support attributes on the call site as well.
    const Function* F = CS.getCalledFunction();
    bool isLeaf =
        F &&
        F->getFnAttribute("gc-leaf-function").getValueAsString().equals("true");
    if (isLeaf) {
        return true;
    }
    return false;
}

// Replaces the given call site (Call or Invoke) with a gc.statepoint
/// intrinsic with an empty deoptimization arguments list.  This does
/// NOT do explicit relocation for GC support.
static Value* ReplaceWithStatepoint(const CallSite& CS, /* to replace */
                                    Pass* P) {
    assert(CS.getInstruction()->getParent()->getParent()->getParent() &&
           "must be set");

    // TODO: technically, a pass is not allowed to get functions from within a
    // function pass since it might trigger a new function addition.  Refactor
    // this logic out to the initialization of the pass.  Doesn't appear to
    // matter in practice.

    // Then go ahead and use the builder do actually do the inserts.  We insert
    // immediately before the previous instruction under the assumption that all
    // arguments will be available here.  We can't insert afterwards since we
    // may
    // be replacing a terminator.
    IRBuilder<> Builder(CS.getInstruction());

    // Note: The gc args are not filled in at this time, that's handled by
    // RewriteStatepointsForGC (which is currently under review).

    // Create the statepoint given all the arguments
    Instruction* Token = nullptr;

    AttributeSet OriginalAttrs = CS.getAttributes();

    bool isStatepoint = OriginalAttrs.getAttribute(AttributeSet::FunctionIndex,
                                                   "needs-statepoint")
                            .isStringAttribute();

    uint64_t stubSize = -1;
    OriginalAttrs.getAttribute(AttributeSet::FunctionIndex, "ic-stub")
        .getValueAsString()
        .getAsInteger(10, stubSize);
    bool isIcStubCall = stubSize != (uint64_t)-1;

    // If its neither of those it should never have been added to the list
    assert(isIcStubCall || isStatepoint);

    uint64_t ID = rjit::JITCompileLayer::singleton.getSafepointId(
        CS->getParent()->getParent());

    AttrBuilder AttrsToRemove;

    if (isStatepoint)
        AttrsToRemove.addAttribute("needs-statepoint");

    if (isIcStubCall) {
        assert(stubSize != (uint64_t)-1);

        // Register this stackmap id to be a patchpoint, such that later phases
        // (i.e. the compile layer) can patch in initial stub calls.
        rjit::JITCompileLayer::singleton.setPatchpoint(ID, stubSize);

        CallInst* i = cast<CallInst>(CS.getInstruction());
        assert(i);

        // The patchpoints (i.e. call stubs) expect the last argument to
        // be the stackmap id.
        i->setArgOperand(
            i->getNumArgOperands() - 1,
            ConstantInt::get(CS->getParent()->getParent()->getContext(),
                             APInt(64, ID)));

        AttrsToRemove.addAttribute("icStub");
    }

    OriginalAttrs = OriginalAttrs.removeAttributes(
        CS.getInstruction()->getContext(), AttributeSet::FunctionIndex,
        AttrsToRemove);

    Value* StatepointTarget =
        isIcStubCall ? ConstantPointerNull::get(
                           cast<PointerType>(CS.getCalledValue()->getType()))
                     : CS.getCalledValue();

    CallInst* ToReplace = cast<CallInst>(CS.getInstruction());
    CallInst* Call = Builder.CreateGCStatepointCall(
        ID, isIcStubCall ? rjit::patchpointSize : 0, StatepointTarget,
        makeArrayRef(CS.arg_begin(), CS.arg_end()), None, None,
        "safepoint_token");
    Call->setTailCall(ToReplace->isTailCall());
    Call->setCallingConv(ToReplace->getCallingConv());

    // In case if we can handle this set of attributes - set up function
    // attributes directly on statepoint and return attributes later for
    // gc_result intrinsic.
    Call->setAttributes(OriginalAttrs.getFnAttributes());

    Token = Call;

    // Put the following gc_result and gc_relocate calls immediately after
    // the
    // the old call (which we're about to delete).
    assert(ToReplace->getNextNode() && "not a terminator, must have next");
    Builder.SetInsertPoint(ToReplace->getNextNode());
    Builder.SetCurrentDebugLocation(ToReplace->getNextNode()->getDebugLoc());

    assert(Token);

    // Handle the return value of the original call - update all uses to use a
    // gc_result hanging off the statepoint node we just inserted

    // Only add the gc_result iff there is actually a used result
    if (!CS.getType()->isVoidTy() && !CS.getInstruction()->use_empty()) {
        std::string TakenName = CS.getInstruction()->hasName()
                                    ? CS.getInstruction()->getName()
                                    : "";
        CallInst* GCResult =
            Builder.CreateGCResult(Token, CS.getType(), TakenName);
        GCResult->setAttributes(OriginalAttrs.getRetAttributes());
        return GCResult;
    } else {
        // No return value for the call.
        return nullptr;
    }
}
