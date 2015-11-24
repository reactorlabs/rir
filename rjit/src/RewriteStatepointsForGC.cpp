//===- RewriteStatepointsForGC.cpp - Make GC relocations explicit ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Rewrite an existing set of gc.statepoints such that they make potential
// relocations performed by the garbage collector explicit in the IR.
//
//===----------------------------------------------------------------------===//

#include "GCPassApi.h"
#include "JITGCStrategy.h"

#include "llvm/Pass.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringRef.h"
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
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Statepoint.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/PromoteMemToReg.h"

#define DEBUG_TYPE "rewrite-statepoints-for-gc"

using namespace llvm;

// Print tracing output
static cl::opt<bool> TraceLSP("trace-rewrite-statepoints", cl::Hidden,
                              cl::init(false));

// Print the liveset found at the insert location
static cl::opt<bool> PrintLiveSet("spp-print-liveset", cl::Hidden,
                                  cl::init(false));
static cl::opt<bool> PrintLiveSetSize("spp-print-liveset-size", cl::Hidden,
                                      cl::init(false));
// Print out the base pointers for debugging
static cl::opt<bool> PrintBasePointers("spp-print-base-pointers", cl::Hidden,
                                       cl::init(false));

// Cost threshold measuring when it is profitable to rematerialize value instead
// of relocating it
static cl::opt<unsigned>
    RematerializationThreshold("spp-rematerialization-threshold", cl::Hidden,
                               cl::init(6));

#ifdef XDEBUG
static bool ClobberNonLive = true;
#else
static bool ClobberNonLive = false;
#endif
static cl::opt<bool, true> ClobberNonLiveOverride("rs4gc-clobber-non-live",
                                                  cl::location(ClobberNonLive),
                                                  cl::Hidden);

namespace {
struct RewriteStatepointsForGC : public ModulePass {
    static char ID; // Pass identification, replacement for typeid

    RewriteStatepointsForGC() : ModulePass(ID) {
        initializeRewriteStatepointsForGCPass(*PassRegistry::getPassRegistry());
    }
    bool runOnFunction(Function& F);
    bool runOnModule(Module& M) override {
        bool Changed = false;
        for (Function& F : M)
            Changed |= runOnFunction(F);

        if (Changed) {
            // stripDereferenceabilityInfo asserts that
            // shouldRewriteStatepointsIn
            // returns true for at least one function in the module.  Since at
            // least
            // one function changed, we know that the precondition is satisfied.
            stripDereferenceabilityInfo(M);
        }

        return Changed;
    }

    void getAnalysisUsage(AnalysisUsage& AU) const override {
        // We add and rewrite a bunch of instructions, but don't really do much
        // else.  We could in theory preserve a lot more analyses here.
        AU.addRequired<DominatorTreeWrapperPass>();
        AU.addRequired<TargetTransformInfoWrapperPass>();
    }

    /// The IR fed into RewriteStatepointsForGC may have had attributes implying
    /// dereferenceability that are no longer valid/correct after
    /// RewriteStatepointsForGC has run.  This is because semantically, after
    /// RewriteStatepointsForGC runs, all calls to gc.statepoint "free" the
    /// entire
    /// heap.  stripDereferenceabilityInfo (conservatively) restores correctness
    /// by erasing all attributes in the module that externally imply
    /// dereferenceability.
    ///
    void stripDereferenceabilityInfo(Module& M);

    // Helpers for stripDereferenceabilityInfo
    void stripDereferenceabilityInfoFromBody(Function& F);
    void stripDereferenceabilityInfoFromPrototype(Function& F);
};
} // namespace

char RewriteStatepointsForGC::ID = 0;

ModulePass* rjit::createRJITRewriteStatepointsForGCPass() {
    return new RewriteStatepointsForGC();
}

INITIALIZE_PASS_BEGIN(RewriteStatepointsForGC, "rewrite-statepoints-for-gc",
                      "Make relocations explicit at statepoints", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_END(RewriteStatepointsForGC, "rewrite-statepoints-for-gc",
                    "Make relocations explicit at statepoints", false, false)

namespace {
struct GCPtrLivenessData {
    /// Values defined in this block.
    DenseMap<BasicBlock*, DenseSet<Value*>> KillSet;
    /// Values used in this block (and thus live); does not included values
    /// killed within this block.
    DenseMap<BasicBlock*, DenseSet<Value*>> LiveSet;

    /// Values live into this basic block (i.e. used by any
    /// instruction in this basic block or ones reachable from here)
    DenseMap<BasicBlock*, DenseSet<Value*>> LiveIn;

    /// Values live out of this basic block (i.e. live into
    /// any successor block)
    DenseMap<BasicBlock*, DenseSet<Value*>> LiveOut;
};

// The type of the internal cache used inside the findBasePointers family
// of functions.  From the callers perspective, this is an opaque type and
// should not be inspected.
//
// In the actual implementation this caches two relations:
// - The base relation itself (i.e. this pointer is based on that one)
// - The base defining value relation (i.e. before base_phi insertion)
// Generally, after the execution of a full findBasePointer call, only the
// base relation will remain.  Internally, we add a mixture of the two
// types, then update all the second type to the first type
typedef DenseMap<Value*, Value*> DefiningValueMapTy;
typedef DenseSet<llvm::Value*> StatepointLiveSetTy;
typedef DenseMap<Instruction*, Value*> RematerializedValueMapTy;

struct PartiallyConstructedSafepointRecord {
    /// The set of values known to be live accross this safepoint
    StatepointLiveSetTy liveset;

    /// The *new* gc.statepoint instruction itself.  This produces the token
    /// that normal path gc.relocates and the gc.result are tied to.
    Instruction* StatepointToken;

    /// Instruction to which exceptional gc relocates are attached
    /// Makes it easier to iterate through them during relocationViaAlloca.
    Instruction* UnwindToken;

    /// Record live values we are rematerialized instead of relocating.
    /// They are not included into 'liveset' field.
    /// Maps rematerialized copy to it's original value.
    RematerializedValueMapTy RematerializedValues;
};
}

/// Compute the live-in set for every basic block in the function
static void computeLiveInValues(DominatorTree& DT, Function& F,
                                GCPtrLivenessData& Data);

/// Given results from the dataflow liveness computation, find the set of live
/// Values at a particular instruction.
static void findLiveSetAtInst(Instruction* inst, GCPtrLivenessData& Data,
                              StatepointLiveSetTy& out);

static bool isGCPointer(const Value* value) {
    return rjit::JITStatepointGC::isGCManaged(value);
}

static bool order_by_name(llvm::Value* a, llvm::Value* b) {
    if (a->hasName() && b->hasName()) {
        return -1 == a->getName().compare(b->getName());
    } else if (a->hasName() && !b->hasName()) {
        return true;
    } else if (!a->hasName() && b->hasName()) {
        return false;
    } else {
        // Better than nothing, but not stable
        return a < b;
    }
}

// Conservatively identifies any definitions which might be live at the
// given instruction. The  analysis is performed immediately before the
// given instruction. Values defined by that instruction are not considered
// live.  Values used by that instruction are considered live.
static void analyzeParsePointLiveness(
    DominatorTree& DT, GCPtrLivenessData& OriginalLivenessData,
    const CallSite& CS, PartiallyConstructedSafepointRecord& result) {
    Instruction* inst = CS.getInstruction();

    StatepointLiveSetTy liveset;
    findLiveSetAtInst(inst, OriginalLivenessData, liveset);

    if (PrintLiveSet) {
        // Note: This output is used by several of the test cases
        // The order of elemtns in a set is not stable, put them in a vec and
        // sort
        // by name
        SmallVector<Value*, 64> temp;
        temp.insert(temp.end(), liveset.begin(), liveset.end());
        std::sort(temp.begin(), temp.end(), order_by_name);
        errs() << "Live Variables:\n";
        for (Value* V : temp) {
            errs() << " " << V->getName(); // no newline
            V->dump();
        }
    }
    if (PrintLiveSetSize) {
        errs() << "Safepoint For: " << CS.getCalledValue()->getName() << "\n";
        errs() << "Number live values: " << liveset.size() << "\n";
    }
    result.liveset = liveset;
}

static Value* findBaseDefiningValue(Value* I);

/// Return a base defining value for the 'Index' element of the given vector
/// instruction 'I'.  If Index is null, returns a BDV for the entire vector
/// 'I'.  As an optimization, this method will try to determine when the
/// element is known to already be a base pointer.  If this can be established,
/// the second value in the returned pair will be true.  Note that either a
/// vector or a pointer typed value can be returned.  For the former, the
/// vector returned is a BDV (and possibly a base) of the entire vector 'I'.
/// If the later, the return pointer is a BDV (or possibly a base) for the
/// particular element in 'I'.
static std::pair<Value*, bool>
findBaseDefiningValueOfVector(Value* I, Value* Index = nullptr) {
    assert(I->getType()->isVectorTy() &&
           cast<VectorType>(I->getType())->getElementType()->isPointerTy() &&
           "Illegal to ask for the base pointer of a non-pointer type");

    // Each case parallels findBaseDefiningValue below, see that code for
    // detailed motivation.

    if (isa<Argument>(I))
        // An incoming argument to the function is a base pointer
        return std::make_pair(I, true);

    // We shouldn't see the address of a global as a vector value?
    assert(!isa<GlobalVariable>(I) &&
           "unexpected global variable found in base of vector");

    // inlining could possibly introduce phi node that contains
    // undef if callee has multiple returns
    if (isa<UndefValue>(I))
        // utterly meaningless, but useful for dealing with partially optimized
        // code.
        return std::make_pair(I, true);

    // Due to inheritance, this must be _after_ the global variable and undef
    // checks
    if (Constant* Con = dyn_cast<Constant>(I)) {
        assert(!isa<GlobalVariable>(I) && !isa<UndefValue>(I) &&
               "order of checks wrong!");
        assert(Con->isNullValue() && "null is the only case which makes sense");
        return std::make_pair(Con, true);
    }

    if (isa<LoadInst>(I))
        return std::make_pair(I, true);

    // For an insert element, we might be able to look through it if we know
    // something about the indexes.
    if (InsertElementInst* IEI = dyn_cast<InsertElementInst>(I)) {
        if (Index) {
            Value* InsertIndex = IEI->getOperand(2);
            // This index is inserting the value, look for its BDV
            if (InsertIndex == Index)
                return std::make_pair(findBaseDefiningValue(IEI->getOperand(1)),
                                      false);
            // Both constant, and can't be equal per above. This insert is
            // definitely
            // not relevant, look back at the rest of the vector and keep
            // trying.
            if (isa<ConstantInt>(Index) && isa<ConstantInt>(InsertIndex))
                return findBaseDefiningValueOfVector(IEI->getOperand(0), Index);
        }

        // We don't know whether this vector contains entirely base pointers or
        // not.  To be conservatively correct, we treat it as a BDV and will
        // duplicate code as needed to construct a parallel vector of bases.
        return std::make_pair(IEI, false);
    }

    if (isa<ShuffleVectorInst>(I))
        // We don't know whether this vector contains entirely base pointers or
        // not.  To be conservatively correct, we treat it as a BDV and will
        // duplicate code as needed to construct a parallel vector of bases.
        // TODO: There a number of local optimizations which could be applied
        // here
        // for particular sufflevector patterns.
        return std::make_pair(I, false);

    // A PHI or Select is a base defining value.  The outer findBasePointer
    // algorithm is responsible for constructing a base value for this BDV.
    assert((isa<SelectInst>(I) || isa<PHINode>(I)) &&
           "unknown vector instruction - no base found for vector element");
    return std::make_pair(I, false);
}

/// Helper function for findBasePointer - Will return a value which either a)
/// defines the base pointer for the input or b) blocks the simple search
/// (i.e. a PHI or Select of two derived pointers)
static Value* findBaseDefiningValue(Value* I) {
    if (I->getType()->isVectorTy())
        return findBaseDefiningValueOfVector(I).first;

    assert(I->getType()->isPointerTy() &&
           "Illegal to ask for the base pointer of a non-pointer type");

    // This case is a bit of a hack - it only handles extracts from vectors
    // which
    // trivially contain only base pointers or cases where we can directly match
    // the index of the original extract element to an insertion into the
    // vector.
    // See note inside the function for how to improve this.
    if (auto* EEI = dyn_cast<ExtractElementInst>(I)) {
        Value* VectorOperand = EEI->getVectorOperand();
        Value* Index = EEI->getIndexOperand();
        std::pair<Value*, bool> pair =
            findBaseDefiningValueOfVector(VectorOperand, Index);
        Value* VectorBase = pair.first;
        if (VectorBase->getType()->isPointerTy())
            // We found a BDV for this specific element with the vector.  This
            // is an
            // optimization, but in practice it covers most of the useful cases
            // created via scalarization.
            return VectorBase;
        else {
            assert(VectorBase->getType()->isVectorTy());
            if (pair.second)
                // If the entire vector returned is known to be entirely base
                // pointers,
                // then the extractelement is valid base for this value.
                return EEI;
            else {
                // Otherwise, we have an instruction which potentially produces
                // a
                // derived pointer and we need findBasePointers to clone code
                // for us
                // such that we can create an instruction which produces the
                // accompanying base pointer.
                // Note: This code is currently rather incomplete.  We don't
                // currently
                // support the general form of shufflevector of insertelement.
                // Conceptually, these are just 'base defining values' of the
                // same
                // variety as phi or select instructions.  We need to update the
                // findBasePointers algorithm to insert new 'base-only' versions
                // of the
                // original instructions. This is relative straight forward to
                // do, but
                // the case which would motivate the work hasn't shown up in
                // real
                // workloads yet.
                assert(
                    (isa<PHINode>(VectorBase) || isa<SelectInst>(VectorBase)) &&
                    "need to extend findBasePointers for generic vector"
                    "instruction cases");
                return VectorBase;
            }
        }
    }

    if (isa<Argument>(I))
        // An incoming argument to the function is a base pointer
        // We should have never reached here if this argument isn't an gc value
        return I;

    if (isa<GlobalVariable>(I))
        // base case
        return I;

    // inlining could possibly introduce phi node that contains
    // undef if callee has multiple returns
    if (isa<UndefValue>(I))
        // utterly meaningless, but useful for dealing with
        // partially optimized code.
        return I;

    // Due to inheritance, this must be _after_ the global variable and undef
    // checks
    if (Constant* Con = dyn_cast<Constant>(I)) {
        assert(!isa<GlobalVariable>(I) && !isa<UndefValue>(I) &&
               "order of checks wrong!");
        // Note: Finding a constant base for something marked for relocation
        // doesn't really make sense.  The most likely case is either a) some
        // screwed up the address space usage or b) your validating against
        // compiled C++ code w/o the proper separation.  The only real exception
        // is a null pointer.  You could have generic code written to index of
        // off a potentially null value and have proven it null.  We also use
        // null pointers in dead paths of relocation phis (which we might later
        // want to find a base pointer for).
        // TODO: find out why this is still not true in all cases
        // assert(isa<ConstantPointerNull>(Con) &&
        //        "null is the only case which makes sense");
        return Con;
    }

    if (CastInst* CI = dyn_cast<CastInst>(I)) {
        Value* Def = CI->stripPointerCasts();
        // If we find a cast instruction here, it means we've found a cast which
        // is
        // not simply a pointer cast (i.e. an inttoptr).  We don't know how to
        // handle int->ptr conversion.
        assert(!isa<CastInst>(Def) && "shouldn't find another cast here");
        return findBaseDefiningValue(Def);
    }

    if (isa<LoadInst>(I))
        return I; // The value loaded is an gc base itself

    if (GetElementPtrInst* GEP = dyn_cast<GetElementPtrInst>(I))
        // The base of this GEP is the base
        return findBaseDefiningValue(GEP->getPointerOperand());

    if (IntrinsicInst* II = dyn_cast<IntrinsicInst>(I)) {
        switch (II->getIntrinsicID()) {
        case Intrinsic::experimental_gc_result_ptr:
        default:
            // fall through to general call handling
            break;
        case Intrinsic::experimental_gc_statepoint:
        case Intrinsic::experimental_gc_result_float:
        case Intrinsic::experimental_gc_result_int:
            llvm_unreachable("these don't produce pointers");
        case Intrinsic::experimental_gc_relocate: {
            // Rerunning safepoint insertion after safepoints are already
            // inserted is not supported.  It could probably be made to work,
            // but why are you doing this?  There's no good reason.
            llvm_unreachable("repeat safepoint insertion is not supported");
        }
        case Intrinsic::gcroot:
            // Currently, this mechanism hasn't been extended to work with
            // gcroot.
            // There's no reason it couldn't be, but I haven't thought about the
            // implications much.
            llvm_unreachable(
                "interaction with the gcroot mechanism is not supported");
        }
    }
    // We assume that functions in the source language only return base
    // pointers.  This should probably be generalized via attributes to support
    // both source language and internal functions.
    if (isa<CallInst>(I) || isa<InvokeInst>(I))
        return I;

    // I have absolutely no idea how to implement this part yet.  It's not
    // neccessarily hard, I just haven't really looked at it yet.
    assert(!isa<LandingPadInst>(I) && "Landing Pad is unimplemented");

    if (isa<AtomicCmpXchgInst>(I))
        // A CAS is effectively a atomic store and load combined under a
        // predicate.  From the perspective of base pointers, we just treat it
        // like a load.
        return I;

    assert(!isa<AtomicRMWInst>(I) &&
           "Xchg handled above, all others are "
           "binary ops which don't apply to pointers");

    // The aggregate ops.  Aggregates can either be in the heap or on the
    // stack, but in either case, this is simply a field load.  As a result,
    // this is a defining definition of the base just like a load is.
    if (isa<ExtractValueInst>(I))
        return I;

    // We should never see an insert vector since that would require we be
    // tracing back a struct value not a pointer value.
    assert(!isa<InsertValueInst>(I) &&
           "Base pointer for a struct is meaningless");

    // The last two cases here don't return a base pointer.  Instead, they
    // return a value which dynamically selects from amoung several base
    // derived pointers (each with it's own base potentially).  It's the job of
    // the caller to resolve these.
    assert((isa<SelectInst>(I) || isa<PHINode>(I)) &&
           "missing instruction case in findBaseDefiningValing");
    return I;
}

// When inserting gc.relocate calls, we need to ensure there are no uses
// of the original value between the gc.statepoint and the gc.relocate call.
// One case which can arise is a phi node starting one of the successor blocks.
// We also need to be able to insert the gc.relocates only on the path which
// goes through the statepoint.  We might need to split an edge to make this
// possible.
static BasicBlock* normalizeForInvokeSafepoint(BasicBlock* BB,
                                               BasicBlock* InvokeParent,
                                               DominatorTree& DT) {
    BasicBlock* Ret = BB;
    if (!BB->getUniquePredecessor()) {
        Ret = SplitBlockPredecessors(BB, InvokeParent, "", nullptr, &DT);
    }

    // Now that 'ret' has unique predecessor we can safely remove all phi nodes
    // from it
    FoldSingleEntryPHINodes(Ret);
    assert(!isa<PHINode>(Ret->begin()));

    // At this point, we can safely insert a gc.relocate as the first
    // instruction
    // in Ret if needed.
    return Ret;
}

// Create new attribute set containing only attributes which can be transfered
// from original call to the safepoint.
static AttributeSet legalizeCallAttributes(AttributeSet AS) {
    AttributeSet ret;

    for (unsigned Slot = 0; Slot < AS.getNumSlots(); Slot++) {
        unsigned index = AS.getSlotIndex(Slot);

        if (index == AttributeSet::ReturnIndex ||
            index == AttributeSet::FunctionIndex) {

            for (auto it = AS.begin(Slot), it_end = AS.end(Slot); it != it_end;
                 ++it) {
                Attribute attr = *it;

                // Do not allow certain attributes - just skip them
                // Safepoint can not be read only or read none.
                if (attr.hasAttribute(Attribute::ReadNone) ||
                    attr.hasAttribute(Attribute::ReadOnly))
                    continue;

                ret =
                    ret.addAttributes(AS.getContext(), index,
                                      AttributeSet::get(AS.getContext(), index,
                                                        AttrBuilder(attr)));
            }
        }

        // Just skip parameter attributes for now
    }

    return ret;
}

static void
makeStatepointExplicitImpl(const CallSite& CS, /* to replace */
                           const SmallVectorImpl<llvm::Value*>& liveVariables,
                           Pass* P,
                           PartiallyConstructedSafepointRecord& result) {
    assert(isStatepoint(CS) &&
           "This method expects to be rewriting a statepoint");

    BasicBlock* BB = CS.getInstruction()->getParent();
    assert(BB);
    Function* F = BB->getParent();
    assert(F && "must be set");
    Module* M = F->getParent();
    (void)M;
    assert(M && "must be set");

    // We're not changing the function signature of the statepoint since the gc
    // arguments go into the var args section.
    Function* gc_statepoint_decl = CS.getCalledFunction();

    // Then go ahead and use the builder do actually do the inserts.  We insert
    // immediately before the previous instruction under the assumption that all
    // arguments will be available here.  We can't insert afterwards since we
    // may
    // be replacing a terminator.
    Instruction* insertBefore = CS.getInstruction();
    IRBuilder<> Builder(insertBefore);
    // Copy all of the arguments from the original statepoint - this includes
    // the
    // target, call args, without deopt args
    SmallVector<llvm::Value*, 64> args;
    args.insert(args.end(), CS.arg_begin(), CS.arg_end());

    // Remove number of deopt params (should be 0)
    args.pop_back();

    ConstantInt* const_numlive = ConstantInt::get(
        Builder.getContext(), APInt(32, liveVariables.size(), false));
    args.push_back(const_numlive);

    // add all the pointers of live vars (gc arguments)
    args.insert(args.end(), liveVariables.begin(), liveVariables.end());

    // Create the statepoint given all the arguments
    Instruction* token = nullptr;
    AttributeSet return_attributes;
    assert(CS.isCall());

    CallInst* toReplace = cast<CallInst>(CS.getInstruction());
    CallInst* call =
        Builder.CreateCall(gc_statepoint_decl, args, "safepoint_token");
    call->setTailCall(toReplace->isTailCall());
    call->setCallingConv(toReplace->getCallingConv());

    // Currently we will fail on parameter attributes and on certain
    // function attributes.
    AttributeSet new_attrs = legalizeCallAttributes(toReplace->getAttributes());
    // In case if we can handle this set of sttributes - set up function
    // attrs
    // directly on statepoint and return attrs later for gc_result
    // intrinsic.
    call->setAttributes(new_attrs.getFnAttributes());
    return_attributes = new_attrs.getRetAttributes();

    token = call;

    // Put the following gc_result and gc_relocate calls immediately after
    // the
    // the old call (which we're about to delete)
    BasicBlock::iterator next(toReplace);
    assert(BB->end() != next && "not a terminator, must have next");
    next++;
    Instruction* IP = &*(next);
    Builder.SetInsertPoint(IP);
    Builder.SetCurrentDebugLocation(IP->getDebugLoc());

    assert(token);

    // Take the name of the original value call if it had one.
    token->takeName(CS.getInstruction());

    {
// The GCResult is already inserted, we just need to find it
#ifndef NDEBUG
        Instruction* toReplace = CS.getInstruction();
        assert((toReplace->hasNUses(0) || toReplace->hasNUses(1)) &&
               "only valid use before rewrite is gc.result");
        assert(!toReplace->hasOneUse() ||
               isGCResult(cast<Instruction>(*toReplace->user_begin())));
#endif
    }

    // Update the gc.result of the original statepoint (if any) to use the newly
    // inserted statepoint.  This is safe to do here since the token can't be
    // considered a live reference.
    CS.getInstruction()->replaceAllUsesWith(token);

    result.StatepointToken = token;
}

namespace {
struct name_ordering {
    Value* derived;
    bool operator()(name_ordering const& a, name_ordering const& b) {
        return -1 == a.derived->getName().compare(b.derived->getName());
    }
};
}
static void stablize_order(SmallVectorImpl<Value*>& livevec) {
    SmallVector<name_ordering, 64> temp;
    for (size_t i = 0; i < livevec.size(); i++) {
        name_ordering v;
        v.derived = livevec[i];
        temp.push_back(v);
    }
    std::sort(temp.begin(), temp.end(), name_ordering());
    for (size_t i = 0; i < livevec.size(); i++) {
        livevec[i] = temp[i].derived;
    }
}

// Replace an existing gc.statepoint with a new one and a set of gc.relocates
// which make the relocations happening at this safepoint explicit.
//
// WARNING: Does not do any fixup to adjust users of the original live
// values.  That's the callers responsibility.
static void
makeStatepointExplicit(DominatorTree& DT, const CallSite& CS, Pass* P,
                       PartiallyConstructedSafepointRecord& result) {
    auto liveset = result.liveset;

    // Convert to vector for efficient cross referencing.
    SmallVector<Value*, 64> livevec;
    livevec.reserve(liveset.size());
    for (Value* L : liveset) {
        livevec.push_back(L);
    }

    // To make the output IR slightly more stable (for use in diffs), ensure a
    // fixed order of the values in the safepoint (by sorting the value name).
    // The order is otherwise meaningless.
    stablize_order(livevec);

    // Do the actual rewriting and delete the old statepoint
    makeStatepointExplicitImpl(CS, livevec, P, result);
    CS.getInstruction()->eraseFromParent();
}

/// Implement a unique function which doesn't require we sort the input
/// vector.  Doing so has the effect of changing the output of a couple of
/// tests in ways which make them less useful in testing fused safepoints.
template <typename T> static void unique_unsorted(SmallVectorImpl<T>& Vec) {
    SmallSet<T, 8> Seen;
    Vec.erase(std::remove_if(Vec.begin(), Vec.end(), [&](const T& V) {
                  return !Seen.insert(V).second;
              }),
              Vec.end());
}

/// Insert holders so that each Value is obviously live through the entire
/// lifetime of the call.
static void insertUseHolderAfter(CallSite& CS, const ArrayRef<Value*> Values,
                                 SmallVectorImpl<CallInst*>& Holders) {
    if (Values.empty())
        // No values to hold live, might as well not insert the empty holder
        return;

    Module* M = CS.getInstruction()->getParent()->getParent()->getParent();
    // Use a dummy vararg function to actually hold the values live
    Function* Func = cast<Function>(M->getOrInsertFunction(
        "__tmp_use",
        FunctionType::get(Type::getVoidTy(M->getContext()), true)));
    if (CS.isCall()) {
        // For call safepoints insert dummy calls right after safepoint
        BasicBlock::iterator Next(CS.getInstruction());
        Next++;
        Holders.push_back(CallInst::Create(Func, Values, "", Next));
        return;
    }
    // For invoke safepooints insert dummy calls both in normal and
    // exceptional destination blocks
    auto* II = cast<InvokeInst>(CS.getInstruction());
    Holders.push_back(CallInst::Create(
        Func, Values, "", II->getNormalDest()->getFirstInsertionPt()));
    Holders.push_back(CallInst::Create(
        Func, Values, "", II->getUnwindDest()->getFirstInsertionPt()));
}

static void findLiveReferences(
    Function& F, DominatorTree& DT, Pass* P, ArrayRef<CallSite> toUpdate,
    MutableArrayRef<struct PartiallyConstructedSafepointRecord> records) {
    GCPtrLivenessData OriginalLivenessData;
    computeLiveInValues(DT, F, OriginalLivenessData);
    for (size_t i = 0; i < records.size(); i++) {
        struct PartiallyConstructedSafepointRecord& info = records[i];
        const CallSite& CS = toUpdate[i];
        analyzeParsePointLiveness(DT, OriginalLivenessData, CS, info);
    }
}

static bool insertParsePoints(Function& F, DominatorTree& DT, Pass* P,
                              SmallVectorImpl<CallSite>& toUpdate) {
#ifndef NDEBUG
    // sanity check the input
    std::set<CallSite> uniqued;
    uniqued.insert(toUpdate.begin(), toUpdate.end());
    assert(uniqued.size() == toUpdate.size() && "no duplicates please!");

    for (size_t i = 0; i < toUpdate.size(); i++) {
        CallSite& CS = toUpdate[i];
        assert(CS.getInstruction()->getParent()->getParent() == &F);
        assert(isStatepoint(CS) && "expected to already be a deopt statepoint");
    }
#endif

    // When inserting gc.relocates for invokes, we need to be able to insert at
    // the top of the successor blocks.  See the comment on
    // normalForInvokeSafepoint on exactly what is needed.  Note that this step
    // may restructure the CFG.
    for (CallSite CS : toUpdate) {
        if (!CS.isInvoke())
            continue;
        InvokeInst* invoke = cast<InvokeInst>(CS.getInstruction());
        normalizeForInvokeSafepoint(invoke->getNormalDest(),
                                    invoke->getParent(), DT);
        normalizeForInvokeSafepoint(invoke->getUnwindDest(),
                                    invoke->getParent(), DT);
    }

    // A list of dummy calls added to the IR to keep various values obviously
    // live in the IR.  We'll remove all of these when done.
    SmallVector<CallInst*, 64> holders;

    // Insert a dummy call with all of the arguments to the vm_state we'll need
    // for the actual safepoint insertion.  This ensures reference arguments in
    // the deopt argument list are considered live through the safepoint (and
    // thus makes sure they get relocated.)
    for (size_t i = 0; i < toUpdate.size(); i++) {
        CallSite& CS = toUpdate[i];
        Statepoint StatepointCS(CS);

        SmallVector<Value*, 64> DeoptValues;
        for (Use& U : StatepointCS.vm_state_args()) {
            Value* Arg = cast<Value>(&U);
            if (isGCPointer(Arg))
                DeoptValues.push_back(Arg);
        }
        insertUseHolderAfter(CS, DeoptValues, holders);
    }

    SmallVector<struct PartiallyConstructedSafepointRecord, 64> records;
    records.reserve(toUpdate.size());
    for (size_t i = 0; i < toUpdate.size(); i++) {
        struct PartiallyConstructedSafepointRecord info;
        records.push_back(info);
    }
    assert(records.size() == toUpdate.size());

    // A) Identify all gc pointers which are staticly live at the given call
    // site.
    findLiveReferences(F, DT, P, toUpdate, records);

    // Now run through and replace the existing statepoints with new ones with
    // the live variables listed.  We do not yet update uses of the values being
    // relocated. We have references to live variables that need to
    // survive to the last iteration of this loop.  (By construction, the
    // previous statepoint can not be a live variable, thus we can and remove
    // the old statepoint calls as we go.)
    for (size_t i = 0; i < records.size(); i++) {
        struct PartiallyConstructedSafepointRecord& info = records[i];
        CallSite& CS = toUpdate[i];
        makeStatepointExplicit(DT, CS, P, info);
    }
    toUpdate.clear(); // prevent accident use of invalid CallSites
    return false;
}

// Handles both return values and arguments for Functions and CallSites.
template <typename AttrHolder>
static void RemoveDerefAttrAtIndex(LLVMContext& Ctx, AttrHolder& AH,
                                   unsigned Index) {
    AttrBuilder R;
    if (AH.getDereferenceableBytes(Index))
        R.addAttribute(Attribute::get(Ctx, Attribute::Dereferenceable,
                                      AH.getDereferenceableBytes(Index)));
    if (AH.getDereferenceableOrNullBytes(Index))
        R.addAttribute(Attribute::get(Ctx, Attribute::DereferenceableOrNull,
                                      AH.getDereferenceableOrNullBytes(Index)));

    if (!R.empty())
        AH.setAttributes(AH.getAttributes().removeAttributes(
            Ctx, Index, AttributeSet::get(Ctx, Index, R)));
}

void
RewriteStatepointsForGC::stripDereferenceabilityInfoFromPrototype(Function& F) {
    LLVMContext& Ctx = F.getContext();

    for (Argument& A : F.args())
        if (isa<PointerType>(A.getType()))
            RemoveDerefAttrAtIndex(Ctx, F, A.getArgNo() + 1);

    if (isa<PointerType>(F.getReturnType()))
        RemoveDerefAttrAtIndex(Ctx, F, AttributeSet::ReturnIndex);
}

void RewriteStatepointsForGC::stripDereferenceabilityInfoFromBody(Function& F) {
    if (F.empty())
        return;

    LLVMContext& Ctx = F.getContext();
    MDBuilder Builder(Ctx);

    for (Instruction& I : inst_range(F)) {
        if (const MDNode* MD = I.getMetadata(LLVMContext::MD_tbaa)) {
            assert(MD->getNumOperands() < 5 && "unrecognized metadata shape!");
            bool IsImmutableTBAA = MD->getNumOperands() == 4 &&
                                   mdconst::extract<ConstantInt>(
                                       MD->getOperand(3))->getValue() == 1;

            if (!IsImmutableTBAA)
                continue; // no work to do, MD_tbaa is already marked mutable

            MDNode* Base = cast<MDNode>(MD->getOperand(0));
            MDNode* Access = cast<MDNode>(MD->getOperand(1));
            uint64_t Offset = mdconst::extract<ConstantInt>(MD->getOperand(2))
                                  ->getZExtValue();

            MDNode* MutableTBAA =
                Builder.createTBAAStructTagNode(Base, Access, Offset);
            I.setMetadata(LLVMContext::MD_tbaa, MutableTBAA);
        }

        if (CallSite CS = CallSite(&I)) {
            for (int i = 0, e = CS.arg_size(); i != e; i++)
                if (isa<PointerType>(CS.getArgument(i)->getType()))
                    RemoveDerefAttrAtIndex(Ctx, CS, i + 1);
            if (isa<PointerType>(CS.getType()))
                RemoveDerefAttrAtIndex(Ctx, CS, AttributeSet::ReturnIndex);
        }
    }
}

/// Returns true if this function should be rewritten by this pass.  The main
/// point of this function is as an extension point for custom logic.
static bool shouldRewriteStatepointsIn(Function& F) {
    // TODO: This should check the GCStrategy
    if (F.hasGC()) {
        const char* FunctionGCName = F.getGC();
        return FunctionGCName == rjit::JITStatepointGC::name();
    } else
        return false;
}

void RewriteStatepointsForGC::stripDereferenceabilityInfo(Module& M) {
#ifndef NDEBUG
    assert(std::any_of(M.begin(), M.end(), shouldRewriteStatepointsIn) &&
           "precondition!");
#endif

    for (Function& F : M)
        stripDereferenceabilityInfoFromPrototype(F);

    for (Function& F : M)
        stripDereferenceabilityInfoFromBody(F);
}

bool RewriteStatepointsForGC::runOnFunction(Function& F) {
    // Nothing to do for declarations.
    if (F.isDeclaration() || F.empty())
        return false;

    // Policy choice says not to rewrite - the most common reason is that we're
    // compiling code without a GCStrategy.
    if (!shouldRewriteStatepointsIn(F))
        return false;

    DominatorTree& DT = getAnalysis<DominatorTreeWrapperPass>(F).getDomTree();

    // Gather all the statepoints which need rewritten.  Be careful to only
    // consider those in reachable code since we need to ask dominance queries
    // when rewriting.  We'll delete the unreachable ones in a moment.
    SmallVector<CallSite, 64> ParsePointNeeded;
    bool HasUnreachableStatepoint = false;
    for (Instruction& I : inst_range(F)) {
        // TODO: only the ones with the flag set!
        if (isStatepoint(I)) {
            if (DT.isReachableFromEntry(I.getParent()))
                ParsePointNeeded.push_back(CallSite(&I));
            else
                HasUnreachableStatepoint = true;
        }
    }

    bool MadeChange = false;

    // Delete any unreachable statepoints so that we don't have unrewritten
    // statepoints surviving this pass.  This makes testing easier and the
    // resulting IR less confusing to human readers.  Rather than be fancy, we
    // just reuse a utility function which removes the unreachable blocks.
    if (HasUnreachableStatepoint)
        MadeChange |= removeUnreachableBlocks(F);

    // Return early if no work to do.
    if (ParsePointNeeded.empty())
        return MadeChange;

    // As a prepass, go ahead and aggressively destroy single entry phi nodes.
    // These are created by LCSSA.  They have the effect of increasing the size
    // of liveness sets for no good reason.  It may be harder to do this post
    // insertion since relocations and base phis can confuse things.
    for (BasicBlock& BB : F)
        if (BB.getUniquePredecessor()) {
            MadeChange = true;
            FoldSingleEntryPHINodes(&BB);
        }

    MadeChange |= insertParsePoints(F, DT, this, ParsePointNeeded);
    return MadeChange;
}

// liveness computation via standard dataflow
// -------------------------------------------------------------------

// TODO: Consider using bitvectors for liveness, the set of potentially
// interesting values should be small and easy to pre-compute.

/// Compute the live-in set for the location rbegin starting from
/// the live-out set of the basic block
static void computeLiveInValues(BasicBlock::reverse_iterator rbegin,
                                BasicBlock::reverse_iterator rend,
                                DenseSet<Value*>& LiveTmp) {

    for (BasicBlock::reverse_iterator ritr = rbegin; ritr != rend; ritr++) {
        Instruction* I = &*ritr;

        // KILL/Def - Remove this definition from LiveIn
        LiveTmp.erase(I);

        // Don't consider *uses* in PHI nodes, we handle their contribution to
        // predecessor blocks when we seed the LiveOut sets
        if (isa<PHINode>(I))
            continue;

        // USE - Add to the LiveIn set for this instruction
        for (Value* V : I->operands()) {
            if (isGCPointer(V) && !isa<Constant>(V)) {
                // The choice to exclude all things constant here is slightly
                // subtle.
                // There are two idependent reasons:
                // - We assume that things which are constant (from LLVM's
                // definition)
                // do not move at runtime.  For example, the address of a global
                // variable is fixed, even though it's contents may not be.
                // - Second, we can't disallow arbitrary inttoptr constants even
                // if the language frontend does.  Optimization passes are free
                // to
                // locally exploit facts without respect to global reachability.
                // This
                // can create sections of code which are dynamically unreachable
                // and
                // contain just about anything.  (see constants.ll in tests)
                LiveTmp.insert(V);
            }
        }
    }
}

static void computeLiveOutSeed(BasicBlock* BB, DenseSet<Value*>& LiveTmp) {

    for (BasicBlock* Succ : successors(BB)) {
        const BasicBlock::iterator E(Succ->getFirstNonPHI());
        for (BasicBlock::iterator I = Succ->begin(); I != E; I++) {
            PHINode* Phi = cast<PHINode>(&*I);
            Value* V = Phi->getIncomingValueForBlock(BB);
            if (isGCPointer(V) && !isa<Constant>(V)) {
                LiveTmp.insert(V);
            }
        }
    }
}

static DenseSet<Value*> computeKillSet(BasicBlock* BB) {
    DenseSet<Value*> KillSet;
    for (Instruction& I : *BB)
        if (isGCPointer(&I))
            KillSet.insert(&I);
    return KillSet;
}

#ifndef NDEBUG
/// Check that the items in 'Live' dominate 'TI'.  This is used as a basic
/// sanity check for the liveness computation.
static void checkBasicSSA(DominatorTree& DT, DenseSet<Value*>& Live,
                          TerminatorInst* TI, bool TermOkay = false) {
    for (Value* V : Live) {
        if (auto* I = dyn_cast<Instruction>(V)) {
            // The terminator can be a member of the LiveOut set.  LLVM's
            // definition
            // of instruction dominance states that V does not dominate itself.
            // As
            // such, we need to special case this to allow it.
            if (TermOkay && TI == I)
                continue;
            assert(
                DT.dominates(I, TI) &&
                "basic SSA liveness expectation violated by liveness analysis");
        }
    }
}

/// Check that all the liveness sets used during the computation of liveness
/// obey basic SSA properties.  This is useful for finding cases where we miss
/// a def.
static void checkBasicSSA(DominatorTree& DT, GCPtrLivenessData& Data,
                          BasicBlock& BB) {
    checkBasicSSA(DT, Data.LiveSet[&BB], BB.getTerminator());
    checkBasicSSA(DT, Data.LiveOut[&BB], BB.getTerminator(), true);
    checkBasicSSA(DT, Data.LiveIn[&BB], BB.getTerminator());
}
#endif

static void computeLiveInValues(DominatorTree& DT, Function& F,
                                GCPtrLivenessData& Data) {

    SmallSetVector<BasicBlock*, 200> Worklist;
    auto AddPredsToWorklist = [&](BasicBlock* BB) {
        // We use a SetVector so that we don't have duplicates in the worklist.
        Worklist.insert(pred_begin(BB), pred_end(BB));
    };
    auto NextItem = [&]() {
        BasicBlock* BB = Worklist.back();
        Worklist.pop_back();
        return BB;
    };

    // Seed the liveness for each individual block
    for (BasicBlock& BB : F) {
        Data.KillSet[&BB] = computeKillSet(&BB);
        Data.LiveSet[&BB].clear();
        computeLiveInValues(BB.rbegin(), BB.rend(), Data.LiveSet[&BB]);

#ifndef NDEBUG
        for (Value* Kill : Data.KillSet[&BB])
            assert(!Data.LiveSet[&BB].count(Kill) && "live set contains kill");
#endif

        Data.LiveOut[&BB] = DenseSet<Value*>();
        computeLiveOutSeed(&BB, Data.LiveOut[&BB]);
        Data.LiveIn[&BB] = Data.LiveSet[&BB];
        set_union(Data.LiveIn[&BB], Data.LiveOut[&BB]);
        set_subtract(Data.LiveIn[&BB], Data.KillSet[&BB]);
        if (!Data.LiveIn[&BB].empty())
            AddPredsToWorklist(&BB);
    }

    // Propagate that liveness until stable
    while (!Worklist.empty()) {
        BasicBlock* BB = NextItem();

        // Compute our new liveout set, then exit early if it hasn't changed
        // despite the contribution of our successor.
        DenseSet<Value*> LiveOut = Data.LiveOut[BB];
        const auto OldLiveOutSize = LiveOut.size();
        for (BasicBlock* Succ : successors(BB)) {
            assert(Data.LiveIn.count(Succ));
            set_union(LiveOut, Data.LiveIn[Succ]);
        }
        // assert OutLiveOut is a subset of LiveOut
        if (OldLiveOutSize == LiveOut.size()) {
            // If the sets are the same size, then we didn't actually add
            // anything
            // when unioning our successors LiveIn  Thus, the LiveIn of this
            // block
            // hasn't changed.
            continue;
        }
        Data.LiveOut[BB] = LiveOut;

        // Apply the effects of this basic block
        DenseSet<Value*> LiveTmp = LiveOut;
        set_union(LiveTmp, Data.LiveSet[BB]);
        set_subtract(LiveTmp, Data.KillSet[BB]);

        assert(Data.LiveIn.count(BB));
        const DenseSet<Value*>& OldLiveIn = Data.LiveIn[BB];
        // assert: OldLiveIn is a subset of LiveTmp
        if (OldLiveIn.size() != LiveTmp.size()) {
            Data.LiveIn[BB] = LiveTmp;
            AddPredsToWorklist(BB);
        }
    } // while( !worklist.empty() )

#ifndef NDEBUG
    // Sanity check our ouput against SSA properties.  This helps catch any
    // missing kills during the above iteration.
    for (BasicBlock& BB : F) {
        checkBasicSSA(DT, Data, BB);
    }
#endif
}

static void findLiveSetAtInst(Instruction* Inst, GCPtrLivenessData& Data,
                              StatepointLiveSetTy& Out) {

    BasicBlock* BB = Inst->getParent();

    // Note: The copy is intentional and required
    assert(Data.LiveOut.count(BB));
    DenseSet<Value*> LiveOut = Data.LiveOut[BB];

    // We want to handle the statepoint itself oddly.  It's
    // call result is not live (normal), nor are it's arguments
    // (unless they're used again later).  This adjustment is
    // specifically what we need to relocate
    BasicBlock::reverse_iterator rend(Inst);
    computeLiveInValues(BB->rbegin(), rend, LiveOut);
    LiveOut.erase(Inst);
    Out.insert(LiveOut.begin(), LiveOut.end());
}
