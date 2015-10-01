#ifndef IR_H
#define IR_H

#include "llvm.h"

#include "RIntlns.h"

#include "Builder.h"

/** \file "rjit/src/ir/ir.h"
 */

namespace rjit {
namespace ir {

/** Type of the IR.
 */
enum class Type {
    ConvertToLogicalNoNA,
    PrintValue,
    StartFor,
    LoopSequenceLength,
    GetForLoopValue,
    MarkVisible,
    MarkInvisible,
    UserConstant,
    GenericGetVar,
    GenericGetEllipsisArg,
    GenericSetVar,
    GenericSetVarParent,
    GetFunction,
    GetGlobalFunction,
    GetSymFunction,
    GetBuiltinFunction,
    GetInternalBuiltinFunction,
    CheckFunction,
    CreatePromise,
    SexpType,
    AddArgument,
    AddKeywordArgument,
    AddEllipsisArgument,
    CallBuiltin,
    CallSpecial,
    CallClosure,
    CreateClosure,
    GenericUnaryMinus,
    GenericUnaryPlus,
    GenericAdd,
    GenericSub,
    GenericMul,
    GenericDiv,
    GenericPow,
    GenericSqrt,
    GenericExp,
    GenericEq,
    GenericNe,
    GenericLt,
    GenericLe,
    GenericGe,
    GenericGt,
    GenericBitAnd,
    GenericBitOr,
    GenericNot,
    GenericGetVarMissOK,
    GenericGetEllipsisValueMissOK,
    CheckSwitchControl,
    SwitchControlCharacter,
    SwitchControlInteger,
    ReturnJump,
    Return,
    Branch,
    Cbr,
    unknown,
};

/** Generic class for all IR objects.

  They all must point to an existing llvm value.
 */
class Instruction {
public:

    /** Returns the IR type of the instruction sequence starting at i and advances i past it. Returns Type::unknown if the sequence start cannot be matched and advances one instruction further.
     */
    static Type match(llvm::BasicBlock::iterator & i);


    /** Each ir instruction can typecast to the underlying llvm bitcode.
     */
    operator llvm::Instruction * () {
        return ins_;
    }

    Instruction(llvm::Instruction * ins):
        ins_(ins) {
    }

protected:

    template<typename T>
    T * ins() {
        return llvm::cast<T>(ins_);
    }

private:
    llvm::Instruction * ins_;

};

class Return : public Instruction {
public:
    Return(llvm::Instruction * ins):
        Instruction(ins) {
        assert(llvm::isa<llvm::ReturnInst>(ins) and "Return expected");
    }

    llvm::Value * result() {
        return ins<llvm::ReturnInst>()->getOperand(0);
    }

    static Return create(Builder & b, llvm::Value * value) {
        llvm::ReturnInst::Create(llvm::getGlobalContext(),value, b);
    }
};

class Branch : public Instruction {
public:
    Branch(llvm::Instruction * ins):
        Instruction(ins) {
        assert(llvm::isa<llvm::BranchInst>(ins) and "Branch instruction expected");
        assert(not llvm::cast<llvm::BranchInst>(ins)->isConditional() and "Branch must be unconditional");
    }

    llvm::BasicBlock * target() {
        return ins<llvm::BranchInst>()->getSuccessor(0);
    }

    static Branch create(Builder & b, llvm::BasicBlock * target) {
        return llvm::BranchInst::Create(target, b);
    }
};


/** Conditional branch.

  Takes three arguments, the condition on which it jumps (this can be any integer) and true and false blocks.

  Conditional branch consists of ICmpInst followed by BranchInst internally.

  TODO We might want to change this in the future and get the comparison out of the branch, but for now, this is the only branch we have and it is a showcase for matching multiple llvm bitcodes to single ir.
 */
class Cbr : public Instruction {
public:
    llvm::Value * cond() {
        return ins<llvm::ICmpInst>()->getOperand(0);
    }

    llvm::BasicBlock * trueCase() {
        llvm::BranchInst * b = llvm::cast<llvm::BranchInst>(ins<llvm::ICmpInst>()->getNextNode());
        return b->getSuccessor(1);
    }

    llvm::BasicBlock * falseCase() {
        llvm::BranchInst * b = llvm::cast<llvm::BranchInst>(ins<llvm::ICmpInst>()->getNextNode());
        return b->getSuccessor(0);
    }

    Cbr(llvm::Instruction * ins):
        Instruction(ins) {
    }

    static void create(Builder & b, llvm::Value * cond, llvm::BasicBlock * trueCase, llvm::BasicBlock * falseCase);

};

/** Base class for all intrinsics.

 */
class Intrinsic : public Instruction {
public:
    /** LLVM metadata kind for the ir type associated with the CallInsts for faster matching.
     */
    static char const * const MD_NAME;

    /** Returns the IR type of the intrinsic call for faster matching.
     */
    static Type getIRType(llvm::Instruction * ins) {
        llvm::MDNode * m = ins->getMetadata(MD_NAME);
        if (m == nullptr)
            return Type::unknown;
        llvm::Metadata * mx = m->getOperand(0);
        llvm::APInt const & ap = llvm::cast<llvm::ConstantInt>(llvm::cast<llvm::ValueAsMetadata>(mx)->getValue())->getUniqueInteger();
        assert(ap.isIntN(32) and "Expected 32bit integer");
        return static_cast<Type>(ap.getSExtValue());
    }

    /** Returns the CallInst associated with the intrinsic.
     */
    llvm::CallInst * ins() {
        return Instruction::ins<llvm::CallInst>();
    }

protected:

    Intrinsic(llvm::Instruction * ins):
        Instruction(ins) {
        assert(llvm::isa<llvm::CallInst>(ins) and "Intrinsics must be llvm calls");
    }

    /** Sets the ir kind for the CallInst.

      It is assumed that this method will be called by the respective intrinsics when they are being created.
     */
    static void setIRType(llvm::CallInst * ins, Type t) {
        std::vector<llvm::Metadata *> v = { llvm::ValueAsMetadata::get(llvm::ConstantInt::get(ins->getContext(), llvm::APInt(32, static_cast<int>(t)))) };
        llvm::MDNode * m = llvm::MDNode::get(ins->getContext(), v);
        ins->setMetadata(MD_NAME, m);
    }

    llvm::Value * getValue(unsigned argIndex) {
        return ins()->getArgOperand(argIndex);
    }

    SEXP getValueSEXP(unsigned argIndex) {
        return Builder::constantPoolSexp(ins()->getArgOperand(argIndex));
    }

    int getValueInt(unsigned argIndex) {
        return Builder::integer(ins()->getArgOperand(argIndex));
    }

};

} // namespace ir

} // namespace rjit


#endif // IR_H

