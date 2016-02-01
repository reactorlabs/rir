#ifndef IR_H
#define IR_H

#include "llvm.h"

#include <cstdint>

#include "RIntlns.h"

#include "Builder.h"

/** \file "rjit/src/ir/Ir.h"
 */

namespace rjit {
namespace ir {

/** Type of the IR.
 */
/** Generic class for all IR objects.

  They all must point to an existing llvm value.
 */
class Pattern {
  public:
    static char const* const MD_NAME;

    /** Depending on how we want the RTTI to behave, either put only leaves
     * (that is actual instructions the user might see) in here, or put them
     * all. I would be in favour of the first option. THe thing we want is not a
     * real RTTI in the end.
     */
    enum PatternKind {
        Invalid,
        ExtractConstantPool,
        UserLiteral,
        Constant,
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
        AddEllipsisArgumentHead,
        AddEllipsisArgumentTail,
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
        VectorGetElement,
        MarkNotMutable,
        IntegerLessThan,
        IntegerEquals,
        UnsignedIntegerLessThan,
        InitClosureContext,
        Switch,
        IntegerAdd,
        EndClosureContext,
        ClosureQuickArgumentAdaptor,
        ClosureNativeCallTrampoline,
        CallNative,
        CAR,
        CDR,
        TAG,
        ConsNr,
        NewEnv,
        unknown,
    };

    /** Returns the IR type of the intrinsic call for faster matching.
     */
    static Pattern* getIR(llvm::Instruction* ins);

    /** Returns the IR type of the instruction sequence starting at i and
     * advances i past it. Returns Type::unknown if the sequence start cannot be
     * matched and advances one instruction further.
     */
    static Pattern* match(llvm::BasicBlock::iterator& i);

    static bool isInstruction(llvm::Instruction* i);

    llvm::Instruction* start() { return start_; }
    virtual llvm::Instruction* end() { return start(); }
    virtual llvm::Instruction* r() { return start(); }

    PatternKind getKind() const {
        assert(kind_ != PatternKind::Invalid);
        return kind_;
    }

  protected:
    template <typename T>
    T* ins() {
        return llvm::cast<T>(start());
    }

    Pattern(llvm::Instruction* start, PatternKind kind)
        : start_(start), kind_(kind) {
        assert(kind != PatternKind::Invalid);
        setIR(start, this);
    }

  private:
    /** Sets the ir kind.
     */
    static void setIR(llvm::Instruction* llvmIns, rjit::ir::Pattern* rjitIns) {
        assert(rjitIns);
        std::vector<llvm::Metadata*> v = {
            llvm::ValueAsMetadata::get(llvm::ConstantInt::get(
                llvmIns->getContext(),
                llvm::APInt(64, reinterpret_cast<std::uintptr_t>(rjitIns))))};
        llvm::MDNode* m = llvm::MDNode::get(llvmIns->getContext(), v);
        llvmIns->setMetadata(MD_NAME, m);
        assert(getIR(llvmIns));
    }

    Pattern(const Pattern&) = delete;

    llvm::Instruction* start_;
    PatternKind kind_;
};

class MultiPattern : public Pattern {
  public:
    llvm::Instruction* end() override { return end_; }

    llvm::Instruction* r() override {
        assert(false and "missing implementation");
    }

  protected:
    MultiPattern(llvm::Instruction* start, llvm::Instruction* end,
                 PatternKind kind)
        : Pattern(start, kind), end_(end) {}

  private:
    llvm::Instruction* end_;
};

class Return : public Pattern {
  public:
    Return(llvm::Instruction* ins) : Pattern(ins, PatternKind::Return) {
        assert(llvm::isa<llvm::ReturnInst>(ins) and "Return expected");
    }

    llvm::Value* value() { return ins<llvm::ReturnInst>()->getOperand(0); }

    static Return* create(Builder& b, llvm::Value* value) {
        return new Return(
            llvm::ReturnInst::Create(llvm::getGlobalContext(), value, b));
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::Return;
    }
};

class Branch : public Pattern {
  public:
    Branch(llvm::Instruction* ins) : Pattern(ins, PatternKind::Branch) {
        assert(llvm::isa<llvm::BranchInst>(ins) and
               "Branch instruction expected");
        assert(not llvm::cast<llvm::BranchInst>(ins)->isConditional() and
               "Branch must be unconditional");
    }

    llvm::BasicBlock* target() {
        return ins<llvm::BranchInst>()->getSuccessor(0);
    }

    static Branch* create(Builder& b, llvm::BasicBlock* target) {
        return new Branch(llvm::BranchInst::Create(target, b));
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::Branch;
    }
};

class IntegerComparison : public Pattern {
  public:
    typedef llvm::ICmpInst::Predicate Predicate;

    IntegerComparison(llvm::Instruction* ins, PatternKind kind)
        : Pattern(ins, kind) {
        assert(llvm::isa<llvm::ICmpInst>(ins) and "ICmpInst expected");
    }

    Predicate predicate() {
        return ins<llvm::ICmpInst>()->getSignedPredicate();
    }

    llvm::Value* lhs() { return ins<llvm::ICmpInst>()->getOperand(0); }

    llvm::Value* rhs() { return ins<llvm::ICmpInst>()->getOperand(1); }
};

class IntegerLessThan : public IntegerComparison {
  public:
    IntegerLessThan(llvm::Instruction* ins)
        : IntegerComparison(ins, PatternKind::IntegerLessThan) {
        assert(llvm::cast<llvm::ICmpInst>(ins)->getSignedPredicate() ==
                   Predicate::ICMP_SLT and
               "Less than comparison expected");
    }

    static IntegerLessThan* create(Builder& b, llvm::Value* lhs,
                                   llvm::Value* rhs) {
        return new IntegerLessThan(
            new llvm::ICmpInst(*b.block(), Predicate::ICMP_SLT, lhs, rhs));
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::IntegerLessThan;
    }
};

class IntegerEquals : public IntegerComparison {
  public:
    IntegerEquals(llvm::Instruction* ins)
        : IntegerComparison(ins, PatternKind::IntegerEquals) {
        assert(llvm::cast<llvm::ICmpInst>(ins)->getSignedPredicate() ==
                   Predicate::ICMP_EQ and
               "Equality comparison expected");
    }

    static llvm::ICmpInst* create(Builder& b, llvm::Value* lhs,
                                  llvm::Value* rhs) {
        return new llvm::ICmpInst(*b.block(), Predicate::ICMP_EQ, lhs, rhs);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::IntegerEquals;
    }
};

class UnsignedIntegerLessThan : public IntegerComparison {
  public:
    UnsignedIntegerLessThan(llvm::Instruction* ins)
        : IntegerComparison(ins, PatternKind::UnsignedIntegerLessThan) {
        assert(llvm::cast<llvm::ICmpInst>(ins)->getSignedPredicate() ==
                   Predicate::ICMP_ULT and
               "Unsigned less than comparison expected");
    }

    static llvm::ICmpInst* create(Builder& b, llvm::Value* lhs,
                                  llvm::Value* rhs) {
        return new llvm::ICmpInst(*b.block(), Predicate::ICMP_ULT, lhs, rhs);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::UnsignedIntegerLessThan;
    }
};

// TODO the hierarchy of this is wrong, but actual thought is required to fix it
class BinaryOperator : public Pattern {
  public:
    BinaryOperator(llvm::Instruction* ins, PatternKind kind)
        : Pattern(ins, kind) {}
};

// TODO the hierarchy here should be better as well
class IntegerAdd : public BinaryOperator {
  public:
    IntegerAdd(llvm::Instruction* ins)
        : BinaryOperator(ins, PatternKind::IntegerAdd) {
        assert(llvm::isa<llvm::BinaryOperator>(ins) and
               "Binary operator expected");
        assert(llvm::cast<llvm::BinaryOperator>(ins)->getOpcode() ==
                   llvm::Instruction::Add and
               "Add opcode expected");
    }

    llvm::Value* lhs() { return ins<llvm::ICmpInst>()->getOperand(0); }

    llvm::Value* rhs() { return ins<llvm::ICmpInst>()->getOperand(1); }

    static IntegerAdd* create(Builder& b, llvm::Value* lhs, llvm::Value* rhs) {
        return new IntegerAdd(llvm::BinaryOperator::Create(
            llvm::Instruction::Add, lhs, rhs, "", b));
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::IntegerAdd;
    }
};

/** Conditional branch.

  Takes three arguments, the condition on which it jumps
  (this can be any integer) and true and false blocks.

  Conditional branch consists of ICmpInst followed by BranchInst internally.

  TODO We might want to change this in the future and get
  the comparison out of the branch, but for now, this is the
  only branch we have and it is a showcase for matching multiple
  llvm bitcodes to single ir.
 */
class Cbr : public MultiPattern {
  public:
    llvm::Value* cond() { return ins<llvm::ICmpInst>()->getOperand(0); }

    llvm::BasicBlock* trueCase() {
        llvm::BranchInst* b =
            llvm::cast<llvm::BranchInst>(ins<llvm::ICmpInst>()->getNextNode());
        return b->getSuccessor(0);
    }

    llvm::BasicBlock* falseCase() {
        llvm::BranchInst* b =
            llvm::cast<llvm::BranchInst>(ins<llvm::ICmpInst>()->getNextNode());
        return b->getSuccessor(1);
    }

    Cbr(llvm::Instruction* cond, llvm::Instruction* branch)
        : MultiPattern(cond, branch, PatternKind::Cbr) {}

    static void create(Builder& b, llvm::Value* cond,
                       llvm::BasicBlock* trueCase, llvm::BasicBlock* falseCase);

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::Cbr;
    }
};

class MarkNotMutable : public MultiPattern {
  public:
    MarkNotMutable(llvm::Instruction* start, llvm::Instruction* end)
        : MultiPattern(start, end, PatternKind::MarkNotMutable) {}

    static void create(llvm::Instruction* insert, LLVMContext& c,
                       llvm::Value* val);

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::MarkNotMutable;
    }
};

class Car : public MultiPattern {
  public:
    Car(llvm::Instruction* start, llvm::Instruction* result)
        : MultiPattern(start, result, PatternKind::CAR) {}

    static Car* create(Builder& b, llvm::Value* sexp);

    llvm::Instruction* r() override { return end(); }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::CAR;
    }
};

class Cdr : public MultiPattern {
  public:
    Cdr(llvm::Instruction* start, llvm::Instruction* result)
        : MultiPattern(start, result, PatternKind::CAR) {}

    static Cdr* create(Builder& b, llvm::Value* sexp);

    llvm::Instruction* r() override { return end(); }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::CDR;
    }
};

class Tag : public MultiPattern {
  public:
    Tag(llvm::Instruction* start, llvm::Instruction* result)
        : MultiPattern(start, result, PatternKind::TAG) {}

    static Tag* create(Builder& b, llvm::Value* sexp);

    llvm::Instruction* r() override { return end(); }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::TAG;
    }
};

class VectorGetElement : public MultiPattern {
  public:
    VectorGetElement(llvm::Instruction* start, llvm::Instruction* result)
        : MultiPattern(start, result, PatternKind::VectorGetElement) {}

    static VectorGetElement* create(llvm::Instruction* insert, LLVMContext& c,
                                    llvm::Value* vector, llvm::Value* index);

    llvm::Instruction* r() override { return end(); }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::VectorGetElement;
    }
};

/** Interface to llvm's switch instruction
  */
class Switch : public Pattern {
  public:
    Switch(llvm::Instruction* ins) : Pattern(ins, PatternKind::Switch) {
        assert(llvm::isa<llvm::SwitchInst>(ins) and
               "Expecting llvm's switch instruction");
    }

    void addCase(int i, llvm::BasicBlock* target) {
        ins<llvm::SwitchInst>()->addCase(Builder::integer(i), target);
    }

    // TODO add meaningful accessors

    static Switch* create(Builder& b, llvm::Value* cond,
                          llvm::BasicBlock* defaultTarget, int numCases) {
        return new Switch(
            llvm::SwitchInst::Create(cond, defaultTarget, numCases, b));
    }

    void setDefaultDest(llvm::BasicBlock* target) {
        ins<llvm::SwitchInst>()->setDefaultDest(target);
    }

    llvm::BasicBlock* getDefaultDest() {
        return ins<llvm::SwitchInst>()->getDefaultDest();
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == PatternKind::Switch;
    }
};

/** Base class for all intrinsics.

 */
class Intrinsic : public Pattern {
  public:
    /** Returns the CallInst associated with the intrinsic.
     */
    llvm::CallInst* ins() { return Pattern::ins<llvm::CallInst>(); }

  protected:
    Intrinsic(llvm::Instruction* ins, PatternKind kind) : Pattern(ins, kind) {
        assert(llvm::isa<llvm::CallInst>(ins) and
               "Intrinsics must be llvm calls");
    }

    llvm::Value* getValue(unsigned argIndex) {
        return ins()->getArgOperand(argIndex);
    }

    SEXP getValueSEXP(unsigned argIndex) {
        assert(false and "NOT IMPLEMENTED");
    }

    int getValueInt(unsigned argIndex) {
        return Builder::integer(ins()->getArgOperand(argIndex));
    }
};

} // namespace ir

} // namespace rjit

#endif // IR_H
