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

class Verifier;

/** Instruction Pattern

  Each llvm instruction can be part of a pattern. Pattern identifies a high
  level view on the instructions. A pattern may contain 1..n consecutive llvm
  instructions that all belong to the same basic block. All instructions that
  form a pattern must be attached to that pattern (i.e. they must hold the
  pattern pointer in their metadata.

  This is important for when we want to delete or move an instruction that is
  part of a pattern. This operation is perfectly legal, but results in
  destroying the pattern and the attachment allows for relatively fast
  identification whether any instruction is part of a pattern or not.
 */
class Pattern {
  public:
    enum class Kind {
        Invalid = 0, // Will catch uninitialized metadata
        unknown,
#include "ir/pattern_kinds.inc"
    };

    static char const* const MD_NAME;

    /** Pattern kind for fast RTTI.
     */
    Kind const kind;

    /** Returns the length of the pattern, i.e. how many consecutive llvm
      instructions it contains.

      TODO this returns 1 by default, which is plainly wrong.
     */
    virtual size_t length() const { return 1; }

    /** Every pattern should return its first and last instruction.

      Note that the implementation of these instructions IS WRONG in general
      case and will only work for single instruction patterns at the moment.

      TODO Originally I had a single instruction base class which implemented
      this and it was left abstract here. However after careful consideration I
      am strongly against having the length of the pattern part of the hierarchy
      - consider patterns like BinaryOperator - they may be a call, a call +
      sth, the acrual unrolled loop, and so on, all are binary operators of
      different lengths.
      */
    virtual llvm::Instruction* first() const { return ins_; }

    virtual llvm::Instruction* last() const { return ins_; }

    /** A pattern returns always the result insruction.

      TODO change this when hierarchy stabilizes.
     */
    virtual llvm::Instruction* result() const { return ins_; }

    /** Returns the pattern associated with current llvm instruction.

      If the instruction is not part of any pattern, returns nullptr.
      */
    static Pattern* get(llvm::Instruction* ins) {
        llvm::MDNode* m = ins->getMetadata(MD_NAME);
        // return nullptr if the instruction is not part of any pattern
        if (m == nullptr)
            return nullptr;
        llvm::Metadata* mx = m->getOperand(0);
        llvm::APInt const& ap =
            llvm::cast<llvm::ConstantInt>(
                llvm::cast<llvm::ValueAsMetadata>(mx)->getValue())
                ->getUniqueInteger();
        assert(ap.isIntN(64) and "Expected 64bit integer");
        Pattern* res = reinterpret_cast<Pattern*>(ap.getZExtValue());
        return res;
    }

    /** Advances given iterator past the pattern.

      This is the generic version of the functionality which increases the
      iterator for as long as the current instruction's pattern is the same as
      current pattern. Other patterns may actually override this method to
      provide specialized versions based on the knowledge of their length.
     */
    virtual void advance(llvm::BasicBlock::iterator& i) const {
        assert(get(i) == this and "advance of pattern is called on instruction "
                                  "that is not part of it");
        while (get(i) == this) {
            if (i->isTerminator()) {
                ++i;
                return;
            }
            ++i;
        }
    }

    // TODO deprecated, perhaps needed for llvm
    Kind getKind() const { return kind; }

  protected:
    friend class Verifier;

    /** Each pattern must know the llvm::instruction that is its result.

      This instruction should not be used publicly, rather result() method
      should be called, which gives the pattern creator option to error if
      pattern's result should never be used.
     */
    llvm::Instruction* const ins_;

    Pattern(llvm::Instruction* result, Kind kind) : kind(kind), ins_(result) {
        // attach the pattern to the result instruction
        attach(result);
    }

    // TODO create destructor that detaches itself from all instructions forming
    // it

    template <typename LLVM_INS>
    LLVM_INS* ins() const {
        return static_cast<LLVM_INS*>(ins_);
    }

    /** Attaches the pattern to given llvm instruction.
     */
    void attach(llvm::Instruction* ins) {
        assert(get(ins) == nullptr and "Instruction already has a pattern");
        std::vector<llvm::Metadata*> v = {
            llvm::ValueAsMetadata::get(llvm::ConstantInt::get(
                ins->getContext(),
                llvm::APInt(64, reinterpret_cast<std::uintptr_t>(this))))};
        llvm::MDNode* m = llvm::MDNode::get(ins->getContext(), v);
        ins->setMetadata(MD_NAME, m);
    }
};

class Return : public Pattern {
  public:
    Return(llvm::Instruction* ins) : Pattern(ins, Kind::Return) {
        assert(llvm::isa<llvm::ReturnInst>(ins) and "Return expected");
    }

    llvm::Value* value() { return ins<llvm::ReturnInst>()->getOperand(0); }

    static Return* create(Builder& b, llvm::Value* value) {
        return new Return(
            llvm::ReturnInst::Create(llvm::getGlobalContext(), value, b));
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::Return;
    }
};

class Branch : public Pattern {
  public:
    Branch(llvm::Instruction* ins) : Pattern(ins, Kind::Branch) {
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
        return s->getKind() == Kind::Branch;
    }
};

class IntegerComparison : public Pattern {
  public:
    typedef llvm::ICmpInst::Predicate Predicate;

    IntegerComparison(llvm::Instruction* ins, Kind kind) : Pattern(ins, kind) {
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
        : IntegerComparison(ins, Kind::IntegerLessThan) {
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
        return s->getKind() == Kind::IntegerLessThan;
    }
};

class IntegerEquals : public IntegerComparison {
  public:
    IntegerEquals(llvm::Instruction* ins)
        : IntegerComparison(ins, Kind::IntegerEquals) {
        assert(llvm::cast<llvm::ICmpInst>(ins)->getSignedPredicate() ==
                   Predicate::ICMP_EQ and
               "Equality comparison expected");
    }

    static llvm::ICmpInst* create(Builder& b, llvm::Value* lhs,
                                  llvm::Value* rhs) {
        return new llvm::ICmpInst(*b.block(), Predicate::ICMP_EQ, lhs, rhs);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::IntegerEquals;
    }
};

class UnsignedIntegerLessThan : public IntegerComparison {
  public:
    UnsignedIntegerLessThan(llvm::Instruction* ins)
        : IntegerComparison(ins, Kind::UnsignedIntegerLessThan) {
        assert(llvm::cast<llvm::ICmpInst>(ins)->getSignedPredicate() ==
                   Predicate::ICMP_ULT and
               "Unsigned less than comparison expected");
    }

    static llvm::ICmpInst* create(Builder& b, llvm::Value* lhs,
                                  llvm::Value* rhs) {
        return new llvm::ICmpInst(*b.block(), Predicate::ICMP_ULT, lhs, rhs);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::UnsignedIntegerLessThan;
    }
};

// TODO the hierarchy of this is wrong, but actual thought is required to fix it
class BinaryOperator : public Pattern {
  public:
    BinaryOperator(llvm::Instruction* ins, Kind kind) : Pattern(ins, kind) {}
};

// TODO the hierarchy here should be better as well
class IntegerAdd : public BinaryOperator {
  public:
    IntegerAdd(llvm::Instruction* ins) : BinaryOperator(ins, Kind::IntegerAdd) {
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
        return s->getKind() == Kind::IntegerAdd;
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
class Cbr : public Pattern {
  public:
    llvm::Instruction* first() const override { return ins_->getPrevNode(); }

    size_t length() const override { return 2; }

    llvm::Value* cond() { return ins_->getPrevNode()->getOperand(0); }

    llvm::BasicBlock* trueCase() {
        return ins<llvm::BranchInst>()->getSuccessor(0);
    }

    llvm::BasicBlock* falseCase() {
        return ins<llvm::BranchInst>()->getSuccessor(1);
    }

    Cbr(llvm::Instruction* cond, llvm::Instruction* branch)
        : Pattern(branch, Kind::Cbr) {
        attach(cond);
    }

    static void create(Builder& b, llvm::Value* cond,
                       llvm::BasicBlock* trueCase,
                       llvm::BasicBlock* falseCase) {
        ICmpInst* test = new ICmpInst(*b.block(), ICmpInst::ICMP_NE, cond,
                                      b.integer(0), "condition");
        auto branch = BranchInst::Create(trueCase, falseCase, test, b);
        new Cbr(test, branch);
    }

    static bool classof(Pattern const* s) { return s->getKind() == Kind::Cbr; }
};

class MarkNotMutable : public Pattern {
  public:
    static void create(llvm::Instruction* insert, LLVMContext& c,
                       llvm::Value* val) {
        ConstantInt* int32_0 =
            ConstantInt::get(c, APInt(32, StringRef("0"), 10));
        ConstantInt* c1 = ConstantInt::get(c, APInt(32, StringRef("-193"), 10));
        ConstantInt* c2 = ConstantInt::get(c, APInt(32, StringRef("128"), 10));
        auto sexpinfo = GetElementPtrInst::Create(
            t::SEXPREC, val, std::vector<Value*>({int32_0, int32_0}), "",
            insert);
        auto sexpint =
            new BitCastInst(sexpinfo, PointerType::get(t::Int, 1), "", insert);
        auto sexpval = new LoadInst(sexpint, "", false, insert);
        sexpval->setAlignment(4);
        auto clear = llvm::BinaryOperator::Create(llvm::Instruction::And,
                                                  sexpval, c1, "", insert);
        auto set = llvm::BinaryOperator::Create(llvm::Instruction::Or, clear,
                                                c2, "", insert);
        auto store = new StoreInst(set, sexpint, insert);
        store->setAlignment(4);
        // TODO if I understand it correctly, MarkNotMutable does not really
        // have usable result? The attachments will be better off in the
        // constructor as they are for others?
        MarkNotMutable* p = new MarkNotMutable(sexpinfo);
        p->attach(sexpint);
        p->attach(sexpval);
        p->attach(clear);
        p->attach(set);
        p->attach(store);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::MarkNotMutable;
    }

  private:
    MarkNotMutable(llvm::Instruction* ins)
        : Pattern(ins, Kind::MarkNotMutable) {}
};

class Car : public Pattern {
  public:
    Car(llvm::Instruction* consValuePtr, llvm::Instruction* ptr,
        llvm::Instruction* result)
        : Pattern(result, Kind::Car) {
        attach(consValuePtr);
        attach(ptr);
    }

    static Car* create(Builder& b, llvm::Value* sexp);

    static bool classof(Pattern const* s) { return s->getKind() == Kind::Car; }
};

class Cdr : public Pattern {
  public:
    Cdr(llvm::Instruction* consValuePtr, llvm::Instruction* ptr,
        llvm::Instruction* result)
        : Pattern(result, Kind::Cdr) {
        attach(consValuePtr);
        attach(ptr);
    }

    static Cdr* create(Builder& b, llvm::Value* sexp);

    static bool classof(Pattern const* s) { return s->getKind() == Kind::Cdr; }
};

class Tag : public Pattern {
  public:
    Tag(llvm::Instruction* consValuePtr, llvm::Instruction* ptr,
        llvm::Instruction* result)
        : Pattern(result, Kind::Tag) {
        attach(consValuePtr);
        attach(ptr);
    }

    static Tag* create(Builder& b, llvm::Value* sexp);

    static bool classof(Pattern const* s) { return s->getKind() == Kind::Tag; }
};

class VectorGetElement : public Pattern {
  public:
    VectorGetElement(llvm::Instruction* result)
        : Pattern(result, Kind::VectorGetElement) {}

    static VectorGetElement* create(llvm::Instruction* insert, LLVMContext& c,
                                    llvm::Value* vector, llvm::Value* index);

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::VectorGetElement;
    }

    virtual llvm::Instruction* first() const override {
        llvm::Instruction* r =
            ins_->getPrevNode()->getPrevNode()->getPrevNode()->getPrevNode();
        // TODO better check for start
        assert(Pattern::get(r) == this);
        return r;
    }
};

/** Interface to llvm's switch instruction
  */
class Switch : public Pattern {
  public:
    Switch(llvm::Instruction* ins) : Pattern(ins, Kind::Switch) {
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
        return s->getKind() == Kind::Switch;
    }
};

/** Anonymous native call.

 */
class CallToAddress : public Pattern {
  public:
    /** Returns the CallInst associated with the pattern.
     */
    llvm::CallInst* ins() { return Pattern::ins<llvm::CallInst>(); }

    static CallToAddress* create(Builder& b, llvm::Value* fun,
                                 std::vector<Value*> args) {
        return new CallToAddress(
            llvm::CallInst::Create(fun, args, "", b.block()));
    }

    static CallToAddress* create(Builder& b, llvm::Value* fun,
                                 std::initializer_list<Value*> args) {
        return new CallToAddress(
            llvm::CallInst::Create(fun, args, "", b.block()));
    }

    CallToAddress(Instruction* ins) : Pattern(ins, Kind::CallToAddress) {
        assert(llvm::isa<llvm::CallInst>(ins) and
               "CallToAddress must be llvm calls");
    }
};

/** Base class for all primitive calls.

 */
class PrimitiveCall : public Pattern {
  public:
    /** Returns the CallInst associated with the primitive call.
     */
    llvm::CallInst* ins() { return Pattern::ins<llvm::CallInst>(); }

  protected:
    PrimitiveCall(llvm::Instruction* ins, Kind kind) : Pattern(ins, kind) {
        assert(llvm::isa<llvm::CallInst>(ins) and
               "Primitive calls must be llvm call instructions");
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
