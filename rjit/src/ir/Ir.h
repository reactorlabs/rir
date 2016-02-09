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
class View;
class Pass;
class Value;
class Builder;

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
        for (size_t l = 0; l < length(); ++l) {
            assert(get(i) == this and "advance of pattern is called on instruction "
                                      "that is not part of it");
            ++i;
        }
    }

    virtual ~Pattern() {
    }


    // TODO deprecated, perhaps needed for llvm
    Kind getKind() const { return kind; }

  protected:
    friend class Verifier;
    friend class View;
    friend class Pass;
    friend class Value;
    friend class Builder;

    /** Each pattern must know the llvm::instruction that is its result.

      This instruction should not be used publicly, rather result() method
      should be called, which gives the pattern creator option to error if
      pattern's result should never be used.
     */
    llvm::Instruction* const ins_;


    /** RAII class for automatic insertion and detachment of a sentinel to basic blocks when inserting patterns at the end.
     */
    class Sentinel {
    public:
        Sentinel(llvm::BasicBlock * b) {
            b->getInstList().push_back(singleton);
        }

        ~Sentinel() {
            singleton->removeFromParent();
        }

        operator llvm::Instruction * () {
            return singleton;
        }
    private:
        /** The NOP pattern used as sentinel for basic blocks when using a builder & create instructions.
         */
        static llvm::Instruction * const singleton;

    };

    Pattern(llvm::Instruction* result, Kind kind) : kind(kind), ins_(result) {
        // attach the pattern to the result instruction
        attach(result);
    }

    template <typename LLVM_INS>
    LLVM_INS* ins() const {
        return static_cast<LLVM_INS*>(ins_);
    }

    /** Attaches the pattern to given llvm instruction.
     */
    void attach(llvm::Instruction* ins) {
        Pattern * p = get(ins);
        if (p != nullptr) {
            assert(false);
        }
        assert(get(ins) == nullptr and "Instruction already has a pattern");
        std::vector<llvm::Metadata*> v = {
            llvm::ValueAsMetadata::get(llvm::ConstantInt::get(
                ins->getContext(),
                llvm::APInt(64, reinterpret_cast<std::uintptr_t>(this))))};
        llvm::MDNode* m = llvm::MDNode::get(ins->getContext(), v);
        ins->setMetadata(MD_NAME, m);
    }

};

/** ir::Value is either a Pattern or llvm::Value.

  This class only exists to allow implicit typecast of Patterns to llvm::Values where such a conversion is appropriate.
 */
class Value {
public:

    Value(llvm::Value * value):
        v(value) {
    }

    Value(Pattern * p):
        v(p->ins_) {
    }

    operator llvm::Value * () const {
        return v;
    }

private:

    llvm::Value * const v;


};



/** Nop instruction, which internally is iadd 0,0.

  Used as a sentinel in basic blocks so that inserting before an instruction and adding at the end has the same semantics. This is also why the pattern does not support the usual create & insertBefore methods.

 */
class Nop : public ir::Pattern {
public:

    static bool classof(Pattern const * s) {
        return s->kind == Kind::Nop;
    }

protected:
   friend class Pattern;

   Nop(llvm::Instruction * ins) : Pattern(ins, Kind::Nop) {
   }

   Nop():
       Pattern(llvm::BinaryOperator::Create(llvm::Instruction::Add, Builder::integer(0), Builder::integer(0), "nop"), Kind::Nop) {
   }

};


class Return : public Pattern {
  public:
    llvm::Value* value() { return ins<llvm::ReturnInst>()->getOperand(0); }

    static Return* create(Builder& b, ir::Value value) {
        Sentinel s(b);
        return insertBefore(s, value);
    }

    static Return * insertBefore(llvm::Instruction * ins, ir::Value value) {
        return new Return(llvm::ReturnInst::Create(llvm::getGlobalContext(), value, ins));
    }

    static Return * insertBefore(Pattern * p, ir::Value value) {
        return insertBefore(p->first(), value);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::Return;
    }
protected:
    Return(llvm::Instruction* ins) : Pattern(ins, Kind::Return) {
        assert(llvm::isa<llvm::ReturnInst>(ins) and "Return expected");
    }

};

class Branch : public Pattern {
  public:

    llvm::BasicBlock* target() {
        return ins<llvm::BranchInst>()->getSuccessor(0);
    }

    static Branch* create(Builder& b, llvm::BasicBlock* target) {
        Sentinel s(b);
        return insertBefore(s, target);
    }

    static Branch * insertBefore(llvm::Instruction * ins, llvm::BasicBlock * target) {
        return new Branch(llvm::BranchInst::Create(target, ins));
    }

    static Branch * insertBefore(Pattern * p, llvm::BasicBlock * target) {
        return insertBefore(p->first(), target);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::Branch;
    }
protected:
    Branch(llvm::Instruction* ins) : Pattern(ins, Kind::Branch) {
        assert(llvm::isa<llvm::BranchInst>(ins) and
               "Branch instruction expected");
        assert(not llvm::cast<llvm::BranchInst>(ins)->isConditional() and
               "Branch must be unconditional");
    }

};

class IntegerComparison : public Pattern {
  public:
    typedef llvm::ICmpInst::Predicate Predicate;

    Predicate predicate() {
        return ins<llvm::ICmpInst>()->getSignedPredicate();
    }

    llvm::Value* lhs() { return ins<llvm::ICmpInst>()->getOperand(0); }

    llvm::Value* rhs() { return ins<llvm::ICmpInst>()->getOperand(1); }
protected:

    IntegerComparison(llvm::Instruction* ins, Kind kind) : Pattern(ins, kind) {
        assert(llvm::isa<llvm::ICmpInst>(ins) and "ICmpInst expected");
    }
};

class IntegerLessThan : public IntegerComparison {
  public:

    static IntegerLessThan* create(Builder& b, ir::Value lhs, ir::Value rhs) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs);
    }

    static IntegerLessThan * insertBefore(llvm::Instruction * ins, ir::Value lhs, ir::Value rhs) {
        return new IntegerLessThan(
            new llvm::ICmpInst(ins, Predicate::ICMP_SLT, lhs, rhs));
    }

    static IntegerLessThan * insertBefore(Pattern * p, ir::Value lhs, ir::Value rhs) {
        return insertBefore(p->first(), lhs, rhs);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::IntegerLessThan;
    }
protected:
    IntegerLessThan(llvm::Instruction* ins)
        : IntegerComparison(ins, Kind::IntegerLessThan) {
        assert(llvm::cast<llvm::ICmpInst>(ins)->getSignedPredicate() ==
                   Predicate::ICMP_SLT and
               "Less than comparison expected");
    }

};

class IntegerEquals : public IntegerComparison {
  public:

    static IntegerEquals * create(Builder& b, ir::Value lhs,
                                  ir::Value rhs) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs);
    }

    static IntegerEquals * insertBefore(llvm::Instruction * ins, ir::Value lhs, ir::Value rhs) {
        return new IntegerEquals(new llvm::ICmpInst(ins, Predicate::ICMP_EQ, lhs, rhs));
    }

    static IntegerEquals * insertBefore(Pattern * p, ir::Value lhs, ir::Value rhs) {
        return insertBefore(p->first(), lhs, rhs);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::IntegerEquals;
    }

protected:
    IntegerEquals(llvm::Instruction* ins)
        : IntegerComparison(ins, Kind::IntegerEquals) {
        assert(llvm::cast<llvm::ICmpInst>(ins)->getSignedPredicate() ==
                   Predicate::ICMP_EQ and
               "Equality comparison expected");
    }
};

class UnsignedIntegerLessThan : public IntegerComparison {
  public:

    static UnsignedIntegerLessThan *  create(Builder& b, ir::Value lhs,
                                  ir::Value rhs) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs);
    }

    static UnsignedIntegerLessThan * insertBefore(llvm::Instruction * ins, ir::Value lhs, ir::Value rhs) {
        return new UnsignedIntegerLessThan(new llvm::ICmpInst(ins, Predicate::ICMP_ULT, lhs, rhs));
    }

    static UnsignedIntegerLessThan * insertBefore(Pattern * p, ir::Value lhs, ir::Value rhs) {
        return insertBefore(p->first(), lhs, rhs);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::UnsignedIntegerLessThan;
    }
protected:
    UnsignedIntegerLessThan(llvm::Instruction* ins)
        : IntegerComparison(ins, Kind::UnsignedIntegerLessThan) {
        assert(llvm::cast<llvm::ICmpInst>(ins)->getUnsignedPredicate() ==
                   Predicate::ICMP_ULT and
               "Unsigned less than comparison expected");
    }
};

// TODO the hierarchy of this is wrong, but actual thought is required to fix it
class BinaryOperator : public Pattern {
  protected:
    BinaryOperator(llvm::Instruction* ins, Kind kind) : Pattern(ins, kind) {}
};

// TODO the hierarchy here should be better as well
class IntegerAdd : public BinaryOperator {
  public:

    llvm::Value* lhs() { return ins<llvm::ICmpInst>()->getOperand(0); }

    llvm::Value* rhs() { return ins<llvm::ICmpInst>()->getOperand(1); }

    static IntegerAdd* create(Builder& b, ir::Value lhs, ir::Value rhs) {
        Sentinel s(b);
        return insertBefore(s, lhs, rhs);
    }

    static IntegerAdd * insertBefore(llvm::Instruction * ins, ir::Value lhs, ir::Value rhs) {
        return new IntegerAdd(llvm::BinaryOperator::Create(llvm::Instruction::Add, lhs, rhs, "", ins));
    }

    static IntegerAdd * insertBefore(Pattern * p, ir::Value lhs, ir::Value rhs) {
        return insertBefore(p->first(), lhs, rhs);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::IntegerAdd;
    }
protected:
    IntegerAdd(llvm::Instruction* ins) : BinaryOperator(ins, Kind::IntegerAdd) {
        assert(llvm::isa<llvm::BinaryOperator>(ins) and
               "Binary operator expected");
        assert(llvm::cast<llvm::BinaryOperator>(ins)->getOpcode() ==
                   llvm::Instruction::Add and
               "Add opcode expected");
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

    static Cbr * create(Builder& b, ir::Value cond,
                       llvm::BasicBlock* trueCase,
                       llvm::BasicBlock* falseCase) {
        Sentinel s(b);
        return insertBefore(s, cond, trueCase, falseCase);
    }

    static Cbr * insertBefore(llvm::Instruction * ins, ir::Value cond, llvm::BasicBlock * trueCase, llvm::BasicBlock * falseCase) {
        ICmpInst* test = new ICmpInst(ins, ICmpInst::ICMP_NE, cond,
                                      Builder::integer(0), "condition");
        auto branch = BranchInst::Create(trueCase, falseCase, test, ins);
        return new Cbr(test, branch);
    }

    static Cbr * insertBefore(Pattern * p, ir::Value cond, llvm::BasicBlock * trueCase, llvm::BasicBlock * falseCase) {
        return insertBefore(p->first(), cond, trueCase, falseCase);
    }

    static bool classof(Pattern const* s) { return s->getKind() == Kind::Cbr; }

protected:
    Cbr(llvm::Instruction* cond, llvm::Instruction* branch)
        : Pattern(branch, Kind::Cbr) {
        attach(cond);
    }
};

class MarkNotMutable : public Pattern {
  public:
    static MarkNotMutable * create(Builder & b,
                       ir::Value val) {
        Sentinel s(b);
        return insertBefore(s, val);
    }

    static MarkNotMutable * insertBefore(llvm::Instruction * ins, ir::Value val) {
        LLVMContext & c = ins->getContext();
        ConstantInt* int32_0 =
            ConstantInt::get(c, APInt(32, StringRef("0"), 10));
        ConstantInt* c1 = ConstantInt::get(c, APInt(32, StringRef("-193"), 10));
        ConstantInt* c2 = ConstantInt::get(c, APInt(32, StringRef("128"), 10));
        auto sexpinfo = GetElementPtrInst::Create(
            t::SEXPREC, val, std::vector<llvm::Value*>({int32_0, int32_0}), "",
            ins);
        auto sexpint =
            new BitCastInst(sexpinfo, PointerType::get(t::Int, 1), "", ins);
        auto sexpval = new LoadInst(sexpint, "", false, ins);
        sexpval->setAlignment(4);
        auto clear = llvm::BinaryOperator::Create(llvm::Instruction::And,
                                                  sexpval, c1, "", ins);
        auto set = llvm::BinaryOperator::Create(llvm::Instruction::Or, clear,
                                                c2, "", ins);
        auto store = new StoreInst(set, sexpint, ins);
        store->setAlignment(4);
        return new MarkNotMutable(sexpinfo, sexpint, sexpval, clear, set, store);
    }

    static MarkNotMutable * insertBefore(Pattern * p, ir::Value value) {
        return insertBefore(p->first(), value);
    }

    static bool classof(Pattern const* s) {
        return s->getKind() == Kind::MarkNotMutable;
    }

    llvm::Instruction * last() const override {
        return ins_->getNextNode()->getNextNode()->getNextNode()->getNextNode()->getNextNode();
    }

    size_t length() const override {
        return 6;
    }

  protected:
    MarkNotMutable(llvm::Instruction* sexpinfo, llvm::Instruction * sexpint, llvm::Instruction * sexpval, llvm::Instruction * clear, llvm::Instruction * set, llvm::Instruction * store):
        Pattern(sexpinfo, Kind::MarkNotMutable) {
        attach(sexpint);
        attach(sexpval);
        attach(clear);
        attach(set);
        attach(store);
    }
};

class Car : public Pattern {
  public:

    static Car* create(Builder& b, ir::Value sexp) {
        Sentinel s(b);
        return insertBefore(s, sexp);
    }

    static Car * insertBefore(llvm::Instruction * ins, ir::Value sexp) {
        LLVMContext & c = ins->getContext();
        ConstantInt* int_0 =
            ConstantInt::get(c, APInt(32, StringRef("0"), 10));
        ConstantInt* int_4 =
            ConstantInt::get(c, APInt(32, StringRef("4"), 10));
        auto consValuePtr = GetElementPtrInst::Create(
            t::SEXPREC, sexp, std::vector<llvm::Value*>({int_0, int_4}), "", ins);
        auto ptr = GetElementPtrInst::Create(t::SEXP_u1, consValuePtr,
                                             std::vector<llvm::Value*>({int_0, int_0}),
                                             "", ins);
        auto value = new LoadInst(ptr, "", false, ins);
        return new Car(consValuePtr, ptr, value);
    }

    static Car * insertBefore(Pattern * p, ir::Value sexp) {
        return insertBefore(p->first(), sexp);
    }

    static bool classof(Pattern const* s) { return s->getKind() == Kind::Car; }

    size_t length() const override {
        return 3;
    }

    llvm::Instruction * first() const override {
        return ins_->getPrevNode()->getPrevNode();
    }

protected:
    Car(llvm::Instruction* consValuePtr, llvm::Instruction* ptr,
        llvm::Instruction* result)
        : Pattern(result, Kind::Car) {
        attach(consValuePtr);
        attach(ptr);
    }

};

class Cdr : public Pattern {
  public:

    static Cdr* create(Builder& b, ir::Value sexp) {
        Sentinel s(b);
        return insertBefore(s, sexp);
    }

    static Cdr * insertBefore(llvm::Instruction * ins, ir::Value sexp) {
        LLVMContext & c = ins->getContext();
        ConstantInt* int_0 =
            ConstantInt::get(c, APInt(32, StringRef("0"), 10));
        ConstantInt* int_1 =
            ConstantInt::get(c, APInt(32, StringRef("1"), 10));
        ConstantInt* int_4 =
            ConstantInt::get(c, APInt(32, StringRef("4"), 10));
        auto consValuePtr = GetElementPtrInst::Create(
            t::SEXPREC, sexp, std::vector<llvm::Value*>({int_0, int_4}), "", ins);
        auto ptr = GetElementPtrInst::Create(t::SEXP_u1, consValuePtr,
                                             std::vector<llvm::Value*>({int_0, int_1}),
                                             "", ins);
        auto value = new LoadInst(ptr, "", false, ins);
        return new Cdr(consValuePtr, ptr, value);
    }

    static Cdr * insertBefore(Pattern * p, ir::Value sexp) {
        return insertBefore(p->first(), sexp);
    }

    static bool classof(Pattern const* s) { return s->getKind() == Kind::Cdr; }

    size_t length() const override {
        return 3;
    }

    llvm::Instruction * first() const override {
        return ins_->getPrevNode()->getPrevNode();
    }

protected:

    Cdr(llvm::Instruction* consValuePtr, llvm::Instruction* ptr,
        llvm::Instruction* result)
        : Pattern(result, Kind::Cdr) {
        attach(consValuePtr);
        attach(ptr);
    }

};

class Tag : public Pattern {
  public:

    static Tag* create(Builder& b, ir::Value sexp) {
        Sentinel s(b);
        return insertBefore(s, sexp);
    }

    static Tag * insertBefore(llvm::Instruction * ins, ir::Value sexp) {
        LLVMContext & c = ins->getContext();
        ConstantInt* int_2 =
            ConstantInt::get(c, APInt(32, StringRef("2"), 10));
        ConstantInt* int_0 =
            ConstantInt::get(c, APInt(32, StringRef("0"), 10));
        ConstantInt* int_4 =
            ConstantInt::get(c, APInt(32, StringRef("4"), 10));
        auto consValuePtr = GetElementPtrInst::Create(
            t::SEXPREC, sexp, std::vector<llvm::Value*>({int_0, int_4}), "", ins);
        auto ptr = GetElementPtrInst::Create(t::SEXP_u1, consValuePtr,
                                             std::vector<llvm::Value*>({int_0, int_2}),
                                             "", ins);
        auto value = new LoadInst(ptr, "", false, ins);
        return new Tag(consValuePtr, ptr, value);
    }

    static Tag * insertBefore(Pattern * p, ir::Value sexp) {
        return insertBefore(p->first(), sexp);
    }

    static bool classof(Pattern const* s) { return s->getKind() == Kind::Tag; }

    size_t length() const override {
        return 3;
    }

    llvm::Instruction * first() const override {
        return ins_->getPrevNode()->getPrevNode();
    }

protected:
    Tag(llvm::Instruction* consValuePtr, llvm::Instruction* ptr,
        llvm::Instruction* result)
        : Pattern(result, Kind::Tag) {
        attach(consValuePtr);
        attach(ptr);
    }
};

class VectorGetElement : public Pattern {
  public:
    static VectorGetElement* create(Builder & b, ir::Value vector, ir::Value index) {
        Sentinel s(b);
        return insertBefore(s, vector, index);
    }

    static VectorGetElement * insertBefore(llvm::Instruction * ins, ir::Value vector, ir::Value index) {
        LLVMContext & c = ins->getContext();
        ConstantInt* int64_1 = ConstantInt::get(c, APInt(64, StringRef("1"), 10));
        auto realVector = new BitCastInst(
            vector, PointerType::get(t::VECTOR_SEXPREC, 1), "", ins);
        auto payload =
            GetElementPtrInst::Create(t::VECTOR_SEXPREC, realVector,
                                      std::vector<llvm::Value*>({int64_1}), "", ins);
        auto payloadPtr =
            new BitCastInst(payload, PointerType::get(t::SEXP, 1), "", ins);
        GetElementPtrInst* el_ptr =
            GetElementPtrInst::Create(t::SEXP, payloadPtr, { index }, "", ins);
        auto res = new LoadInst(el_ptr, "", false, ins);
        res->setAlignment(8);
        VectorGetElement* p = new VectorGetElement(realVector, payload, payloadPtr, el_ptr, res);
        return p;
    }

    static VectorGetElement * insertBefore(Pattern * p, ir::Value vector, ir::Value index) {
        return insertBefore(p->first(), vector, index);
    }

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

    size_t length() const override {
        return 5;
    }

private:
    VectorGetElement(llvm::Instruction * realVector, llvm::Instruction * payload, llvm::Instruction * payloadPtr, llvm::Instruction * el_ptr, llvm::Instruction * result)
        : Pattern(result, Kind::VectorGetElement) {
        attach(realVector);
        attach(payload);
        attach(payloadPtr);
        attach(el_ptr);
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

    static Switch* create(Builder& b, ir::Value cond,
                          llvm::BasicBlock* defaultTarget, int numCases) {
        Sentinel s(b);
        return insertBefore(s, cond, defaultTarget, numCases);
    }

    static Switch * insertBefore(llvm::Instruction * ins, ir::Value cond, llvm::BasicBlock * defaultTarget, int numCases) {
        return new Switch(
            llvm::SwitchInst::Create(cond, defaultTarget, numCases, ins));
    }

    static Switch * insertBefore(Pattern * p, ir::Value cond, llvm::BasicBlock * defaultTarget, int numCases) {
        return insertBefore(p->first(), cond, defaultTarget, numCases);
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

    static CallToAddress* create(Builder& b, ir::Value fun,
                                 std::vector<llvm::Value*> args) {
        return new CallToAddress(llvm::CallInst::Create(fun, args, "", b));
    }

    static CallToAddress* create(Builder& b, llvm::Value* fun,
                                 std::vector<ir::Value> args) {
        return create(b, fun, std::vector<llvm::Value*>(args.begin(), args.end()));
    }

    static CallToAddress* create(Builder& b, ir::Value fun,
                                 std::initializer_list<llvm::Value*> args) {
        return new CallToAddress(
            llvm::CallInst::Create(fun, args, "", b.block()));
    }

    static CallToAddress* create(Builder& b, ir::Value fun,
                                 std::initializer_list<ir::Value> args) {
        return create(b, fun, std::vector<llvm::Value*>(args.begin(), args.end()));
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

    /** Returns the llvm::Function corresponding to the primitive of given name.
      If such function is not present in the module yet, it is declared using
      the given type.

      NOTE that this function assumes that the intrinsic does not use varargs.
     */
    template <typename INTRINSIC>
    static llvm::Function* primitiveFunction(llvm::Module * m) {
        llvm::Function* result = m->getFunction(INTRINSIC::intrinsicName());
        // if the intrinsic has not been declared, declare it
        if (result == nullptr)
            result = llvm::Function::Create(INTRINSIC::intrinsicType(),
                                            llvm::GlobalValue::ExternalLinkage,
                                            INTRINSIC::intrinsicName(), m);
        return result;
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

static_assert(sizeof(ir::Value) == sizeof(llvm::Value*), "ir::Value must have the same representation as llvm::Value (vector typecasting)");

} // namespace rjit



#endif // IR_H
