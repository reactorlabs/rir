#ifndef BUILDER_H
#define BUILDER_H

#include "llvm.h"

#include "RIntlns.h"

#include "Types.h"
#include "StackMap.h"

namespace rjit {

// TODO This should be static of builder
// TODO this guy also unprotects the objects - perhaps the caller should do this so that it is more explicit and consistent
SEXP createNativeSXP(RFunctionPtr fptr, SEXP ast, std::vector<SEXP> const& objects, llvm::Function* f);

namespace ir {

/** Helper class that aids with building and modifying LLVM IR for functions.

  The interface provided is intended for the intrinsic wrappers.

  */
class Builder {
public:

    Builder(std::string const & moduleName):
        m_(new llvm::Module(moduleName, llvm::getGlobalContext())) {
    }

    /** Builder can typecast to the current module.
     */
    operator llvm::Module * () {
        return m_;
    }

    /** Builder can typecast to the current function.
     */
    operator llvm::Function * () {
        return c_->f;
    }

    /** Builder can typecast to current basic block.
     */
    operator llvm::BasicBlock * () {
        return c_->b;
    }

    /** Returns the current basic block.
     */
    llvm::BasicBlock * block() {
        return c_->b;
    }

    /** Returns the current break target.
     */
    llvm::BasicBlock * breakTarget() {
        assert(c_ != nullptr and c_->breakTarget != nullptr and "Not in loop context");
        return c_->breakTarget;
    }

    /** Returns the current next target.
     */
    llvm::BasicBlock * nextTarget() {
        assert(c_ != nullptr and c_->nextTarget != nullptr and "Not in loop context");
        return c_->nextTarget;
    }

    llvm::BasicBlock * createBasicBlock() {
        return llvm::BasicBlock::Create(m_->getContext(), "", c_->f);
    }

    llvm::BasicBlock * createBasicBlock(std::string const & name) {
        return llvm::BasicBlock::Create(m_->getContext(), name, c_->f);
    }

    /** Returns the environment of the current context.
     */
    llvm::Value * rho() {
        return c_->rho;
    }

    llvm::Value * consts() {
        return c_->consts;
    }

    /** Creates new context for given function name.

      Creates the llvm Function and initial basic block objects, sets the function attributes and context's rho value.

      Adds the ast of the function as first argument to the function's constant pool.
     */
    void openFunction(std::string const & name, SEXP ast, bool isPromise);

    /** Creates new context for a loop. Initializes the basic blocks for break and next targets. */
    void openLoop();

    /** Closes the open loop and pops its context.
     */
    void closeLoop();

    /** Closes a function context.

      Returns the SEXP corresponding to that function w/o the native code, which will be added at the time the module is jitted. The function's SEXP is therefore automatically added to the relocations for the module.
     */
    SEXP closeFunction();

    /** Returns the llvm::Function corresponding to the intrinsic of given name. If such intrinsic is not present in the module yet, it is declared using the given type.

      NOTE that this function assumes that the intrinsic does not use varargs.
     */
    template<typename INTRINSIC>
    llvm::Function * intrinsic() {
        llvm::Function * result = m_->getFunction(INTRINSIC::intrinsicName());
        // if the intrinsic has not been declared, declare it
        if (result == nullptr)
            result = llvm::Function::Create(INTRINSIC::intrinsicType(), llvm::GlobalValue::ExternalLinkage, INTRINSIC::intrinsicName(), m_);
        return result;
    }

    /** Converts integer constant to a llvm::Value.
     */
    static llvm::ConstantInt * integer(int value) {
        return llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(32, value));
    }

    /** Given an llvm::Value * that is a constant integer, returns the constant integer associated with it.
     */
    static int integer(llvm::Value * value) {
        llvm::APInt const & ap = llvm::cast<llvm::ConstantInt>(value)->getUniqueInteger();
        assert(ap.isIntN(32) and "Expected 32bit integer");
        return ap.getSExtValue();
    }

    /** Given a call instruction, sets its attributes wrt stack map statepoints.
     */
    llvm::CallInst * insertCall(llvm::CallInst * f) {
        if (c_->functionId != (uint64_t)-1) {
            assert(c_->functionId > 1);
            assert(c_->functionId < StackMap::nextStackmapId);

            llvm::AttributeSet PAL;
            {
                llvm::SmallVector<llvm::AttributeSet, 4> Attrs;
                llvm::AttributeSet PAS;
                {
                    llvm::AttrBuilder B;
                    B.addAttribute("statepoint-id", std::to_string(c_->functionId));
                    PAS = llvm::AttributeSet::get(m_->getContext(), ~0U, B);
                }
                Attrs.push_back(PAS);
                PAL = llvm::AttributeSet::get(m_->getContext(), Attrs);
            }
            f->setAttributes(PAL);
        }
        return f;
    }

    /** Takes the given SEXP, stores it to the constant pool and returns the index under which it is stored.

      In a trivial way (O(n)) checks whether such constant already exists to avoid duplicates in the constant pool.
     */
    int constantPoolIndex(SEXP object) {
        for (unsigned i = 0; i < c_->cp.size(); ++i)
            if (c_->cp[i] == object)
                return i;
        c_->cp.push_back(object);
        return c_->cp.size() - 1;
    }

    /** Returns the index-th object in the constant pool.
     */
    SEXP constantPool(int index) const {
        return c_->cp[index];
    }




private:
    class Context {
    public:

        Context(Builder & builder, std::string const & name, llvm::Module * m, bool isPromise);

        Context(Context * from):
            isReturnJumpNeeded(from->isReturnJumpNeeded),
            isResultVisible(from->isResultVisible),
            f(from->f),
            b(from->b),
            breakTarget(from->breakTarget),
            nextTarget(from->nextTarget),
            rho(from->rho),
            body(from->body),
            consts(from->consts),
            functionId(from->functionId),
            cp(std::move(from->cp)) {
        }

        /** Adds the given object into the constant pool served by the objects field.
         */
        void addConstantPoolObject(SEXP object) {
            PROTECT(object);
            cp.push_back(object);
        }

        bool isReturnJumpNeeded;
        bool isResultVisible;

        llvm::Function * f;
        llvm::BasicBlock * b;

        llvm::BasicBlock * breakTarget;
        llvm::BasicBlock * nextTarget;

        llvm::Value * rho;
        llvm::Value * body;
        llvm::Value * consts;

        unsigned functionId;

        /** Constant pool of the function.

          The constant pool always starts with the AST of the function being compiled, followed by any additional constants (notably created promises).
         */
        std::vector<SEXP> cp;
    };

    /** The module into which we are currently building.
     */
    llvm::Module * m_;

    /** Current context.
     */
    Context * c_;

    /** Stack of active contexts.
     */
    std::stack<Context *> contextStack_;

    /** List of relocations to be done when compiling.

      When a function is compiled, it is first translated to bitcode and a
      native SXP is created for it using nullptr for the native code. The
      function's SXP is added to the list of relocations here. When the
      compilation is done, the module is finalized and all SEXPs in the
      relocation lists are patched so that they point to correct native
      functions.
      */
    std::vector<SEXP> relocations_;


};

} // namespace ir
} // namespace rjit

#endif // BUILDER_H

