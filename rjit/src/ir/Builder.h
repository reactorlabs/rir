#ifndef BUILDER_H
#define BUILDER_H

#include "llvm.h"

#include "RIntlns.h"

#include "Types.h"
#include "StackMap.h"
#include "JITCompileLayer.h"
#include "JITModule.h"
#include "Instrumentation.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"

namespace rjit {

namespace ir {

class Nop;

/** Helper class that aids with building and modifying LLVM IR for functions.

  The interface provided is intended for the intrinsic wrappers.

  <b>Sentinels</b>

  The builder inserts a sentinel (ir::Nop) to each basic block when they are
  created. The sentinel is then used by create() methods of all patterns so that
  they can route to insertBefore() methods and we do not have to deal with code
  duplication in these two methods (create effectively becomes insertBefore
  sentinel).

  However adding the sentinels creates problems for llvm as it means that
  terminators are not last instructions in a bb. So closing functions for ICs
  and functions now call removeSentinels() which removes the sentinels from all
  basic blocks in the function. Of course after this, the create methods may no
  longer be used on the function.

  When adding llvm instructions manually one has to be aware of this and instead
  of llvm::BasicBlock * atEnd functions, insertBefore ones should be used, where
  Builder.blockSentinel()->first() gives the instruction to insert before.

  */
class Builder {
  public:
    Builder(const Builder&) = delete;

    void doGcCallback(void (*forward_node)(SEXP));

    explicit Builder(std::string const& moduleName)
        : Builder(new JITModule(moduleName, llvm::getGlobalContext())) {}

    explicit Builder(JITModule* m) : m_(m) {}

    /** Builder can typecast to the current module.
     */
    operator JITModule*() { return m_; }

    /** Builder can typecast to the current function.
     */
    operator llvm::Function*() { return c_->f; }

    /** Builder can typecast to current basic block.
     */
    operator llvm::BasicBlock*() { return c_->b; }

    /** Returns the current basic block.
     */
    llvm::BasicBlock* block() { return c_->b; }

    /** Returns the current break target.
     */
    llvm::BasicBlock* breakTarget() {
        assert(c_ != nullptr and c_->breakTarget != nullptr and
               "Not in loop context");
        return c_->breakTarget;
    }

    /** Returns the current next target.
     */
    llvm::BasicBlock* nextTarget() {
        assert(c_ != nullptr and c_->nextTarget != nullptr and
               "Not in loop context");
        return c_->nextTarget;
    }

    llvm::BasicBlock* createBasicBlock();

    llvm::BasicBlock* createBasicBlock(std::string const& name);

    /** Returns the environment of the current context.
     */
    llvm::Value* rho() { return c_->rho(); }

    const std::vector<llvm::Value*>& args() { return c_->args(); }

    llvm::Value* consts() { return c_->consts(); }
    llvm::Value* closure() { return c_->closure(); }

    /** Creates new context for given function name.

      Creates the llvm Function and initial basic block objects, sets the
      function attributes and context's rho value.

      Adds the ast of the function as first argument to the function's constant
      pool.
     */
    void openPromise(std::string const& name, SEXP ast);
    void openFunction(std::string const& name, SEXP ast, SEXP formals,
                      TypeFeedback* tf = nullptr);
    void openFunctionOrPromise(SEXP ast);

    void openIC(std::string const& name, FunctionType* ty) {
        assert(contextStack_.empty());
        c_ = new ICContext(name, m_, ty);
    }

    llvm::Function* closeIC();

    /** Creates new context for a loop. Initializes the basic blocks for break
     * and next targets. */
    void openLoop() {
        assert(c_ != nullptr and
               "Cannot open loop context when not in function");
        contextStack_.push_back(c_);
        c_ = c_->clone();
        c_->breakTarget = createBasicBlock("break");
        c_->nextTarget = createBasicBlock("next");
    }

    /** Closes the open loop and pops its context.
     */
    void closeLoop() {
        assert(not contextStack_.empty() and
               (contextStack_.back()->f == c_->f) and
               "Cannot close loop w/o loop context");
        // we are guaranteed to have a previous context and the context is from
        // same function
        Context* x = contextStack_.back();
        contextStack_.pop_back();
        // update the current basic block and objects from popped context to the
        // next current one
        x->b = c_->b;
        x->cp = std::move(c_->cp);
        x->instrumentationIndex = std::move(c_->instrumentationIndex);
        delete c_;
        c_ = x;
    }

    /** Closes a function context.

      Returns the SEXP corresponding to that function w/o the native code, which
      will be added at the time the module is jitted. The function's SEXP is
      therefore automatically added to the relocations for the module.
     */
    SEXP closeFunctionOrPromise();
    SEXP closeFunction();
    SEXP closePromise();

    bool isFunction() { return c_->isFunction(); }

    /** Returns the llvm::Function corresponding to the intrinsic of given name.
      If such intrinsic is not present in the module yet, it is declared using
      the given type.

      NOTE that this function assumes that the intrinsic does not use varargs.

      TODO deprecated, use PrimitiveCall::primitiveFunction instead.
     */
    template <typename INTRINSIC>
    llvm::Function* intrinsic() {
        llvm::Function* result = m_->getFunction(INTRINSIC::intrinsicName());
        // if the intrinsic has not been declared, declare it
        if (result == nullptr)
            result = llvm::Function::Create(INTRINSIC::intrinsicType(),
                                            llvm::GlobalValue::ExternalLinkage,
                                            INTRINSIC::intrinsicName(), m_);
        return result;
    }

    /** Given an llvm::Value * that is a constant integer, returns the constant
     * integer associated with it.
     */
    static int integer(llvm::Value* value) {
        llvm::APInt const& ap =
            llvm::cast<llvm::ConstantInt>(value)->getUniqueInteger();
        assert(ap.isIntN(32) and "Expected 32bit integer");
        return ap.getSExtValue();
    }

    /** Given a call instruction, sets its attributes wrt stack map statepoints.
     */
    static llvm::CallInst* markSafepoint(llvm::CallInst* f) {
        llvm::Module* m_ = f->getModule();
        llvm::AttributeSet PAL;
        {
            llvm::SmallVector<llvm::AttributeSet, 4> Attrs;
            llvm::AttributeSet PAS;
            {
                llvm::AttrBuilder B;
                B.addAttribute("needs-statepoint");
                PAS = llvm::AttributeSet::get(m_->getContext(), ~0U, B);
            }
            Attrs.push_back(PAS);
            PAL = llvm::AttributeSet::get(m_->getContext(), Attrs);
        }
        f->setAttributes(PAL);
        return f;
    }

    // Getter for context function and environment
    llvm::Function* f() { return c_->f; }

    /**  Setters for Jump and Visible
     */
    void setResultJump(bool value) { c_->isReturnJumpNeeded = value; }

    bool getResultJump() { return c_->isReturnJumpNeeded; }

    void setResultVisible(bool value) { c_->isResultVisible = value; }

    bool getResultVisible() { return c_->isResultVisible; }

    void addConstantPoolObject(SEXP object) {
        c_->addConstantPoolObject(object);
    }

    void setBlock(llvm::BasicBlock* block) { c_->b = block; }

    /** Set the breakTarget.
     */
    void setBreakTarget(llvm::BasicBlock* block) { c_->breakTarget = block; }

    /** Set the nextTarget.
     */
    void setNextTarget(llvm::BasicBlock* block) { c_->nextTarget = block; }

    JITModule* module() { return m_; }

    llvm::LLVMContext& getContext() { return m_->getContext(); }

    /** Takes the given SEXP, stores it to the constant pool and returns the
      index under which it is stored.

      In a trivial way (O(n)) checks whether such constant already exists to
      avoid duplicates in the constant pool.
     */
    int constantPoolIndex(SEXP object) {
        for (unsigned i = c_->reserved(); i < c_->cp.size(); ++i)
            if (c_->cp[i] == object)
                return i;
        return c_->addConstantPoolObject(object);
    }

    int getInstrumentationIndex(SEXP sym) {
        return c_->getInstrumentationIndex(sym);
    }

    /** Returns the index-th object in the constant pool.
     */
    SEXP constantPool(int index) const { return c_->cp[index]; }

    static llvm::ConstantInt* integer(int value, unsigned precision = 32) {
        return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                      llvm::APInt(precision, value));
    }

    /** Converts the given SEXP to a pointer to it.
     */
    static llvm::Value* convertToPointer(SEXP what) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(llvm::getGlobalContext(),
                                   llvm::APInt(64, (std::uint64_t)what)),
            t::SEXP);
    }

  private:
    class Context {
      public:
        virtual ~Context() {}

        /** Adds the given object into the constant pool served by the objects
         * field.
         */
        unsigned addConstantPoolObject(SEXP object) {
            cp.push_back(object);
            return cp.size() - 1;
        }

        std::unordered_map<SEXP, int> instrumentationIndex;
        int getInstrumentationIndex(SEXP sym) {
            if (instrumentationIndex.count(sym))
                return instrumentationIndex.at(sym);
            int next = instrumentationIndex.size();
            instrumentationIndex[sym] = next;
            return next;
        }

        bool isReturnJumpNeeded = false;
        bool isResultVisible = true;

        virtual bool isFunction() { return false; }

        llvm::Function* f;
        llvm::BasicBlock* b;

        llvm::BasicBlock* breakTarget;
        llvm::BasicBlock* nextTarget;

        const std::vector<llvm::Value*>& args() { return args_; }

        virtual llvm::Value* rho() = 0;
        virtual llvm::Value* consts() = 0;
        virtual llvm::Value* closure() = 0;
        virtual Context* clone() {
            assert(false);
            return nullptr;
        }

        virtual unsigned reserved() {
            // slots 0-3 for ast and typefeedback
            return 4;
        }

        /** Constant pool of the function.

          The constant pool always starts with the AST of the function being
          compiled, followed by any additional constants (notably created
          promises).
         */
        std::vector<SEXP> cp;

      protected:
        // TODO check contexts - the rho handling seems to be
        std::vector<llvm::Value*> args_;

        Context(Context* from)
            : instrumentationIndex(std::move(from->instrumentationIndex)),
              isReturnJumpNeeded(from->isReturnJumpNeeded),
              isResultVisible(from->isResultVisible), f(from->f), b(from->b),
              breakTarget(from->breakTarget), nextTarget(from->nextTarget),
              cp(std::move(from->cp)), args_(from->args_) {}

        Context(std::string const& name, JITModule* m, llvm::FunctionType* ty,
                bool isReturnJumpNeeded);
    };

    class ClosureContext : public Context {
      public:
        ClosureContext(std::string name, JITModule* m, SEXP formals,
                       bool isReturnJumpNeeded = false);

        llvm::Value* rho() override {
            assert(args_.size() == 3 and "Code assumes the signature of native "
                                         "function is (consts, rho, closure)");
            return args_[1];
        }

        llvm::Value* consts() override {
            assert(args_.size() == 3 and "Code assumes the signature of native "
                                         "function is (consts, rho, closure)");
            return args_[0];
        }

        llvm::Value* closure() override {
            assert(args_.size() == 3 and "Code assumes the signature of native "
                                         "function is (consts, rho, closure)");
            return args_[2];
        }

        bool isFunction() override { return true; }

        ClosureContext(Context* from) : Context(from) {}

        Context* clone() override { return new ClosureContext(this); }

        SEXP formals;
    };

    class PromiseContext : public ClosureContext {
      public:
        PromiseContext(std::string name, JITModule* m)
            : ClosureContext(name, m, R_NilValue, true) {}
        PromiseContext(Context* from) : ClosureContext(from) {}

        Context* clone() override { return new PromiseContext(this); }

        bool isFunction() override { return false; }

        unsigned reserved() override {
            // slots 0 for ast
            return 1;
        }
    };

    class ICContext : public Context {
      public:
        ICContext(std::string name, JITModule* m, llvm::FunctionType* ty);
        ICContext(Context* from) = delete;
        llvm::Value* rho() override {
            // Environment is third last argument to call ICS
            return args_.at(args_.size() - 3);
        }

        llvm::Value* closure() override {
            // Closure is 4th last argument to call ICs
            return args_.at(args_.size() - 4);
        }

        llvm::Value* consts() override {
            assert(false and "NOT IMPLEMENTED");
            return nullptr;
        }
    };

    /** The module into which we are currently building.
     */
    JITModule* m_ = nullptr;

    /** Current context.
     */
    Context* c_ = nullptr;

    /** Stack of active contexts.
     */
    std::deque<Context*> contextStack_;

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
