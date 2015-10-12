#ifndef BUILDER_H
#define BUILDER_H

#include "llvm.h"

#include "RIntlns.h"

#include "Types.h"
#include "StackMap.h"
#include "JITCompileLayer.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"


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

    Builder(const Builder&) = delete;

    llvm::Function* patchpoint;
    llvm::Function* stackmap;

    Builder(std::string const & moduleName) : Builder(new llvm::Module(moduleName, llvm::getGlobalContext())) {
        stackmap = Function::Create(t::stackmap_t, GlobalValue::ExternalLinkage,
                                "llvm.experimental.stackmap", m_);

        patchpoint = Function::Create(t::patchpoint_t, GlobalValue::ExternalLinkage,
                                  "llvm.experimental.patchpoint.void", m_);
        
        m_->setDataLayout(*EngineBuilder().selectTarget()->getDataLayout());
    }

    Builder(llvm::Module * m) : m_(m) {}

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
        return c_->rho();
    }

    const std::vector<llvm::Value *> & args() {
        return c_->args();
    }


    /** Creates new context for given function name.

      Creates the llvm Function and initial basic block objects, sets the function attributes and context's rho value.

      Adds the ast of the function as first argument to the function's constant pool.
     */
    void openFunction(std::string const & name, SEXP ast, bool isPromise) {
        if (c_ != nullptr)
            contextStack_.push(c_);
        if (isPromise) {
            c_ = new PromiseContext(name, m_);
        } else {
            c_ = new ClosureContext(name, m_);
        }
        c_->addConstantPoolObject(ast);
    }
    
    void openIC(std::string const & name, FunctionType * ty) {
        if (c_ != nullptr)
            contextStack_.push(c_);
        c_ = new ICContext(name, m_, ty);
    }

    /** Creates new context for a loop. Initializes the basic blocks for break and next targets. */
    void openLoop() {
        assert(c_ != nullptr and "Cannot open loop context when not in function");
        contextStack_.push(c_);
        c_ = c_->clone();
        c_->breakTarget = createBasicBlock("break");
        c_->nextTarget = createBasicBlock("next");
    }

    /** Closes the open loop and pops its context.
     */
    void closeLoop() {
        assert(not contextStack_.empty() and (contextStack_.top()->f == c_->f) and "Cannot close loop w/o loop context");
        // we are guaranteed to have a previous context and the context is from same function
        Context * x = contextStack_.top();
        contextStack_.pop();
        // update the current basic block and objects from popped context to the next current one
        x->b = c_->b;
        x->cp = std::move(c_->cp);
        delete c_;
        c_ = x;
    }

    /** Closes a function context.

      Returns the SEXP corresponding to that function w/o the native code, which will be added at the time the module is jitted. The function's SEXP is therefore automatically added to the relocations for the module.
     */
    SEXP closeFunction() {
        assert((contextStack_.empty() or (contextStack_.top()->f != c_->f)) and "Not a function context");
        SEXP result = createNativeSXP(nullptr, c_->cp[0], c_->cp, c_->f);
        relocations_.push_back(result);
        delete c_;
        if (contextStack_.empty()) {
            c_ = nullptr;
        } else {
            c_ = contextStack_.top();
            contextStack_.pop();
        }
        return result;
    }

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

    /** Returns a llvm::Value created from a constant pool value.

      Note that we do not need to add the constant to the constant 
      pool because the first constant pool element is the whole AST 
      and the understanding is that any constants used are just subtrees 
      of the original AST. In the rare cases when this is not true 
      (new promises being created), they must be inserted to the constant 
      pool explicitly.
     */
    static llvm::Value * constantPoolSexp(SEXP value) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(64, (std::uint64_t)value)),
            t::SEXP);
    }

    /** Given a llvm::Value *, returns the SEXP from constant pool it points to.
     */
    static SEXP constantPoolSexp(llvm::Value * value) {
        // get the value
        llvm::Value * v = llvm::cast<llvm::ConstantExpr>(value)->getOperand(0);
        // get the ap out of the value
        llvm::APInt const & ap = llvm::cast<llvm::ConstantInt>(v)->getUniqueInteger();
        assert(ap.isIntN(64) and "Expected 64bit address");
        // convert the int to sexp addr
        return reinterpret_cast<SEXP>(ap.getSExtValue());
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
        return f;
    }



    // Getter for context function and environment
    llvm::Function * f(){
        return c_->f;
    }

    /**  Setters for Jump and Visible
     */
    void setResultJump(bool value){
       c_->isReturnJumpNeeded = value;
    }

    bool getResultJump(){
        return c_->isReturnJumpNeeded;
    }

    void setResultVisible(bool value){
       c_->isResultVisible = value;
    }

    bool getResultVisible(){
        return c_->isResultVisible;
    }

    void addConstantPoolObject(SEXP object){
        c_->addConstantPoolObject(object);
    }

    void setBlock(llvm::BasicBlock * block){
        c_->b = block;
    }

    /** Set the breakTarget.
     */
    void setBreakTarget(llvm::BasicBlock * block){
        c_->breakTarget = block;
    }

    /** Set the nextTarget.
     */
    void setNextTarget(llvm::BasicBlock * block){
        c_->nextTarget = block;
    }

    llvm::Module * module() {
        return m_;
    }

    llvm::LLVMContext& getContext (){
        return m_->getContext();
    }

    llvm::Function* getStackmap(){
        return stackmap;   
    }



private:
    class Context {
    public:
        virtual ~Context() {}

        /** Adds the given object into the constant pool served by the objects field.
         */
        void addConstantPoolObject(SEXP object) {
            PROTECT(object);
            cp.push_back(object);
        }

        bool isReturnJumpNeeded = false;
        bool isResultVisible = true;

        llvm::Function * f;
        llvm::BasicBlock * b;

        llvm::BasicBlock * breakTarget;
        llvm::BasicBlock * nextTarget;

        const std::vector<llvm::Value *> & args() {
            return args_;
        }

        virtual llvm::Value * rho() = 0;
        virtual Context * clone() {
            assert(false);
            return nullptr;
        };


        unsigned functionId;

        /** Constant pool of the function.

          The constant pool always starts with the AST of the function being compiled, followed by any additional constants (notably created promises).
         */
        std::vector<SEXP> cp;

    protected:
        std::vector<llvm::Value *> args_;

        Context(Context * from):
            isReturnJumpNeeded(from->isReturnJumpNeeded),
            isResultVisible(from->isResultVisible),
            f(from->f),
            b(from->b),
            breakTarget(from->breakTarget),
            nextTarget(from->nextTarget),
            functionId(from->functionId),
            cp(std::move(from->cp)),
            args_(from->args_) { }

        Context(std::string const & name, llvm::Module * m, llvm::FunctionType * ty, bool isReturnJumpNeeded);
    };

    class ClosureContext : public Context {
    public:
        ClosureContext(std::string name, llvm::Module * m, bool isReturnJumpNeeded = false);
        
        llvm::Value * rho() override {
            return args_.at(1);
        }
        
        ClosureContext(Context * from): Context(from) {}

        Context * clone() override {
            return new ClosureContext(this);
        }
    };

    class PromiseContext : public ClosureContext {
    public:
        PromiseContext(std::string name, llvm::Module * m) : ClosureContext(name, m) {}
        PromiseContext(Context * from): ClosureContext(from) {}

        Context * clone() override {
            return new PromiseContext(this);
        }
    };
    
    class ICContext : public Context {
    public:
        ICContext(std::string name, llvm::Module * m, llvm::FunctionType * ty);
        ICContext(Context * from) = delete;
        llvm::Value * rho() override {
            return args_.at(args_.size() - 3);
        }
    };
    
    /** The module into which we are currently building.
     */
    llvm::Module * m_;

    /** Current context.
     */
    Context * c_ = nullptr;

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

