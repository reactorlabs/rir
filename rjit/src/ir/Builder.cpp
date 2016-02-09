#include "Builder.h"
#include "primitive_calls.h"

#include "Verifier.h"

using namespace llvm;

namespace rjit {
namespace ir {

Builder::Context::Context(std::string const& name, JITModule* m,
                          FunctionType* ty, bool isReturnJumpNeeded)
    : isReturnJumpNeeded(isReturnJumpNeeded) {
    // TODO the type is ugly
    f = Function::Create(ty, Function::ExternalLinkage, name, m);

    f->setGC("rjit");
    auto attrs = f->getAttributes();
    attrs = attrs.addAttribute(f->getContext(), AttributeSet::FunctionIndex,
                               "no-frame-pointer-elim", "true");
    f->setAttributes(attrs);

    // create first basic block
    b = llvm::BasicBlock::Create(llvm::getGlobalContext(), "start", f, nullptr);
}

Builder::ClosureContext::ClosureContext(std::string name, JITModule* m,
                                        SEXP formals, bool isReturnJumpNeeded)
    : Builder::Context(name, m, t::sexp_sexpsexpint, isReturnJumpNeeded),
      formals(formals) {
    // get rho value into context->rho for easier access
    llvm::Function::arg_iterator args = f->arg_begin();
    llvm::Value* consts = args++;
    consts->setName("consts");
    args_.push_back(consts);
    llvm::Value* rho = args++;
    rho->setName("rho");
    args_.push_back(rho);
    llvm::Value* useCache = args++;
    useCache->setName("useCache");
    args_.push_back(useCache);
}

Builder::ICContext::ICContext(std::string name, JITModule* m,
                              llvm::FunctionType* ty)
    : Builder::Context(name, m, ty, false) {
    auto size = ty->getNumParams() - 5;

    // Load the args in the same order as the stub
    Function::arg_iterator argI = f->arg_begin();
    for (unsigned i = 0; i < size; i++) {
        args_.push_back(argI++);
    }

    auto call = argI++;
    call->setName("call");
    args_.push_back(call);
    auto fun = argI++;
    fun->setName("op");
    args_.push_back(fun);
    auto rho = argI++;
    rho->setName("rho");
    args_.push_back(rho);
    auto caller = argI++;
    caller->setName("caller");
    args_.push_back(caller);
    auto stackmapId = argI++;
    stackmapId->setName("stackmapId");
    args_.push_back(stackmapId);
}

llvm::BasicBlock* Builder::createBasicBlock() {
    return llvm::BasicBlock::Create(m_->getContext(), "", c_->f);
}


llvm::BasicBlock* Builder::createBasicBlock(std::string const& name) {
    return llvm::BasicBlock::Create(m_->getContext(), name, c_->f);
}


void Builder::openFunction(std::string const& name, SEXP ast, SEXP formals) {
    if (c_ != nullptr)
        contextStack_.push(c_);
    c_ = new ClosureContext(name, m_, formals);
    c_->addConstantPoolObject(ast);
}



SEXP Builder::closeFunction() {
    assert((contextStack_.empty() or (contextStack_.top()->f != c_->f)) and
           "Not a function context");

    ClosureContext* cc = dynamic_cast<ClosureContext*>(c_);
    SEXP result = module()->getNativeSXP(cc->formals, c_->cp[0], c_->cp, c_->f);
    // c_->f->dump();
    assert(ir::Verifier::check(c_->f));
    delete c_;
    if (contextStack_.empty()) {
        c_ = nullptr;
    } else {
        c_ = contextStack_.top();
        contextStack_.pop();
    }
    return result;
}

llvm::Function* Builder::closeIC() {
    assert(contextStack_.empty());
    assert(ir::Verifier::check(c_->f));
    llvm::Function* f = c_->f;
    delete c_;
    c_ = nullptr;
    return f;
}

void Builder::openPromise(std::string const& name, SEXP ast) {
    if (c_ != nullptr)
        contextStack_.push(c_);
    c_ = new PromiseContext(name, m_);
    c_->addConstantPoolObject(ast);
}

} // namespace ir
} // namespace rjit
