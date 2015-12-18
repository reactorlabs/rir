#include "Builder.h"
#include "Intrinsics.h"

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

void Builder::openFunction(std::string const& name, SEXP ast, SEXP formals) {
    if (c_ != nullptr)
        contextStack_.push(c_);
    c_ = new ClosureContext(name, m_, formals);
    c_->addConstantPoolObject(ast);
}

void Builder::openPromise(std::string const& name, SEXP ast) {
    if (c_ != nullptr)
        contextStack_.push(c_);
    c_ = new PromiseContext(name, m_);
    c_->addConstantPoolObject(ast);
}

} // namespace ir
} // namespace rjit
