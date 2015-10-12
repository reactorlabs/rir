#include "Builder.h"

using namespace llvm;

namespace rjit {
namespace ir {

Builder::Context::Context(std::string const & name, Module * m, FunctionType* ty, bool isReturnJumpNeeded) : isReturnJumpNeeded(isReturnJumpNeeded) {
    // TODO the type is ugly
    f = Function::Create(ty, Function::ExternalLinkage, name, m);

    functionId = StackMap::nextStackmapId++;

    f->setGC("rjit");
    auto attrs = f->getAttributes();
    attrs = attrs.addAttribute(f->getContext(), AttributeSet::FunctionIndex,
                               "no-frame-pointer-elim", "true");
    attrs = attrs.addAttribute(f->getContext(), AttributeSet::FunctionIndex,
                               "statepoint-id", std::to_string(functionId));
    f->setAttributes(attrs);

    // create first basic block
    b = llvm::BasicBlock::Create(llvm::getGlobalContext(), "start", f, nullptr);
}

Builder::ClosureContext::ClosureContext(std::string name, llvm::Module * m, bool isReturnJumpNeeded) : Builder::Context(name, m, t::sexp_sexpsexpint, isReturnJumpNeeded) {
        // get rho value into context->rho for easier access
        llvm::Function::arg_iterator args = f->arg_begin();
        llvm::Value* body = args++;
        body->setName("body");
        args_.push_back(body);
        llvm::Value* rho = args++;
        rho->setName("rho");
        args_.push_back(rho);
        llvm::Value* useCache = args++;
        useCache->setName("useCache");
        args_.push_back(useCache);
}

Builder::ICContext::ICContext(std::string name, llvm::Module * m, llvm::FunctionType * ty) : Builder::Context(name, m, ty, false) {
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


} // namespace ir
} //namespace rjit
