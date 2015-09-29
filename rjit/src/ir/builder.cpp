#include "Builder.h"

using namespace llvm;

namespace rjit {
namespace ir {

Builder::Context::Context(std::string const & name, Module * m, bool isPromise) {
    // TODO the type is ugly
    f = Function::Create(t::sexp_sexpsexpint, Function::ExternalLinkage, name, m);

    functionId = StackMap::nextStackmapId++;

    f->setGC("statepoint-example");
    auto attrs = f->getAttributes();
    attrs = attrs.addAttribute(f->getContext(), AttributeSet::FunctionIndex,
                               "no-frame-pointer-elim", "true");
    attrs = attrs.addAttribute(f->getContext(), AttributeSet::FunctionIndex,
                               "statepoint-id", std::to_string(functionId));
    f->setAttributes(attrs);

    // get rho value into context->rho for easier access
    llvm::Function::arg_iterator args = f->arg_begin();
    llvm::Value* body = args++;
    body->setName("body");
    rho = args++;
    rho->setName("rho");
    llvm::Value* useCache = args++;
    useCache->setName("useCache");

    // create first basic block
    b = llvm::BasicBlock::Create(llvm::getGlobalContext(), "start", f, nullptr);

    isReturnJumpNeeded = isPromise;
}



} // namespace ir
} //namespace rjit
