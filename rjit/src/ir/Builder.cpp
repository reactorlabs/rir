#include "Builder.h"
#include "intrinsics.h"

using namespace llvm;

namespace rjit {
namespace ir {

Builder::Context::Context(std::string const& name, Module* m, FunctionType* ty,
                          bool isReturnJumpNeeded)
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

Builder::ClosureContext::ClosureContext(std::string name, llvm::Module* m,
                                        bool isReturnJumpNeeded)
    : Builder::Context(name, m, t::sexp_sexpsexpint, isReturnJumpNeeded) {
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

Builder::ICContext::ICContext(std::string name, llvm::Module* m,
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

void Builder::openFunction(std::string const& name, SEXP ast, bool isPromise) {
    if (c_ != nullptr)
        contextStack_.push(c_);
    if (isPromise)
        c_ = new PromiseContext(name, m_);
    else
        c_ = new ClosureContext(name, m_);
    c_->addConstantPoolObject(ast);
}

SEXP Builder::createNativeSXP(RFunctionPtr fptr, SEXP ast,
                              std::vector<SEXP> const& objects, Function* f) {

    SEXP objs = allocVector(VECSXP, objects.size());
    PROTECT(objs);
    for (size_t i = 0; i < objects.size(); ++i)
        SET_VECTOR_ELT(objs, i, objects[i]);
    SEXP result = CONS(reinterpret_cast<SEXP>(fptr), objs);
    // all objects in objects + objs itself (now part of result)
    UNPROTECT(objects.size() + 1);
    SET_TAG(result, reinterpret_cast<SEXP>(f));
    SET_TYPEOF(result, NATIVESXP);
    return result;
}

} // namespace ir
} // namespace rjit
