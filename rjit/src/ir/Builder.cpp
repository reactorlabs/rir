#include "Builder.h"
#include "primitive_calls.h"

#include "Verifier.h"

using namespace llvm;

#include "RIntlns.h"
#include "Protect.h"
#include "TypeInfo.h"

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
    : Builder::Context(name, m, t::nativeFunction_t, isReturnJumpNeeded),
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
    useCache->setName("closure");
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

void Builder::openFunction(std::string const& name, SEXP ast, SEXP formals,
                           TypeFeedback* tf) {
    if (c_ != nullptr)
        contextStack_.push_back(c_);
    c_ = new ClosureContext(name, m_, formals);
    openFunctionOrPromise(ast);
    // Add nil as a placeholder. Runtime type feedback will be stored in a
    // vector allocated at the second and third constant pool slot.
    c_->addConstantPoolObject(R_NilValue);
    c_->addConstantPoolObject(R_NilValue);
    c_->addConstantPoolObject(R_NilValue);

    if (tf) {
        tf->attach(c_->f);
    }
}

void Builder::openFunctionOrPromise(SEXP ast) {
    // First entry in the const pool needs to be the ast
    c_->addConstantPoolObject(ast);
}

SEXP Builder::closePromise() {
    assert(dynamic_cast<PromiseContext*>(c_) and "Not a promise context");
    return closeFunctionOrPromise();
}

SEXP Builder::closeFunctionOrPromise() {
    assert((contextStack_.empty() or (contextStack_.back()->f != c_->f)) and
           "Not a function context");
    ClosureContext* cc = dynamic_cast<ClosureContext*>(c_);
    SEXP result = module()->getNativeSXP(cc->formals, c_->cp[0], c_->cp, c_->f);
    assert(ir::Verifier::check(c_->f));
    delete c_;
    if (contextStack_.empty()) {
        c_ = nullptr;
    } else {
        c_ = contextStack_.back();
        contextStack_.pop_back();
    }
    return result;
}

SEXP Builder::closeFunction() {
    assert(dynamic_cast<ClosureContext*>(c_) and "Not a closure context");
    assert((contextStack_.empty() or (contextStack_.back()->f != c_->f)) and
           "Not a function context");

    // Replace slot 1 with a vector to hold runtime type feedback
    assert(c_->cp[1] == R_NilValue);
    assert(c_->cp[2] == R_NilValue);
    assert(c_->cp[3] == R_NilValue);

    SEXP typeFeedback = allocVector(INTSXP, c_->instrumentationIndex.size());
    SEXP typeFeedbackName =
        allocVector(VECSXP, c_->instrumentationIndex.size());
    Protect p;
    p(typeFeedback);
    p(typeFeedbackName);
    for (auto e : c_->instrumentationIndex) {
        INTEGER(typeFeedback)[e.second] = TypeInfo();
        SET_VECTOR_ELT(typeFeedbackName, e.second, e.first);
    }
    c_->cp[1] = typeFeedback;
    c_->cp[2] = typeFeedbackName;
    SEXP invocationCount = allocVector(INTSXP, 1);
    p(invocationCount);
    INTEGER(invocationCount)[0] = 0;
    c_->cp[3] = invocationCount;

    return closeFunctionOrPromise();
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
        contextStack_.push_back(c_);
    c_ = new PromiseContext(name, m_);
    openFunctionOrPromise(ast);
}

void Builder::doGcCallback(void (*forward_node)(SEXP)) {
    m_->doGcCallback(forward_node);
    for (Context* c : contextStack_) {
        for (SEXP el : c->cp) {
            forward_node(el);
        }
    }
}

} // namespace ir
} // namespace rjit
