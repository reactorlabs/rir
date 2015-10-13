#include "Builder.h"
#include "intrinsics.h"

using namespace llvm;

namespace rjit {
namespace ir {

Builder::Context::Context(Builder & builder, std::string const & name, Module * m, bool isPromise) {
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
    body = args++;
    body->setName("body");
    rho = args++;
    rho->setName("rho");
    llvm::Value* useCache = args++;
    useCache->setName("useCache");

    // create first basic block
    b = llvm::BasicBlock::Create(llvm::getGlobalContext(), "start", f, nullptr);
    isReturnJumpNeeded = isPromise;
}

void Builder::openFunction(std::string const & name, SEXP ast, bool isPromise) {
    if (c_ != nullptr)
        contextStack_.push(c_);
    c_ = new Context(*this, name, m_, isPromise);
    c_->addConstantPoolObject(ast);
    c_->consts = ExtractConstantPool::create(*this, c_->body);
}

/** Creates new context for a loop. Initializes the basic blocks for break and next targets. */
void Builder::openLoop() {
    assert(c_ != nullptr and "Cannot open loop context when not in function");
    contextStack_.push(c_);
    c_ = new Context(c_);
    c_->breakTarget = createBasicBlock("break");
    c_->nextTarget = createBasicBlock("next");
}

/** Closes the open loop and pops its context.
 */
void Builder::closeLoop() {
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
SEXP Builder::closeFunction() {
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



} // namespace ir
} //namespace rjit
