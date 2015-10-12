#ifndef JIT_MODULE_H_
#define JIT_MODULE_H_

#include "Types.h"

namespace rjit {

#define DECLARE(name, type)                                                    \
    llvm::Function* name = llvm::Function::Create(                             \
        t::type, llvm::Function::ExternalLinkage, #name, m)

class JITModule {
  public:
    llvm::Module* m;
    DECLARE(markVisible, void_void);
    DECLARE(markInvisible, void_void);
    DECLARE(userConstant, void_sexp);
    DECLARE(genericGetVar, sexp_sexpsexp);
    DECLARE(checkFunction, void_sexp);
    DECLARE(getFunction, sexp_sexpsexp);
    DECLARE(sexpType, int_sexp);
    DECLARE(callBuiltin, sexp_sexpsexpsexpsexp);
    DECLARE(callSpecial, sexp_sexpsexpsexpsexp);
    DECLARE(callClosure, sexp_sexpsexpsexpsexp);
    DECLARE(callNative, sexp_sexpsexp);
    DECLARE(createPromise, sexp_sexpsexp);
    //    DECLARE(createArgument, sexp_sexp);
    //    DECLARE(createKeywordArgument, sexp_sexpsexp);
    //    DECLARE(addArgument, sexp_sexpsexp);
    //    DECLARE(addKeywordArgument, sexp_sexpsexpsexp);
    DECLARE(genericUnaryMinus, sexp_sexpsexpsexp);
    DECLARE(genericUnaryPlus, sexp_sexpsexpsexp);
    DECLARE(genericAdd, sexp_sexpsexpsexpsexp);
    DECLARE(genericSub, sexp_sexpsexpsexpsexp);
    DECLARE(genericMul, sexp_sexpsexpsexpsexp);
    DECLARE(genericDiv, sexp_sexpsexpsexpsexp);
    DECLARE(genericPow, sexp_sexpsexpsexpsexp);
    DECLARE(genericSqrt, sexp_sexpsexpsexp);
    DECLARE(genericExp, sexp_sexpsexpsexp);
    DECLARE(genericEq, sexp_sexpsexpsexpsexp);
    DECLARE(genericNe, sexp_sexpsexpsexpsexp);
    DECLARE(genericLe, sexp_sexpsexpsexpsexp);
    DECLARE(genericLt, sexp_sexpsexpsexpsexp);
    DECLARE(genericGt, sexp_sexpsexpsexpsexp);
    DECLARE(genericGe, sexp_sexpsexpsexpsexp);
    DECLARE(genericBitAnd, sexp_sexpsexpsexpsexp);
    DECLARE(genericBitOr, sexp_sexpsexpsexpsexp);
    DECLARE(genericNot, sexp_sexpsexpsexp);
    DECLARE(genericSetVar, void_sexpsexpsexp);
    DECLARE(genericSetVarParent, void_sexpsexpsexp);
    DECLARE(convertToLogicalNoNA, int_sexpsexp);
    DECLARE(createClosure, sexp_sexpsexpsexp);
    DECLARE(returnJump, void_sexpsexp);
    DECLARE(startFor, sexp_sexpsexp);
    DECLARE(loopSequenceLength, int_sexpsexp);
    DECLARE(getForLoopValue, sexp_sexpint);
    DECLARE(checkSwitchControl, void_sexpsexp);
    DECLARE(switchControlInteger, int_sexpint);
    DECLARE(switchControlCharacter, int_sexpsexpsexp);
    DECLARE(addArgument, sexp_sexpsexp);
    DECLARE(addKeywordArgument, sexp_sexpsexpsexp);
    DECLARE(addEllipsisArgumentHead, sexp_sexpsexpint);
    DECLARE(addEllipsisArgumentTail, sexp_sexpsexpint);
    DECLARE(CONS_NR, sexp_sexpsexp);
    DECLARE(closureQuickArgumentAdaptor, sexp_sexpsexp);
    DECLARE(initClosureContext, void_cntxtsexpsexpsexpsexpsexp);
    DECLARE(endClosureContext, void_cntxtsexp);
    DECLARE(closureNativeCallTrampoline, sexp_contxtsexpsexp);

    DECLARE(compileIC, compileIC_t);
    DECLARE(patchIC, patchIC_t);

    llvm::Function* stackmap;
    llvm::Function* patchpoint;

    JITModule(std::string const& name);

    operator llvm::Module*() { return m; }

    llvm::LLVMContext& getContext() { return m->getContext(); }

    void dump() { m->dump(); }

    llvm::Module* getM() { return m; }
};

#undef DECLARE
}

#endif // JIT_MODULE_H_
