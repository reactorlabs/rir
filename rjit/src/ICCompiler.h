#ifndef IC_COMPILER_H
#define IC_COMPILER_H

#include "ir/Builder.h"

#include "RDefs.h"

namespace rjit {
class ICCompiler {
  public:
    ICCompiler(unsigned size, ir::Builder& b, std::string name);
    ICCompiler(unsigned size, ir::Builder& b);

    static llvm::Function* getStub(unsigned size, ir::Builder& b);

    void* compile(SEXP inCall, SEXP inFun, SEXP inRho);

    static std::string stubName(unsigned size);

  private:
    llvm::Value* call() { return b.args().at(size); }
    llvm::Value* fun() { return b.args().at(size + 1); }
    llvm::Value* rho() { return b.rho(); }
    llvm::Value* caller() { return b.args().at(size + 3); }
    llvm::Value* stackmapId() { return b.args().at(size + 4); }

    void* finalize();

    llvm::Function* compileCallStub();
    void callIcMiss();

    bool compileIc(SEXP inCall, SEXP inFun);

    bool compileGenericIc(SEXP inCall, SEXP inFun);

    /** Compiles arguments for given function.

      Creates the pairlist of arguments used in R from the arguments and their
      names.
      */
    llvm::Value* compileArguments(SEXP argAsts, bool eager);

    /** Compiles a single argument.

      Self evaluating literals are always returned as SEXP constants, anything
      else is either evaluated directly if eager is true, or they are compiled
      as new promises.
     */
    llvm::Value* compileArgument(llvm::Value* arglist, SEXP argAst, int argnum,
                                 bool eager);

    llvm::Value* constant(SEXP value);

    template <typename... Values>
    llvm::Value* INTRINSIC_NO_SAFEPOINT(llvm::Value* fun, Values... args) {
        return INTRINSIC_NO_SAFEPOINT(fun,
                                      std::vector<llvm::Value*>({args...}));
    }

    llvm::Value* INTRINSIC_NO_SAFEPOINT(llvm::Value* fun,
                                        std::vector<llvm::Value*> args);

    template <typename... Values>
    llvm::Value* INTRINSIC(llvm::Value* fun, Values... args) {
        return INTRINSIC(fun, std::vector<llvm::Value*>({args...}));
    }

    llvm::Value* INTRINSIC(llvm::Value* fun, std::vector<llvm::Value*> args);

    llvm::FunctionType* ic_t;

    ir::Builder& b;
    unsigned size;
    std::string name;

#define DECLARE(name) llvm::Function* name
    DECLARE(CONS_NR);
    DECLARE(closureQuickArgumentAdaptor);
    DECLARE(initClosureContext);
    DECLARE(endClosureContext);
    DECLARE(closureNativeCallTrampoline);
    DECLARE(compileIC);
    DECLARE(patchIC);
    DECLARE(callNative);
#undef DECLARE
};

} // namespace rjit

#endif
