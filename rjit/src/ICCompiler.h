#include "JITModule.h"

#include "RDefs.h"

namespace rjit {
class ICCompiler {
  public:
    ICCompiler(unsigned size, JITModule& m);

    static llvm::Function* getStub(unsigned size, JITModule& m);

    void* compile(SEXP inCall, SEXP inFun, SEXP inRho);

  private:
    void initFunction(std::string name);

    static std::string stubName(unsigned size);

    void* finalize();

    llvm::Function* compileCallStub();
    llvm::Value* callMyStub();

    bool compileIc(SEXP inCall, SEXP inFun);

    bool compileGenericIc(SEXP inCall, SEXP inFun);

    llvm::Value* compileCall(SEXP call, SEXP op);

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

    /** Converts given integer to bitcode value. This is just a simple shorthand
     * function, no magic here.
      */
    static llvm::ConstantInt* constant(int value) {
        return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                      llvm::APInt(32, value));
    }

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

    llvm::Function* f = nullptr;
    llvm::BasicBlock* b;

    llvm::Value* rho;
    llvm::Value* fun;
    llvm::Value* caller;
    llvm::Value* stackmapId;
    llvm::Value* call;
    std::vector<llvm::Value*> icArgs;

    JITModule& m;
    unsigned size;

    unsigned functionId;

    static std::vector<bool> hasStub;
};

} // namespace rjit
