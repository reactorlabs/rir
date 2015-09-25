#ifndef COMPILER_H_
#define COMPILER_H_

#include "JITModule.h"

#include "RDefs.h"

namespace rjit {

#define JUMP(block) BranchInst::Create(block, context->b)

class Compiler {
  public:
    Compiler(std::string const& moduleName) : m(moduleName) {}

    SEXP compile(std::string const& name, SEXP bytecode) {
        SEXP result = compileFunction(name, bytecode);
        return result;
    }

    SEXP compileFunction(std::string const& name, SEXP ast,
                         bool isPromise = false);

    void jitAll();

  private:
    class Context {
      public:
        Context(std::string const& name, llvm::Module* m);

        void addObject(SEXP object);

        /** True if return jump is needed instead of return - this happens in
         * promises
         */
        bool returnJump;

        /** True if result of the expression should be visible, false otherwise.
         * Each expression resets the visibleResult to true.
         */
        bool visibleResult;

        llvm::Function* f;

        llvm::BasicBlock* b;

        /** Basic block to which break() statements should jump.
         */
        llvm::BasicBlock* breakBlock;

        /** Basic block to which next() statements should jump.
         */
        llvm::BasicBlock* nextBlock;

        llvm::Value* rho;

        std::vector<SEXP> objects;

        unsigned functionId;
    };

    /** Compiles an expression.

      The expression as a result is always visible by default, which can be
      changed in the respective compiling functions.

      An expression is either a constant, or symbol (variable read), or a
      function call.
      */
    llvm::Value* compileExpression(SEXP value);

    /** Compiles user constant, which constant marked with userConstant
     * intrinsic.
      */
    llvm::Value* compileConstant(SEXP value);

    /** Compiles a symbol, which reads as variable read using genericGetVar
     * intrinsic.
      */
    llvm::Value* compileSymbol(SEXP value);

    llvm::Value* compileICCallStub(llvm::Value* call, llvm::Value* op,
                                   std::vector<llvm::Value*>& callArgs);

    llvm::Value* compileCall(SEXP call);

    void compileArguments(SEXP argAsts, std::vector<llvm::Value*>& res);

    llvm::Value* compileArgument(SEXP arg, SEXP name);

    /** Many function calls may be compiled using intrinsics directly and not
      the R calling mechanism itself.

      This function determines based on the function symbol whether a
      compilation using intrinsics is possible and attempts it. It returns the
      result value of the compilation if successful, or nullptr if the function
      cannot be compiled using intrinsics.

      TODO this now uses even simpler approach than R bytecode compiler, I am
      simply assuming that these will never be overloaded. But we can change
      this when we want to.
      */
    llvm::Value* compileIntrinsic(SEXP call);

    /** Block (a call to {) is compiled as a simple sequence of its statements
     * with its return value being the result of the last statement. If a block
     * is empty, a visible R_NilValue is returned.
      */
    llvm::Value* compileBlock(SEXP block);

    /** Parenthesis expects a single argument only. Ellipsis is allowed, but not
      supported with the intrinsics at the moment so we default to R call.

      Otherwise markVisible intrinsic is applied to the result in accordance to
      the manual.
      */
    llvm::Value* compileParenthesis(SEXP arg);

    /** Similar to R bytecode compiler, only the body of the created function is
      compiled, the default arguments are left in their ast forms for now.

      TODO this should change.
     */
    llvm::Value* compileFunctionDefinition(SEXP fdef);

    /** Simple assignments (that is to a symbol) are compiled using the
     * genericSetVar intrinsic.
      */
    llvm::Value* compileAssignment(SEXP e);

    /** Super assignment is compiled as genericSetVarParentIntrinsic
     */
    llvm::Value* compileSuperAssignment(SEXP e);

    /** Return calls or returns in general are compiled depending on the
     * context. Usually a simple return instruction in bitcode is enough, but
     * while in promises, we must use longjmp, which is done by calling
     * returnJump intrinsic.
      */
    llvm::Value* compileReturn(llvm::Value* value, bool tail = false);

    /** Condition is compiled using the convertToLogicalNoNA intrinsic. True
     * block has to be always present, but false block does not have to be
     * present in which case an invisible R_NilValue should be returned.
      */
    llvm::Value* compileCondition(SEXP e);
    /** Compiles break. Whenever we see break in the compiler, we know it is for
      a loop where context was skipped and therefore it must always be
      translated as direct jump in bitcode.

      TODO The error is probably not right.
       */
    llvm::Value* compileBreak(SEXP ast);

    /** Compiles next. Whenever we see next in the compiler, we know it is for a
      loop where context was skipped and therefore it must always be translated
      as direct jump in bitcode.

      TODO The error is probably not right.
       */
    llvm::Value* compileNext(SEXP ast);

    /** Compiles repeat loop. This is simple infinite loop. Only break can exit
      it.

      Return value of break loop is invisible R_NilValue.
     */
    llvm::Value* compileRepeatLoop(SEXP ast);

    /** Compiles while loop.

      Return value of a while loop is invisible R_NilValue.
     */
    llvm::Value* compileWhileLoop(SEXP ast);

    /** For loop is compiled into the following structure:

          get the sequence
          length = sequence length
          index = 0
          goto forCond
      forCond:
          goto (index < length) ? forBody : forBreak
      forBody:
          setVar(controlVar, getForLoopValue(seq, index)
          body of the loop
          goto forNext
      forNext:
          index += 1
          goto forCond
      forBreak:

      This uses a jump too many, but it simplifies the SSA considerations and
      will be optimized by LLVM anyhow when we go for LLVM optimizations.
      */
    llvm::Value* compileForLoop(SEXP ast);

    /** Determines whether we can skip creation of the loop context or not. The
     * code is taken from Luke's bytecode compiler.
     */
    bool canSkipLoopContext(SEXP ast, bool breakOK = true);

    bool canSkipLoopContextList(SEXP ast, bool breakOK);

    /** Compiles the switch statement.

      There are two kinds of switch - integral and character one and they differ
      in what they are doing. The integral switch can be used always, and in its
      case the control variable is simple index to the cases. Contrary to the

          ctrl = evaluate switch control
          checkSwitchControl()
          goto sexptype() == STRSXP ? switchCharacter : switchIntegral
      switchIntegral:
          t = switchControlInteger(ctrl, numCases)
          switch (t):
      switchCharacter:
          t = switchControlCharacter(ctrl, call, caseStrings)
          switch (t):
      switchCase1:
          ...
          goto switchNext
      switchCaseN:
          ...
          goto switchNext
      switchNext:
          phi for the cases


     */
    llvm::Value* compileSwitch(SEXP call);

    /** Compiles operators that can be either binary, or unary, based on the
     * number of call arguments. Takes the binary and unary intrinsics to be
     * used and the full call ast.
      */
    llvm::Value* compileBinaryOrUnary(llvm::Function* b, llvm::Function* u,
                                      SEXP call);

    /** Compiles binary operator using the given intrinsic and full call ast.
      */
    llvm::Value* compileBinary(llvm::Function* f, SEXP call);

    /** Compiles unary operator using the given intrinsic and full call ast.
      */
    llvm::Value* compileUnary(llvm::Function* f, SEXP call);

    llvm::Value* constant(SEXP value);

    /** Converts given integer to bitcode value. This is just a simple shorthand
     * function, no magic here.
      */
    static llvm::ConstantInt* constant(int value) {
        return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                      llvm::APInt(32, value));
    }

    template <typename... Values>
    llvm::Value* INTRINSIC(llvm::Value* fun, Values... args) {
        return INTRINSIC(fun, std::vector<llvm::Value*>({args...}));
    }

    llvm::Value* INTRINSIC(llvm::Value* fun, std::vector<llvm::Value*> args);

    /** Current compilation module.

      The module contains the intrinsic function declarations as well as all
      compiled functions.
     */
    JITModule m;

    /** The context of current compilation.

      Each compiled function (including promises) has its own context. The
      context contains information about current return condition and
      visibility, break and next targets, R objects required and so on.
     */
    Context* context;

    /** List of relocations to be done when compiling.

      When a function is compiled, it is first translated to bitcode and a
      native SXP is created for it using nullptr for the native code. The
      function's SXP is added to the list of relocations here. When the
      compilation is done, the module is finalized and all SEXPs in the
      relocation lists are patched so that they point to correct native
      functions.
      */
    std::vector<SEXP> relocations;
};

} // namespace rjit

#endif // COMPILER_H_
