#ifndef COMPILER_CPP
#define COMPILER_CPP

#include <cstdint>
#include <sstream>

#include <llvm/IR/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/Analysis/Passes.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"


#include "compiler.h"

using namespace llvm;

namespace {


namespace symbol {
#define DECLARE(name, txt) SEXP name = Rf_install(txt)

DECLARE(Block, "{");
DECLARE(Parenthesis, "(");
DECLARE(Assign, "<-");
DECLARE(If, "if");
DECLARE(Function, "function");
DECLARE(Return, "return");
DECLARE(For, "for");
DECLARE(While, "while");
DECLARE(Repeat, "repeat");
DECLARE(Break, "break");
DECLARE(Next, "next");
DECLARE(Add, "+");
DECLARE(Sub, "-");
DECLARE(Mul, "*");
DECLARE(Div, "/");
DECLARE(Pow, "^");
DECLARE(Sqrt, "sqrt");
DECLARE(Exp, "exp");
DECLARE(Eq, "==");
DECLARE(Ne, "!=");
DECLARE(Lt, "<");
DECLARE(Le, "<=");
DECLARE(Ge, ">=");
DECLARE(Gt, ">");
DECLARE(BitAnd, "&");
DECLARE(BitOr, "|");
DECLARE(Not, "!");
DECLARE(Ellipsis, "...");


#undef DECLARE
}

PointerType * initializeTypes();




namespace t {

PointerType * SEXP = initializeTypes();

StructType * SEXPREC;

FunctionType * void_void;
FunctionType * void_sexp;
FunctionType * void_sexpsexp;
FunctionType * void_sexpsexpsexp;
FunctionType * sexp_sexp;
FunctionType * sexp_sexpsexp;
FunctionType * sexp_sexpsexpsexp;
FunctionType * sexp_sexpsexpsexpsexp;

FunctionType * sexp_sexpsexpint;
FunctionType * int_sexp;
FunctionType * int_sexpsexp;

}

PointerType * initializeTypes() {
    LLVMContext & context = getGlobalContext();
    std::vector<Type*> fields;
    Type * t_Int = IntegerType::get(context, 32);
    StructType * t_sxpinfo_struct = StructType::create(context, "struct.sxpinfo_struct");
    // sxpinfo_struct is just int32 in a structure, the bitmasking is not a concern of the type
    fields = { t_Int };
    t_sxpinfo_struct->setBody(fields, false);
    // SEXPREC
    t::SEXPREC = StructType::create(context, "struct.SEXPREC");
    // SEXP
    t::SEXP = PointerType::get(t::SEXPREC, 0);
    // SEXPREC, first the union
    StructType * u1 = StructType::create(context,"union.SEXP_SEXP_SEXP");
    fields = { t::SEXP, t::SEXP, t::SEXP };
    u1->setBody(fields, false);
    // now the real SEXPREC
    fields = { t_sxpinfo_struct, t::SEXP, t::SEXP, t::SEXP, u1 };
    t::SEXPREC->setBody(fields, false);
    // API function types
    Type * t_void = Type::getVoidTy(context);
#define DECLARE(name, ret, ...) fields = { __VA_ARGS__ }; t::name = FunctionType::get(ret, fields, false)
    DECLARE(void_void, t_void);
    DECLARE(void_sexp, t_void, t::SEXP);
    DECLARE(void_sexpsexp, t_void, t::SEXP, t::SEXP);
    DECLARE(void_sexpsexpsexp, t_void, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(sexp_sexp, t::SEXP, t::SEXP);
    DECLARE(sexp_sexpsexp, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(sexp_sexpsexpsexp, t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(sexp_sexpsexpsexpsexp, t::SEXP, t::SEXP, t::SEXP, t::SEXP, t::SEXP);
    DECLARE(sexp_sexpsexpint, t::SEXP, t::SEXP, t::SEXP, t_Int);
    DECLARE(int_sexp, t_Int, t::SEXP);
    DECLARE(int_sexpsexp, t_Int, t::SEXP, t::SEXP);
#undef DECLARE
    // initialize LLVM backend
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
    return t::SEXP;
}

#define DECLARE(name, type) Function * name = Function::Create(t::type, Function::ExternalLinkage, #name, m)


class JITModule {
public:
    Module * m;
    DECLARE(markVisible, void_void);
    DECLARE(markInvisible, void_void);
    DECLARE(userConstant, void_sexp);
    DECLARE(genericGetVar, sexp_sexpsexp);
    DECLARE(checkFunction, void_sexp);
    DECLARE(getFunction, sexp_sexpsexp);
    DECLARE(sexpType, int_sexp);
    DECLARE(call, sexp_sexpsexpsexpsexp);
    DECLARE(createPromise, sexp_sexpsexp);
    DECLARE(createArgument, sexp_sexp);
    DECLARE(createKeywordArgument, sexp_sexpsexp);
    DECLARE(addArgument, sexp_sexpsexp);
    DECLARE(addKeywordArgument, sexp_sexpsexpsexp);
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
    DECLARE(convertToLogicalNoNA, int_sexpsexp);
    DECLARE(createClosure, sexp_sexpsexpsexp);
    DECLARE(returnJump, void_sexpsexp);

    JITModule(std::string const & name):
        m(new Module(name, getGlobalContext())) {}

    operator Module * () {
        return m;
    }
};

#undef DECLARE


SEXP createNativeSXP(RFunctionPtr fptr, SEXP ast, std::vector<SEXP> const & objects, Function * f) {
    SEXP objs = allocVector(VECSXP, objects.size() + 1);
    PROTECT(objs);
    SET_VECTOR_ELT(objs, 0, ast);
    for (size_t i = 0; i < objects.size(); ++i)
        SET_VECTOR_ELT(objs, i +1, objects[i]);
    SEXP result = CONS(reinterpret_cast<SEXP>(fptr), objs);
    UNPROTECT(objects.size() + 1); // all objects in objects + objs itself which is now part of result
    SET_TAG(result, reinterpret_cast<SEXP>(f));
    SET_TYPEOF(result, NATIVESXP);
    return result;
}


#define ARGS(...) std::vector<Value *>({ __VA_ARGS__ })
#define INTRINSIC(name, ...) CallInst::Create(name, ARGS(__VA_ARGS__), "", context->b)
#define JUMP(block) BranchInst::Create(block, context->b);



class Compiler {
public:

    Compiler(std::string const & moduleName):
        m(moduleName) {}

    SEXP compile(std::string const & name, SEXP bytecode) {
        SEXP result = compileFunction(name, bytecode);
        // create execution engine and finalize the module
        ExecutionEngine * engine = EngineBuilder(std::unique_ptr<Module>(m)).create();
        engine->finalizeObject();
        // perform all the relocations
        for (SEXP s : relocations)
            SETCAR(s, reinterpret_cast<SEXP>(engine->getPointerToFunction(reinterpret_cast<Function*>(TAG(s)))));
        return result;
    }

private:
    class Context {
    public:

        Context(std::string const & name, Module * m) {
            f = Function::Create(t::sexp_sexpsexpint, Function::ExternalLinkage, name, m);
            Function::arg_iterator args = f->arg_begin();
            Value * body = args++;
            body->setName("body");
            rho = args++;
            rho->setName("rho");
            Value * useCache = args++;
            useCache->setName("useCache");
            b = BasicBlock::Create(getGlobalContext(), "start", f, nullptr);
            returnJump = false;
        }

        void addObject(SEXP object) {
            PROTECT(object);
            objects.push_back(object);
        }

        /** True if return jump is needed instead of return - this happens in promises
         */
        bool returnJump;

        /** True if result of the expression should be visible, false otherwise. Each expression resets the visibleResult to true.
         */
        bool visibleResult;

        Function * f;

        BasicBlock * b;

        /** Basic block to which break() statements should jump.
         */
        BasicBlock * breakBlock;

        /** Basic block to which next() statements should jump.
         */
        BasicBlock * nextBlock;

        Value * rho;

        std::vector<SEXP> objects;

    };

    SEXP compileFunction(std::string const & name, SEXP ast, bool isPromise = false) {
        Context * old = context;
        context = new Context(name, m);
        if (isPromise)
            context->returnJump = true;
        Value * last = compileExpression(ast);
        // since we are going to insert implicit return, which is a simple return even from a promise
        context->returnJump = false;
        if (last != nullptr)
            compileReturn(last, /*tail=*/ true);
        // now we create the NATIVESXP
        SEXP result = createNativeSXP(nullptr, ast, context->objects, context->f);
        // add the non-jitted SEXP to relocations
        relocations.push_back(result);
        delete context;
        context = old;
        return result;
    }

    /** Compiles an expression.

      The expression as a result is always visible by default, which can be changed in the respective compiling functions.

      An expression is either a constant, or symbol (variable read), or a function call.
      */
    Value * compileExpression(SEXP value) {
        context->visibleResult = true;
        switch (TYPEOF(value)) {
        case SYMSXP:
            return compileSymbol(value);
        case LANGSXP:
            return compileCall(value);
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
            return compileConstant(value);
        default:
            assert(false && "Unknown SEXP type in compiled ast.");
        }
    }

    /** Compiles user constant, which is SEXP constant marked with userConstant intrinsic.
      */
    Value * compileConstant(SEXP value) {
        Value * result = constant(value);
        INTRINSIC(m.userConstant, result);
        return result;
    }

    /** Compiles a symbol, which reads as variable read using genericGetVar intrinsic.
      */
    Value * compileSymbol(SEXP value) {
        return INTRINSIC(m.genericGetVar, constant(value), context->rho);
    }

    /** Compiles a call expressed in the AST.

      For simple calls checks if these can be compiled using intrinsics and does so, if possible. For others emits the function getting / checking code and then generates the arguments and calls the function.

      This differs depending on the function type. Special functions do not evaluate their arguments at all, builtin functions are always eager and normal functions are lazy.
     */
    Value * compileCall(SEXP call) {
        Value * f;
        if (TYPEOF(CAR(call)) != SYMSXP) {
            // it is a complex function, first get the value of the function and then check it
            f = compileExpression(CAR(call));
            INTRINSIC(m.checkFunction, f);
        } else {
            // it is simple function - try compiling it with intrinsics
            f = compileIntrinsic(call);
            if (f != nullptr)
                return f;
            // otherwise just do get function
            f = INTRINSIC(m.getFunction, constant(CAR(call)), context->rho);
        }
        // now we must compile the arguments, this depends on the actual type of the function - promises by default, eager for builtins and no evaluation for specials
        Value * ftype = INTRINSIC(m.sexpType, f);
        // switch the function call execution based on the function type
        BasicBlock * special = BasicBlock::Create(getGlobalContext(), "special", context->f, nullptr);
        BasicBlock * builtin = BasicBlock::Create(getGlobalContext(), "builtin", context->f, nullptr);

        BasicBlock * closure = BasicBlock::Create(getGlobalContext(), "closure", context->f, nullptr);
        BasicBlock * next = BasicBlock::Create(getGlobalContext(), "next", context->f, nullptr);
        SwitchInst * sw = SwitchInst::Create(ftype, closure, 2, context->b);
        sw->addCase(constant(SPECIALSXP), special);
        sw->addCase(constant(BUILTINSXP), builtin);
        // in special case, do not evaluate arguments and just call the function
        context->b = special;
        Value * specialResult = INTRINSIC(m.call, constant(call), f, constant(R_NilValue), context->rho);
        JUMP(next);
        // in builtin mode evaluate all arguments eagerly
        context->b = builtin;
        Value * args = compileArguments(CDR(call), /*eager=*/ true);
        Value * builtinResult = INTRINSIC(m.call, constant(call), f, args, context->rho);
        builtin = context->b; // bb might have changed during arg evaluation
        JUMP(next);
        // in general closure case the arguments will become promises
        context->b = closure;
        args = compileArguments(CDR(call), /*eager=*/ false);
        Value * closureResult = INTRINSIC(m.call, constant(call), f, args, context->rho);
        JUMP(next);
        // add a phi node for the call result
        context->b = next;
        PHINode * phi = PHINode::Create(t::SEXP, 3, "", context->b);
        phi->addIncoming(specialResult, special);
        phi->addIncoming(builtinResult, builtin);
        phi->addIncoming(closureResult, closure);
        return phi;
    }

    /** Compiles arguments for given function.

      Creates the pairlist of arguments used in R from the arguments and their names.
      */
    Value * compileArguments(SEXP args, bool eager) {
        // if there are no arguments
        if (args == R_NilValue)
            return constant(R_NilValue);
        Value * av = compileArgument(CAR(args), eager);
        Value * result;
        if (TAG(args) == R_NilValue)
            result = INTRINSIC(m.createArgument, av);
        else
            result = INTRINSIC(m.createKeywordArgument, av, constant(TAG(args)));
        Value * last = result;
        args = CDR(args);
        while (args != R_NilValue) {
            av = compileArgument(CAR(args), eager);
            if (TAG(args) == R_NilValue)
                last = INTRINSIC(m.addArgument, av, last);
            else
                last = INTRINSIC(m.addKeywordArgument, av, constant(TAG(args)), last);
            args = CDR(args);
        }
        return result;
    }

    /** Compiles a single argument.

      Self evaluating literals are always returned as SEXP constants, anything else is either evaluated directly if eager is true, or they are compiled as new promises.
     */
    Value * compileArgument(SEXP arg, bool eager) {
        switch (TYPEOF(arg)) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
            // literals are self-evaluating
            return constant(arg);
        default:
            if (eager)
                return compileExpression(arg);
            // we must create a promise out of the argument
            SEXP code = compileFunction("promise", arg, /*isPromise=*/ true);
            context->addObject(code);
            return INTRINSIC(m.createPromise, constant(code), context->rho);
        }
    }

    /** Many function calls may be compiled using intrinsics directly and not the R calling mechanism itself.

      This function determines based on the function symbol whether a compilation using intrinsics is possible and attempts it. It returns the result value of the compilation if successful, or nullptr if the function cannot be compiled using intrinsics.

      TODO this now uses even simpler approach than R bytecode compiler, I am simply assuming that these will never be overloaded. But we can change this when we want to.
      */
    Value * compileIntrinsic(SEXP call) {
#define CASE(sym) if (CAR(call) == sym)
        CASE(symbol::Block)
            return compileBlock(CDR(call));
        CASE(symbol::Parenthesis)
            return compileParenthesis(CDR(call));
        CASE(symbol::Function)
            return compileFunctionDefinition(CDR(call));
        CASE(symbol::Return) {
            return (CDR(call) == R_NilValue) ? compileReturn(constant(R_NilValue)) : compileReturn(compileExpression(CAR(CDR(call))));
        }
        CASE(symbol::Assign)
            return compileAssignment(call);
        CASE(symbol::If)
            return compileCondition(call);
        CASE(symbol::Break)
            return compileBreak(call);
        CASE(symbol::Next)
            return compileNext(call);
        CASE(symbol::Repeat)
            return compileRepeatLoop(call);
        CASE(symbol::While)
            return compileWhileLoop(call);
        CASE(symbol::For)
            return compileForLoop(call);
        CASE(symbol::Add)
            return compileBinaryOrUnary(m.genericAdd, m.genericUnaryPlus, call);
        CASE(symbol::Sub)
            return compileBinaryOrUnary(m.genericSub, m.genericUnaryMinus, call);
        CASE(symbol::Mul)
            return compileBinary(m.genericMul, call);
        CASE(symbol::Div)
            return compileBinary(m.genericDiv, call);
        CASE(symbol::Pow)
            return compileBinary(m.genericPow, call);
        CASE(symbol::Sqrt)
            return compileUnary(m.genericSqrt, call);
        CASE(symbol::Exp)
            return compileUnary(m.genericExp, call);
        CASE(symbol::Eq)
            return compileBinary(m.genericEq, call);
        CASE(symbol::Ne)
            return compileBinary(m.genericNe, call);
        CASE(symbol::Lt)
            return compileBinary(m.genericLt, call);
        CASE(symbol::Le)
            return compileBinary(m.genericLe, call);
        CASE(symbol::Ge)
            return compileBinary(m.genericGe, call);
        CASE(symbol::Gt)
            return compileBinary(m.genericGt, call);
        CASE(symbol::BitAnd)
            return compileBinary(m.genericBitAnd, call);
        CASE(symbol::BitOr)
            return compileBinary(m.genericBitOr, call);
        CASE(symbol::Not)
            return compileUnary(m.genericNot, call);

        return nullptr;
#undef CASE
    }

    /** Block (a call to {) is compiled as a simple sequence of its statements with its return value being the result of the last statement. If a block is empty, a visible R_NilValue is returned.
      */
    Value * compileBlock(SEXP block) {
        Value * result = nullptr;
        while (block != R_NilValue) {
            result = compileExpression(CAR(block));
            block = CDR(block);
        }
        if (result == nullptr)
            result = constant(R_NilValue);
        return result;
    }

    /** Parenthesis expects a single argument only. Ellipsis is allowed, but not supported with the intrinsics at the moment so we default to R call.

      Otherwise markVisible intrinsic is applied to the result in accordance to the manual.
      */
    Value * compileParenthesis(SEXP arg) {\
        arg = CAR(arg);
        if (arg == symbol::Ellipsis)
            return nullptr; // we can't yet do this
        Value * result = compileExpression(arg);
        context->visibleResult = true;
        return result;
    }

    /** Similar to R bytecode compiler, only the body of the created function is compiled, the default arguments are left in their ast forms for now.

      TODO this should change.
     */
    Value * compileFunctionDefinition(SEXP fdef) {
        SEXP forms = CAR(fdef);
        SEXP body = compileFunction("function", CAR(CDR(fdef)));
        context->addObject(body);
        return INTRINSIC(m.createClosure, constant(forms), constant(body), context->rho);
    }

    /** Simple assignments (that is to a symbol) are compiled using the genericSetVar intrinsic.
      */
    Value * compileAssignment(SEXP e) {
        e = CDR(e);
        // intrinsic only handles simple assignments
        if (TYPEOF(CAR(e)) != SYMSXP)
            return nullptr;
        Value * v = compileExpression(CAR(CDR(e)));
        INTRINSIC(m.genericSetVar, constant(CAR(e)), v, context->rho);
        context->visibleResult = false;
        return v;
    }

    /** Return calls or returns in general are compiled depending on the context. Usually a simple return instruction in bitcode is enough, but while in promises, we must use longjmp, which is done by calling returnJump intrinsic.
      */
    Value * compileReturn(Value * value, bool tail = false) {
        if (not context->visibleResult)
            INTRINSIC(m.markInvisible);
        if (context->returnJump) {
            INTRINSIC(m.returnJump, value, context->rho);
            // we need to have a return instruction as well to fool LLVM into believing the basic block has a terminating instruction
            ReturnInst::Create(getGlobalContext(), constant(R_NilValue), context->b);
        } else {
            ReturnInst::Create(getGlobalContext(), value, context->b);
        }
        // this is here to allow compilation of wrong code where statements are even after return
        if (not tail)
            context->b = BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
        return nullptr;
    }

    /** Condition is compiled using the convertToLogicalNoNA intrinsic. True block has to be always present, but false block does not have to be present in which case an invisible R_NilValue should be returned.
      */
    Value * compileCondition(SEXP e) {
        e = CDR(e);
        SEXP condAst = CAR(e);
        e = CDR(e);
        SEXP trueAst = CAR(e);
        e = CDR(e);
        SEXP falseAst = (e != R_NilValue) ? CAR(e) : nullptr;
        Value * cond2 = compileExpression(condAst);
        Value * cond = INTRINSIC(m.convertToLogicalNoNA, cond2, constant(condAst));
        BasicBlock * ifTrue = BasicBlock::Create(getGlobalContext(), "ifTrue", context->f, nullptr);

        BasicBlock * ifFalse = BasicBlock::Create(getGlobalContext(), "ifFalse", context->f, nullptr);
        BasicBlock * next = BasicBlock::Create(getGlobalContext(), "next", context->f, nullptr);
        ICmpInst * test = new ICmpInst(*(context->b), ICmpInst::ICMP_EQ, cond, constant(TRUE), "condition");
        BranchInst::Create(ifTrue, ifFalse, test, context->b);
        // true case has to be always present
        context->b = ifTrue;
        Value * trueResult = compileExpression(trueAst);
        JUMP(next);
        ifTrue = context->b;
        // false case may not be present in which case invisible R_NilValue should be returned
        context->b = ifFalse;
        Value * falseResult;
        if (falseAst == nullptr) {
            falseResult = constant(R_NilValue);
            context->visibleResult = false;
        } else {
            falseResult = compileExpression(falseAst);
            ifFalse = context->b;
        }
        JUMP(next);
        // add a phi node for the result
        context->b = next;
        PHINode * phi = PHINode::Create(t::SEXP, 2, "", context->b);
        phi->addIncoming(trueResult, ifTrue);
        phi->addIncoming(falseResult, ifFalse);
        return phi;
    }

    /** Compiles break. Whenever we see break in the compiler, we know it is for a loop where context was skipped and therefore it must always be translated as direct jump in bitcode.

      TODO The error is probably not right.
       */
    Value * compileBreak(SEXP ast) {
        assert(context->breakBlock != nullptr and "Break outside loop");
        JUMP(context->breakBlock);
        // TODO this is really simple, but fine for us - dead code elimination will remove the block if required
        context->b = BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
        return constant(R_NilValue);
    }

    /** Compiles next. Whenever we see next in the compiler, we know it is for a loop where context was skipped and therefore it must always be translated as direct jump in bitcode.

      TODO The error is probably not right.
       */
    Value * compileNext(SEXP ast) {
        assert(context->nextBlock != nullptr and "Next outside loop");
        JUMP(context->nextBlock);
        // TODO this is really simple, but fine for us - dead code elimination will remove the block if required
        context->b = BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
        return constant(R_NilValue);
    }

    /** Compiles repeat loop. This is simple infinite loop. Only break can exit it.

      Return value of break loop is invisible R_NilValue.
     */
    Value * compileRepeatLoop(SEXP ast) {
        SEXP bodyAst = CAR(CDR(ast));
        if (not canSkipLoopContext(bodyAst))
            return nullptr;
        // save old loop pointers from the context
        BasicBlock * oldBreak = context->breakBlock;
        BasicBlock * oldNext = context->nextBlock;
        // create the body and next basic blocks
        context->nextBlock = BasicBlock::Create(getGlobalContext(), "repeatBody", context->f, nullptr);
        context->breakBlock = BasicBlock::Create(getGlobalContext(), "repeatNext", context->f, nullptr);
        JUMP(context->nextBlock);
        context->b = context->nextBlock;
        compileExpression(bodyAst);
        JUMP(context->nextBlock);
        context->b = context->breakBlock;
        // restore the old loop pointers in the context
        context->breakBlock = oldBreak;
        context->nextBlock = oldNext;
        // return R_NilValue
        context->visibleResult = false;
        return constant(R_NilValue);
    }

    /** Compiles while loop.

      Return value of a while loop is invisible R_NilValue.
     */
    Value * compileWhileLoop(SEXP ast) {
        SEXP condAst = CAR(CDR(ast));
        SEXP bodyAst = CAR(CDR(CDR(ast)));
        if (not canSkipLoopContext(bodyAst))
            return nullptr;
        // save old loop pointers from the context
        BasicBlock * oldBreak = context->breakBlock;
        BasicBlock * oldNext = context->nextBlock;
        // create the body and next basic blocks
        context->nextBlock = BasicBlock::Create(getGlobalContext(), "whileCond", context->f, nullptr);
        context->breakBlock = BasicBlock::Create(getGlobalContext(), "whileNext", context->f, nullptr);
        JUMP(context->nextBlock);
        context->b = context->nextBlock;
        // compile the condition
        Value * cond2 = compileExpression(condAst);
        Value * cond = INTRINSIC(m.convertToLogicalNoNA, cond2, constant(condAst));
        BasicBlock * whileBody = BasicBlock::Create(getGlobalContext(), "whileBody", context->f, nullptr);
        ICmpInst * test = new ICmpInst(*(context->b), ICmpInst::ICMP_EQ, cond, constant(TRUE), "condition");
        BranchInst::Create(whileBody, context->breakBlock, test, context->b);
        // compile the body
        context-> b = whileBody;
        compileExpression(bodyAst);
        JUMP(context->nextBlock);
        context->b = context->breakBlock;
        // restore the old loop pointers in the context
        context->breakBlock = oldBreak;
        context->nextBlock = oldNext;
        // return R_NilValue
        context->visibleResult = false;
        return constant(R_NilValue);
    }

    Value * compileForLoop(SEXP ast) {
        return nullptr;
    }

    /** Determines whether we can skip creation of the loop context or not. The code is taken from Luke's bytecode compiler.
     */
    bool canSkipLoopContext(SEXP ast, bool breakOK = true) {
        if (TYPEOF(ast) == LANGSXP) {
            SEXP cs = CAR(ast);
            if (TYPEOF(cs) == SYMSXP)
                if (not breakOK and (cs == symbol::Break or cs == symbol::Next))
                    return false;
                else if (cs == symbol::Function or cs == symbol::For or cs == symbol::While or cs == symbol::Repeat)
                    return true;
                else if (cs == symbol::Parenthesis or cs == symbol::Block or cs == symbol::If)
                    return canSkipLoopContextList(CDR(ast), breakOK);
                else
                    return canSkipLoopContextList(CDR(ast), false);
            // this is change to Luke's code - I believe that whatever will return us the function to call might be compiled using intrinsics and therefore should follow the breakOK rules, not the rules for promises the arguments of user functions do
            return canSkipLoopContext(CAR(ast), breakOK) and canSkipLoopContextList(CDR(ast), false);
        } else {
            return true;
        }
    }

    bool canSkipLoopContextList(SEXP ast, bool breakOK) {
        while (ast != R_NilValue) {
            if (not canSkipLoopContext(CAR(ast), breakOK))
                return false;
            ast = CDR(ast);
        }
        return true;
    }

    /** Compiles operators that can be either binary, or unary, based on the number of call arguments. Takes the binary and unary intrinsics to be used and the full call ast.
      */
    Value * compileBinaryOrUnary(Function * b, Function * u, SEXP call) {
        Value * lhs = compileExpression(CAR(CDR(call)));
        if (CDR(CDR(call)) != R_NilValue) {
            Value * rhs = compileExpression(CAR(CDR(CDR(call))));
            return INTRINSIC(b, lhs, rhs, constant(call), context->rho);
        } else {
            return INTRINSIC(u, lhs, constant(call), context->rho);
        }
    }

    /** Compiles binary operator using the given intrinsic and full call ast.
      */
    Value * compileBinary(Function * f, SEXP call) {
        Value * lhs = compileExpression(CAR(CDR(call)));
        Value * rhs = compileExpression(CAR(CDR(CDR(call))));
        return INTRINSIC(f, lhs, rhs, constant(call), context->rho);
    }

    /** Compiles unary operator using the given intrinsic and full call ast.
      */
    Value * compileUnary(Function * f, SEXP call) {
        Value * op = compileExpression(CAR(CDR(call)));
        return INTRINSIC(f, op, constant(call), context->rho);
    }

    /** Converts given SEXP to a bitcode constant. The SEXP address is taken as an integer constant into LLVM which is then converted to SEXP.

      NOTE that this approach assumes that any GC used is non-moving. We are using it because it removes one level of indirection when reading it from the constants vector as R bytecode compiler does.
      */
    Value * constant(SEXP value) {
        return ConstantExpr::getCast(Instruction::IntToPtr, ConstantInt::get(getGlobalContext(), APInt(64, (std::uint64_t)value)), t::SEXP);
    }

    /** Converts given integer to bitcode value. This is just a simple shorthand function, no magic here.
      */
    ConstantInt * constant(int value) {
        return ConstantInt::get(getGlobalContext(), APInt(32, value));
    }

    /** Current compilation module.

      The module contains the intrinsic function declarations as well as all compiled functions.
     */
    JITModule m;

    /** The context of current compilation.

      Each compiled function (including promises) has its own context. The context contains information about current return condition and visibility, break and next targets, R objects required and so on.
     */
    Context * context;

    /** List of relocations to be done when compiling.

      When a function is compiled, it is first translated to bitcode and a native SXP is created for it using nullptr for the native code. The function's SXP is added to the list of relocations here. When the compilation is done, the module is finalized and all SEXPs in the relocation lists are patched so that they point to correct native functions.
      */
    std::vector<SEXP> relocations;


};


} // namespace




REXPORT SEXP compile(SEXP ast) {
    return Compiler("module").compile("rfunction", ast);
}

REXPORT SEXP printBitcode(SEXP ast) {

}

REXPORT SEXP nativeAST(SEXP ast) {

}



#endif // COMPILER_CPP

