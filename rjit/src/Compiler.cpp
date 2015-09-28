#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/Analysis/Passes.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"

#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCs.h"

#include "Compiler.h"
#include "JITCompileLayer.h"
#include "StackMap.h"
#include "StackMapParser.h"
#include "ICCompiler.h"
#include "Symbols.h"
#include "Runtime.h"

#include "RIntlns.h"

#include <memory>

using namespace llvm;

namespace {

void emitStackmap(uint64_t id, std::vector<Value*> values, rjit::JITModule& m,
                  BasicBlock* b) {
    ConstantInt* const_0 =
        ConstantInt::get(m.getContext(), APInt(32, StringRef("0"), 10));
    Constant* const_null =
        ConstantExpr::getCast(Instruction::IntToPtr, const_0, rjit::t::i8ptr);
    ConstantInt* const_num_bytes = ConstantInt::get(
        m.getContext(), APInt(32, rjit::patchpointSize, false));
    ConstantInt* const_id =
        ConstantInt::get(m.getContext(), APInt(64, id, false));

    std::vector<Value*> sm_args;

    // Args to the stackmap
    sm_args.push_back(const_id);
    sm_args.push_back(const_num_bytes);
    sm_args.push_back(const_null);
    sm_args.push_back(const_0);

    // Values to record
    for (auto arg : values) {
        sm_args.push_back(arg);
    }

    CallInst::Create(m.patchpoint, sm_args, "", b);
}

SEXP createNativeSXP(RFunctionPtr fptr, SEXP ast,
                     std::vector<SEXP> const& objects, Function* f) {
    SEXP objs = allocVector(VECSXP, objects.size() + 1);
    PROTECT(objs);
    SET_VECTOR_ELT(objs, 0, ast);
    for (size_t i = 0; i < objects.size(); ++i)
        SET_VECTOR_ELT(objs, i + 1, objects[i]);
    SEXP result = CONS(reinterpret_cast<SEXP>(fptr), objs);
    UNPROTECT(
        objects.size() +
        1); // all objects in objects + objs itself which is now part of result
    SET_TAG(result, reinterpret_cast<SEXP>(f));
    SET_TYPEOF(result, NATIVESXP);
    return result;
}

} // namespace

namespace rjit {

/** Converts given SEXP to a bitcode constant.
 * The SEXP address is taken as an integer constant into LLVM which is then
 * converted to SEXP.
 * NOTE that this approach assumes that any GC used is non-moving.
 * We are using it because it removes one level of indirection when reading
 * it from the constants vector as R bytecode compiler does.
 */
Value* loadConstant(SEXP value, Module* m, BasicBlock* b) {
    return ConstantExpr::getCast(
        Instruction::IntToPtr,
        ConstantInt::get(getGlobalContext(), APInt(64, (std::uint64_t)value)),
        rjit::t::SEXP);
}

Value* insertCall(Value* fun, std::vector<Value*> args, BasicBlock* b,
                  rjit::JITModule& m, uint64_t function_id) {

    auto res = CallInst::Create(fun, args, "", b);

    if (function_id != (uint64_t)-1) {
        assert(function_id > 1);
        assert(function_id < StackMap::nextStackmapId);

        AttributeSet PAL;
        {
            SmallVector<AttributeSet, 4> Attrs;
            AttributeSet PAS;
            {
                AttrBuilder B;
                B.addAttribute("statepoint-id", std::to_string(function_id));
                PAS = AttributeSet::get(m.getContext(), ~0U, B);
            }
            Attrs.push_back(PAS);
            PAL = AttributeSet::get(m.getContext(), Attrs);
        }
        res->setAttributes(PAL);
    }

    return res;
}

void setupFunction(Function& f, uint64_t functionId) {
    f.setGC("statepoint-example");
    auto attrs = f.getAttributes();
    attrs = attrs.addAttribute(f.getContext(), AttributeSet::FunctionIndex,
                               "no-frame-pointer-elim", "true");
    attrs = attrs.addAttribute(f.getContext(), AttributeSet::FunctionIndex,
                               "statepoint-id", std::to_string(functionId));

    f.setAttributes(attrs);
}

void Compiler::Context::addObject(SEXP object) {
    PROTECT(object);
    objects.push_back(object);
}

Compiler::Context::Context(std::string const& name, llvm::Module* m) {
    f = llvm::Function::Create(t::sexp_sexpsexpint,
                               llvm::Function::ExternalLinkage, name, m);
    functionId = StackMap::nextStackmapId++;
    setupFunction(*f, functionId);
    llvm::Function::arg_iterator args = f->arg_begin();
    llvm::Value* body = args++;
    body->setName("body");
    rho = args++;
    rho->setName("rho");
    llvm::Value* useCache = args++;
    useCache->setName("useCache");
    b = llvm::BasicBlock::Create(llvm::getGlobalContext(), "start", f, nullptr);
    returnJump = false;
}

SEXP Compiler::compileFunction(std::string const& name, SEXP ast,
                               bool isPromise) {
    Context* old = context;
    context = new Context(name, m);
    if (isPromise)
        context->returnJump = true;
    Value* last = compileExpression(ast);
    // since we are going to insert implicit return, which is a simple return
    // even from a promise
    context->returnJump = false;
    if (last != nullptr)
        compileReturn(last, /*tail=*/true);
    // now we create the NATIVESXP
    SEXP result = createNativeSXP(nullptr, ast, context->objects, context->f);
    // add the non-jitted SEXP to relocations
    relocations.push_back(result);
    delete context;
    context = old;
    return result;
}

void Compiler::jitAll() {

    auto handle = JITCompileLayer::getHandle(m.getM());

    // perform all the relocations
    for (SEXP s : relocations) {
        auto f = reinterpret_cast<Function*>(TAG(s));
        auto fp = JITCompileLayer::get(handle, f->getName());
        SETCAR(s, reinterpret_cast<SEXP>(fp));
    }
}

/** Compiles an expression.

  The expression as a result is always visible by default, which can be changed
  in the respective compiling functions.

  An expression is either a constant, or symbol (variable read), or a function
  call.
  */
Value* Compiler::compileExpression(SEXP value) {
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
    case NILSXP:
    case CLOSXP:
        return compileConstant(value);
    case BCODESXP:
    // TODO: reuse the compiled fun
    case NATIVESXP:
        return compileExpression(VECTOR_ELT(CDR(value), 0));
    default:
        assert(false && "Unknown SEXP type in compiled ast.");
    }
}

/** Compiles user constant, which constant marked with userConstant intrinsic.
  */
Value* Compiler::compileConstant(SEXP value) {
    Value* result = constant(value);
    INTRINSIC(m.userConstant, result);
    return result;
}

/** Compiles a symbol, which reads as variable read using genericGetVar
 * intrinsic.
  */
Value* Compiler::compileSymbol(SEXP value) {
    return INTRINSIC(m.genericGetVar, constant(value), context->rho);
}

Value* Compiler::compileICCallStub(Value* call, Value* op,
                                   std::vector<Value*>& callArgs) {
    uint64_t smid = StackMap::nextStackmapId++;

    auto ic_stub = ICCompiler::getStub(callArgs.size(), m);

    std::vector<Value*> ic_args;
    // Closure arguments
    for (auto arg : callArgs) {
        ic_args.push_back(arg);
    }

    // Additional IC arguments
    ic_args.push_back(call);
    ic_args.push_back(op);
    ic_args.push_back(context->rho);
    ic_args.push_back(context->f);
    ic_args.push_back(ConstantInt::get(getGlobalContext(), APInt(64, smid)));

    // Record a patch point
    emitStackmap(smid, {{ic_stub}}, m, context->b);

    return INTRINSIC(ic_stub, ic_args);
}

Value* Compiler::compileCall(SEXP call) {
    Value* f;

    if (TYPEOF(CAR(call)) != SYMSXP) {
        // it is a complex function, first get the value of the function and
        // then check it
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

    std::vector<Value*> args;
    compileArguments(CDR(call), args);

    return compileICCallStub(constant(call), f, args);
}

void Compiler::compileArguments(SEXP argAsts, std::vector<Value*>& res) {
    while (argAsts != R_NilValue) {
        res.push_back(compileArgument(CAR(argAsts), TAG(argAsts)));
        argAsts = CDR(argAsts);
    }
}

Value* Compiler::compileArgument(SEXP arg, SEXP name) {
    switch (TYPEOF(arg)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case NILSXP:
        // literals are self-evaluating
        return constant(arg);
        break;
    case SYMSXP:
        if (arg == R_DotsSymbol) {
            return constant(arg);
        }
    default: {
        SEXP code = compileFunction("promise", arg, /*isPromise=*/true);
        context->addObject(code);
        return constant(code);
    }
    }
}

/** Many function calls may be compiled using intrinsics directly and not the R
  calling mechanism itself.

  This function determines based on the function symbol whether a compilation
  using intrinsics is possible and attempts it. It returns the result value of
  the compilation if successful, or nullptr if the function cannot be compiled
  using intrinsics.

  TODO this now uses even simpler approach than R bytecode compiler, I am simply
  assuming that these will never be overloaded. But we can change this when we
  want to.
  */
Value* Compiler::compileIntrinsic(SEXP call) {
#define CASE(sym) if (CAR(call) == sym)
    CASE(symbol::Block)
    return compileBlock(CDR(call));
    CASE(symbol::Parenthesis)
    return compileParenthesis(CDR(call));
    CASE(symbol::Function)
    return compileFunctionDefinition(CDR(call));
    CASE(symbol::Return) {
        return (CDR(call) == R_NilValue)
                   ? compileReturn(constant(R_NilValue))
                   : compileReturn(compileExpression(CAR(CDR(call))));
    }
    CASE(symbol::Assign)
    return compileAssignment(call);
    CASE(symbol::Assign2)
    return compileAssignment(call);
    CASE(symbol::SuperAssign)
    return compileSuperAssignment(call);
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
    CASE(symbol::Switch)
    return compileSwitch(call);
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

/** Block (a call to {) is compiled as a simple sequence of its statements with
 * its return value being the result of the last statement. If a block is empty,
 * a visible R_NilValue is returned.
  */
Value* Compiler::compileBlock(SEXP block) {
    Value* result = nullptr;
    while (block != R_NilValue) {
        result = compileExpression(CAR(block));
        block = CDR(block);
    }
    if (result == nullptr)
        result = constant(R_NilValue);
    return result;
}

/** Parenthesis expects a single argument only. Ellipsis is allowed, but not
  supported with the intrinsics at the moment so we default to R call.

  Otherwise markVisible intrinsic is applied to the result in accordance to the
  manual.
  */
Value* Compiler::compileParenthesis(SEXP arg) {
    arg = CAR(arg);
    if (arg == symbol::Ellipsis)
        return nullptr; // we can't yet do this
    Value* result = compileExpression(arg);
    context->visibleResult = true;
    return result;
}

/** Similar to R bytecode compiler, only the body of the created function is
  compiled, the default arguments are left in their ast forms for now.

  TODO this should change.
 */
Value* Compiler::compileFunctionDefinition(SEXP fdef) {
    SEXP forms = CAR(fdef);
    SEXP body = compileFunction("function", CAR(CDR(fdef)));
    context->addObject(body);
    return INTRINSIC(m.createClosure, constant(forms), constant(body),
                     context->rho);
}

/** Simple assignments (that is to a symbol) are compiled using the
 * genericSetVar intrinsic.
  */
Value* Compiler::compileAssignment(SEXP e) {
    e = CDR(e);
    // intrinsic only handles simple assignments
    if (TYPEOF(CAR(e)) != SYMSXP)
        return nullptr;
    Value* v = compileExpression(CAR(CDR(e)));
    INTRINSIC(m.genericSetVar, constant(CAR(e)), v, context->rho);
    context->visibleResult = false;
    return v;
}

/** Super assignment is compiled as genericSetVarParentIntrinsic
 */
Value* Compiler::compileSuperAssignment(SEXP e) {
    e = CDR(e);
    // intrinsic only handles simple assignments
    if (TYPEOF(CAR(e)) != SYMSXP)
        return nullptr;
    Value* v = compileExpression(CAR(CDR(e)));
    INTRINSIC(m.genericSetVarParent, constant(CAR(e)), v, context->rho);
    context->visibleResult = false;
    return v;
}

/** Return calls or returns in general are compiled depending on the context.
 * Usually a simple return instruction in bitcode is enough, but while in
 * promises, we must use longjmp, which is done by calling returnJump intrinsic.
  */
Value* Compiler::compileReturn(Value* value, bool tail) {
    if (not context->visibleResult)
        INTRINSIC(m.markInvisible);
    if (context->returnJump) {
        INTRINSIC(m.returnJump, value, context->rho);
        // we need to have a return instruction as well to fool LLVM into
        // believing the basic block has a terminating instruction
        ReturnInst::Create(getGlobalContext(), constant(R_NilValue),
                           context->b);
    } else {
        ReturnInst::Create(getGlobalContext(), value, context->b);
    }
    // this is here to allow compilation of wrong code where statements are even
    // after return
    if (not tail)
        context->b =
            BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
    return nullptr;
}

/** Condition is compiled using the convertToLogicalNoNA intrinsic. True block
 * has to be always present, but false block does not have to be present in
 * which case an invisible R_NilValue should be returned.
  */
Value* Compiler::compileCondition(SEXP e) {
    e = CDR(e);
    SEXP condAst = CAR(e);
    e = CDR(e);
    SEXP trueAst = CAR(e);
    e = CDR(e);
    SEXP falseAst = (e != R_NilValue) ? CAR(e) : nullptr;
    Value* cond2 = compileExpression(condAst);
    Value* cond = INTRINSIC(m.convertToLogicalNoNA, cond2, constant(condAst));
    BasicBlock* ifTrue =
        BasicBlock::Create(getGlobalContext(), "ifTrue", context->f, nullptr);

    BasicBlock* ifFalse =
        BasicBlock::Create(getGlobalContext(), "ifFalse", context->f, nullptr);
    BasicBlock* next =
        BasicBlock::Create(getGlobalContext(), "next", context->f, nullptr);
    ICmpInst* test = new ICmpInst(*(context->b), ICmpInst::ICMP_EQ, cond,
                                  constant(TRUE), "condition");
    BranchInst::Create(ifTrue, ifFalse, test, context->b);

    // true case has to be always present
    context->b = ifTrue;
    Value* trueResult = compileExpression(trueAst);
    JUMP(next);
    ifTrue = context->b;

    // false case may not be present in which case invisible R_NilValue should
    // be returned
    context->b = ifFalse;
    Value* falseResult;
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
    PHINode* phi = PHINode::Create(t::SEXP, 2, "", context->b);
    phi->addIncoming(trueResult, ifTrue);
    phi->addIncoming(falseResult, ifFalse);
    return phi;
}

/** Compiles break. Whenever we see break in the compiler, we know it is for a
  loop where context was skipped and therefore it must always be translated as
  direct jump in bitcode.

  TODO The error is probably not right.
   */
Value* Compiler::compileBreak(SEXP ast) {
    assert(context->breakBlock != nullptr and "Break outside loop");
    JUMP(context->breakBlock);
    // TODO this is really simple, but fine for us - dead code elimination will
    // remove the block if required
    context->b = BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
    return constant(R_NilValue);
}

/** Compiles next. Whenever we see next in the compiler, we know it is for a
  loop where context was skipped and therefore it must always be translated as
  direct jump in bitcode.

  TODO The error is probably not right.
   */
Value* Compiler::compileNext(SEXP ast) {
    assert(context->nextBlock != nullptr and "Next outside loop");
    JUMP(context->nextBlock);
    // TODO this is really simple, but fine for us - dead code elimination will
    // remove the block if required
    context->b = BasicBlock::Create(getGlobalContext(), "deadcode", context->f);
    return constant(R_NilValue);
}

/** Compiles repeat loop. This is simple infinite loop. Only break can exit it.

  Return value of break loop is invisible R_NilValue.
 */
Value* Compiler::compileRepeatLoop(SEXP ast) {
    SEXP bodyAst = CAR(CDR(ast));
    if (not canSkipLoopContext(bodyAst))
        return nullptr;
    // save old loop pointers from the context
    BasicBlock* oldBreak = context->breakBlock;
    BasicBlock* oldNext = context->nextBlock;
    // create the body and next basic blocks
    context->nextBlock = BasicBlock::Create(getGlobalContext(), "repeatBody",
                                            context->f, nullptr);
    context->breakBlock = BasicBlock::Create(getGlobalContext(), "repeatBreak",
                                             context->f, nullptr);
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
Value* Compiler::compileWhileLoop(SEXP ast) {
    SEXP condAst = CAR(CDR(ast));
    SEXP bodyAst = CAR(CDR(CDR(ast)));
    if (not canSkipLoopContext(bodyAst))
        return nullptr;
    // save old loop pointers from the context
    BasicBlock* oldBreak = context->breakBlock;
    BasicBlock* oldNext = context->nextBlock;
    // create the body and next basic blocks
    context->nextBlock = BasicBlock::Create(getGlobalContext(), "whileCond",
                                            context->f, nullptr);
    context->breakBlock = BasicBlock::Create(getGlobalContext(), "whileBreak",
                                             context->f, nullptr);
    JUMP(context->nextBlock);
    context->b = context->nextBlock;
    // compile the condition
    Value* cond2 = compileExpression(condAst);
    Value* cond = INTRINSIC(m.convertToLogicalNoNA, cond2, constant(condAst));
    BasicBlock* whileBody = BasicBlock::Create(getGlobalContext(), "whileBody",
                                               context->f, nullptr);
    ICmpInst* test = new ICmpInst(*(context->b), ICmpInst::ICMP_EQ, cond,
                                  constant(TRUE), "condition");
    BranchInst::Create(whileBody, context->breakBlock, test, context->b);
    // compile the body
    context->b = whileBody;
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

  This uses a jump too many, but it simplifies the SSA considerations and will
  be optimized by LLVM anyhow when we go for LLVM optimizations.
  */
Value* Compiler::compileForLoop(SEXP ast) {
    SEXP controlAst = CAR(CDR(ast));
    assert(TYPEOF(controlAst) == SYMSXP and
           "Only symbols allowed as loop control variables");
    SEXP seqAst = CAR(CDR(CDR(ast)));
    SEXP bodyAst = CAR(CDR(CDR(CDR(ast))));
    if (not canSkipLoopContext(bodyAst))
        return nullptr;
    // save old loop pointers from the context
    BasicBlock* oldBreak = context->breakBlock;
    BasicBlock* oldNext = context->nextBlock;
    // create the body and next basic blocks
    context->nextBlock =
        BasicBlock::Create(getGlobalContext(), "forNext", context->f, nullptr);
    context->breakBlock =
        BasicBlock::Create(getGlobalContext(), "forBreak", context->f, nullptr);
    // This is a simple basic block to which all next's jump and which then
    // jumps to forCond so that there is a simpler phi node at forCond.
    BasicBlock* forCond =
        BasicBlock::Create(getGlobalContext(), "forCond", context->f, nullptr);
    BasicBlock* forBody =
        BasicBlock::Create(getGlobalContext(), "forBody", context->f, nullptr);
    // now initialize the loop control structures
    Value* seq2 = compileExpression(seqAst);
    Value* seq = INTRINSIC(m.startFor, seq2, context->rho);
    Value* seqLength = INTRINSIC(m.loopSequenceLength, seq, constant(ast));
    BasicBlock* forStart = context->b;
    JUMP(forCond);
    context->b = forCond;
    PHINode* control = PHINode::Create(t::Int, 2, "loopControl", context->b);
    control->addIncoming(constant(0), forStart);
    // now check if control is smaller than length
    ICmpInst* test = new ICmpInst(*(context->b), ICmpInst::ICMP_ULT, control,
                                  seqLength, "condition");
    BranchInst::Create(forBody, context->breakBlock, test, context->b);
    // move to the for loop body, where we have to set the control variable
    // properly
    context->b = forBody;
    Value* controlValue = INTRINSIC(m.getForLoopValue, seq, control);
    INTRINSIC(m.genericSetVar, constant(controlAst), controlValue,
              context->rho);
    // now compile the body of the loop
    compileExpression(bodyAst);
    JUMP(context->nextBlock);
    // in the next block, increment the internal control variable and jump to
    // forCond
    context->b = context->nextBlock;
    Value* control1 = BinaryOperator::Create(Instruction::Add, control,
                                             constant(1), "", context->b);
    control->addIncoming(control1, context->nextBlock);
    JUMP(forCond);
    context->b = context->breakBlock;
    // restore the old loop pointers in the context
    context->breakBlock = oldBreak;
    context->nextBlock = oldNext;
    // return R_NilValue
    context->visibleResult = false;
    return constant(R_NilValue);
}

/** Determines whether we can skip creation of the loop context or not. The code
 * is taken from Luke's bytecode compiler.
 */
bool Compiler::canSkipLoopContext(SEXP ast, bool breakOK) {
    if (TYPEOF(ast) == LANGSXP) {
        SEXP cs = CAR(ast);
        if (TYPEOF(cs) == SYMSXP) {
            if (not breakOK and (cs == symbol::Break or cs == symbol::Next)) {
                return false;
            } else if (cs == symbol::Function or cs == symbol::For or
                       cs == symbol::While or cs == symbol::Repeat) {
                return true;
            } else if (cs == symbol::Parenthesis or cs == symbol::Block or
                       cs == symbol::If) {
                return canSkipLoopContextList(CDR(ast), breakOK);
            } else {
                return canSkipLoopContextList(CDR(ast), false);
            }
        }
        // this is change to Luke's code - I believe that whatever will return
        // us the function to call might be compiled using intrinsics and
        // therefore should follow the breakOK rules, not the rules for promises
        // the arguments of user functions do
        return canSkipLoopContext(CAR(ast), breakOK) and
               canSkipLoopContextList(CDR(ast), false);
    } else {
        return true;
    }
}

bool Compiler::canSkipLoopContextList(SEXP ast, bool breakOK) {
    while (ast != R_NilValue) {
        if (not canSkipLoopContext(CAR(ast), breakOK))
            return false;
        ast = CDR(ast);
    }
    return true;
}

/** Compiles the switch statement.

  There are two kinds of switch - integral and character one and they differ in
  what they are doing. The integral switch can be used always, and in its case
  the control variable is simple index to the cases. Contrary to the

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
Value* Compiler::compileSwitch(SEXP call) {
    SEXP x = CDR(call);
    SEXP condAst = CAR(x);
    x = CDR(x);
    std::vector<SEXP> caseAsts;
    std::vector<SEXP> caseNames;
    int defaultIdx = -1;
    int i = 0;
    while (x != R_NilValue) {
        caseAsts.push_back(CAR(x));
        SEXP name = TAG(x);
        if (name == R_NilValue)
            if (defaultIdx == -1)
                defaultIdx = i;
            else
                defaultIdx = -2;
        else
            caseNames.push_back(name);
        x = CDR(x);
        ++i;
    }
    // actual switch compilation - get the control value and check it
    Value* control = compileExpression(condAst);
    INTRINSIC(m.checkSwitchControl, control, constant(call));
    Value* ctype = INTRINSIC(m.sexpType, control);
    ICmpInst* cond = new ICmpInst(*context->b, ICmpInst::ICMP_EQ, ctype,
                                  constant(STRSXP), "");
    BasicBlock* switchIntegral = BasicBlock::Create(
        getGlobalContext(), "switchIntegral", context->f, nullptr);
    BasicBlock* switchCharacter = BasicBlock::Create(
        getGlobalContext(), "switchCharacter", context->f, nullptr);
    BasicBlock* switchNext = BasicBlock::Create(
        getGlobalContext(), "switchNext", context->f, nullptr);
    BranchInst::Create(switchCharacter, switchIntegral, cond, context->b);
    // integral switch is simple
    context->b = switchIntegral;
    Value* caseIntegral =
        INTRINSIC(m.switchControlInteger, control, constant(caseAsts.size()));
    SwitchInst* swInt = SwitchInst::Create(caseIntegral, switchNext,
                                           caseAsts.size(), context->b);
    // for character switch we need to construct the vector,
    context->b = switchCharacter;
    SEXP cases;
    if (defaultIdx != -2) {
        cases = allocVector(STRSXP, caseNames.size());
        for (size_t i = 0; i < caseNames.size(); ++i)
            SET_STRING_ELT(cases, i, PRINTNAME(caseNames[i]));
    } else {
        cases = R_NilValue;
    }
    context->addObject(cases);
    Value* caseCharacter = INTRINSIC(m.switchControlCharacter, control,
                                     constant(call), constant(cases));
    SwitchInst* swChar = SwitchInst::Create(caseCharacter, switchNext,
                                            caseAsts.size(), context->b);
    // create the phi node at the end
    context->b = switchNext;
    PHINode* result = PHINode::Create(t::SEXP, caseAsts.size(), "", context->b);
    // walk the cases and create their blocks, add them to switches and their
    // results to the phi node
    BasicBlock* last;
    for (unsigned i = 0; i < caseAsts.size(); ++i) {
        context->b = last = BasicBlock::Create(getGlobalContext(), "switchCase",
                                               context->f, nullptr);
        swInt->addCase(constant(i), last);
        if (defaultIdx == -1 or defaultIdx > i) {
            swChar->addCase(constant(i), last);
        } else if (defaultIdx < i) {
            swChar->addCase(constant(i - 1), last);
        } else {
            swChar->addCase(constant(caseAsts.size() - 1), last);
            swChar->setDefaultDest(last);
        }
        Value* caseResult = compileExpression(caseAsts[i]);
        JUMP(switchNext);
        result->addIncoming(caseResult, context->b);
    }
    if (swChar->getDefaultDest() == switchNext)
        swChar->setDefaultDest(last);
    swInt->setDefaultDest(last);
    context->b = switchNext;
    return result;
}

/** Compiles operators that can be either binary, or unary, based on the number
 * of call arguments. Takes the binary and unary intrinsics to be used and the
 * full call ast.
  */
Value* Compiler::compileBinaryOrUnary(Function* b, Function* u, SEXP call) {
    Value* lhs = compileExpression(CAR(CDR(call)));
    Value* res;
    if (CDR(CDR(call)) != R_NilValue) {
        Value* rhs = compileExpression(CAR(CDR(CDR(call))));
        res = INTRINSIC(b, lhs, rhs, constant(call), context->rho);
    } else {
        res = INTRINSIC(u, lhs, constant(call), context->rho);
    }
    return res;
}

/** Compiles binary operator using the given intrinsic and full call ast.
  */
Value* Compiler::compileBinary(Function* f, SEXP call) {
    Value* lhs = compileExpression(CAR(CDR(call)));
    Value* rhs = compileExpression(CAR(CDR(CDR(call))));
    return INTRINSIC(f, lhs, rhs, constant(call), context->rho);
}

/** Compiles unary operator using the given intrinsic and full call ast.
  */
Value* Compiler::compileUnary(Function* f, SEXP call) {
    Value* op = compileExpression(CAR(CDR(call)));
    return INTRINSIC(f, op, constant(call), context->rho);
}

Value* Compiler::constant(SEXP value) {
    return loadConstant(value, m.getM(), context->b);
}

Value* Compiler::INTRINSIC(llvm::Value* fun, std::vector<Value*> args) {
    return insertCall(fun, args, context->b, m, context->functionId);
}
}
