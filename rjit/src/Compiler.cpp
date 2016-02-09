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
#include "ir/Builder.h"
#include "ir/primitive_calls.h"
#include "ir/Ir.h"

#include "api.h"

#include "RIntlns.h"

using namespace llvm;

namespace rjit {

SEXP Compiler::compilePromise(std::string const& name, SEXP ast) {
    b.openPromise(name, ast);
    return finalizeCompile(ast);
}

SEXP Compiler::compileFunction(std::string const& name, SEXP ast,
                               SEXP formals) {
    b.openFunction(name, ast, formals);
    return finalizeCompile(ast);
}

SEXP Compiler::finalizeCompile(SEXP ast) {
    Value* last = compileExpression(ast);

    // since we are going to insert implicit return, which is a simple return
    // even from a promise
    b.setResultJump(false);
    if (last != nullptr)
        compileReturn(last, /*tail=*/true);
    // now we create the NATIVESXP
    // NATIVESXP should be a static builder, but this is not how it works
    // at the moment
    SEXP result = b.closeFunction();
    return result;
}

void Compiler::jitAll() {
    auto engine = JITCompileLayer::singleton.getEngine(b);

    if (!RJIT_DEBUG) {
        // Keep the llvm ir around
        engine->removeModule(b.module());
        delete engine;
    }
}

/** Compiles an expression.

  The expression as a result is always visible by default, which can be changed
  in the respective compiling functions.

  An expression is either a constant, or symbol (variable read), or a function
  call.
  */
Value* Compiler::compileExpression(SEXP value) {
    b.setResultVisible(true);
    switch (TYPEOF(value)) {
    case SYMSXP:
        return compileSymbol(value);
    case LANGSXP:
        return compileCall(value);
    case LGLSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case NILSXP:
    case CLOSXP:
    case INTSXP: {
        return ir::UserLiteral::create(b, value)->result();
    }
    case BCODESXP:
    // TODO: reuse the compiled fun
    case NATIVESXP:
        return compileExpression(VECTOR_ELT(CDR(value), 0));
    default:
        assert(false && "Unknown SEXP type in compiled ast.");
    }
    return nullptr;
}

/** Compiles a symbol, which reads as variable read using genericGetVar
 * intrinsic.
  */
Value* Compiler::compileSymbol(SEXP value) {
    assert(TYPEOF(value) == SYMSXP);
    auto name = CHAR(PRINTNAME(value));
    assert(strlen(name));
    Value* res = ir::GenericGetVar::create(b, b.rho(), value)->result();
    res->setName(name);
    return res;
}

/** Inline caching for a function (call) with operator (op)
 *  that have arguments (callArgs).
 */

Value* Compiler::compileICCallStub(Value* call, Value* op,
                                   std::vector<Value*>& callArgs) {
    uint64_t smid = JITCompileLayer::singleton.getSafepointId(b.f());

    auto size = callArgs.size();

    Function* ic_stub;
    {
        ir::Builder b_(b.module());
        ic_stub = ICCompiler::getStub(callArgs.size(), b_);
    }
    JITCompileLayer::singleton.setPatchpoint(smid, size);

    std::vector<Value*> ic_args;
    // Closure arguments
    for (auto arg : callArgs) {
        ic_args.push_back(arg);
    }

    // Additional IC arguments
    ic_args.push_back(call);
    ic_args.push_back(op);
    ic_args.push_back(b.rho());
    ic_args.push_back(b.f());
    ic_args.push_back(ConstantInt::get(getGlobalContext(), APInt(64, smid)));

    auto res = CallInst::Create(ic_stub, ic_args, "", b);
    AttributeSet PAL;
    {
        SmallVector<AttributeSet, 4> Attrs;
        AttributeSet PAS;
        {
            AttrBuilder B;
            B.addAttribute("statepoint-id", std::to_string(smid));
            B.addAttribute("statepoint-num-patch-bytes",
                           std::to_string(patchpointSize));
            PAS = AttributeSet::get(b.getContext(), ~0U, B);
        }
        Attrs.push_back(PAS);
        PAL = AttributeSet::get(b.getContext(), Attrs);
    }
    res->setAttributes(PAL);

    return res;
}

Value* Compiler::compileCall(SEXP call) {
    Value* f;

    if (TYPEOF(CAR(call)) != SYMSXP) {
        // it is a complex function, first get the value of the function and
        // then check it
        f = compileExpression(CAR(call));
        ir::CheckFunction::create(b, f);
    } else {
        // it is simple function - try compiling it with intrinsics
        f = compileIntrinsic(call);
        if (f != nullptr)
            return f;
        // otherwise just do get function
        f = ir::GetFunction::create(b, b.rho(), CAR(call))->result();
        f->setName(CHAR(PRINTNAME(CAR(call))));
    }

    std::vector<Value*> args;
    compileArguments(CDR(call), args);

    return compileICCallStub(ir::Constant::create(b, call)->result(), f, args);
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
        return ir::UserLiteral::create(b, arg)->result();
    case SYMSXP:
        if (arg == R_DotsSymbol) {
            return ir::Constant::create(b, arg)->result();
        }
        if (arg == R_MissingArg) {
            return ir::Constant::create(b, arg)->result();
        }
    default: {
        SEXP code = compilePromise("promise", arg);
        // Should the objects be inside the builder?
        // not needed with new API, compile constant adds automaatically if not
        // present yet
        // b.addConstantPoolObject(code);
        return ir::UserLiteral::create(b, code)->result();
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
                   ? compileReturn(
                         ir::Constant::create(b, R_NilValue)->result())
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
    return compileBinaryOrUnary<ir::GenericAdd, ir::GenericUnaryPlus>(call);
    CASE(symbol::Sub)
    return compileBinaryOrUnary<ir::GenericSub, ir::GenericUnaryMinus>(call);
    CASE(symbol::Mul)
    return compileBinary<ir::GenericMul>(call);
    CASE(symbol::Div)
    return compileBinary<ir::GenericDiv>(call);
    CASE(symbol::Pow)
    return compileBinary<ir::GenericPow>(call);
    CASE(symbol::Sqrt)
    return compileUnary<ir::GenericSqrt>(call);
    CASE(symbol::Exp)
    return compileUnary<ir::GenericExp>(call);
    CASE(symbol::Eq)
    return compileBinary<ir::GenericEq>(call);
    CASE(symbol::Ne)
    return compileBinary<ir::GenericNe>(call);
    CASE(symbol::Lt)
    return compileBinary<ir::GenericLt>(call);
    CASE(symbol::Le)
    return compileBinary<ir::GenericLe>(call);
    CASE(symbol::Ge)
    return compileBinary<ir::GenericGe>(call);
    CASE(symbol::Gt)
    return compileBinary<ir::GenericGt>(call);
    CASE(symbol::BitAnd)
    return compileBinary<ir::GenericBitAnd>(call);
    CASE(symbol::BitOr)
    return compileBinary<ir::GenericBitOr>(call);
    CASE(symbol::Not)
    return compileUnary<ir::GenericNot>(call);

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
        result = ir::Constant::create(b, R_NilValue)->result();
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
    b.setResultVisible(true);
    return result;
}

/** Similar to R bytecode compiler, only the body of the created function is
  compiled, the default arguments are left in their ast forms for now.

  TODO this should change.
 */
Value* Compiler::compileFunctionDefinition(SEXP fdef) {
    SEXP forms = CAR(fdef);
    SEXP body = compileFunction("function", CAR(CDR(fdef)), forms);
    return ir::CreateClosure::create(b, b.rho(), forms, body)->result();
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
    ir::GenericSetVar::create(b, v, b.rho(), CAR(e));
    b.setResultVisible(false);
    return v;
}

/**
Value* Compiler::compileAssignment(SEXP e) {
    e = CDR(e);
    // intrinsic only handles simple assignments
    if (TYPEOF(CAR(e)) != SYMSXP)
        return nullptr;
    Value* v = compileExpression(CAR(CDR(e)));
    genericSetVar::create(b, constant(CAR(e)), v, rho);
    isResultVisible = false;
    return v;
}
*/

/** Super assignment is compiled as genericSetVarParentIntrinsic
 */
Value* Compiler::compileSuperAssignment(SEXP e) {
    e = CDR(e);
    // intrinsic only handles simple assignments
    if (TYPEOF(CAR(e)) != SYMSXP)
        return nullptr;
    Value* v = compileExpression(CAR(CDR(e)));
    ir::GenericSetVarParent::create(b, v, b.rho(), CAR(e));
    b.setResultVisible(false);
    return v;
}

/** Return calls or returns in general are compiled depending on the context.
 * Usually a simple return instruction in bitcode is enough, but while in
 * promises, we must use longjmp, which is done by calling returnJump intrinsic.
  */
Value* Compiler::compileReturn(Value* value, bool tail) {
    if (not b.getResultVisible())
        ir::MarkInvisible::create(b);
    if (b.getResultJump()) {
        ir::ReturnJump::create(b, value, b.rho());
        // we need to have a return instruction as well to fool LLVM into
        // believing the basic block has a terminating instruction
        ir::Return::create(b, ir::Constant::create(b, R_NilValue)->result());
    } else {
        ir::Return::create(b, value);
    }
    // this is here to allow compilation of wrong code where statements are even
    // after return
    if (not tail)
        b.setBlock(b.createBasicBlock("deadcode"));
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
    Value* cond = ir::ConvertToLogicalNoNA::create(b, cond2, condAst)->result();
    BasicBlock* ifTrue = b.createBasicBlock("ifTrue");
    BasicBlock* ifFalse = b.createBasicBlock("ifFalse");
    BasicBlock* next = b.createBasicBlock("next");
    ir::Cbr::create(b, cond, ifTrue, ifFalse);

    // true case has to be always present
    b.setBlock(ifTrue);
    Value* trueResult = compileExpression(trueAst);
    ir::Branch::create(b, next);
    ifTrue = b.block();

    // false case may not be present in which case invisible R_NilValue should
    // be returned
    b.setBlock(ifFalse);
    Value* falseResult;
    if (falseAst == nullptr) {
        falseResult = ir::Constant::create(b, R_NilValue)->result();
        b.setResultVisible(false);
    } else {
        falseResult = compileExpression(falseAst);
    }
    ifFalse = b.block();
    ir::Branch::create(b, next);

    // add a phi node for the result
    b.setBlock(next);
    PHINode* phi = PHINode::Create(t::SEXP, 2, "", b);
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
    llvm::BasicBlock* bb = b.breakTarget();
    ir::Branch::create(b, bb);
    // TODO this is really simple, but fine for us - dead code elimination will
    // remove the block if required
    b.setBlock(b.createBasicBlock("deadcode"));
    return ir::Constant::create(b, R_NilValue)->result();
}

/** Compiles next. Whenever we see next in the compiler, we know it is for a
  loop where context was skipped and therefore it must always be translated as
  direct jump in bitcode.

  TODO The error is probably not right.
   */
Value* Compiler::compileNext(SEXP ast) {
    llvm::BasicBlock* bb = b.nextTarget();
    ir::Branch::create(b, bb);
    // TODO this is really simple, but fine for us - dead code elimination will
    // remove the block if required
    b.setBlock(b.createBasicBlock("deadcode"));
    return ir::Constant::create(b, R_NilValue)->result();
}

/** Compiles repeat loop. This is simple infinite loop. Only break can exit it.

  Return value of break loop is invisible R_NilValue.
 */
Value* Compiler::compileRepeatLoop(SEXP ast) {
    SEXP bodyAst = CAR(CDR(ast));
    if (not canSkipLoopContext(bodyAst))
        return nullptr;
    // save old loop pointers from the context
    // create the body and next basic blocks
    b.openLoop();

    ir::Branch::create(b, b.nextTarget());
    b.setBlock(b.nextTarget());

    compileExpression(bodyAst);
    ir::Branch::create(b, b.nextTarget());
    b.setBlock(b.breakTarget());
    // restore the old loop pointers in the context
    b.closeLoop();
    // return R_NilValue
    b.setResultVisible(false);
    return ir::Constant::create(b, R_NilValue)->result();
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
    // create the body and next basic blocks
    b.openLoop();

    ir::Branch::create(b, b.nextTarget());
    b.setBlock(b.nextTarget());
    // compile the condition
    Value* cond2 = compileExpression(condAst);
    Value* cond = ir::ConvertToLogicalNoNA::create(b, cond2, condAst)->result();
    BasicBlock* whileBody = b.createBasicBlock("whileBody");
    ir::Cbr::create(b, cond, whileBody, b.breakTarget());
    // compile the body
    b.setBlock(whileBody);
    compileExpression(bodyAst);
    ir::Branch::create(b, b.nextTarget());
    b.setBlock(b.breakTarget());
    // restore the old loop pointers in the context
    b.closeLoop();
    // return R_NilValue
    b.setResultVisible(false);
    return ir::Constant::create(b, R_NilValue)->result();
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
    b.openLoop();
    // create the body and next basic blocks
    // This is a simple basic block to which all next's jump and which then
    // jumps to forCond so that there is a simpler phi node at forCond.
    BasicBlock* forCond = b.createBasicBlock("forCond");
    BasicBlock* forBody = b.createBasicBlock("forBody");
    // now initialize the loop control structures
    Value* seq2 = compileExpression(seqAst);
    Value* seq = ir::StartFor::create(b, seq2, b.rho())->result();
    Value* seqLength = ir::LoopSequenceLength::create(b, seq, ast)->result();
    BasicBlock* forStart = b.block();
    ir::Branch::create(b, forCond);
    b.setBlock(forCond);
    PHINode* control = PHINode::Create(t::Int, 2, "loopControl", b);
    control->addIncoming(b.integer(0), forStart);
    // now check if control is smaller than length
    auto test = ir::UnsignedIntegerLessThan::create(b, control, seqLength);
    BranchInst::Create(forBody, b.breakTarget(), test->result(), b);

    // move to the for loop body, where we have to set the control variable
    // properly
    b.setBlock(forBody);
    Value* controlValue =
        ir::GetForLoopValue::create(b, seq, control)->result();
    ir::GenericSetVar::create(b, controlValue, b.rho(), controlAst);
    // now compile the body of the loop
    compileExpression(bodyAst);
    ir::Branch::create(b, b.nextTarget());
    // in the next block, increment the internal control variable and jump to
    // forCond
    b.setBlock(b.nextTarget());

    // TODO: Need an intrinsic function for BinaryOperator
    Value* control1 =
        ir::IntegerAdd::create(b, control, b.integer(1))->result();
    control->addIncoming(control1, b.nextTarget());

    ir::Branch::create(b, forCond);
    b.setBlock(b.breakTarget());
    // restore the old loop pointers in the context
    b.closeLoop();
    // return R_NilValue
    b.setResultVisible(false);
    return ir::Constant::create(b, R_NilValue)->result();
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

    if (condAst == R_NilValue) {
        return nullptr;
    }

    // actual switch compilation - get the control value and check it
    Value* control = compileExpression(condAst);

    if (caseAsts.size() == 0) {
        return compileExpression(R_NilValue);
    }

    ir::CheckSwitchControl::create(b, control, call);
    Value* ctype = ir::SexpType::create(b, control)->result();
    auto cond = ir::IntegerEquals::create(b, ctype, b.integer(STRSXP));
    BasicBlock* switchIntegral = b.createBasicBlock("switchIntegral");
    BasicBlock* switchCharacter = b.createBasicBlock("switchCharacter");
    BasicBlock* switchNext = b.createBasicBlock("switchNext");

    BranchInst::Create(switchCharacter, switchIntegral, cond->result(), b);

    // integral switch is simple
    b.setBlock(switchIntegral);

    Value* caseIntegral =
        ir::SwitchControlInteger::create(b, control, caseAsts.size())->result();
    auto swInt =
        ir::Switch::create(b, caseIntegral, switchNext, caseAsts.size());
    // for character switch we need to construct the vector,
    b.setBlock(switchCharacter);
    SEXP cases;
    if (defaultIdx != -2) {
        cases = allocVector(STRSXP, caseNames.size());
        for (size_t i = 0; i < caseNames.size(); ++i)
            SET_STRING_ELT(cases, i, PRINTNAME(caseNames[i]));
    } else {
        cases = R_NilValue;
    }
    //
    b.addConstantPoolObject(cases);
    Value* caseCharacter =
        ir::SwitchControlCharacter::create(b, control, call, cases)->result();

    auto swChar =
        ir::Switch::create(b, caseCharacter, switchNext, caseAsts.size());
    // create the phi node at the end
    b.setBlock(switchNext);
    PHINode* result = PHINode::Create(t::SEXP, caseAsts.size(), "", b);
    // walk the cases and create their blocks, add them to switches and their
    // results to the phi node
    // TODO: fix empty switch
    BasicBlock* last = nullptr;
    BasicBlock* fallThrough = nullptr;
    for (unsigned i = 0; i < caseAsts.size(); ++i) {
        last = b.createBasicBlock("switchCase");
        if (fallThrough != nullptr) {
            ir::Branch::create(b, last);
            fallThrough = nullptr;
        }
        b.setBlock(last);
        swInt->addCase(i, last);
        if (defaultIdx == -1 or defaultIdx > static_cast<int>(i)) {
            swChar->addCase(i, last);
        } else if (defaultIdx < static_cast<int>(i)) {
            swChar->addCase(i - 1, last);
        } else {
            swChar->addCase(caseAsts.size() - 1, last);
            swChar->setDefaultDest(last);
        }
        SEXP value = caseAsts[i];
        if (TYPEOF(value) == SYMSXP && !strlen(CHAR(PRINTNAME(value)))) {
            fallThrough = b.block();
        } else {
            Value* caseResult = compileExpression(caseAsts[i]);
            ir::Branch::create(b, switchNext);
            result->addIncoming(caseResult, b.block());
        }
    }
    if (swChar->getDefaultDest() == switchNext)
        swChar->setDefaultDest(last);
    swInt->setDefaultDest(last);
    if (fallThrough != nullptr) {
        result->addIncoming(ir::Constant::create(b, R_NilValue)->result(),
                            b.block());
        ir::Branch::create(b, switchNext);
    }
    b.setBlock(switchNext);
    return result;
}

/** Compiles operators that can be either binary, or unary, based on the number
 * of call arguments. Takes the binary and unary intrinsics to be used and the
 * full call ast.
  */

/** Compiles binary operator using the given intrinsic and full call ast.

Value* Compiler::compileBinary(Function* f, SEXP call) {
    Value* lhs = compileExpression(CAR(CDR(call)));
    Value* rhs = compileExpression(CAR(CDR(CDR(call))));
    return INTRINSIC(f, lhs, rhs, constant(call), context->rho);
}


// Compiles unary operator using the given intrinsic and full call ast.

Value* Compiler::compileUnary(Function* f, SEXP call) {
    Value* op = compileExpression(CAR(CDR(call)));
    return INTRINSIC(f, op, constant(call), context->rho);
}

Value* Compiler::constant(SEXP value) {
    return loadConstant(value, m.getM(), b);
}

Value* Compiler::INTRINSIC(llvm::Value* fun, std::vector<Value*> args) {
    return insertCall(fun, args, context->f, context->b, m, true);
}
*/
}
