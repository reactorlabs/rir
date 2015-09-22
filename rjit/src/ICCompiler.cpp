
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/Analysis/Passes.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCs.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"

#include "Compiler.h"
#include "JITMemoryManager.h"
#include "StackMap.h"
#include "StackMapParser.h"

#include "ICCompiler.h"

#include "RIntlns.h"

using namespace llvm;

namespace rjit {

Value* loadConstant(SEXP value, Module* m, BasicBlock* b);

Value* insertCall(Value* fun, std::vector<Value*> args, BasicBlock* b,
                  rjit::JITModule& m, uint64_t function_id);

void setupFunction(Function& f);

ExecutionEngine* jitModule(Module* m);

void recordStackmaps(std::vector<uint64_t> functionIds);

ICCompiler::ICCompiler(uint64_t stackmapIdC, int size, JITModule& m,
                       unsigned fid)
    : m(m), size(size), functionId(fid) {
    // Set up a function type which corresponds to the ICStub signature
    std::vector<Type*> argT;
    for (int i = 0; i < size + 3; i++) {
        argT.push_back(t::SEXP);
    }
    argT.push_back(t::nativeFunctionPtr_t);

    auto funT = FunctionType::get(t::SEXP, argT, false);
    ic_t = funT;

    f = Function::Create(funT, Function::ExternalLinkage, "callIC", m);
    setupFunction(*f);
    b = BasicBlock::Create(getGlobalContext(), "start", f, nullptr);

    // Load the args in the same order as the stub
    Function::arg_iterator argI = f->arg_begin();
    for (int i = 0; i < size; i++) {
        icArgs.push_back(argI++);
    }

    call = argI++;
    call->setName("call");
    fun = argI++;
    fun->setName("op");
    rho = argI++;
    rho->setName("rho");
    caller = argI++;
    caller->setName("caller");

    stackmapId =
        ConstantInt::get(m.getContext(), APInt(64, stackmapIdC, false));
}

Function* ICCompiler::compileStub() {
    Value* res = compileCallStub();

    ReturnInst::Create(getGlobalContext(), res, b);

    return f;
}

void* ICCompiler::compile(SEXP inCall, SEXP inFun, SEXP inRho) {

    if (!compileIc(inCall, inFun))
        compileGenericIc(inCall, inFun);

    return finalize();
}

void* ICCompiler::finalize() {
    // FIXME: Allocate a NATIVESXP, or link it to the caller??

    // m.dump();
    ExecutionEngine* engine = jitModule(m.getM());
    void* ic = engine->getPointerToFunction(f);

    recordStackmaps({functionId});
    new_stackmap_addr = nullptr;

    return ic;
}

Value* ICCompiler::compileCallStub() {
    Value* icAddr = INTRINSIC(
        m.compileIC, ConstantInt::get(getGlobalContext(), APInt(64, size)),
        call, fun, rho, stackmapId);

    INTRINSIC(m.patchIC, icAddr, stackmapId, caller);

    Value* ic = new BitCastInst(icAddr, PointerType::get(ic_t, 0), "", b);

    std::vector<Value*> allArgs;
    allArgs.insert(allArgs.end(), icArgs.begin(), icArgs.end());
    allArgs.push_back(call);
    allArgs.push_back(fun);
    allArgs.push_back(rho);
    allArgs.push_back(caller);

    return INTRINSIC_NO_SAFEPOINT(ic, allArgs);
}

bool ICCompiler::compileIc(SEXP inCall, SEXP inFun) {
    if (TYPEOF(inFun) == CLOSXP) {
        std::vector<bool> promarg(icArgs.size(), false);

        // Check for named args or ...
        SEXP arg = CDR(inCall);
        SEXP form = FORMALS(inFun);
        unsigned i = 0;
        while (arg != R_NilValue && form != R_NilValue) {
            // We do not yet do the static version of match.c, thus cannot
            // support named args
            if (TAG(arg) != R_NilValue)
                return false;

            // We cannot inline ellipsis
            if (CAR(arg) == R_DotsSymbol)
                return false;

            switch (TYPEOF(CAR(arg))) {
            case LGLSXP:
            case INTSXP:
            case REALSXP:
            case CPLXSXP:
            case STRSXP:
                break;
            default:
                promarg[i] = true;
            }
            i++;
            arg = CDR(arg);
            form = CDR(form);
        }

        // number of args != number of formal args, fallback to generic
        if (form != R_NilValue || i != icArgs.size())
            return false;

        SEXP body = CDR(inFun);
        // TODO: If the body is not native we could jit it here
        if (TYPEOF(body) == NATIVESXP) {

            BasicBlock* icMatch =
                BasicBlock::Create(getGlobalContext(), "icMatch", f, nullptr);
            BasicBlock* icMiss =
                BasicBlock::Create(getGlobalContext(), "icMiss", f, nullptr);
            BasicBlock* end =
                BasicBlock::Create(getGlobalContext(), "end", f, nullptr);

            // Insert a guard to check if the incomming function matches
            // the one we got this time
            ICmpInst* test = new ICmpInst(*b, ICmpInst::ICMP_EQ, fun,
                                          constant(inFun), "guard");
            BranchInst::Create(icMatch, icMiss, test, b);

            b = icMatch;

            // This is an inlined version of applyNativeClosure
            Value* arglist = constant(R_NilValue);

            // This reverses the arglist, but quickArgumentAdapter
            // reverses again
            // TODO: construct the environment in one go,
            // without using quickArgumentAdapter
            for (unsigned i = 0; i < icArgs.size(); ++i) {
                Value* arg = icArgs[i];
                if (promarg[i])
                    arg = INTRINSIC(m.createPromise, arg, rho);
                arglist = INTRINSIC(m.CONS_NR, arg, arglist);
            }

            Value* newrho =
                INTRINSIC(m.closureQuickArgumentAdaptor, fun, arglist);

            Value* cntxt = new AllocaInst(t::cntxt, "", b);

            INTRINSIC(m.initClosureContext, cntxt, call, newrho, rho, arglist,
                      fun);

            Value* res = INTRINSIC_NO_SAFEPOINT(m.closureNativeCallTrampoline,
                                                cntxt, constant(body), newrho);

            INTRINSIC(m.endClosureContext, cntxt, res);

            BranchInst::Create(end, b);
            b = icMiss;

            Value* missRes = compileCallStub();

            BranchInst::Create(end, b);
            b = end;

            PHINode* phi = PHINode::Create(t::SEXP, 2, "", b);
            phi->addIncoming(res, icMatch);
            phi->addIncoming(missRes, icMiss);
            ReturnInst::Create(getGlobalContext(), phi, b);

            return true;
        }
    }
    return false;
}

bool ICCompiler::compileGenericIc(SEXP inCall, SEXP inFun) {
    Value* call = compileCall(inCall, inFun);
    ReturnInst::Create(getGlobalContext(), call, b);

    return true;
}

Value* ICCompiler::compileCall(SEXP call, SEXP op) {
    // TODO: only emit one branch depending on the type we currently see

    BasicBlock* icTestType =
        BasicBlock::Create(getGlobalContext(), "icTypeTest", f, nullptr);
    BasicBlock* icMatch =
        BasicBlock::Create(getGlobalContext(), "icMatch", f, nullptr);
    BasicBlock* icMiss =
        BasicBlock::Create(getGlobalContext(), "icMiss", f, nullptr);
    BasicBlock* end = BasicBlock::Create(getGlobalContext(), "end", f, nullptr);

    ICmpInst* test =
        new ICmpInst(*b, ICmpInst::ICMP_EQ, fun, constant(op), "guard");
    BranchInst::Create(icMatch, icTestType, test, b);

    b = icTestType;

    Value* ftype = INTRINSIC(m.sexpType, fun);
    switch (TYPEOF(op)) {
    case SPECIALSXP:
        test = new ICmpInst(*b, ICmpInst::ICMP_EQ, ftype, constant(SPECIALSXP),
                            "guard");
        break;
    case BUILTINSXP:
        test = new ICmpInst(*b, ICmpInst::ICMP_EQ, ftype, constant(BUILTINSXP),
                            "guard");
        break;
    case CLOSXP:
        test = new ICmpInst(*b, ICmpInst::ICMP_EQ, ftype, constant(CLOSXP),
                            "guard");
        break;
    default:
        assert(false);
    }

    BranchInst::Create(icMatch, icMiss, test, b);

    b = icMatch;

    Value* res;
    switch (TYPEOF(op)) {
    case SPECIALSXP:
        res = INTRINSIC(m.callSpecial, constant(call), fun,
                        constant(R_NilValue), rho);
        break;
    case BUILTINSXP: {
        Value* args = compileArguments(CDR(call), /*eager=*/true);
        res = INTRINSIC(m.callBuiltin, constant(call), fun, args, rho);
        break;
    }
    case CLOSXP: {
        Value* args = compileArguments(CDR(call), /*eager=*/false);
        res = INTRINSIC(m.callClosure, constant(call), fun, args, rho);
        break;
    }
    default:
        assert(false);
    }
    BranchInst::Create(end, b);

    b = icMiss;

    Value* missRes = compileCallStub();

    BranchInst::Create(end, b);
    b = end;

    PHINode* phi = PHINode::Create(t::SEXP, 2, "", b);
    phi->addIncoming(res, icMatch);
    phi->addIncoming(missRes, icMiss);

    return phi;
}

/** Compiles arguments for given function.

  Creates the pairlist of arguments used in R from the arguments and their
  names.
  */
Value* ICCompiler::compileArguments(SEXP argAsts, bool eager) {
    Value* arglistHead = nullptr;
    Value* arglist = constant(R_NilValue);

    // if there are no arguments
    int argnum = 0;
    while (argAsts != R_NilValue) {
        arglist = compileArgument(arglist, argAsts, argnum++, eager);
        if (!arglistHead)
            arglistHead = arglist;
        argAsts = CDR(argAsts);
    }
    if (arglistHead)
        return arglistHead;
    return constant(R_NilValue);
}

/** Compiles a single argument.

  Self evaluating literals are always returned as SEXP constants, anything else
  is either evaluated directly if eager is true, or they are compiled as new
  promises.
 */
Value* ICCompiler::compileArgument(Value* arglist, SEXP argAst, int argnum,
                                   bool eager) {
    SEXP arg = CAR(argAst);
    Value* result;
    // This list has to stay in sync with Compiler::compileArgument
    // note: typeof(arg) does not correspond to the runtime type of the ic
    // arg, since the caller already converts non self evaluating arguments
    // to promises or native code.
    switch (TYPEOF(arg)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case NILSXP:
        // literals are self-evaluating
        result = icArgs[argnum];
        break;
    case SYMSXP:
        if (arg == R_DotsSymbol) {
            return INTRINSIC(m.addEllipsisArgument, arglist, rho,
                             eager ? constant(TRUE) : constant(FALSE));
        }
    // Fall through:
    default:
        if (eager) {
            // TODO make this more efficient?
            result = INTRINSIC(m.callNative, icArgs[argnum], rho);
        } else {
            // we must create a promise out of the argument
            result = INTRINSIC(m.createPromise, icArgs[argnum], rho);
        }
        break;
    }
    SEXP name = TAG(argAst);
    if (name != R_NilValue)
        return INTRINSIC(m.addKeywordArgument, arglist, result, constant(name));

    return INTRINSIC(m.addArgument, arglist, result);
}

Value* ICCompiler::constant(SEXP value) {
    return loadConstant(value, m.getM(), b);
}

Value* ICCompiler::INTRINSIC_NO_SAFEPOINT(llvm::Value* fun,
                                          std::vector<Value*> args) {
    return insertCall(fun, args, b, m, -1);
}

Value* ICCompiler::INTRINSIC(llvm::Value* fun, std::vector<Value*> args) {
    return insertCall(fun, args, b, m, functionId);
}

} // namespace rjit
