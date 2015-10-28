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

#include "ICCompiler.h"

#include "Compiler.h"
#include "JITMemoryManager.h"
#include "JITCompileLayer.h"
#include "StackMap.h"
#include "StackMapParser.h"
#include "CodeCache.h"
#include "ir/Builder.h"
#include "ir/intrinsics.h"
#include "ir/ir.h"

#include "JITCompileLayer.h"
#include "api.h"

#include "RIntlns.h"

#include <sstream>

using namespace llvm;

namespace rjit {

ICCompiler::ICCompiler(unsigned size, ir::Builder& b)
    : ICCompiler(size, b, stubName(size)) {}

ICCompiler::ICCompiler(unsigned size, ir::Builder& b, std::string name)
    : b(b), size(size), name(name) {
#define DECLARE(name, type)                                                    \
    name = llvm::Function::Create(t::type, llvm::Function::ExternalLinkage,    \
                                  #name, b.module())

    DECLARE(CONS_NR, sexp_sexpsexp);
    DECLARE(closureQuickArgumentAdaptor, sexp_sexpsexp);
    DECLARE(initClosureContext, void_cntxtsexpsexpsexpsexpsexp);
    DECLARE(endClosureContext, void_cntxtsexp);
    DECLARE(closureNativeCallTrampoline, sexp_contxtsexpsexp);
    DECLARE(compileIC, compileIC_t);
    DECLARE(patchIC, patchIC_t);
    DECLARE(callNative, sexp_sexpsexp);
#undef DECLARE

    // Set up a function type which corresponds to the ICStub signature
    std::vector<Type*> argT;
    for (unsigned i = 0; i < size + 3; i++) {
        argT.push_back(t::SEXP);
    }
    argT.push_back(t::nativeFunctionPtr_t);
    argT.push_back(t::t_i64);

    auto funT = FunctionType::get(t::SEXP, argT, false);
    ic_t = funT;
}

std::string ICCompiler::stubName(unsigned size) {
    std::ostringstream os;
    os << "icStub_" << size;
    return os.str();
}

Function* ICCompiler::getStub(unsigned size, ir::Builder& b) {

    return CodeCache::get(stubName(size),
                          [size, &b]() {
                              ICCompiler stubCompiler(size, b);
                              return stubCompiler.compileCallStub();
                          },
                          b.module());
}

void* ICCompiler::compile(SEXP inCall, SEXP inFun, SEXP inRho) {
    b.openIC(name, ic_t);

    if (RJIT_DEBUG)
        std::cout << " Compiling IC " << b.f()->getName().str() << " @ "
                  << (void*)b.f() << "\n";

    if (!compileIc(inCall, inFun))
        compileGenericIc(inCall, inFun);

    return finalize();
}

void* ICCompiler::finalize() {
    // FIXME: Allocate a NATIVESXP, or link it to the caller??

    auto engine = JITCompileLayer::singleton.getEngine(b.module());
    auto ic = engine->getPointerToFunction(b.f());

    if (!RJIT_DEBUG)
        delete engine;

    return ic;
}

Function* ICCompiler::compileCallStub() {
    b.openIC(name, ic_t);

    Value* icAddr = INTRINSIC(
        compileIC, ConstantInt::get(getGlobalContext(), APInt(64, size)),
        call(), fun(), rho(), stackmapId());

    INTRINSIC(patchIC, icAddr, stackmapId(), caller());
    // create new intrinics function for patchIC (maybe?)

    Value* ic = new BitCastInst(icAddr, PointerType::get(ic_t, 0), "", b);

    auto res = INTRINSIC_NO_SAFEPOINT(ic, b.args());
    ReturnInst::Create(getGlobalContext(), res, b);

    return b.f();
}

bool ICCompiler::compileIc(SEXP inCall, SEXP inFun) {
    auto f = b.f();

    if (TYPEOF(inFun) == CLOSXP) {
        std::vector<bool> promarg(size, false);

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
            if (CAR(arg) == R_DotsSymbol || TAG(form) == R_DotsSymbol)
                return false;

            // TODO: figure out how to handle those
            if (CAR(arg) == R_MissingArg)
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
        if (form != R_NilValue || i != size)
            return false;

        SEXP inBody = CDR(inFun);
        if (TYPEOF(inBody) == NATIVESXP) {

            BasicBlock* icMatch =
                BasicBlock::Create(getGlobalContext(), "icMatch", f, nullptr);
            BasicBlock* icMiss =
                BasicBlock::Create(getGlobalContext(), "icMiss", f, nullptr);

            // Insert a guard to check if the incomming function matches
            // the one we got this time
            ICmpInst* test =
                new ICmpInst(*b.block(), ICmpInst::ICMP_EQ, fun(),
                             ir::Builder::convertToPointer(inFun), "guard");
            BranchInst::Create(icMatch, icMiss, test, b.block());
            b.setBlock(icMatch);

            // This is an inlined version of applyNativeClosure
            Value* arglist = ir::Builder::convertToPointer(R_NilValue);

            // This reverses the arglist, but quickArgumentAdapter
            // reverses again
            // TODO: construct the environment in one go,
            // without using quickArgumentAdapter
            for (unsigned i = 0; i < size; ++i) {
                Value* arg = b.args()[i];
                if (promarg[i])
                    arg =
                        INTRINSIC(b.intrinsic<ir::CreatePromise>(), arg, rho());
                arglist = INTRINSIC(CONS_NR, arg, arglist);
            }

            Value* newrho =
                INTRINSIC(closureQuickArgumentAdaptor, fun(), arglist);

            Value* cntxt = new AllocaInst(t::cntxt, "", b.block());

            INTRINSIC(initClosureContext, cntxt, call(), newrho, rho(), arglist,
                      fun());

            Value* res =
                INTRINSIC_NO_SAFEPOINT(closureNativeCallTrampoline, cntxt,
                                       b.convertToPointer(inBody), newrho);

            INTRINSIC(endClosureContext, cntxt, res);
            ir::Return::create(b, res);

            b.setBlock(icMiss);
            callIcMiss();

            return true;
        }
    }
    return false;
}

void ICCompiler::callIcMiss() {
    // auto d = llvm::Function::Create(
    //         FunctionType::get(t::t_void,{},false),
    //         llvm::Function::ExternalLinkage, "debugBreak", b.module());
    // INTRINSIC_NO_SAFEPOINT(d,{});
    auto stub = getStub(size, b);
    auto res = INTRINSIC_NO_SAFEPOINT(stub, b.args());
    ir::Return::create(b, res);
}

bool ICCompiler::compileGenericIc(SEXP inCall, SEXP inFun) {
    // TODO: only emit one branch depending on the type we currently see
    BasicBlock* icMatch = b.createBasicBlock("icMatch");
    BasicBlock* icMiss = b.createBasicBlock("icMiss");

    Value* test;
    switch (TYPEOF(inFun)) {
    case SPECIALSXP: {
        // Specials only care about the ast, so we can call any special through
        // this ic
        Value* ftype = ir::SexpType::create(b, fun());
        test = new ICmpInst(*b.block(), ICmpInst::ICMP_EQ, ftype,
                            b.integer(SPECIALSXP), "guard");
        break;
    }
    case BUILTINSXP:
    case CLOSXP: {
        test = new ICmpInst(*b.block(), ICmpInst::ICMP_EQ, fun(),
                            b.convertToPointer(inFun), "guard");
        break;
    }
    default:
        assert(false);
    }

    BranchInst::Create(icMatch, icMiss, test, b.block());
    b.setBlock(icMatch);

    Value* res;
    switch (TYPEOF(inFun)) {
    case SPECIALSXP:
        res = ir::CallSpecial::create(b, b.convertToPointer(inCall), fun(),
                                      b.convertToPointer(R_NilValue), b.rho());
        break;
    case BUILTINSXP: {
        Value* args = compileArguments(CDR(inCall), /*eager=*/true);
        res = ir::CallBuiltin::create(b, b.convertToPointer(inCall), fun(),
                                      args, rho());
        break;
    }
    case CLOSXP: {
        Value* args = compileArguments(CDR(inCall), /*eager=*/false);
        res = ir::CallClosure::create(b, b.convertToPointer(inCall), fun(),
                                      args, rho());
        break;
    }
    default:
        assert(false);
    }
    ir::Return::create(b, res);

    b.setBlock(icMiss);
    callIcMiss();

    return true;
}

/** Compiles arguments for given function.

  Creates the pairlist of arguments used in R from the arguments and their
  names.
  */
Value* ICCompiler::compileArguments(SEXP argAsts, bool eager) {
    Value* arglistHead = nullptr;
    Value* arglist = b.convertToPointer(R_NilValue);

    // if there are no arguments
    int argnum = 0;
    bool seendots = false;
    while (argAsts != R_NilValue) {
        if (CAR(argAsts) == R_DotsSymbol) {
            assert(!seendots);
            seendots = true;

            // first only get the first dots arg to get the top of the list
            arglist = ir::AddEllipsisArgumentHead::create(
                b, arglist, rho(), eager ? b.integer(TRUE) : b.integer(FALSE));
            if (!arglistHead)
                arglistHead = arglist;

            // then add the rest
            arglist = ir::AddEllipsisArgumentTail::create(
                b, arglist, rho(), eager ? b.integer(TRUE) : b.integer(FALSE));
            argnum++;
        } else {
            arglist = compileArgument(arglist, argAsts, argnum++, eager);
            if (!arglistHead)
                arglistHead = arglist;
        }
        argAsts = CDR(argAsts);
    }
    if (arglistHead)
        return arglistHead;
    return b.convertToPointer(R_NilValue);
}

/** Compiles a single argument.

  Self evaluating literals are always returned as SEXP constants, anything else
  is either evaluated directly if eager is true, or they are compiled as new
  promises.
 */
Value* ICCompiler::compileArgument(Value* arglist, SEXP argAst, int argnum,
                                   bool eager) {
    SEXP arg = CAR(argAst);
    SEXP name = TAG(argAst);
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
        result = b.args()[argnum];
        break;
    case SYMSXP:
        assert(arg != R_DotsSymbol);
        if (arg == R_MissingArg) {
            return ir::AddKeywordArgument::create(
                b, arglist, b.convertToPointer(R_MissingArg),
                b.convertToPointer(name));
        }
    // Fall through:
    default:
        if (eager) {
            // TODO make this more efficient?
            result = INTRINSIC(callNative, b.args()[argnum], rho());
        } else {
            // we must create a promise out of the argument
            result = INTRINSIC(b.intrinsic<ir::CreatePromise>(),
                               b.args()[argnum], rho());
        }
        break;
    }
    if (name != R_NilValue)
        return ir::AddKeywordArgument::create(b, arglist, result,
                                              b.convertToPointer(name));

    return ir::AddArgument::create(b, arglist, result);
}

Value* ICCompiler::INTRINSIC_NO_SAFEPOINT(llvm::Value* fun,
                                          std::vector<Value*> args) {
    return llvm::CallInst::Create(fun, args, "", b.block());
}

Value* ICCompiler::INTRINSIC(llvm::Value* fun, std::vector<Value*> args) {
    llvm::CallInst* ins = llvm::CallInst::Create(fun, args, "", b.block());
    return b.insertCall(ins);
}

} // namespace rjit
