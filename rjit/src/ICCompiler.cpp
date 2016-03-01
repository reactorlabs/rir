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
#include "ir/primitive_calls.h"
#include "ir/Ir.h"

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

    return CodeCache::get(stubName(size), [size, &b]() {
        ICCompiler stubCompiler(size, b);
        return stubCompiler.compileCallStub();
    }, b.module());
}

void* ICCompiler::getSpecialIC(unsigned size) {
    return (void*)CodeCache::getAddress(specialName(size), [size]() {
        ir::Builder b("ic");
        ICCompiler compiler(size, b, specialName(size));
        compiler.compileSpecialIC();
        return (uint64_t)compiler.finalize();
    });
}

void* ICCompiler::compile(SEXP inCall, SEXP inFun, SEXP inRho) {
    assert(TYPEOF(inFun) != SPECIALSXP);

    b.openIC(name, ic_t);

    if (RJIT_DEBUG)
        std::cout << " Compiling IC " << b.f()->getName().str() << " @ "
                  << (void*)b.f() << "\n";

    if (!compileIc(inCall, inFun))
        compileGenericIc(inCall, inFun);

    return finalize();
}

void* ICCompiler::finalize() {
    // FIXME: return nativesxp and not naked ptr?
    auto f = b.closeIC();

    auto engine = JITCompileLayer::singleton.finalize(b);
    auto ic = engine->getPointerToFunction(f);

    if (!RJIT_DEBUG)
        delete engine;

    return ic;
}

Function* ICCompiler::compileCallStub() {
    b.openIC(name, ic_t);

    Value* icAddr =
        ir::CompileIC::create(
            b, ConstantInt::get(getGlobalContext(), APInt(64, size)), call(),
            fun(), rho(), stackmapId())
            ->result();

    ir::PatchIC::create(b, icAddr, stackmapId(), caller());

    // TODO adding llvm instruction directly w/o builder is not such a good idea
    Value* ic = new BitCastInst(icAddr, PointerType::get(ic_t, 0), "", b);

    auto res = ir::CallToAddress::create(b, ic, b.args())->result();
    // TODO adding llvm instruction directly w/o builder is not such a good idea
    ReturnInst::Create(getGlobalContext(), res, b);

    auto stub = b.f();
    b.closeIC();

    return stub;
}

bool ICCompiler::compileIc(SEXP inCall, SEXP inFun) {
    // auto f = b.f();

    if (TYPEOF(inFun) == CLOSXP) {
        std::vector<bool> promarg(size, false);
        std::vector<long> positionalArg;
        std::unordered_map<long, SEXP> namedArg;
        std::unordered_map<SEXP, long> formals;

        // Check for named args or ...
        SEXP arg = CDR(inCall);
        SEXP form = FORMALS(inFun);

        unsigned i = 0;
        while (arg != R_NilValue && form != R_NilValue) {
            if (TAG(arg) != R_NilValue) {
                namedArg[i] = TAG(arg);
            } else {
                positionalArg.push_back(i);
            }
            formals[TAG(form)] = i;

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

        // Static version of gnur match.c : mapping given arguments to formal
        // arguments by exact match.
        std::vector<long> argOrder(size, -1);
        for (auto p : namedArg) {
            long argnum = std::get<0>(p);
            SEXP name = std::get<1>(p);
            if (!formals.count(name)) {
                // Named argument does not match formal, drop to generic case
                return false;
            }
            long pos = formals[name];
            argOrder[pos] = argnum;
        }
        unsigned position = 0;
        for (long argnum : positionalArg) {
            while (position < size && argOrder[position] != -1)
                ++position;
            assert(position < size);
            argOrder[position] = argnum;
        }

        SEXP inBody = CDR(inFun);
        if (TYPEOF(inBody) == NATIVESXP) {

            BasicBlock* icMatch = b.createBasicBlock("icMatch");
            BasicBlock* icMiss = b.createBasicBlock("icMiss");

            // Insert a guard to check if the incomming function matches
            // the one we got this time
            Value* nativeFun = ir::Cdr::create(b, fun())->result();
            ICmpInst* test = new ICmpInst(
                *b.block(), ICmpInst::ICMP_EQ, nativeFun,
                ir::Builder::convertToPointer(BODY(inFun)), "guard");
            BranchInst::Create(icMatch, icMiss, test, b);
            b.setBlock(icMatch);

            // This is an inlined version of applyNativeClosure
            Value* actuals = ir::Builder::convertToPointer(R_NilValue);

            for (unsigned i = size; i > 0; --i) {
                long pos = argOrder[i - 1];
                Value* arg = b.args()[pos];
                if (promarg[pos])
                    arg = ir::CreatePromise::create(b, arg, rho())->result();
                actuals = ir::ConsNr::create(b, arg, actuals)->result();
                // TODO:
                // ir::EnableRefcnt(actuals);
            }

            Value* newrho =
                ir::NewEnv::create(b, ir::Car::create(b, fun())->result(),
                                   actuals, ir::Tag::create(b, fun())->result())
                    ->result();

            Value* cntxt = new AllocaInst(t::cntxt, "", b);

            ir::InitClosureContext::create(b, cntxt, call(), newrho, rho(),
                                           actuals, fun());

            Value* res =
                ir::ClosureNativeCallTrampoline::create(
                    b, cntxt, b.convertToPointer(inBody), newrho, b.closure())
                    ->result();

            ir::EndClosureContext::create(b, cntxt, res);
            ir::Return::create(b, res);

            b.setBlock(icMiss);
            callIcMiss();

            return true;
        }
    }
    return false;
}

void ICCompiler::callIcMiss() {
    auto stub = getStub(size, b);
    auto res = ir::CallToAddress::create(b, stub, b.args())->result();
    ir::Return::create(b, res);
}

std::string ICCompiler::specialName(unsigned size) {
    std::ostringstream os;
    os << "callSpecialIC_" << size;
    return os.str();
}

void ICCompiler::compileSpecialIC() {
    b.openIC(name, ic_t);

    // TODO: only emit one branch depending on the type we currently see
    BasicBlock* icMatch = b.createBasicBlock("icMatch");
    BasicBlock* icMiss = b.createBasicBlock("icMiss");

    // Specials only care about the ast, so we can call any special through
    // this ic
    Value* ftype = ir::SexpType::create(b, fun())->result();
    Value* test = new ICmpInst(*b.block(), ICmpInst::ICMP_EQ, ftype,
                               b.integer(SPECIALSXP), "guard");

    BranchInst::Create(icMatch, icMiss, test, b);
    b.setBlock(icMatch);

    Value* res = ir::CallSpecial::create(
                     b, call(), fun(), b.convertToPointer(R_NilValue), b.rho())
                     ->result();
    ir::Return::create(b, res);

    b.setBlock(icMiss);
    callIcMiss();
}

bool ICCompiler::compileGenericIc(SEXP inCall, SEXP inFun) {
    // TODO: only emit one branch depending on the type we currently see
    BasicBlock* icMatch = b.createBasicBlock("icMatch");
    BasicBlock* icMiss = b.createBasicBlock("icMiss");

    Value* body = ir::Cdr::create(b, fun())->result();
    Value* test =
        new ICmpInst(*b.block(), ICmpInst::ICMP_EQ, body,
                     ir::Builder::convertToPointer(BODY(inFun)), "guard");

    BranchInst::Create(icMatch, icMiss, test, b);
    b.setBlock(icMatch);

    Value* res;
    switch (TYPEOF(inFun)) {
    case BUILTINSXP: {
        Value* args = compileArguments(CDR(inCall), /*eager=*/true);
        res = ir::CallBuiltin::create(b, call(), fun(), args, rho())->result();
        break;
    }
    case CLOSXP: {
        Value* args = compileArguments(CDR(inCall), /*eager=*/false);
        res = ir::CallClosure::create(b, call(), fun(), args, rho())->result();
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
                          b, arglist, rho(),
                          eager ? b.integer(TRUE, 1) : b.integer(FALSE, 1))
                          ->result();
            if (!arglistHead)
                arglistHead = arglist;

            // then add the rest
            arglist = ir::AddEllipsisArgumentTail::create(
                          b, arglist, rho(),
                          eager ? b.integer(TRUE, 1) : b.integer(FALSE, 1))
                          ->result();
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
                       b.convertToPointer(name))
                ->result();
        }
    // Fall through:
    default:
        if (eager) {
            // TODO make this more efficient?
            result = ir::CallNative::create(b, b.args()[argnum], rho(),
                                            b.convertToPointer(R_NilValue))
                         ->result();
        } else {
            // we must create a promise out of the argument
            result =
                ir::CreatePromise::create(b, b.args()[argnum], rho())->result();
        }
        break;
    }
    if (name != R_NilValue)
        return ir::AddKeywordArgument::create(b, arglist, result,
                                              b.convertToPointer(name))
            ->result();

    return ir::AddArgument::create(b, arglist, result)->result();
}

} // namespace rjit
