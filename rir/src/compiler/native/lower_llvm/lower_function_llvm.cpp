#include "lower_function_llvm.h"
#include "builtins.h"
#include "jit_llvm.h"
#include "types_llvm.h"

#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/liveness.h"
#include "compiler/analysis/reference_count.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include "interpreter/LazyEnvironment.h"
#include "interpreter/builtins.h"
#include "interpreter/instance.h"
#include "runtime/DispatchTable.h"
#include "utils/Pool.h"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace rir {
namespace pir {

using namespace llvm;

static LLVMContext& C = rir::pir::JitLLVM::C;

extern "C" size_t R_NSize;
extern "C" size_t R_NodesInUse;

void LowerFunctionLLVM::setVisible(int i) {
    builder.CreateStore(c(i), convertToPointer(&R_Visible, t::IntPtr));
}

llvm::Value* LowerFunctionLLVM::force(Instruction* i, llvm::Value* arg) {

    auto isProm = BasicBlock::Create(C, "", fun);
    auto needsEval = BasicBlock::Create(C, "", fun);
    auto isVal = BasicBlock::Create(C, "", fun);
    auto isPromVal = BasicBlock::Create(C, "", fun);
    auto done = BasicBlock::Create(C, "", fun);

    auto res = phiBuilder(t::SEXP);

    checkIsSexp(arg, "force argument");

    auto type = sexptype(arg);
    auto tt = builder.CreateICmpEQ(type, c(PROMSXP));

    builder.CreateCondBr(tt, isProm, isVal);

    builder.SetInsertPoint(isProm);
    auto val = car(arg);
    checkIsSexp(arg, "prval");
    auto tv = builder.CreateICmpEQ(val, constant(R_UnboundValue, t::SEXP));
    builder.CreateCondBr(tv, needsEval, isPromVal);

    builder.SetInsertPoint(needsEval);
    auto evaled = call(NativeBuiltins::forcePromise, {arg});
    checkIsSexp(evaled, "force result");
    res.addInput(evaled);
    builder.CreateBr(done);

    builder.SetInsertPoint(isVal);
    res.addInput(arg);
    builder.CreateBr(done);

    builder.SetInsertPoint(isPromVal);
    res.addInput(val);
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
    auto result = res();
#ifdef ENABLE_SLOWASSERT
    insn_assert(builder.CreateICmpNE(sexptype(result), c(PROMSXP)),
                "Force returned promise");
#endif
    return result;
}

void LowerFunctionLLVM::insn_assert(llvm::Value* v, const char* msg) {
    auto nok = BasicBlock::Create(C, "assertFail", fun);
    auto ok = BasicBlock::Create(C, "assertOk", fun);

    builder.CreateCondBr(v, ok, nok, branchAlwaysTrue);

    builder.SetInsertPoint(nok);
    call(NativeBuiltins::assertFail, {convertToPointer((void*)msg)});
    builder.CreateRet(builder.CreateIntToPtr(c(nullptr), t::SEXP));

    builder.SetInsertPoint(ok);
}

llvm::Value* LowerFunctionLLVM::constantSexp(SEXP co) {
    static std::unordered_set<SEXP> eternal = {R_TrueValue,  R_NilValue,
                                               R_FalseValue, R_UnboundValue,
                                               R_MissingArg, R_GlobalEnv};

    if (TYPEOF(co) == SYMSXP || eternal.count(co))
        return convertToPointer(co);

    auto i = Pool::insert(co);
    llvm::Value* pos = builder.CreateLoad(constantpool);
    pos = builder.CreateBitCast(dataPtr(pos, false),
                                PointerType::get(t::SEXP, 0));
    pos = builder.CreateGEP(pos, c(i));
    auto res = builder.CreateLoad(pos);
    return res;
}

llvm::Value* LowerFunctionLLVM::constant(SEXP co, llvm::Type* needed) {
    switch (Representation(needed).t) {
    case Representation::Type::Integer:
        // Note: This is different from prev, in that we can't represent sexps
        // with attributes
        if (IS_SIMPLE_SCALAR(co, INTSXP))
            return c(INTEGER(co)[0]);
        if (IS_SIMPLE_SCALAR(co, REALSXP))
            return c((int)REAL(co)[0]);
        if (IS_SIMPLE_SCALAR(co, LGLSXP))
            return c((int)LOGICAL(co)[0]);
        assert(false && "can't represent this as int");
    case Representation::Type::Real:
        // Note: This is different from prev, in that we can't represent sexps
        // with attributes, and can represent logicals
        if (IS_SIMPLE_SCALAR(co, INTSXP))
            return c((double)INTEGER(co)[0]);
        if (IS_SIMPLE_SCALAR(co, REALSXP))
            return c(REAL(co)[0]);
        if (IS_SIMPLE_SCALAR(co, LGLSXP))
            return c((double)LOGICAL(co)[0]);
        assert(false && "can't represent this as double");
    case Representation::Type::Sexp:
        return constantSexp(co);
    case Representation::Type::Bottom:
    default:
        assert(false && "unknown representation");
    }
}

llvm::Value* LowerFunctionLLVM::nodestackPtr() {
    return builder.CreateLoad(nodestackPtrAddr);
}

llvm::Value* LowerFunctionLLVM::stack(int i) {
    auto offset = -(i + 1);
    auto pos = builder.CreateGEP(nodestackPtr(), {c(offset), c(1)});
    return builder.CreateLoad(t::SEXP, pos);
}

void LowerFunctionLLVM::stack(const std::vector<llvm::Value*>& args) {
    auto stackptr = nodestackPtr();
    // set type tag to 0
    builder.CreateMemSet(builder.CreateGEP(stackptr, c(-args.size())), c(0, 8),
                         args.size() * sizeof(R_bcstack_t), 1);
    auto pos = -args.size();
    for (auto arg = args.begin(); arg != args.end(); arg++) {
        // store the value
        auto valS = builder.CreateGEP(stackptr, {c(pos), c(1)});
        builder.CreateStore(*arg, valS);
        pos++;
    }
    assert(pos == 0);
}

void LowerFunctionLLVM::setLocal(size_t i, llvm::Value* v) {
    assert(i < numLocals);
    assert(v->getType() == t::SEXP);
    auto pos = builder.CreateGEP(basepointer, {c(i), c(1)});
    builder.CreateStore(v, pos, true);
}

llvm::Value* LowerFunctionLLVM::getLocal(size_t i) {
    assert(i < numLocals);
    auto pos = builder.CreateGEP(basepointer, {c(i), c(1)});
    return builder.CreateLoad(pos);
}

void LowerFunctionLLVM::incStack(int i, bool zero) {
    if (i == 0)
        return;
    auto cur = nodestackPtr();
    auto offset = sizeof(R_bcstack_t) * i;
    if (zero)
        builder.CreateMemSet(cur, c(0, 8), offset, 1);
    auto up = builder.CreateGEP(cur, c(i));
    builder.CreateStore(up, nodestackPtrAddr);
}

void LowerFunctionLLVM::decStack(int i) {
    if (i == 0)
        return;
    auto cur = nodestackPtr();
    auto up = builder.CreateGEP(cur, c(-i));
    builder.CreateStore(up, nodestackPtrAddr);
}

llvm::Value* LowerFunctionLLVM::callRBuiltin(SEXP builtin,
                                             const std::vector<Value*>& args,
                                             int srcIdx, CCODE builtinFun,
                                             llvm::Value* env) {
    if (supportsFastBuiltinCall(builtin)) {
        return withCallFrame(args, [&]() -> llvm::Value* {
            return call(NativeBuiltins::callBuiltin,
                        {
                            paramCode(),
                            c(srcIdx),
                            constant(builtin, t::SEXP),
                            env,
                            c(args.size()),
                        });
        });
    }

    auto f = convertToPointer((void*)builtinFun, t::builtinFunctionPtr);

    auto arglist = constant(R_NilValue, t::SEXP);
    for (auto v = args.rbegin(); v != args.rend(); v++) {
        auto a = loadSxp(*v);
#ifdef ENABLE_SLOWASSERT
        insn_assert(builder.CreateICmpNE(sexptype(a), c(PROMSXP)),
                    "passing promise to builtin");
#endif
        arglist = call(NativeBuiltins::consNr, {a, arglist});
    }
    if (args.size() > 0)
        protectTemp(arglist);

    auto ast = constant(cp_pool_at(globalContext(), srcIdx), t::SEXP);
    // TODO: ensure that we cover all the fast builtin cases
    int flag = getFlag(builtin);
    if (flag < 2)
        setVisible(flag != 1);
    auto res = builder.CreateCall(f, {
                                         ast,
                                         constant(builtin, t::SEXP),
                                         arglist,
                                         env,
                                     });
    if (flag < 2)
        setVisible(flag != 1);
    return res;
}

llvm::Value*
LowerFunctionLLVM::withCallFrame(const std::vector<Value*>& args,
                                 const std::function<llvm::Value*()>& theCall,
                                 bool pop) {
    auto nargs = args.size();
    incStack(nargs, false);
    std::vector<llvm::Value*> jitArgs;
    for (auto& arg : args)
        jitArgs.push_back(load(arg, Representation::Sexp));
    stack(jitArgs);
    auto res = theCall();
    if (pop)
        decStack(nargs);
    return res;
}

llvm::Value* LowerFunctionLLVM::load(Value* v, Representation r) {
    return load(v, v->type, r);
}

llvm::Value* LowerFunctionLLVM::load(Value* v) {
    return load(v, v->type, representationOf(v));
}
llvm::Value* LowerFunctionLLVM::loadSxp(Value* v) {
    return load(v, Representation::Sexp);
}

llvm::Value* LowerFunctionLLVM::load(Value* val, PirType type,
                                     Representation needed) {
    llvm::Value* res;

    if (auto cast = CastType::Cast(val)) {
        auto arg = cast->arg(0).val();
        if (cast->kind == CastType::Upcast && !cast->type.isA(type))
            type = cast->type;
        if (cast->kind == CastType::Downcast && !type.isA(cast->type))
            type = cast->type;

        return load(arg, type, needed);
    }

    if (auto cp = PirCopy::Cast(val)) {
        if (!variables.count(cp))
            return load(cp->arg(0).val(), type, needed);
    }

    //    if (representationOf(val->type) == t::Int && needed == t::SEXP) {
    //        if (auto i = Instruction::Cast(val)) {
    //            if (currentInstr && !currentInstr->bb()->isDeopt() &&
    //                !LdConst::Cast(i)) {
    //                i->print(std::cout);
    //                std::cout << " for ";
    //                currentInstr->print(std::cout);
    //                std::cout << "\n";
    //            }
    //        }
    //    }

    auto vali = Instruction::Cast(val);
    if (vali && variables.count(vali))
        res = variables.at(vali).get(builder);
    else if (val == Env::elided())
        res = constant(R_NilValue, needed);
    else if (auto e = Env::Cast(val)) {
        if (e == Env::notClosed()) {
            res = tag(paramClosure());
        } else if (e == Env::nil()) {
            res = constant(R_NilValue, needed);
        } else if (Env::isStaticEnv(e)) {
            res = constant(e->rho, t::SEXP);
        } else {
            assert(false);
        }
    } else if (val == True::instance())
        res = constant(R_TrueValue, needed);
    else if (val == False::instance())
        res = constant(R_FalseValue, needed);
    else if (val == MissingArg::instance())
        res = constant(R_MissingArg, t::SEXP);
    else if (val == UnboundValue::instance())
        res = constant(R_UnboundValue, t::SEXP);
    else if (auto ld = LdConst::Cast(val))
        res = constant(ld->c(), needed);
    else if (val == NaLogical::instance())
        res = constant(R_LogicalNAValue, needed);
    else if (val == Nil::instance())
        res = constant(R_NilValue, needed);
    else {
        val->printRef(std::cerr);
        assert(false);
    }

    if (res->getType() == t::SEXP && needed != t::SEXP) {
        if (type.isA(PirType(RType::integer).scalar().notObject())) {
            res = unboxInt(res);
            assert(res->getType() == t::Int);
        } else if (type.isA((PirType() | RType::integer | RType::logical)
                                .scalar()
                                .notObject())) {
            res = unboxIntLgl(res);
            assert(res->getType() == t::Int);
        } else if (type.isA(PirType(RType::real).scalar().notObject())) {
            res = unboxReal(res);
            assert(res->getType() == t::Double);
        } else if (type.isA(
                       (PirType(RType::real) | RType::integer | RType::logical)
                           .scalar()
                           .notObject())) {
            res = unboxRealIntLgl(res);
            assert(res->getType() == t::Double);
        } else {
            // code->printCode(std::cout, true, true);
            std::cout << "Don't know how to unbox a " << type << "\n";
            val->printRef(std::cout);
            std::cout << "\n";
            assert(false);
        }
        // fall through, since more conversions might be needed after
        // unboxing
    }

    if (res->getType() == t::Int && needed == t::Double) {
        // TODO should we deal with na here?
        res = builder.CreateSIToFP(res, t::Double);
    } else if (res->getType() == t::Double && needed == t::Int) {
        // TODO should we deal with na here?
        res = builder.CreateFPToSI(res, t::Int);
    } else if ((res->getType() == t::Int || res->getType() == t::Double) &&
               needed == t::SEXP) {
        if (type.isA(PirType() | RType::integer))
            res = boxInt(res);
        else if (type.isA(PirType() | RType::logical))
            res = boxLgl(res);
        else if (type.isA(NativeType::test))
            res = boxTst(res);
        else if (type.isA(RType::real)) {
            res = boxReal(res);
        } else {
            std::cout << "Failed to convert int/float to " << type << "\n";
            Instruction::Cast(val)->print(std::cout);
            std::cout << "\n";
            code->printCode(std::cout, true, true);
            assert(false);
        }
    }

    if (res->getType() != needed) {
        std::cout << "Failed to load ";
        if (auto i = Instruction::Cast(val))
            i->print(std::cout, true);
        else
            val->printRef(std::cout);
        std::cout << " in the representation " << needed << "\n";
        assert(false);
    }

    return res;
}

llvm::Value* LowerFunctionLLVM::computeAndCheckIndex(Value* index,
                                                     llvm::Value* vector,
                                                     BasicBlock* fallback,
                                                     llvm::Value* max) {
    BasicBlock* hit1 = BasicBlock::Create(C, "", fun);
    BasicBlock* hit = BasicBlock::Create(C, "", fun);

    auto representation = representationOf(index);
    llvm::Value* nativeIndex = load(index);

    if (representation == Representation::Sexp) {
        if (representationOf(index->type) == Representation::Integer) {
            nativeIndex = unboxInt(nativeIndex);
            representation = Representation::Integer;
        } else {
            nativeIndex = unboxRealIntLgl(nativeIndex);
            representation = Representation::Real;
        }
    }

    if (representation == Representation::Real) {
        auto indexUnderRange = builder.CreateFCmpULT(nativeIndex, c(1.0));
        auto indexOverRange =
            builder.CreateFCmpUGE(nativeIndex, c((double)ULONG_MAX));
        auto indexNa = builder.CreateFCmpUNE(nativeIndex, nativeIndex);
        auto fail = builder.CreateOr(indexUnderRange,
                                     builder.CreateOr(indexOverRange, indexNa));

        builder.CreateCondBr(fail, fallback, hit1, branchMostlyFalse);
        builder.SetInsertPoint(hit1);

        nativeIndex = builder.CreateFPToUI(nativeIndex, t::i64);
    } else {
        assert(representation == Representation::Integer);
        auto indexUnderRange = builder.CreateICmpSLT(nativeIndex, c(1));
        auto indexNa = builder.CreateICmpEQ(nativeIndex, c(NA_INTEGER));
        auto fail = builder.CreateOr(indexUnderRange, indexNa);

        builder.CreateCondBr(fail, fallback, hit1, branchMostlyFalse);
        builder.SetInsertPoint(hit1);

        nativeIndex = builder.CreateZExt(nativeIndex, t::i64);
    }
    // R indexing is 1-based
    nativeIndex = builder.CreateSub(nativeIndex, c(1ul), "", true, true);

    auto ty = vector->getType();
    assert(ty == t::SEXP || ty == t::Int || ty == t::Double);
    if (!max)
        max = (ty == t::SEXP) ? vectorLength(vector) : c(1ul);
    auto indexOverRange = builder.CreateICmpUGE(nativeIndex, max);
    builder.CreateCondBr(indexOverRange, fallback, hit, branchMostlyFalse);
    builder.SetInsertPoint(hit);
    return nativeIndex;
}

void LowerFunctionLLVM::compilePopContext(Instruction* i) {
    auto popc = PopContext::Cast(i);
    auto data = contexts.at(popc->push());
    auto arg = load(popc->result());
    builder.CreateStore(arg, data.result);
    builder.CreateBr(data.popContextTarget);

    builder.SetInsertPoint(data.popContextTarget);
    llvm::Value* ret = builder.CreateLoad(data.result);
    llvm::Value* boxedRet = ret;
    if (ret->getType() == t::Int) {
        boxedRet = boxInt(ret, false);
    } else if (ret->getType() == t::Double) {
        boxedRet = boxReal(ret, false);
    }
    call(NativeBuiltins::endClosureContext, {data.rcntxt, boxedRet});
    inPushContext--;
    setVal(i, ret);
}

void LowerFunctionLLVM::compilePushContext(Instruction* i) {
    auto ct = PushContext::Cast(i);
    auto ast = loadSxp(ct->arg(0).val());
    auto op = loadSxp(ct->arg(1).val());
    auto sysparent = loadSxp(ct->env());

    inPushContext++;

    // initialize a RCNTXT on the stack
    auto& data = contexts[i];
    call(NativeBuiltins::initClosureContext, {ast, data.rcntxt, sysparent, op});

    // Create a copy of all live variables to be able to restart
    // SEXPs are stored as local vars, primitive values are placed in an
    // alloca'd buffer
    std::vector<std::pair<Instruction*, Variable>> savedLocals;
    {
        for (auto& v : variables) {
            auto& var = v.second;
            if (!var.initialized)
                continue;
            auto j = v.first;
            if (liveness.live(i, j)) {
                if (representationOf(j) == t::SEXP) {
                    savedLocals.push_back({j, Variable::MutableRVariable(
                                                  j, data.savedSexpPos.at(j),
                                                  builder, basepointer)});
                } else {
                    savedLocals.push_back(
                        {j,
                         Variable::Mutable(j, topAlloca(representationOf(j)))});
                }
            }
        }
        for (auto& v : savedLocals)
            v.second.set(builder, variables.at(v.first).get(builder));
    }

    // Do a setjmp
    auto didLongjmp = BasicBlock::Create(C, "", fun);
    auto cont = BasicBlock::Create(C, "", fun);
    {
#ifdef __APPLE__
        auto setjmpBuf = builder.CreateGEP(data.rcntxt, {c(0), c(2), c(0)});
        auto setjmpType = FunctionType::get(
            t::i32, {PointerType::get(t::i32, 0), t::i32}, false);
        auto setjmpFun =
            JitLLVM::getFunctionDeclaration("sigsetjmp", setjmpType, builder);
#else
        auto setjmpBuf = builder.CreateGEP(data.rcntxt, {c(0), c(2)});
        auto setjmpType =
            FunctionType::get(t::i32, {t::setjmp_buf_ptr, t::i32}, false);
        auto setjmpFun =
            JitLLVM::getFunctionDeclaration("__sigsetjmp", setjmpType, builder);
#endif
        auto longjmp = builder.CreateCall(setjmpFun, {setjmpBuf, c(0)});

        builder.CreateCondBr(builder.CreateICmpEQ(longjmp, c(0)), cont,
                             didLongjmp);
    }

    // Handle Incomming longjumps
    {
        builder.SetInsertPoint(didLongjmp);
        llvm::Value* returned = builder.CreateLoad(
            builder.CreateIntToPtr(c((void*)&R_ReturnedValue), t::SEXP_ptr));
        auto restart =
            builder.CreateICmpEQ(returned, constant(R_RestartToken, t::SEXP));

        auto longjmpRestart = BasicBlock::Create(C, "", fun);
        auto longjmpRet = BasicBlock::Create(C, "", fun);
        builder.CreateCondBr(restart, longjmpRestart, longjmpRet);

        // The longjump returned a restart token.
        // In this case we need to restore all local variables as we
        // preserved them before the setjmp and then continue
        // execution
        builder.SetInsertPoint(longjmpRestart);
        for (auto& v : savedLocals)
            variables.at(v.first).update(builder, v.second.get(builder));

        // Also clear all binding caches
        for (const auto& be : bindingsCache)
            for (const auto& b : be.second)
                builder.CreateStore(
                    convertToPointer(nullptr, t::SEXP),
                    builder.CreateGEP(bindingsCacheBase, c(b.second)));
        builder.CreateBr(cont);

        // The longjump returned a value to return.
        // In this case we store the result and skip everything
        // until the matching popcontext
        builder.SetInsertPoint(longjmpRet);
        if (data.result->getType()->getPointerElementType() == t::Int) {
            returned = unboxIntLgl(returned);
        } else if (data.result->getType()->getPointerElementType() ==
                   t::Double) {
            returned = unboxRealIntLgl(returned);
        }
        builder.CreateStore(returned, data.result);
        builder.CreateBr(data.popContextTarget);
    }

    builder.SetInsertPoint(cont);
}

llvm::Value* LowerFunctionLLVM::dataPtr(llvm::Value* v, bool enableAsserts) {
    assert(v->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
    if (enableAsserts)
        insn_assert(builder.CreateNot(isAltrep(v)),
                    "Trying to access an altrep vector");
#endif
    auto pos = builder.CreateBitCast(v, t::VECTOR_SEXPREC_ptr);
    return builder.CreateGEP(pos, c(1));
}

bool LowerFunctionLLVM::vectorTypeSupport(Value* vector) {
    auto type = vector->type;
    return type.isA(PirType(RType::vec).notObject()) ||
           type.isA(PirType(RType::integer).notObject()) ||
           type.isA(PirType(RType::logical).notObject()) ||
           type.isA(PirType(RType::real).notObject());
}

llvm::Value* LowerFunctionLLVM::vectorPositionPtr(llvm::Value* vector,
                                                  llvm::Value* position,
                                                  PirType type) {
    assert(vector->getType() == t::SEXP);
    PointerType* nativeType;
    if (type.isA(PirType(RType::integer).notObject()) ||
        type.isA(PirType(RType::logical).notObject())) {
        nativeType = t::IntPtr;
    } else if (type.isA(PirType(RType::real).notObject())) {
        nativeType = t::DoublePtr;
    } else if (type.isA(PirType(RType::vec).notObject())) {
        nativeType = t::SEXP_ptr;
    } else {
        nativeType = t::SEXP_ptr;
        assert(false);
    }
    auto pos = builder.CreateBitCast(dataPtr(vector), nativeType);
    return builder.CreateInBoundsGEP(pos, builder.CreateZExt(position, t::i64));
}

llvm::Value* LowerFunctionLLVM::accessVector(llvm::Value* vector,
                                             llvm::Value* position,
                                             PirType type) {
    return builder.CreateLoad(vectorPositionPtr(vector, position, type));
}

llvm::Value* LowerFunctionLLVM::assignVector(llvm::Value* vector,
                                             llvm::Value* position,
                                             llvm::Value* value, PirType type) {
    return builder.CreateStore(value,
                               vectorPositionPtr(vector, position, type));
}

llvm::Value* LowerFunctionLLVM::unbox(llvm::Value* val, Representation to) {
    assert(val->getType() == t::SEXP);
    switch (to.t) {
    case Representation::Type::Integer:
        return unboxIntLgl(val);
    case Representation::Type::Real:
        return unboxRealIntLgl(val);
    case Representation::Type::Sexp:
        assert(false && "never actually happens");
        return val;
    case Representation::Type::Bottom:
    default:
        assert(false && "don't know how to unbox this representation");
    }
}

llvm::Value* LowerFunctionLLVM::unboxIntLgl(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    checkSexptype(v, {LGLSXP, INTSXP});
    auto pos = builder.CreateBitCast(dataPtr(v), t::IntPtr);
    return builder.CreateLoad(pos);
}
llvm::Value* LowerFunctionLLVM::unboxInt(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
    checkSexptype(v, {INTSXP});
    insn_assert(isScalar(v), "expected scalar int");
#endif
    auto pos = builder.CreateBitCast(dataPtr(v), t::IntPtr);
    return builder.CreateLoad(pos);
}
llvm::Value* LowerFunctionLLVM::unboxLgl(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
    checkSexptype(v, {LGLSXP});
    insn_assert(isScalar(v), "expected scalar lgl");
#endif
    auto pos = builder.CreateBitCast(dataPtr(v), t::IntPtr);
    return builder.CreateLoad(pos);
}
llvm::Value* LowerFunctionLLVM::unboxReal(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
    checkSexptype(v, {REALSXP});
    insn_assert(isScalar(v), "expected scalar real");
#endif
    auto pos = builder.CreateBitCast(dataPtr(v), t::DoublePtr);
    auto res = builder.CreateLoad(pos);
    return res;
}
llvm::Value* LowerFunctionLLVM::unboxRealIntLgl(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto done = BasicBlock::Create(C, "", fun);
    auto isReal = BasicBlock::Create(C, "isReal", fun);
    auto notReal = BasicBlock::Create(C, "notReal", fun);

    auto res = phiBuilder(t::Double);

    auto type = sexptype(v);
    auto tt = builder.CreateICmpEQ(type, c(REALSXP));
    builder.CreateCondBr(tt, isReal, notReal);

    builder.SetInsertPoint(notReal);

    auto intres = unboxIntLgl(v);

    auto isNaBr = BasicBlock::Create(C, "isNa", fun);
    nacheck(intres, isNaBr);

    res.addInput(builder.CreateSIToFP(intres, t::Double));
    builder.CreateBr(done);

    builder.SetInsertPoint(isNaBr);
    res.addInput(c(R_NaN));
    builder.CreateBr(done);

    builder.SetInsertPoint(isReal);
    res.addInput(unboxReal(v));
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
    return res();
}

llvm::Value* LowerFunctionLLVM::argument(int i) {
    auto pos = builder.CreateGEP(paramArgs(), c(i));
    pos = builder.CreateGEP(pos, {c(0), c(1)});
    return builder.CreateLoad(t::SEXP, pos);
}

AllocaInst* LowerFunctionLLVM::topAlloca(llvm::Type* t, size_t len) {
    auto cur = builder.GetInsertBlock();
    builder.SetInsertPoint(entryBlock);
    auto res = builder.CreateAlloca(t, 0, c(len));
    builder.SetInsertPoint(cur);
    return res;
}

llvm::Value* LowerFunctionLLVM::intToDouble(llvm::Value* val) {
    return builder.CreateSelect(builder.CreateICmpEQ(val, c(NA_INTEGER)),
                                c(NA_REAL),
                                builder.CreateSIToFP(val, t::Double));
}

llvm::Value* LowerFunctionLLVM::doubleToInt(llvm::Value* val) {
    return builder.CreateSelect(builder.CreateFCmpUNE(val, val), c(NA_INTEGER),
                                builder.CreateFPToSI(val, t::Int));
}

llvm::Value* LowerFunctionLLVM::convert(llvm::Value* val, PirType toType,
                                        bool protect) {
    auto to = representationOf(toType);
    auto from = val->getType();
    if (from == to)
        return val;

    if (from == t::SEXP && to != t::SEXP) {
        return unbox(val, to);
    } else if (from != t::SEXP && to == t::SEXP) {
        return box(val, toType, protect);
    } else if (from == t::Int && to == t::Double) {
        return intToDouble(val);
    } else if (from == t::Double && to == t::Int) {
        return doubleToInt(val);
    } else {
        std::cout << "\nFailed to convert a " << val->getType() << " to "
                  << toType << "\n";
        assert(false);
        return nullptr;
    }
}

void LowerFunctionLLVM::setVal(Instruction* i, llvm::Value* val) {
    assert(i->producesRirResult() && !CastType::Cast(i) &&
           !PushContext::Cast(i));
    val = convert(val, i->type, false);
    if (!val->hasName())
        val->setName(i->getRef());

    variables.at(i).set(builder, val,
                        inPushContext && escapesInlineContext.count(i));
}

llvm::Value* LowerFunctionLLVM::isExternalsxp(llvm::Value* v, uint32_t magic) {
    assert(v->getType() == t::SEXP);
    auto isExternalsxp = builder.CreateICmpEQ(c(EXTERNALSXP), sexptype(v));
    auto es = builder.CreateBitCast(dataPtr(v, false),
                                    PointerType::get(t::RirRuntimeObject, 0));
    auto magicVal = builder.CreateLoad(builder.CreateGEP(es, {c(0), c(2)}));
    auto isCorrectMagic = builder.CreateICmpEQ(magicVal, c(magic));
    return builder.CreateAnd(isExternalsxp, isCorrectMagic);
}

void LowerFunctionLLVM::checkSexptype(llvm::Value* v,
                                      const std::vector<SEXPTYPE>& types) {
#ifdef ENABLE_SLOWASSERT
    auto type = sexptype(v);
    llvm::Value* match = builder.getTrue();
    assert(types.size());
    for (auto t : types) {
        auto test = builder.CreateICmpEQ(type, c(t));
        match = builder.CreateOr(match, test);
    }
    insn_assert(match, "unexpexted sexptype");
#endif
}

void LowerFunctionLLVM::checkIsSexp(llvm::Value* v, const std::string& msg) {
#ifdef ENABLE_SLOWASSERT
    static bool checking = false;
    if (checking)
        return;
    checking = true;
    static std::vector<std::string> strings;
    strings.push_back(std::string("expected sexp got null ") + msg);
    insn_assert(builder.CreateICmpNE(convertToPointer(nullptr, t::SEXP), v),
                strings.back().c_str());
    auto type = sexptype(v);
    auto validType =
        builder.CreateOr(builder.CreateICmpULE(type, c(EXTERNALSXP)),
                         builder.CreateICmpEQ(type, c(FUNSXP)));
    strings.push_back(std::string("invalid sexptype ") + msg);
    insn_assert(validType, strings.back().c_str());
    checking = false;
#endif
}

llvm::Value* LowerFunctionLLVM::sxpinfoPtr(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    checkIsSexp(v, "in sxpinfoPtr");
    auto sxpinfoPtr = builder.CreateGEP(t::SEXPREC, v, {c(0), c(0)});
    sxpinfoPtr->setName("sxpinfo");
    return builder.CreateBitCast(sxpinfoPtr, t::i64ptr);
}

void LowerFunctionLLVM::setSexptype(llvm::Value* v, int t) {
    auto ptr = sxpinfoPtr(v);
    llvm::Value* sxpinfo = builder.CreateLoad(ptr);
    sxpinfo =
        builder.CreateAnd(sxpinfo, c(~((unsigned long)(MAX_NUM_SEXPTYPE - 1))));
    sxpinfo = builder.CreateOr(sxpinfo, c(t, 64));
    builder.CreateStore(sxpinfo, ptr);
}

llvm::Value* LowerFunctionLLVM::sexptype(llvm::Value* v) {
    auto sxpinfo = builder.CreateLoad(sxpinfoPtr(v));
    auto t = builder.CreateAnd(sxpinfo, c(MAX_NUM_SEXPTYPE - 1, 64));
    return builder.CreateTrunc(t, t::Int);
}

llvm::Value* LowerFunctionLLVM::tag(llvm::Value* v) {
    auto pos = builder.CreateGEP(v, {c(0), c(4), c(2)});
    return builder.CreateLoad(pos);
}

void LowerFunctionLLVM::setCar(llvm::Value* x, llvm::Value* y,
                               bool needsWriteBarrier) {
    auto fast = [&]() {
        auto xx = builder.CreateGEP(x, {c(0), c(4), c(0)});
        builder.CreateStore(y, xx);
    };
    if (!needsWriteBarrier) {
        fast();
        return;
    }
    writeBarrier(x, y, fast, [&]() { call(NativeBuiltins::setCar, {x, y}); });
}

void LowerFunctionLLVM::setCdr(llvm::Value* x, llvm::Value* y,
                               bool needsWriteBarrier) {
    auto fast = [&]() {
        auto xx = builder.CreateGEP(x, {c(0), c(4), c(1)});
        builder.CreateStore(y, xx);
    };
    if (!needsWriteBarrier) {
        fast();
        return;
    }
    writeBarrier(x, y, fast, [&]() { call(NativeBuiltins::setCdr, {x, y}); });
}

void LowerFunctionLLVM::setTag(llvm::Value* x, llvm::Value* y,
                               bool needsWriteBarrier) {
    auto fast = [&]() {
        auto xx = builder.CreateGEP(x, {c(0), c(4), c(2)});
        builder.CreateStore(y, xx);
    };
    if (!needsWriteBarrier) {
        fast();
        return;
    }
    writeBarrier(x, y, fast, [&]() { call(NativeBuiltins::setTag, {x, y}); });
}

llvm::Value* LowerFunctionLLVM::car(llvm::Value* v) {
    v = builder.CreateGEP(v, {c(0), c(4), c(0)});
    return builder.CreateLoad(v);
}

llvm::Value* LowerFunctionLLVM::cdr(llvm::Value* v) {
    v = builder.CreateGEP(v, {c(0), c(4), c(1)});
    return builder.CreateLoad(v);
}

llvm::Value* LowerFunctionLLVM::attr(llvm::Value* v) {
    auto pos = builder.CreateGEP(v, {c(0), c(1)});
    return builder.CreateLoad(pos);
}

llvm::Value* LowerFunctionLLVM::isScalar(llvm::Value* v) {
    auto va = builder.CreateBitCast(v, t::VECTOR_SEXPREC_ptr);
    auto lp = builder.CreateGEP(va, {c(0), c(4), c(0)});
    auto l = builder.CreateLoad(lp);
    return builder.CreateICmpEQ(l, c(1, 64));
}

llvm::Value* LowerFunctionLLVM::isSimpleScalar(llvm::Value* v, SEXPTYPE t) {
    auto sxpinfo = builder.CreateLoad(sxpinfoPtr(v));

    auto type = builder.CreateAnd(sxpinfo, c(MAX_NUM_SEXPTYPE - 1, 64));
    auto okType = builder.CreateICmpEQ(c(t), builder.CreateTrunc(type, t::Int));

    auto isScalar = builder.CreateICmpNE(
        c(0, 64),
        builder.CreateAnd(sxpinfo, c((unsigned long)(1ul << (TYPE_BITS)))));

    auto noAttrib =
        builder.CreateICmpEQ(attr(v), constant(R_NilValue, t::SEXP));

    return builder.CreateAnd(okType, builder.CreateAnd(isScalar, noAttrib));
}

llvm::Value* LowerFunctionLLVM::vectorLength(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto pos = builder.CreateBitCast(v, t::VECTOR_SEXPREC_ptr);
    pos = builder.CreateGEP(pos, {c(0), c(4), c(0)});
    return builder.CreateLoad(pos);
}
void LowerFunctionLLVM::assertNamed(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = builder.CreateBitCast(sxpinfoPtr(v), t::i64ptr);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1) << 32;
    auto named = builder.CreateAnd(sxpinfo, c(namedMask));
    auto isNotNamed = builder.CreateICmpEQ(named, c(0, 64));

    auto notNamed = BasicBlock::Create(C, "notNamed", fun);
    auto ok = BasicBlock::Create(C, "", fun);

    builder.CreateCondBr(isNotNamed, notNamed, ok);

    builder.SetInsertPoint(notNamed);
    insn_assert(builder.getFalse(), "Value is not named");
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
};

llvm::Value* LowerFunctionLLVM::shared(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = builder.CreateBitCast(sxpinfoPtr(v), t::i64ptr);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1);
    auto named = builder.CreateLShr(sxpinfo, c(32ul));
    named = builder.CreateAnd(named, c(namedMask));
    return builder.CreateICmpUGT(named, c(1ul));
}

void LowerFunctionLLVM::ensureNamed(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = builder.CreateBitCast(sxpinfoPtr(v), t::i64ptr);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1) << 32;
    unsigned long namedLSB = 1ul << 32;

    auto named = builder.CreateAnd(sxpinfo, c(namedMask));
    auto isNotNamed = builder.CreateICmpEQ(named, c(0, 64));

    auto notNamed = BasicBlock::Create(C, "notNamed", fun);
    auto ok = BasicBlock::Create(C, "", fun);

    builder.CreateCondBr(isNotNamed, notNamed, ok);

    builder.SetInsertPoint(notNamed);
    auto namedSxpinfo = builder.CreateOr(sxpinfo, c(namedLSB));
    builder.CreateStore(namedSxpinfo, sxpinfoP);
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
};

void LowerFunctionLLVM::ensureShared(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = sxpinfoPtr(v);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1);
    static auto namedNegMask = ~(namedMask << 32);

    auto named = builder.CreateLShr(sxpinfo, c(32, 64));
    named = builder.CreateAnd(named, c(namedMask));

    auto isNamedShared = builder.CreateICmpUGE(named, c(2, 64));

    auto incrementBr = BasicBlock::Create(C, "", fun);
    auto done = BasicBlock::Create(C, "", fun);

    builder.CreateCondBr(isNamedShared, done, incrementBr);

    builder.SetInsertPoint(incrementBr);
    auto newNamed = c(2ul << 32, 64);

    auto newSxpinfo = builder.CreateAnd(sxpinfo, c(namedNegMask));
    newSxpinfo = builder.CreateOr(newSxpinfo, newNamed);
    builder.CreateStore(newSxpinfo, sxpinfoP);
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
};

void LowerFunctionLLVM::incrementNamed(llvm::Value* v, int max) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = sxpinfoPtr(v);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1);
    static auto namedNegMask = ~(namedMask << 32);

    auto named = builder.CreateLShr(sxpinfo, c(32, 64));
    named = builder.CreateAnd(named, c(namedMask));

    auto isNamedMax = builder.CreateICmpEQ(named, c(max, 64));

    auto incrementBr = BasicBlock::Create(C, "", fun);
    auto done = BasicBlock::Create(C, "", fun);

    builder.CreateCondBr(isNamedMax, done, incrementBr);

    builder.SetInsertPoint(incrementBr);
    auto newNamed = builder.CreateAdd(named, c(1, 64), "", true, true);
    newNamed = builder.CreateShl(newNamed, c(32, 64));

    auto newSxpinfo = builder.CreateAnd(sxpinfo, c(namedNegMask));
    newSxpinfo = builder.CreateOr(newSxpinfo, newNamed);
    builder.CreateStore(newSxpinfo, sxpinfoP);
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
};

void LowerFunctionLLVM::nacheck(llvm::Value* v, BasicBlock* isNa,
                                BasicBlock* notNa) {
    if (!notNa)
        notNa = BasicBlock::Create(C, "", fun);
    if (v->getType() == t::Double) {
        auto isNotNa = builder.CreateFCmpUEQ(v, v);
        builder.CreateCondBr(isNotNa, notNa, isNa, branchMostlyTrue);
    } else {
        assert(v->getType() == t::Int);
        auto isNotNa = builder.CreateICmpNE(v, c(NA_INTEGER));
        builder.CreateCondBr(isNotNa, notNa, isNa, branchMostlyTrue);
    }
    builder.SetInsertPoint(notNa);
}

void LowerFunctionLLVM::checkMissing(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto ok = BasicBlock::Create(C, "", fun);
    auto nok = BasicBlock::Create(C, "", fun);
    auto t = builder.CreateICmpEQ(v, constant(R_MissingArg, t::SEXP));
    builder.CreateCondBr(t, nok, ok, branchAlwaysFalse);

    builder.SetInsertPoint(nok);
    call(NativeBuiltins::error, {});
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
}

void LowerFunctionLLVM::checkUnbound(llvm::Value* v) {
    auto ok = BasicBlock::Create(C, "", fun);
    auto nok = BasicBlock::Create(C, "", fun);
    auto t = builder.CreateICmpEQ(v, constant(R_UnboundValue, t::SEXP));
    builder.CreateCondBr(t, nok, ok, branchAlwaysFalse);

    builder.SetInsertPoint(nok);
    call(NativeBuiltins::error, {});
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
}

llvm::CallInst* LowerFunctionLLVM::call(const NativeBuiltin& builtin,
                                        const std::vector<llvm::Value*>& args) {
#ifdef ENABLE_SLOWASSERT
    // abuse BB lable as comment
    auto callBB = BasicBlock::Create(C, builtin.name, fun);
    builder.CreateBr(callBB);
    builder.SetInsertPoint(callBB);
#endif
    llvm::Type* tp = PointerType::get(builtin.llvmSignature, 0);
    auto trg = builder.CreateIntToPtr(c(builtin.fun), tp);
    return builder.CreateCall(trg, args);
}

llvm::Value* LowerFunctionLLVM::box(llvm::Value* v, PirType t, bool protect) {
    llvm::Value* res = nullptr;
    if (t.isA(PirType(RType::integer).notObject()))
        res = boxInt(v, protect);
    if (t.isA(PirType(RType::logical).notObject()))
        res = boxLgl(v, protect);
    if (t.isA(PirType(RType::real).notObject()))
        res = boxReal(v, protect);
    assert(res);
    if (protect)
        protectTemp(res);
    return res;
}
llvm::Value* LowerFunctionLLVM::boxInt(llvm::Value* v, bool protect) {
    if (v->getType() == t::Int)
        return call(NativeBuiltins::newInt, {v});
    assert(v->getType() == t::Double);
    return call(NativeBuiltins::newIntFromReal, {v});
}
llvm::Value* LowerFunctionLLVM::boxReal(llvm::Value* v, bool protect) {
    if (v->getType() == t::Double)
        return call(NativeBuiltins::newReal, {v});
    assert(v->getType() == t::Int);
    return call(NativeBuiltins::newRealFromInt, {v});
}
llvm::Value* LowerFunctionLLVM::boxLgl(llvm::Value* v, bool protect) {
    if (v->getType() == t::Int)
        return call(NativeBuiltins::newLgl, {v});
    assert(v->getType() == t::Double);
    return call(NativeBuiltins::newLglFromReal, {v});
}
llvm::Value* LowerFunctionLLVM::boxTst(llvm::Value* v, bool protect) {
    assert(v->getType() == t::Int);
    return builder.CreateSelect(builder.CreateICmpNE(v, c(0)),
                                constant(R_TrueValue, t::SEXP),
                                constant(R_FalseValue, t::SEXP));
}

void LowerFunctionLLVM::protectTemp(llvm::Value* val) {
    assert(numTemps < MAX_TEMPS);
    setLocal(numLocals - 1 - numTemps++, val);
}

llvm::Value* LowerFunctionLLVM::depromise(llvm::Value* arg) {

    auto isProm = BasicBlock::Create(C, "isProm", fun);
    auto isVal = BasicBlock::Create(C, "", fun);
    auto ok = BasicBlock::Create(C, "", fun);

    auto res = phiBuilder(t::SEXP);

    auto type = sexptype(arg);
    auto tt = builder.CreateICmpEQ(type, c(PROMSXP));
    builder.CreateCondBr(tt, isProm, isVal);

    builder.SetInsertPoint(isProm);
    auto val = car(arg);
    res.addInput(val);
    builder.CreateBr(ok);

    builder.SetInsertPoint(isVal);
#ifdef ENABLE_SLOWASSERT
    insn_assert(builder.CreateICmpNE(sexptype(arg), c(PROMSXP)),
                "Depromise returned promise");
#endif
    res.addInput(arg);
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
    return res();
}

void LowerFunctionLLVM::compileRelop(
    Instruction* i,
    const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& intInsert,
    const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& fpInsert,
    BinopKind kind) {
    auto rep = representationOf(i);
    auto lhs = i->arg(0).val();
    auto rhs = i->arg(1).val();
    auto lhsRep = representationOf(lhs);
    auto rhsRep = representationOf(rhs);

    if (lhsRep == Representation::Sexp || rhsRep == Representation::Sexp) {
        auto a = loadSxp(lhs);
        auto b = loadSxp(rhs);

        llvm::Value* res;
        if (i->hasEnv()) {
            auto e = loadSxp(i->env());
            res = call(NativeBuiltins::binopEnv,
                       {a, b, e, c(i->srcIdx), c((int)kind)});
        } else {
            res = call(NativeBuiltins::binop, {a, b, c((int)kind)});
        }
        setVal(i, res);
        return;
    }

    auto isNaBr = BasicBlock::Create(C, "isNa", fun);
    auto done = BasicBlock::Create(C, "", fun);

    auto res = phiBuilder(t::Int);
    auto a = load(lhs, lhsRep);
    auto b = load(rhs, rhsRep);

    nacheck(a, isNaBr);
    nacheck(b, isNaBr);

    if (a->getType() == t::Int && b->getType() == t::Int) {
        res.addInput(builder.CreateZExt(intInsert(a, b), t::Int));
    } else {
        if (a->getType() == t::Int)
            a = builder.CreateSIToFP(a, t::Double);
        if (b->getType() == t::Int)
            b = builder.CreateSIToFP(b, t::Double);
        res.addInput(builder.CreateZExt(fpInsert(a, b), t::Int));
    }

    builder.CreateBr(done);

    builder.SetInsertPoint(isNaBr);
    res.addInput(c(NA_INTEGER));
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
    if (rep == Representation::Sexp) {
        setVal(i, boxLgl(res(), false));
    } else {
        setVal(i, res());
    }
};

void LowerFunctionLLVM::compileBinop(
    Instruction* i, Value* lhs, Value* rhs,
    const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& intInsert,
    const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& fpInsert,
    BinopKind kind) {
    auto rep = representationOf(i);
    auto lhsRep = representationOf(lhs);
    auto rhsRep = representationOf(rhs);

    if (lhsRep == Representation::Sexp || rhsRep == Representation::Sexp ||
        (!fpInsert && (lhsRep != Representation::Integer ||
                       rhsRep != Representation::Integer))) {
        auto a = loadSxp(lhs);
        auto b = loadSxp(rhs);

        llvm::Value* res = nullptr;
        if (i->hasEnv()) {
            auto e = loadSxp(i->env());
            res = call(NativeBuiltins::binopEnv,
                       {a, b, e, c(i->srcIdx), c((int)kind)});
        } else {
            res = call(NativeBuiltins::binop, {a, b, c((int)kind)});
        }

        setVal(i, res);
        return;
    }

    BasicBlock* isNaBr = nullptr;
    auto done = BasicBlock::Create(C, "", fun);

    auto r = (lhsRep == Representation::Real || rhsRep == Representation::Real)
                 ? t::Double
                 : t::Int;

    auto res = phiBuilder(r);
    auto a = load(lhs, lhsRep);
    auto b = load(rhs, rhsRep);

    auto checkNa = [&](llvm::Value* v, Representation r) {
        if (r == Representation::Integer) {
            if (!isNaBr)
                isNaBr = BasicBlock::Create(C, "isNa", fun);
            nacheck(v, isNaBr);
        }
    };
    checkNa(a, lhsRep);
    checkNa(b, rhsRep);

    if (a->getType() == t::Int && b->getType() == t::Int) {
        res.addInput(intInsert(a, b));
    } else {
        if (a->getType() == t::Int)
            a = builder.CreateSIToFP(a, t::Double);
        if (b->getType() == t::Int)
            b = builder.CreateSIToFP(b, t::Double);
        res.addInput(fpInsert(a, b));
    }
    builder.CreateBr(done);

    if (lhsRep == Representation::Integer ||
        rhsRep == Representation::Integer) {
        if (isNaBr) {
            builder.SetInsertPoint(isNaBr);

            if (r == t::Int)
                res.addInput(c(NA_INTEGER));
            else
                res.addInput(c((double)R_NaN));

            builder.CreateBr(done);
        }
    }

    builder.SetInsertPoint(done);
    if (rep == Representation::Sexp) {
        setVal(i, box(res(), lhs->type.mergeWithConversion(rhs->type), false));
    } else {
        setVal(i, res());
    }
};

void LowerFunctionLLVM::compileUnop(
    Instruction* i, Value* arg,
    const std::function<llvm::Value*(llvm::Value*)>& intInsert,
    const std::function<llvm::Value*(llvm::Value*)>& fpInsert, UnopKind kind) {
    auto argRep = representationOf(arg);

    if (argRep == Representation::Sexp) {
        auto a = loadSxp(arg);

        llvm::Value* res = nullptr;
        if (i->hasEnv()) {
            auto e = loadSxp(i->env());
            res = call(NativeBuiltins::unopEnv,
                       {a, e, c(i->srcIdx), c((int)kind)});
        } else {
            res = call(NativeBuiltins::unop, {a, c((int)kind)});
        }

        setVal(i, res);
        return;
    }

    BasicBlock* isNaBr = nullptr;
    auto done = BasicBlock::Create(C, "", fun);

    auto r = (argRep == Representation::Real) ? t::Double : t::Int;

    auto res = phiBuilder(r);
    auto a = load(arg, argRep);

    auto checkNa = [&](llvm::Value* v, Representation r) {
        if (r == Representation::Integer) {
            if (!isNaBr)
                isNaBr = BasicBlock::Create(C, "isNa", fun);
            nacheck(v, isNaBr);
        }
    };
    checkNa(a, argRep);

    if (a->getType() == t::Int) {
        res.addInput(intInsert(a));
    } else {
        res.addInput(fpInsert(a));
    }
    builder.CreateBr(done);

    if (argRep == Representation::Integer) {
        if (isNaBr) {
            builder.SetInsertPoint(isNaBr);

            if (r == t::Int)
                res.addInput(c(NA_INTEGER));
            else
                res.addInput(c((double)R_NaN));

            builder.CreateBr(done);
        }
    }

    builder.SetInsertPoint(done);
    setVal(i, res());
};

void LowerFunctionLLVM::writeBarrier(llvm::Value* x, llvm::Value* y,
                                     std::function<void()> no,
                                     std::function<void()> yes) {
    auto sxpinfoX = builder.CreateLoad(sxpinfoPtr(x));

    auto markBitPos = c((unsigned long)(1ul << (TYPE_BITS + 19)));
    auto genBitPos = c((unsigned long)(1ul << (TYPE_BITS + 23)));

    auto done = BasicBlock::Create(C, "", fun);
    auto noBarrier = BasicBlock::Create(C, "", fun);
    auto maybeNeedsBarrier = BasicBlock::Create(C, "", fun);
    auto maybeNeedsBarrier2 = BasicBlock::Create(C, "", fun);
    auto needsBarrier = BasicBlock::Create(C, "", fun);

    auto markBitX =
        builder.CreateICmpNE(builder.CreateAnd(sxpinfoX, markBitPos), c(0, 64));
    builder.CreateCondBr(markBitX, maybeNeedsBarrier, noBarrier);

    builder.SetInsertPoint(maybeNeedsBarrier);
    auto sxpinfoY = builder.CreateLoad(sxpinfoPtr(y));
    auto markBitY =
        builder.CreateICmpNE(builder.CreateAnd(sxpinfoY, markBitPos), c(0, 64));
    builder.CreateCondBr(markBitY, maybeNeedsBarrier2, needsBarrier);
    builder.SetInsertPoint(maybeNeedsBarrier2);

    auto genBitX = builder.CreateAnd(sxpinfoX, genBitPos);
    auto genBitY = builder.CreateAnd(sxpinfoY, genBitPos);
    auto olderGen = builder.CreateICmpUGT(genBitX, genBitY);
    builder.CreateCondBr(olderGen, needsBarrier, noBarrier, branchMostlyFalse);

    builder.SetInsertPoint(noBarrier);
    no();
    builder.CreateBr(done);

    builder.SetInsertPoint(needsBarrier);
    yes();
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
};

bool LowerFunctionLLVM::compileDotcall(
    Instruction* i, const std::function<llvm::Value*()>& callee,
    const std::function<SEXP(size_t)>& names) {
    auto calli = CallInstruction::CastCall(i);
    assert(calli);
    std::vector<Value*> args;
    std::vector<BC::PoolIdx> newNames;
    bool seenDots = false;
    size_t pos = 0;
    calli->eachCallArg([&](Value* v) {
        if (auto exp = ExpandDots::Cast(v)) {
            args.push_back(exp->arg(0).val());
            newNames.push_back(Pool::insert(R_DotsSymbol));
            seenDots = true;
        } else {
            assert(!DotsList::Cast(v));
            newNames.push_back(Pool::insert(names(pos)));
            args.push_back(v);
        }
        pos++;
    });
    if (!seenDots)
        return false;
    Assumptions asmpt = calli->inferAvailableAssumptions();
    auto namesConst = c(newNames);
    auto namesStore = globalConst(namesConst);

    setVal(i,
           withCallFrame(
               args,
               [&]() -> llvm::Value* {
                   return call(NativeBuiltins::dotsCall,
                               {
                                   paramCode(),
                                   c(i->srcIdx),
                                   callee(),
                                   i->hasEnv() ? loadSxp(i->env())
                                               : constant(R_BaseEnv, t::SEXP),
                                   c(calli->nCallArgs()),
                                   builder.CreateBitCast(namesStore, t::IntPtr),
                                   c(asmpt.toI()),
                               });
               },
               /* dotCall pops arguments : */ false));
    return true;
}

llvm::Value* LowerFunctionLLVM::envStubGet(llvm::Value* x, int i, size_t size) {
    // We could use externalsxpGetEntry, but this is faster
    assert(x->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
    insn_assert(isExternalsxp(x, LAZY_ENVIRONMENT_MAGIC),
                "envStubGet on something which is not an env stub");
#endif
    auto le = builder.CreateBitCast(dataPtr(x, false),
                                    PointerType::get(t::LazyEnvironment, 0));
    auto missingBits =
        builder.CreateBitCast(builder.CreateGEP(le, c(1)), t::i8ptr);
    auto payload = builder.CreateBitCast(
        builder.CreateGEP(missingBits, c(size)), t::SEXP_ptr);
    auto pos = builder.CreateGEP(payload, c(i + LazyEnvironment::ArgOffset));
    return builder.CreateLoad(pos);
}

void LowerFunctionLLVM::envStubSetNotMissing(llvm::Value* x, int i) {
    auto le = builder.CreateBitCast(dataPtr(x, false),
                                    PointerType::get(t::LazyEnvironment, 0));
    auto missingBits =
        builder.CreateBitCast(builder.CreateGEP(le, c(1)), t::i8ptr);
    auto pos = builder.CreateGEP(missingBits, c(i));
    builder.CreateStore(c(1, 8), pos);
}

void LowerFunctionLLVM::envStubSet(llvm::Value* x, int i, llvm::Value* y,
                                   size_t size, bool setNotMissing) {
    // We could use externalsxpSetEntry, but this is faster
    writeBarrier(
        x, y,
        [&]() {
            assert(x->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
            insn_assert(isExternalsxp(x, LAZY_ENVIRONMENT_MAGIC),
                        "envStubGet on something which is not an env stub");
#endif
            auto le = builder.CreateBitCast(
                dataPtr(x, false), PointerType::get(t::LazyEnvironment, 0));
            auto missingBits =
                builder.CreateBitCast(builder.CreateGEP(le, c(1)), t::i8ptr);
            auto payload = builder.CreateBitCast(
                builder.CreateGEP(missingBits, c(size)), t::SEXP_ptr);
            auto pos =
                builder.CreateGEP(payload, c(i + LazyEnvironment::ArgOffset));
            builder.CreateStore(y, pos);
        },
        [&]() {
            call(NativeBuiltins::externalsxpSetEntry,
                 {{x, c(i + LazyEnvironment::ArgOffset), y}});
        });
    if (setNotMissing) {
        auto le = builder.CreateBitCast(
            dataPtr(x, false), PointerType::get(t::LazyEnvironment, 0));
        auto missingBits =
            builder.CreateBitCast(builder.CreateGEP(le, c(1)), t::i8ptr);
        auto pos = builder.CreateGEP(missingBits, c(i));
        builder.CreateStore(c(1, 8), pos);
    }
}

llvm::Value* LowerFunctionLLVM::isObj(llvm::Value* v) {
    checkIsSexp(v, "in IsObj");
    auto sxpinfo = builder.CreateLoad(sxpinfoPtr(v));
    return builder.CreateICmpNE(
        c(0, 64),
        builder.CreateAnd(sxpinfo, c((unsigned long)(1ul << (TYPE_BITS + 1)))));
};

llvm::Value* LowerFunctionLLVM::fastVeceltOkNative(llvm::Value* v) {
    checkIsSexp(v, "in IsFastVeceltOkNative");
    auto attrs = attr(v);
    auto isNil = builder.CreateICmpEQ(attrs, constant(R_NilValue, t::SEXP));
    auto isMatr1 =
        builder.CreateICmpEQ(tag(attrs), constant(R_DimSymbol, t::SEXP));
    auto isMatr2 =
        builder.CreateICmpEQ(cdr(attrs), constant(R_NilValue, t::SEXP));
    auto isMatr = builder.CreateAnd(isMatr1, isMatr2);
    return builder.CreateOr(isNil, isMatr);
};

llvm::Value* LowerFunctionLLVM::isAltrep(llvm::Value* v) {
    checkIsSexp(v, "in is altrep");
    auto sxpinfo = builder.CreateLoad(sxpinfoPtr(v));
    return builder.CreateICmpNE(
        c(0, 64),
        builder.CreateAnd(sxpinfo, c((unsigned long)(1ul << (TYPE_BITS + 2)))));
};

} // namespace pir
} // namespace rir