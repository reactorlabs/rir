#include "lower_function_llvm.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/reference_count.h"
#include "compiler/native/builtins.h"
#include "compiler/native/representation_llvm.h"
#include "compiler/native/types_llvm.h"
#include "compiler/parameter.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/lowering/allocators.h"
#include "compiler/util/visitor.h"
#include "interpreter/builtins.h"
#include "interpreter/instance.h"
#include "interpreter/profiler.h"
#include "runtime/DispatchTable.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"

#include "llvm/IR/Intrinsics.h"
#include <llvm/IR/Constants.h>

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalObject.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace rir {
namespace pir {

using namespace llvm;

extern "C" size_t R_NSize;
extern "C" size_t R_NodesInUse;

static_assert(sizeof(unsigned long) == sizeof(uint64_t),
              "sizeof(unsigned long) and sizeof(uint64_t) should match");

void LowerFunctionLLVM::PhiBuilder::addInput(llvm::Value* v) {
    addInput(v, builder.GetInsertBlock());
}
llvm::Value* LowerFunctionLLVM::PhiBuilder::operator()(size_t numInputs) {
    assert(!created);
    created = true;
    if (numInputs == 0) {
        numInputs = inputs.size();
        if (numInputs == 1)
            return inputs[0].first;
        assert(builder.GetInsertBlock()->hasNPredecessors(numInputs));
    }
    assert(numInputs > 0);
    phi_ = builder.CreatePHI(type, inputs.size());
    return phi_;
}

class NativeAllocator : public SSAAllocator {
  public:
    NativeAllocator(Code* code, const LivenessIntervals& livenessIntervals)
        : SSAAllocator(code, livenessIntervals) {}

    bool needsAVariable(Value* v) const {
        return Instruction::Cast(v) && !LdArg::Cast(v) && !v->type.isVoid() &&
               !v->type.isVirtualValue() && !v->type.isCompositeValue();
    }
    bool needsASlot(Value* v) const override final {
        return needsAVariable(v) && Rep::Of(v) == Rep::SEXP;
    }
    bool interfere(Instruction* a, Instruction* b) const override final {
        // Ensure we preserve slots for variables with typefeedback to make them
        // accessible to the runtime profiler.
        // TODO: this needs to be replaced by proper mapping of slots.
        if (RuntimeProfiler::enabled() && a != b &&
            (a->typeFeedback().feedbackOrigin.pc() ||
             b->typeFeedback().feedbackOrigin.pc()))
            return true;
        return SSAAllocator::interfere(a, b);
    }
};

llvm::Value* LowerFunctionLLVM::globalConst(llvm::Constant* init,
                                            llvm::Type* ty) {
    if (!ty)
        ty = init->getType();
    return new llvm::GlobalVariable(getModule(), ty, true,
                                    llvm::GlobalValue::PrivateLinkage, init);
}

llvm::FunctionCallee
LowerFunctionLLVM::getBuiltin(const rir::pir::NativeBuiltin& b) {
    return getModule().getOrInsertFunction(b.name, b.llvmSignature);
}

llvm::Value* LowerFunctionLLVM::convertToPointer(const void* what,
                                                 llvm::Type* ty,
                                                 bool constant) {
    assert(what);
    char name[21];
    sprintf(name, "ept_%lx", (uintptr_t)what);
    return getModule().getOrInsertGlobal(name, ty, [&]() {
        return new llvm::GlobalVariable(
            getModule(), ty, constant,
            llvm::GlobalValue::LinkageTypes::AvailableExternallyLinkage,
            nullptr, name, nullptr,
            llvm::GlobalValue::ThreadLocalMode::NotThreadLocal, 0, true);
    });
}

llvm::FunctionCallee
LowerFunctionLLVM::convertToFunction(const void* what, llvm::FunctionType* ty) {
    assert(what);
    char name[21];
    sprintf(name, "efn_%lx", (uintptr_t)what);
    return getModule().getOrInsertFunction(name, ty);
}

void LowerFunctionLLVM::setVisible(int i) {
    builder.CreateStore(c(i), convertToPointer(&R_Visible, t::Int));
}

llvm::Value* LowerFunctionLLVM::force(Instruction* i, llvm::Value* arg) {

    auto isProm = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto needsEval = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto isVal = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto isPromVal = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

    auto res = phiBuilder(t::SEXP);

    checkIsSexp(arg, "force argument");

    auto type = sexptype(arg);
    auto tt = builder.CreateICmpEQ(type, c(PROMSXP));

    builder.CreateCondBr(tt, isProm, isVal);

    builder.SetInsertPoint(isProm);
    auto val = promsxpValue(arg);
    checkIsSexp(arg, "prval");
    auto tv = builder.CreateICmpEQ(val, constant(R_UnboundValue, t::SEXP));
    builder.CreateCondBr(tv, needsEval, isPromVal, branchMostlyFalse);

    builder.SetInsertPoint(needsEval);
    auto evaled =
        call(NativeBuiltins::get(NativeBuiltins::Id::forcePromise), {arg});
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

void LowerFunctionLLVM::insn_assert(llvm::Value* v, const char* msg,
                                    llvm::Value* p) {
    auto nok = BasicBlock::Create(PirJitLLVM::getContext(), "assertFail", fun);
    auto ok = BasicBlock::Create(PirJitLLVM::getContext(), "assertOk", fun);

    builder.CreateCondBr(v, ok, nok, branchAlwaysTrue);

    builder.SetInsertPoint(nok);
    if (p)
        call(NativeBuiltins::get(NativeBuiltins::Id::printValue), {p});
    call(NativeBuiltins::get(NativeBuiltins::Id::assertFail),
         {convertToPointer((void*)msg, t::i8, true)});

    builder.CreateUnreachable();
    builder.SetInsertPoint(ok);


}

llvm::Value* LowerFunctionLLVM::constant(SEXP co, llvm::Type* needed) {
    return constant(co, Rep(needed));
}

llvm::Value* LowerFunctionLLVM::constant(SEXP co, const Rep& needed) {
    if (needed == Rep::i32) {
        assert(Rf_length(co) == 1);
        if (TYPEOF(co) == INTSXP)
            return llvm::ConstantInt::get(PirJitLLVM::getContext(),
                                          llvm::APInt(32, INTEGER(co)[0]));
        if (TYPEOF(co) == REALSXP) {
            if (std::isnan(REAL(co)[0]))
                return llvm::ConstantInt::get(PirJitLLVM::getContext(),
                                              llvm::APInt(32, NA_INTEGER));
            return llvm::ConstantInt::get(PirJitLLVM::getContext(),
                                          llvm::APInt(32, (int)REAL(co)[0]));
        }
        if (TYPEOF(co) == LGLSXP)
            return llvm::ConstantInt::get(PirJitLLVM::getContext(),
                                          llvm::APInt(32, LOGICAL(co)[0]));
    }

    if (needed == Rep::f64) {
        assert(Rf_length(co) == 1);
        if (TYPEOF(co) == INTSXP) {
            if (INTEGER(co)[0] == NA_INTEGER)
                return llvm::ConstantFP::get(PirJitLLVM::getContext(),
                                             llvm::APFloat(R_NaN));
            return llvm::ConstantFP::get(PirJitLLVM::getContext(),
                                         llvm::APFloat((double)INTEGER(co)[0]));
        }
        if (TYPEOF(co) == REALSXP)
            return llvm::ConstantFP::get(PirJitLLVM::getContext(),
                                         llvm::APFloat(REAL(co)[0]));
        if (TYPEOF(co) == LGLSXP) {
            if (LOGICAL(co)[0] == NA_LOGICAL)
                return llvm::ConstantFP::get(PirJitLLVM::getContext(),
                                             llvm::APFloat(R_NaN));
            return llvm::ConstantInt::get(PirJitLLVM::getContext(),
                                          llvm::APInt(32, LOGICAL(co)[0]));
        }
    }

    assert(needed == Rep::SEXP);
    // Normalize scalar logicals
    if (IS_SIMPLE_SCALAR(co, LGLSXP)) {
        auto t = LOGICAL(co)[0];
        if (t == 0) {
            co = R_FalseValue;
        } else if (t == NA_LOGICAL) {
            co = R_LogicalNAValue;
        } else {
            co = R_TrueValue;
        }
    }

    static std::unordered_set<SEXP> eternal = {R_GlobalEnv, R_BaseEnv,
                                               R_BaseNamespace};
    if (TYPEOF(co) == SYMSXP || eternal.count(co))
        return convertToPointer(co);

    static std::unordered_set<SEXP> eternalConst = {
        R_TrueValue,  R_NilValue,       R_FalseValue, R_UnboundValue,
        R_MissingArg, R_LogicalNAValue, R_EmptyEnv};
    if (TYPEOF(co) == BUILTINSXP || TYPEOF(co) == SPECIALSXP ||
        eternalConst.count(co))
        return convertToPointer(co, true);

    auto i = Pool::insert(co);
    llvm::Value* pos = builder.CreateLoad(constantpool);
    pos = builder.CreateBitCast(dataPtr(pos, false),
                                PointerType::get(t::SEXP, 0));
    pos = builder.CreateGEP(pos, c(i));
    return builder.CreateLoad(pos);
}

llvm::Value* LowerFunctionLLVM::nodestackPtr() {
    return builder.CreateLoad(nodestackPtrAddr);
}

llvm::Value* LowerFunctionLLVM::stack(int i) {
    auto offset = -(i + 1);
    auto pos = builder.CreateGEP(nodestackPtr(), {c(offset), c(2)});
    return builder.CreateLoad(t::SEXP, pos);
}

void LowerFunctionLLVM::stack(const std::vector<llvm::Value*>& args) {
    auto stackptr = nodestackPtr();
    // set type tag to 0
    builder.CreateMemSet(builder.CreateGEP(stackptr, c(-args.size())), c(0, 8),
                         args.size() * sizeof(R_bcstack_t),
                         MaybeAlign(alignof(R_bcstack_t)));
    auto pos = -args.size();
    for (auto arg = args.begin(); arg != args.end(); arg++) {
        // store the value
        auto valS = builder.CreateGEP(stackptr, {c(pos), c(2)});
        builder.CreateStore(*arg, valS);
        pos++;
    }
    assert(pos == 0);
}

void LowerFunctionLLVM::setLocal(size_t i, llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto pos = builder.CreateGEP(basepointer, {c(i), c(2)});
    builder.CreateStore(v, pos, true);
}

void LowerFunctionLLVM::incStack(int i, bool zero) {
    if (i == 0)
        return;
    auto cur = nodestackPtr();
    auto offset = sizeof(R_bcstack_t) * i;
    if (zero)
        builder.CreateMemSet(cur, c(0, 8), offset,
                             MaybeAlign(alignof(R_bcstack_t)));
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
    if (supportsFastBuiltinCall(builtin, args.size())) {
        return withCallFrame(args, [&]() -> llvm::Value* {
            return call(NativeBuiltins::get(NativeBuiltins::Id::callBuiltin),
                        {
                            paramCode(),
                            c(srcIdx),
                            constant(builtin, t::SEXP),
                            env,
                            c(args.size()),
                        });
        });
    }

    auto f = convertToFunction((void*)builtinFun, t::builtinFunction);

    std::stack<llvm::Value*> loadedArgs;
    auto n = numTemps;
    for (auto v : args)
        loadedArgs.push(loadSxp(v));

    auto arglist = constant(R_NilValue, t::SEXP);
    for (auto v = args.rbegin(); v != args.rend(); v++) {
        auto a = loadedArgs.top();
        loadedArgs.pop();
#ifdef ENABLE_SLOWASSERT
        insn_assert(builder.CreateICmpNE(sexptype(a), c(PROMSXP)),
                    "passing promise to builtin");
#endif
        arglist =
            call(NativeBuiltins::get(NativeBuiltins::Id::consNr), {a, arglist});
    }
    // Once we protect the list, the individual elements do not need protection
    // anymory, thus we can reset the numTemps.
    numTemps = n;
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
        jitArgs.push_back(load(arg, Rep::SEXP));
    stack(jitArgs);
    auto res = theCall();
    if (pop)
        decStack(nargs);
    return res;
}

llvm::Value* LowerFunctionLLVM::load(Value* v, Rep r) {
    return load(v, v->type, r);
}

llvm::Value* LowerFunctionLLVM::load(Value* v) {
    return load(v, v->type, Rep::Of(v));
}
llvm::Value* LowerFunctionLLVM::loadSxp(Value* v) { return load(v, Rep::SEXP); }

llvm::Value* LowerFunctionLLVM::load(Value* val, PirType type, Rep needed) {
    llvm::Value* res;
    auto vali = Instruction::Cast(val);

    if (auto ct = CastType::Cast(val)) {
        if (Const::Cast(ct->arg(0).val())) {
            return load(ct->arg(0).val(), type, needed);
        }
    }

    if (auto a = LdArg::Cast(val)) {
        res = argument(a->pos);
    } else if (vali && variables_.count(vali)) {
        res = getVariable(vali);
    } else if (val == Env::elided()) {
        res = constant(R_NilValue, needed);
    } else if (auto e = Env::Cast(val)) {
        if (e == Env::notClosed()) {
            res = closxpEnv(paramClosure());
        } else if (e == Env::nil()) {
            res = constant(R_NilValue, needed);
        } else if (Env::isStaticEnv(e)) {
            res = constant(e->rho, t::SEXP);
        } else {
            assert(false);
        }
    } else if (val->asRValue()) {
        res = constant(val->asRValue(), needed);
    } else if (val == OpaqueTrue::instance()) {
        static int one = 1;
        // Something that is always true, but llvm does not know about
        res = builder.CreateLoad(convertToPointer(&one, t::Int, true));
    } else if (auto ld = Const::Cast(val)) {
        res = constant(ld->c(), needed);
    } else if (val->tag == Tag::DeoptReason) {
        auto dr = (DeoptReasonWrapper*)val;
        auto srcAddr = (Constant*)builder.CreateIntToPtr(
            llvm::ConstantInt::get(
                PirJitLLVM::getContext(),
                llvm::APInt(64,
                            reinterpret_cast<uint64_t>(dr->reason.srcCode()),
                            false)),
            t::voidPtr);
        auto drs = llvm::ConstantStruct::get(
            t::DeoptReason, {c(dr->reason.reason, 32),
                             c(dr->reason.origin.offset(), 32), srcAddr});
        res = globalConst(drs);
    } else {
        val->printRef(std::cerr);
        if (auto i = Instruction::Cast(val))
            i->printRecursive(std::cerr, 1);
        else
            val->printRef(std::cerr);
        std::cerr << "\nCould not be loaded in:\n";
        code->printCode(std::cerr, 1, 0);
        assert(false);
    }

    if (res->getType() == t::SEXP && needed != Rep::SEXP) {
        if (type.isA(PirType::simpleScalarInt())) {
            res = unboxInt(res);
            assert(res->getType() == t::Int);
        } else if (type.isA(PirType::simpleScalarLogical())) {
            res = unboxLgl(res);
            assert(res->getType() == t::Int);
        } else if (type.isA((PirType() | RType::integer | RType::logical)
                                .simpleScalar())) {
            res = unboxIntLgl(res);
            assert(res->getType() == t::Int);
        } else if (type.isA(PirType::simpleScalarReal())) {
            res = unboxReal(res);
            assert(res->getType() == t::Double);
        } else if (type.isA(
                       (PirType(RType::real) | RType::integer | RType::logical)
                           .simpleScalar())) {
            res = unboxRealIntLgl(res, type);
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

    if (res->getType() == t::Int && needed == Rep::f64) {
        // TODO should we deal with na here?
        res = builder.CreateSIToFP(res, t::Double);
    } else if (res->getType() == t::Double && needed == Rep::i32) {
        // TODO should we deal with na here?
        res = builder.CreateFPToSI(res, t::Int);
    } else if ((res->getType() == t::Int || res->getType() == t::Double) &&
               needed == Rep::SEXP) {
        if (type.isA(PirType() | RType::integer)) {
            res = boxInt(res);
        } else if (type.isA(PirType::test())) {
            res = boxTst(res);
        } else if (type.isA(PirType() | RType::logical)) {
            res = boxLgl(res);
        } else if (type.isA(PirType() | RType::real)) {
            res = boxReal(res);
        } else {
            std::cout << "Failed to convert int/float to " << type << "\n";
            Instruction::Cast(val)->print(std::cout);
            std::cout << "\n";
            code->printCode(std::cout, true, true);
            assert(false);
        }
    }

    if (needed.toLlvm() != res->getType()) {
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
    BasicBlock* hit1 = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    BasicBlock* hit = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

    auto representation = Rep::Of(index);
    llvm::Value* nativeIndex = load(index);

    if (representation == Rep::SEXP) {
        if (Rep::Of(index->type) == Rep::i32) {
            nativeIndex = unboxInt(nativeIndex);
            representation = Rep::i32;
        } else {
            nativeIndex = unboxRealIntLgl(nativeIndex, index->type);
            representation = Rep::f64;
        }
    }

    if (representation == Rep::f64) {
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
        assert(representation == Rep::i32);
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
    auto res = popc->result();

    builder.CreateStore(load(res, Rep::Of(i)), data.result);
    builder.CreateBr(data.popContextTarget);

    builder.SetInsertPoint(data.popContextTarget);
    llvm::Value* ret = builder.CreateLoad(data.result);
    llvm::Value* boxedRet = ret;
    auto storeType = data.result->getType()->getPointerElementType();
    if (storeType != t::SEXP) {
        if (i->type.isA(PirType::test())) {
            boxedRet = boxTst(ret);
        } else if (i->type.isA(RType::logical)) {
            boxedRet = boxLgl(ret);
        } else if (i->type.isA(RType::integer)) {
            boxedRet = boxInt(ret, false);
        } else if (i->type.isA(RType::real)) {
            boxedRet = boxReal(ret, false);
        } else {
            assert(false);
        }
    }
    call(NativeBuiltins::get(NativeBuiltins::Id::endClosureContext),
         {data.rcntxt, boxedRet});
    inPushContext--;
    setVal(i, Rep::Of(i) == Rep::SEXP ? boxedRet : ret);
}

void LowerFunctionLLVM::compilePushContext(Instruction* i) {
    auto ct = PushContext::Cast(i);
    auto ast = loadSxp(ct->ast());
    auto op = loadSxp(ct->op());
    auto sysparent = loadSxp(ct->env());

    inPushContext++;

    // initialize a RCNTXT on the stack
    auto& data = contexts[i];

    std::vector<Value*> arglist;
    for (size_t i = 0; i < ct->narglist(); ++i) {
        arglist.push_back(ct->arg(i).val());
    }

    auto callId = ArglistOrder::NOT_REORDERED;
    if (ct->isReordered())
        callId = pushArgReordering(ct->getArgOrderOrig());

    withCallFrame(arglist,
                  [&]() -> llvm::Value* {
                      return call(NativeBuiltins::get(
                                      NativeBuiltins::Id::initClosureContext),
                                  {c(callId), paramCode(), ast, data.rcntxt,
                                   sysparent, op, c(ct->narglist())});
                  },
                  false);

    // Create a copy of all live variables to be able to restart
    // SEXPs are stored as local vars, primitive values are placed in an
    // alloca'd buffer
    std::vector<std::pair<Instruction*, Variable>> savedLocals;
    {
        for (auto& v : variables_) {
            auto& var = v.second;
            if (!var.initialized)
                continue;
            auto j = v.first;
            if (liveness.live(i, j)) {
                if (Rep::Of(j) == Rep::SEXP) {
                    savedLocals.push_back({j, Variable::MutableRVariable(
                                                  j, data.savedSexpPos.at(j),
                                                  builder, basepointer)});
                } else {
                    savedLocals.push_back(
                        {j,
                         Variable::Mutable(j, topAlloca(Rep::Of(j).toLlvm()))});
                }
            }
        }
        for (auto& v : savedLocals)
            v.second.set(builder, getVariable(v.first));
    }

    // Do a setjmp
    auto didLongjmp = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto cont = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    {
        auto setjmp = NativeBuiltins::get(NativeBuiltins::Id::sigsetjmp);
#ifdef __APPLE__
        auto setjmpBuf = builder.CreateGEP(data.rcntxt, {c(0), c(2), c(0)});
#else
        auto setjmpBuf = builder.CreateGEP(data.rcntxt, {c(0), c(2)});
#endif
        auto longjmp =
            builder.CreateCall(getBuiltin(setjmp), {setjmpBuf, c(0)});

        builder.CreateCondBr(builder.CreateICmpEQ(longjmp, c(0)), cont,
                             didLongjmp);
    }

    // Handle incoming longjumps
    {
        builder.SetInsertPoint(didLongjmp);
        llvm::Value* returned = builder.CreateLoad(
            builder.CreateIntToPtr(c((void*)&R_ReturnedValue), t::SEXP_ptr));
        auto restart =
            builder.CreateICmpEQ(returned, constant(R_RestartToken, t::SEXP));

        auto longjmpRestart =
            BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
        auto longjmpRet = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
        builder.CreateCondBr(restart, longjmpRestart, longjmpRet);

        // The longjump returned a restart token.
        // In this case we need to restore all local variables as we
        // preserved them before the setjmp and then continue
        // execution
        builder.SetInsertPoint(longjmpRestart);
        for (auto& v : savedLocals) {
            auto loc = v.first;
            auto val = v.second.get(builder);
            if (LLVMDebugInfo() && diVariables_.count(loc)) {
                DIB->insertDbgValueIntrinsic(val, diVariables_[loc],
                                             DIB->createExpression(),
                                             builder.getCurrentDebugLocation(),
                                             builder.GetInsertBlock());
            }
            updateVariable(loc, val);
        }

        // Also clear all binding caches
        for (const auto& be : bindingsCache)
            for (const auto& b : be.second)
                builder.CreateStore(
                    llvm::ConstantPointerNull::get(t::SEXP),
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
            returned = unboxRealIntLgl(returned, PirType::simpleScalarReal());
        }
        builder.CreateStore(returned, data.result);
        if (data.popContextTarget)
            builder.CreateBr(data.popContextTarget);
        else
            builder.CreateUnreachable();
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
    return type.isA(PirType(RType::vec).orFastVecelt()) ||
           type.isA(PirType(RType::integer).orFastVecelt()) ||
           type.isA(PirType(RType::logical).orFastVecelt()) ||
           type.isA(PirType(RType::real).orFastVecelt());
}

llvm::Value* LowerFunctionLLVM::vectorPositionPtr(llvm::Value* vector,
                                                  llvm::Value* position,
                                                  PirType type) {
    assert(vector->getType() == t::SEXP);
    PointerType* nativeType;
    if (type.isA(PirType(RType::integer).orAttribsOrObj().fastVecelt()) ||
        type.isA(PirType(RType::logical).orAttribsOrObj().fastVecelt())) {
        nativeType = t::IntPtr;
    } else if (type.isA(PirType(RType::real).orAttribsOrObj().fastVecelt())) {
        nativeType = t::DoublePtr;
    } else if (type.isA(PirType(RType::vec).orAttribsOrObj().fastVecelt())) {
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
#ifdef ENABLE_SLOWASSERT
    insn_assert(builder.CreateNot(shared(vector)),
                "assigning to shared vector");
#endif
    return builder.CreateStore(value,
                               vectorPositionPtr(vector, position, type));
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
    insn_assert(isSimpleScalar(v, INTSXP), "expected scalar int");
#endif
    auto pos = builder.CreateBitCast(dataPtr(v), t::IntPtr);
    return builder.CreateLoad(pos);
}
llvm::Value* LowerFunctionLLVM::unboxLgl(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
    insn_assert(isSimpleScalar(v, LGLSXP), "expected scalar lgl");
#endif
    auto pos = builder.CreateBitCast(dataPtr(v), t::IntPtr);
    auto unbox = builder.CreateLoad(pos);
    // Normalize the unboxed lgl to 0,1,NA.
    return builder.CreateSelect(
        builder.CreateICmpEQ(unbox, c(0)), c(0),
        builder.CreateSelect(builder.CreateICmpEQ(unbox, c(NA_LOGICAL)),
                             c(NA_LOGICAL), c(1)));
}
llvm::Value* LowerFunctionLLVM::unboxReal(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
    insn_assert(isSimpleScalar(v, REALSXP), "expected scalar real");
#endif
    auto pos = builder.CreateBitCast(dataPtr(v), t::DoublePtr);
    auto res = builder.CreateLoad(pos);
    return res;
}
llvm::Value* LowerFunctionLLVM::unboxRealIntLgl(llvm::Value* v,
                                                PirType toType) {
    assert(v->getType() == t::SEXP);
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto isReal = BasicBlock::Create(PirJitLLVM::getContext(), "isReal", fun);
    auto notReal = BasicBlock::Create(PirJitLLVM::getContext(), "notReal", fun);

    auto res = phiBuilder(t::Double);

    auto type = sexptype(v);
    auto tt = builder.CreateICmpEQ(type, c(REALSXP));
    builder.CreateCondBr(tt, isReal, notReal);

    builder.SetInsertPoint(notReal);

    auto intres = unboxIntLgl(v);

    auto isNaBr = BasicBlock::Create(PirJitLLVM::getContext(), "isNa", fun);
    nacheck(intres, toType, isNaBr);

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
    if ((int)loadedArgs.size() <= i)
        loadedArgs.resize(i + 1);
    if (loadedArgs.at(i))
        return loadedArgs.at(i);

    auto cur = builder.GetInsertBlock();
    builder.SetInsertPoint(entryBlock);

#ifdef ENABLE_SLOWASSERT
    auto tagPos = builder.CreateGEP(paramArgs(), {c(i), c(0)});
    insn_assert(builder.CreateICmpEQ(builder.CreateLoad(tagPos), c(0)),
                "Expected boxed arg");
#endif
    auto pos = builder.CreateGEP(paramArgs(), {c(i), c(2)});
#ifdef ENABLE_SLOWASSERT
    insn_assert(builder.CreateIsNotNull(builder.CreateLoad(pos)), "null arg");
#endif

    loadedArgs.at(i) = builder.CreateLoad(pos);
    entryBlock = builder.GetInsertBlock();
    builder.SetInsertPoint(cur);

    return argument(i);
}

AllocaInst* LowerFunctionLLVM::topAlloca(llvm::Type* t, size_t len) {
    auto cur = builder.GetInsertBlock();
    builder.SetInsertPoint(entryBlock);
    auto res = builder.CreateAlloca(t, 0, c(len));
    builder.SetInsertPoint(cur);
    return res;
}

llvm::Value* LowerFunctionLLVM::convert(llvm::Value* val, PirType toType,
                                        bool protect) {
    auto to = Rep::Of(toType);
    auto from = val->getType();
    if (to.toLlvm() == from)
        return val;

    if (from == t::SEXP && to == Rep::i32)
        return unboxIntLgl(val);
    if (from == t::SEXP && to == Rep::f64)
        return unboxRealIntLgl(val, toType);
    if (from != t::SEXP && to == Rep::SEXP)
        return box(val, toType, protect);

    if (from == t::Int && to == Rep::f64) {
        return builder.CreateSelect(builder.CreateICmpEQ(val, c(NA_INTEGER)),
                                    c(NA_REAL),
                                    builder.CreateSIToFP(val, t::Double));
    }
    if (from == t::Double && to == Rep::i32) {
        return builder.CreateSelect(builder.CreateFCmpUNE(val, val),
                                    c(NA_INTEGER),
                                    builder.CreateFPToSI(val, t::Int));
    }

    std::cout << "\nFailed to convert a " << val->getType() << " to " << toType
              << "\n";
    assert(false);
    return nullptr;
}

void LowerFunctionLLVM::setVal(Instruction* i, llvm::Value* val) {
    val = convert(val, i->type, false);
    if (!val->hasName())
        val->setName(i->getRef());

    if (LLVMDebugInfo() && diVariables_.count(i)) {
        DIB->insertDbgValueIntrinsic(
            val, diVariables_[i], DIB->createExpression(),
            builder.getCurrentDebugLocation(), builder.GetInsertBlock());
    }
    setVariable(i, val, inPushContext && escapesInlineContext.count(i));
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

    insn_assert(
        builder.CreateICmpNE(llvm::ConstantPointerNull::get(t::SEXP), v),
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
    auto sxpinfoPtr = builder.CreateGEP(t::SEXPREC, v, {c(0), c(0), c(0)});
    sxpinfoPtr->setName("sxpinfo");
    return sxpinfoPtr;
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

llvm::Value* LowerFunctionLLVM::isVector(llvm::Value* v) {
    auto t = sexptype(v);
    return builder.CreateOr(
        builder.CreateICmpEQ(t, c(LGLSXP)),
        builder.CreateOr(
            builder.CreateICmpEQ(t, c(INTSXP)),
            builder.CreateOr(
                builder.CreateICmpEQ(t, c(REALSXP)),
                builder.CreateOr(
                    builder.CreateICmpEQ(t, c(CPLXSXP)),
                    builder.CreateOr(
                        builder.CreateICmpEQ(t, c(STRSXP)),
                        builder.CreateOr(
                            builder.CreateICmpEQ(t, c(RAWSXP)),
                            builder.CreateOr(
                                builder.CreateICmpEQ(t, c(VECSXP)),
                                builder.CreateICmpEQ(t, c(EXPRSXP)))))))));
}

llvm::Value* LowerFunctionLLVM::isMatrix(llvm::Value* v) {
    auto res = phiBuilder(t::i1);
    auto isVec = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto notVec = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    builder.CreateCondBr(isVector(v), isVec, notVec);

    builder.SetInsertPoint(isVec);
    auto t = call(NativeBuiltins::get(NativeBuiltins::Id::getAttrb),
                  {v, constant(R_DimSymbol, t::SEXP)});
    res.addInput(
        builder.CreateAnd(builder.CreateICmpEQ(sexptype(t), c(INTSXP)),
                          builder.CreateICmpEQ(vectorLength(t), c(2, 64))));
    builder.CreateBr(done);

    builder.SetInsertPoint(notVec);
    res.addInput(builder.getFalse());
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
    return res();
}

llvm::Value* LowerFunctionLLVM::isArray(llvm::Value* v) {
    auto res = phiBuilder(t::i1);
    auto isVec = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto notVec = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    builder.CreateCondBr(isVector(v), isVec, notVec);

    builder.SetInsertPoint(isVec);
    auto t = call(NativeBuiltins::get(NativeBuiltins::Id::getAttrb),
                  {v, constant(R_DimSymbol, t::SEXP)});
    res.addInput(
        builder.CreateAnd(builder.CreateICmpEQ(sexptype(t), c(INTSXP)),
                          builder.CreateICmpUGT(vectorLength(t), c(0, 64))));
    builder.CreateBr(done);

    builder.SetInsertPoint(notVec);
    res.addInput(builder.getFalse());
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
    return res();
}

llvm::Value* LowerFunctionLLVM::car(llvm::Value* v) {
    v = builder.CreateGEP(v, {c(0), c(4), c(0)});
    return builder.CreateLoad(v);
}

llvm::Value* LowerFunctionLLVM::cdr(llvm::Value* v) {
    v = builder.CreateGEP(v, {c(0), c(4), c(1)});
    return builder.CreateLoad(v);
}

llvm::Value* LowerFunctionLLVM::tag(llvm::Value* v) {
    v = builder.CreateGEP(v, {c(0), c(4), c(2)});
    return builder.CreateLoad(v);
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
    writeBarrier(x, y, fast, [&]() {
        auto skip = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
        auto update = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
        builder.CreateCondBr(builder.CreateICmpNE(car(x), y), update, skip);

        builder.SetInsertPoint(update);
        call(NativeBuiltins::get(NativeBuiltins::Id::setCar), {x, y});
        builder.CreateBr(skip);

        builder.SetInsertPoint(skip);
    });
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
    writeBarrier(x, y, fast, [&]() {
        auto skip = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
        auto update = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
        builder.CreateCondBr(builder.CreateICmpNE(cdr(x), y), update, skip);

        builder.SetInsertPoint(update);
        call(NativeBuiltins::get(NativeBuiltins::Id::setCdr), {x, y});
        builder.CreateBr(skip);

        builder.SetInsertPoint(skip);
    });
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
    writeBarrier(x, y, fast, [&]() {
        call(NativeBuiltins::get(NativeBuiltins::Id::setTag), {x, y});
    });
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

    isScalar = builder.CreateAnd(okType, isScalar);

    auto noAttrib =
        builder.CreateICmpEQ(attr(v), constant(R_NilValue, t::SEXP));

    return builder.CreateAnd(isScalar, noAttrib);
}

llvm::Value* LowerFunctionLLVM::vectorLength(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto pos = builder.CreateBitCast(v, t::VECTOR_SEXPREC_ptr);
    pos = builder.CreateGEP(pos, {c(0), c(4), c(0)});
    return builder.CreateLoad(pos);
}
llvm::Value* LowerFunctionLLVM::isNamed(llvm::Value* v) {
    auto sxpinfoP = builder.CreateBitCast(sxpinfoPtr(v), t::i64ptr);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1) << 32;
    auto named = builder.CreateAnd(sxpinfo, c(namedMask));
    return builder.CreateICmpNE(named, c(0, 64));
}
void LowerFunctionLLVM::assertNamed(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = builder.CreateBitCast(sxpinfoPtr(v), t::i64ptr);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1) << 32;
    auto named = builder.CreateAnd(sxpinfo, c(namedMask));
    auto isNotNamed = builder.CreateICmpEQ(named, c(0, 64));

    auto notNamed =
        BasicBlock::Create(PirJitLLVM::getContext(), "notNamed", fun);
    auto ok = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

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

llvm::Value* LowerFunctionLLVM::cloneIfShared(llvm::Value* v) {
    auto s = shared(v);
    return createSelect2(
        s,
        [&]() {
            return call(
                NativeBuiltins::get(NativeBuiltins::Id::shallowDuplicate), {v});
        },
        [&]() { return v; });
}

void LowerFunctionLLVM::ensureNamedIfNeeded(Instruction* i, llvm::Value* val) {
    if (Rep::Of(i) == Rep::SEXP && variables_.count(i) &&
        variables_.at(i).initialized) {

        auto adjust = refcount.atCreation.find(i);
        if (adjust != refcount.atCreation.end()) {
            if (adjust->second == NeedsRefcountAdjustment::SetShared) {
                if (!val)
                    val = load(i);
                ensureShared(val);
            } else if (adjust->second == NeedsRefcountAdjustment::EnsureNamed) {
                if (!val)
                    val = load(i);
                ensureNamed(val);
            }
        }
    }
}

void LowerFunctionLLVM::ensureNamed(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = builder.CreateBitCast(sxpinfoPtr(v), t::i64ptr);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1) << 32;
    unsigned long namedLSB = 1ul << 32;

    auto named = builder.CreateAnd(sxpinfo, c(namedMask));
    auto isNotNamed = builder.CreateICmpEQ(named, c(0, 64));

    auto notNamed =
        BasicBlock::Create(PirJitLLVM::getContext(), "notNamed", fun);
    auto ok = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

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

    auto incrementBr = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

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

    auto incrementBr = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

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

void LowerFunctionLLVM::nacheck(llvm::Value* v, PirType type, BasicBlock* isNa,
                                BasicBlock* notNa) {
    assert(type.isA(PirType::num().scalar()));
    if (!notNa)
        notNa = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    llvm::Value* isNotNa;
    if (!type.maybeNAOrNaN()) {
        // Don't actually check NA
        isNotNa = builder.getTrue();
    } else if (v->getType() == t::Double) {
        isNotNa = builder.CreateFCmpUEQ(v, v);
    } else {
        assert(v->getType() == t::Int);
        isNotNa = builder.CreateICmpNE(v, c(NA_INTEGER));
    }
    builder.CreateCondBr(isNotNa, notNa, isNa, branchMostlyTrue);
    builder.SetInsertPoint(notNa);
}

llvm::Value* LowerFunctionLLVM::checkDoubleToInt(llvm::Value* ld,
                                                 const PirType& type) {
    if (type.isA(RType::integer))
        return builder.getTrue();

    assert(INT_MIN == NA_INTEGER); // used for lower limit
    auto lower = c((double)INT_MIN);
    auto upper = c((double)INT_MAX + 1);
    auto gt = type.maybeNAOrNaN() ? builder.CreateFCmpUGT(ld, lower)
                                  : builder.CreateFCmpOGT(ld, lower);
    auto lt = type.maybeNAOrNaN() ? builder.CreateFCmpULT(ld, upper)
                                  : builder.CreateFCmpOLT(ld, upper);
    auto inrange = builder.CreateAnd(lt, gt);
    auto conv = createSelect2(inrange,
                              [&]() {
                                  // converting to signed int is not undefined
                                  // here since we first check that it does not
                                  // overflow
                                  auto conv = builder.CreateFPToSI(ld, t::Int);
                                  conv = builder.CreateSIToFP(conv, t::Double);
                                  return builder.CreateFCmpOEQ(ld, conv);
                              },
                              [&]() { return builder.getFalse(); });
    return conv;
}

void LowerFunctionLLVM::checkMissing(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto ok = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto nok = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto t = builder.CreateICmpEQ(v, constant(R_MissingArg, t::SEXP));
    builder.CreateCondBr(t, nok, ok, branchAlwaysFalse);

    builder.SetInsertPoint(nok);
    auto msg =
        builder.CreateGlobalString("argument is missing, with no default");
    call(NativeBuiltins::get(NativeBuiltins::Id::error),
         {builder.CreateInBoundsGEP(msg, {c(0), c(0)})});
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
}

void LowerFunctionLLVM::checkUnbound(llvm::Value* v) {
    auto ok = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto nok = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto t = builder.CreateICmpEQ(v, constant(R_UnboundValue, t::SEXP));
    builder.CreateCondBr(t, nok, ok, branchAlwaysFalse);

    builder.SetInsertPoint(nok);
    auto msg = builder.CreateGlobalString("object not found");
    call(NativeBuiltins::get(NativeBuiltins::Id::error),
         {builder.CreateInBoundsGEP(msg, {c(0), c(0)})});
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
}

llvm::Value* LowerFunctionLLVM::container(llvm::Value* v) {
    auto casted = builder.CreatePtrToInt(v, t::i64);
    auto container = builder.CreateSub(casted, c(sizeof(VECTOR_SEXPREC)));
    return builder.CreateIntToPtr(container, t::SEXP);
}

llvm::CallInst* LowerFunctionLLVM::call(const NativeBuiltin& builtin,
                                        const std::vector<llvm::Value*>& args) {
    return builder.CreateCall(getBuiltin(builtin), args);
}

llvm::Value* LowerFunctionLLVM::box(llvm::Value* v, PirType t, bool protect) {
    llvm::Value* res = nullptr;
    if (t.isA(PirType(RType::integer).notObject()))
        res = boxInt(v, protect);
    if (t.isA(PirType(RType::logical).notObject()))
        res = boxLgl(v);
    if (t.isA(PirType(RType::real).notObject()))
        res = boxReal(v, protect);
    assert(res);
    return res;
}
llvm::Value* LowerFunctionLLVM::boxInt(llvm::Value* v, bool protect) {
    llvm::Value* res = nullptr;
    if (v->getType() == t::Int) {
        // std::ostringstream dbg;
        // (*currentInstr)->printRecursive(dbg, 2);
        // auto l = new std::string;
        // l->append(dbg.str());
        // return call(NativeBuiltins::get(NativeBuiltins::Id::newIntDebug),
        //             {v, c((unsigned long)l->data())});
        res = call(NativeBuiltins::get(NativeBuiltins::Id::newInt), {v});
    } else {
        assert(v->getType() == t::Double);
        res =
            call(NativeBuiltins::get(NativeBuiltins::Id::newIntFromReal), {v});
    }
    if (protect)
        protectTemp(res);
    return res;
}
llvm::Value* LowerFunctionLLVM::boxReal(llvm::Value* v, bool protect) {
    llvm::Value* res = nullptr;
    if (v->getType() == t::Double) {
        res = call(NativeBuiltins::get(NativeBuiltins::Id::newReal), {v});
    } else {
        assert(v->getType() == t::Int);
        res =
            call(NativeBuiltins::get(NativeBuiltins::Id::newRealFromInt), {v});
    }
    if (protect)
        protectTemp(res);
    return res;
}
llvm::Value* LowerFunctionLLVM::boxLgl(llvm::Value* v) {
    if (v->getType() == t::Int) {
        insn_assert(
            builder.CreateOr(
                builder.CreateICmpEQ(v, c(0)),
                builder.CreateOr(builder.CreateICmpEQ(v, c(1)),
                                 builder.CreateICmpEQ(v, c(NA_LOGICAL)))),
            "expected unboxed logical to be 0,1, or NA");
        return builder.CreateSelect(
            builder.CreateICmpEQ(v, c(0)), constant(R_FalseValue, t::SEXP),
            builder.CreateSelect(builder.CreateICmpEQ(v, c(NA_LOGICAL)),
                                 constant(R_LogicalNAValue, t::SEXP),
                                 constant(R_TrueValue, t::SEXP)));
    }
    assert(v->getType() == t::Double);
    return builder.CreateSelect(
        builder.CreateFCmpUEQ(v, c(0.0)), constant(R_FalseValue, t::SEXP),
        builder.CreateSelect(builder.CreateFCmpUNE(v, v),
                             constant(R_LogicalNAValue, t::SEXP),
                             constant(R_TrueValue, t::SEXP)));
}
llvm::Value* LowerFunctionLLVM::boxTst(llvm::Value* v) {
    assert(v->getType() == t::Int);
    return builder.CreateSelect(builder.CreateICmpNE(v, c(0)),
                                constant(R_TrueValue, t::SEXP),
                                constant(R_FalseValue, t::SEXP));
}

void LowerFunctionLLVM::protectTemp(llvm::Value* val) {
    setLocal(numLocals + numTemps, val);
    numTemps++;
    if (numTemps > maxTemps)
        maxTemps = numTemps;
}

llvm::Value* LowerFunctionLLVM::depromise(
    llvm::Value* arg, const PirType& t,
    const std::function<void(llvm::Value*)>& extraPromiseCase,
    const std::function<void()>& nonPromiseCase) {

    if (!t.maybePromiseWrapped()) {
#ifdef ENABLE_SLOWASSERT
        insn_assert(builder.CreateICmpNE(sexptype(arg), c(PROMSXP)),
                    "Expected no promise");
#endif
        return arg;
    }
    auto isProm = BasicBlock::Create(PirJitLLVM::getContext(), "isProm", fun);
    auto isVal = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto ok = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

    auto res = phiBuilder(t::SEXP);

    auto type = sexptype(arg);
    auto tt = builder.CreateICmpEQ(type, c(PROMSXP));
    builder.CreateCondBr(tt, isProm, isVal, branchMostlyFalse);

    builder.SetInsertPoint(isProm);
    auto val = promsxpValue(arg);
    extraPromiseCase(val);
    res.addInput(val);
    builder.CreateBr(ok);

    builder.SetInsertPoint(isVal);
#ifdef ENABLE_SLOWASSERT
    insn_assert(builder.CreateICmpNE(sexptype(arg), c(PROMSXP)),
                "Depromise returned promise");
#endif
    nonPromiseCase();
    res.addInput(arg);
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
    return res();
}

llvm::Value* LowerFunctionLLVM::depromise(Value* v) {
    if (!v->type.maybePromiseWrapped())
        return loadSxp(v);
    assert(Rep::Of(v) == Rep::SEXP);
    return depromise(loadSxp(v), v->type);
}

void LowerFunctionLLVM::compileRelop(
    Instruction* i,
    const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& intInsert,
    const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& fpInsert,
    BinopKind kind, bool testNa) {
    auto rep = Rep::Of(i);
    auto lhs = i->arg(0).val();
    auto rhs = i->arg(1).val();
    auto lhsRep = Rep::Of(lhs);
    auto rhsRep = Rep::Of(rhs);
    if (lhsRep == Rep::SEXP || rhsRep == Rep::SEXP) {
        auto a = loadSxp(lhs);
        auto b = loadSxp(rhs);

        llvm::Value* res;
        if (i->hasEnv()) {
            auto e = loadSxp(i->env());
            res = call(NativeBuiltins::get(NativeBuiltins::Id::binopEnv),
                       {a, b, e, c(i->srcIdx), c((int)kind)});
        } else {
            res = call(NativeBuiltins::get(NativeBuiltins::Id::binop),
                       {a, b, c((int)kind)});
        }
        setVal(i, res);
        return;
    }

    BasicBlock* isNaBr = nullptr;
    if (testNa)
        isNaBr = BasicBlock::Create(PirJitLLVM::getContext(), "isNa", fun);
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

    auto res = phiBuilder(t::Int);
    auto a = load(lhs, lhsRep);
    auto b = load(rhs, rhsRep);

    if (testNa) {
        nacheck(a, lhs->type, isNaBr);
        nacheck(b, rhs->type, isNaBr);
    }

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

    if (testNa) {
        builder.SetInsertPoint(isNaBr);
        res.addInput(c(NA_INTEGER));
        builder.CreateBr(done);
    }

    builder.SetInsertPoint(done);
    if (rep == Rep::SEXP) {
        setVal(i, boxLgl(res()));
    } else {
        setVal(i, res());
    }
};

void LowerFunctionLLVM::compileBinop(
    Instruction* i, Value* lhs, Value* rhs,
    const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& intInsert,
    const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& fpInsert,
    BinopKind kind) {
    auto rep = Rep::Of(i);
    auto lhsRep = Rep::Of(lhs);
    auto rhsRep = Rep::Of(rhs);

    if (lhsRep == Rep::SEXP || rhsRep == Rep::SEXP ||
        (!fpInsert && (lhsRep != Rep::i32 || rhsRep != Rep::i32))) {
        auto a = loadSxp(lhs);
        auto b = loadSxp(rhs);

        llvm::Value* res = nullptr;
        if (i->hasEnv()) {
            auto e = loadSxp(i->env());
            res = call(NativeBuiltins::get(NativeBuiltins::Id::binopEnv),
                       {a, b, e, c(i->srcIdx), c((int)kind)});
        } else {
            res = call(NativeBuiltins::get(NativeBuiltins::Id::binop),
                       {a, b, c((int)kind)});
        }

        setVal(i, res);
        return;
    }

    BasicBlock* isNaBr = nullptr;
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

    auto r = (lhsRep == Rep::f64 || rhsRep == Rep::f64) ? t::Double : t::Int;

    auto res = phiBuilder(r);
    auto a = load(lhs, lhsRep);
    auto b = load(rhs, rhsRep);

    auto checkNa = [&](llvm::Value* llvmValue, PirType type, Rep r) {
        if (type.maybeNAOrNaN()) {
            if (r == Rep::i32) {
                if (!isNaBr)
                    isNaBr = BasicBlock::Create(PirJitLLVM::getContext(),
                                                "isNa", fun);
                nacheck(llvmValue, type, isNaBr);
            }
        }
    };
    checkNa(a, lhs->type, lhsRep);
    checkNa(b, rhs->type, rhsRep);

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

    if (lhsRep == Rep::i32 || rhsRep == Rep::i32) {
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
    if (rep == Rep::SEXP) {
        setVal(i, box(res(), lhs->type.mergeWithConversion(rhs->type), false));
    } else {
        setVal(i, res());
    }
};

void LowerFunctionLLVM::compileUnop(
    Instruction* i, Value* arg,
    const std::function<llvm::Value*(llvm::Value*)>& intInsert,
    const std::function<llvm::Value*(llvm::Value*)>& fpInsert, UnopKind kind) {
    auto argRep = Rep::Of(arg);

    if (argRep == Rep::SEXP) {
        auto a = loadSxp(arg);

        llvm::Value* res = nullptr;
        if (i->hasEnv()) {
            auto e = loadSxp(i->env());
            res = call(NativeBuiltins::get(NativeBuiltins::Id::unopEnv),
                       {a, e, c(i->srcIdx), c((int)kind)});
        } else {
            res = call(NativeBuiltins::get(NativeBuiltins::Id::unop),
                       {a, c((int)kind)});
        }

        setVal(i, res);
        return;
    }

    BasicBlock* isNaBr = nullptr;
    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

    auto r = (argRep == Rep::f64) ? t::Double : t::Int;

    auto res = phiBuilder(r);
    auto a = load(arg, argRep);

    auto checkNa = [&](llvm::Value* value, PirType type, Rep r) {
        if (type.maybeNAOrNaN()) {
            if (r == Rep::i32) {
                if (!isNaBr)
                    isNaBr = BasicBlock::Create(PirJitLLVM::getContext(),
                                                "isNa", fun);
                nacheck(value, type, isNaBr);
            }
        }
    };
    checkNa(a, arg->type, argRep);

    if (a->getType() == t::Int) {
        res.addInput(intInsert(a));
    } else {
        res.addInput(fpInsert(a));
    }
    builder.CreateBr(done);

    if (argRep == Rep::i32) {
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
    auto theRes = res();
    if (Rep::Of(i) == Rep::SEXP)
        theRes = box(theRes, arg->type);
    setVal(i, theRes);
};

void LowerFunctionLLVM::writeBarrier(llvm::Value* x, llvm::Value* y,
                                     std::function<void()> no,
                                     std::function<void()> yes) {
    auto sxpinfoX = builder.CreateLoad(sxpinfoPtr(x));

    auto markBitPos = c((unsigned long)(1ul << (TYPE_BITS + 19)));
    auto genBitPos = c((unsigned long)(1ul << (TYPE_BITS + 23)));

    auto done = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto noBarrier = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto maybeNeedsBarrier =
        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto maybeNeedsBarrier2 =
        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto needsBarrier = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

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
            args.push_back(exp);
            newNames.push_back(Pool::insert(symbol::expandDotsTrigger));
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
    Context asmpt = calli->inferAvailableAssumptions();
    auto namesConst = c(newNames);
    auto namesStore = globalConst(namesConst);

    auto callId = ArglistOrder::NOT_REORDERED;
    if (calli->isReordered())
        callId = pushArgReordering(calli->getArgOrderOrig());

    setVal(i, withCallFrame(
                  args,
                  [&]() -> llvm::Value* {
                      return call(
                          NativeBuiltins::get(NativeBuiltins::Id::dotsCall),
                          {
                              c(callId),
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

llvm::Value* LowerFunctionLLVM::loadPromise(llvm::Value* x, int i) {
    assert(x->getType() != t::SEXP);
    auto code = builder.CreatePtrToInt(x, t::i64);
    auto extraPos = builder.CreateAdd(code, c(rir::Code::extraPtrOffset(), 64));
    auto extraPtr = builder.CreateIntToPtr(extraPos, t::SEXP_ptr);
    auto extra = builder.CreateLoad(extraPtr);
    return accessVector(extra, c(i), PirType(RType::vec).notObject());
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
    builder.CreateStore(c(0, 8), pos);
}

void LowerFunctionLLVM::envStubSetMissing(llvm::Value* x, int i) {
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
            call(NativeBuiltins::get(NativeBuiltins::Id::externalsxpSetEntry),
                 {{x, c(i + LazyEnvironment::ArgOffset), y}});
        });
    if (setNotMissing) {
        auto le = builder.CreateBitCast(
            dataPtr(x, false), PointerType::get(t::LazyEnvironment, 0));
        auto missingBits =
            builder.CreateBitCast(builder.CreateGEP(le, c(1)), t::i8ptr);
        auto pos = builder.CreateGEP(missingBits, c(i));
        builder.CreateStore(c(0, 8), pos);
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
    auto ok = builder.CreateAnd(builder.CreateNot(isObj(v)), isNil);
    return createSelect2(
        ok, [&]() { return builder.getTrue(); },
        [&]() {
            auto isMatr1 = builder.CreateICmpEQ(tag(attrs),
                                                constant(R_DimSymbol, t::SEXP));
            auto isMatr2 =
                builder.CreateICmpEQ(cdr(attrs), constant(R_NilValue, t::SEXP));
            return builder.CreateAnd(isMatr1, isMatr2);
        });
};

llvm::Value* LowerFunctionLLVM::isAltrep(llvm::Value* v) {
    checkIsSexp(v, "in is altrep");
    auto sxpinfo = builder.CreateLoad(sxpinfoPtr(v));
    return builder.CreateICmpNE(
        c(0, 64),
        builder.CreateAnd(sxpinfo, c((unsigned long)(1ul << (TYPE_BITS + 2)))));
};

llvm::Value* LowerFunctionLLVM::createSelect2(
    llvm::Value* cond, std::function<llvm::Value*()> trueValueAction,
    std::function<llvm::Value*()> falseValueAction) {

    auto intialInsertionPoint = builder.GetInsertBlock();

    auto trueBranch = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto truePred = intialInsertionPoint;
    builder.SetInsertPoint(trueBranch);
    auto trueValue = trueValueAction();
    auto trueBranchIsEmpty = trueBranch->empty();
    if (trueBranchIsEmpty) {
        trueBranch->removeFromParent();
        delete trueBranch;
    } else {
        truePred = builder.GetInsertBlock();
    }

    auto falseBranch = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    auto falsePred = intialInsertionPoint;
    builder.SetInsertPoint(falseBranch);
    auto falseValue = falseValueAction();
    auto falseBranchIsEmpty = falseBranch->empty();
    if (falseBranchIsEmpty) {
        falseBranch->removeFromParent();
        delete falseBranch;
    } else {
        falsePred = builder.GetInsertBlock();
    }

    if (trueBranchIsEmpty && falseBranchIsEmpty) {
        builder.SetInsertPoint(intialInsertionPoint);
        return builder.CreateSelect(cond, trueValue, falseValue);
    }

    auto next = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

    PhiBuilder res(builder, trueValue->getType());

    auto trueBranchForCond = trueBranch;
    builder.SetInsertPoint(truePred);
    if (!trueBranchIsEmpty)
        builder.CreateBr(next);
    else
        trueBranchForCond = next;
    res.addInput(trueValue);

    auto falseBranchForCond = falseBranch;
    builder.SetInsertPoint(falsePred);
    if (!falseBranchIsEmpty)
        builder.CreateBr(next);
    else
        falseBranchForCond = next;
    res.addInput(falseValue);

    builder.SetInsertPoint(intialInsertionPoint);
    builder.CreateCondBr(cond, trueBranchForCond, falseBranchForCond);

    builder.SetInsertPoint(next);
    auto r = res();

    return r;
};

void LowerFunctionLLVM::compile() {

    {
        auto arg = fun->arg_begin();
        for (size_t i = 0; i < argNames.size(); ++i) {
            args.push_back(arg);
            args.back()->setName(argNames[i]);
            arg++;
        }
    }

    std::vector<BasicBlock*> exitBlocks;

    std::unordered_map<BB*, BasicBlock*> blockMapping_;
    auto getBlock = [&](BB* bb) {
        auto b = blockMapping_.find(bb);
        if (b != blockMapping_.end()) {
            return b->second;
        }
        std::stringstream ss;
        ss << "BB" << bb->id;
        return blockMapping_[bb] =
                   BasicBlock::Create(PirJitLLVM::getContext(), ss.str(), fun);
    };
    entryBlock = BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
    builder.SetInsertPoint(entryBlock);

    if (LLVMDebugInfo()) {
        DI->emitLocation(builder, 0);
        std::array<llvm::DIType*, 4> argDITypes = {
            DI->VoidPtrType, DI->VoidPtrType, DI->SEXPType, DI->SEXPType};
        auto arg = fun->arg_begin();
        for (size_t i = 0; i < argNames.size(); ++i) {
            auto store =
                builder.CreateAlloca(arg->getType(), nullptr, argNames[i]);
            builder.CreateStore(arg, store);
            DILocalVariable* D =
                DIB->createParameterVariable(DI->getScope(), argNames[i], i + 1,
                                             DI->File, 1, argDITypes[i], true);
            DIB->insertDeclare(store, D, DIB->createExpression(),
                               builder.getCurrentDebugLocation(),
                               builder.GetInsertBlock());
            arg++;
        }
    }

    nodestackPtrAddr = convertToPointer(&R_BCNodeStackTop, t::stackCellPtr);
    basepointer = nodestackPtr();

    size_t additionalStackSlots = 0;
    if (RuntimeProfiler::enabled()) {
        // Store the code object as the first element of our frame, for the
        // value profiler to find it.
        incStack(1, false);
        stack({container(paramCode())});
        additionalStackSlots++;
    }
    {
        SmallSet<std::pair<Value*, SEXP>> bindings;
        Visitor::run(code->entry, [&](Instruction* i) {
            SEXP varName = nullptr;
            if (auto l = LdVar::Cast(i))
                varName = l->varName;
            else if (auto l = StVar::Cast(i))
                varName = l->varName;
            else if (LdDots::Cast(i))
                varName = R_DotsSymbol;

            if (varName) {
                auto e = MkEnv::Cast(i->env());
                if ((e && !e->stub) ||
                    (LdFunctionEnv::Cast(i->env()) && cls->isContinuation())) {
                    bindings.insert(std::pair<Value*, SEXP>(i->env(), varName));
                }
            }
        });
        size_t idx = 0;
        for (auto& b : bindings) {
            bindingsCache[b.first][b.second] = idx++;
        }
        bindingsCacheBase = topAlloca(t::SEXP, idx);
    }

    for (auto b : bindingsCache)
        if (LdFunctionEnv::Cast(b.first))
            for (auto b : b.second)
                builder.CreateStore(
                    llvm::ConstantPointerNull::get(t::SEXP),
                    builder.CreateGEP(bindingsCacheBase, c(b.second)));

    std::unordered_map<Instruction*, Instruction*> phis;
    {
        NativeAllocator allocator(code, liveness);
        allocator.compute();
        allocator.verify();
        auto numLocalsBase = numLocals;
        numLocals += allocator.slots();

        auto createVariable = [&](Instruction* i, bool mut) -> void {
            if (Rep::Of(i) == Rep::SEXP) {
                if (mut)
                    variables_[i] = Variable::MutableRVariable(
                        i, allocator[i] + numLocalsBase, builder, basepointer);
                else
                    variables_[i] = Variable::RVariable(
                        i, allocator[i] + numLocalsBase, builder, basepointer);
            } else {
                if (mut)
                    variables_[i] =
                        Variable::Mutable(i, topAlloca(Rep::Of(i).toLlvm()));
                else
                    variables_[i] = Variable::Immutable(i);
            }
        };

        constantpool = builder.CreateIntToPtr(c(globalContext()), t::SEXP_ptr);
        constantpool = builder.CreateGEP(constantpool, c(1));

        Visitor::run(code->entry, [&](BB* bb) {
            for (auto i : *bb) {
                if (!liveness.count(i) || !allocator.needsAVariable(i))
                    continue;
                if (auto phi = Phi::Cast(i)) {
                    createVariable(phi, true);
                    phi->eachArg([&](BB*, Value* v) {
                        auto i = Instruction::Cast(v);
                        assert(i);
                        phis[i] = phi;
                    });
                }
            }
        });

        std::unordered_map<PushContext*, PopContext*> contextResTy;
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto pop = PopContext::Cast(i)) {
                auto push = pop->push();
                contextResTy[push] = pop;
            }
        });
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto push = PushContext::Cast(i)) {
                auto popI = contextResTy.find(push);
                PopContext* pop = nullptr;
                if (popI != contextResTy.end())
                    pop = popI->second;
                Rep resRep = Rep::SEXP;
                if (pop)
                    resRep = Rep::Of(pop);
                auto resStore = topAlloca(resRep.toLlvm());
                auto rcntxt = topAlloca(t::RCNTXT);
                contexts[push] = {
                    rcntxt, resStore,
                    pop ? BasicBlock::Create(PirJitLLVM::getContext(), "", fun)
                        : nullptr};

                // Everything which is live at the Push context needs to be
                // mutable, to be able to restore on restart
                Visitor::run(code->entry, [&](Instruction* j) {
                    if (allocator.needsAVariable(j)) {
                        if (Rep::Of(j) == Rep::SEXP && liveness.live(push, j)) {
                            contexts[push].savedSexpPos[j] = numLocals++;
                        }
                        if (!liveness.live(push, j) && pop &&
                            liveness.live(pop, j))
                            escapesInlineContext.insert(j);
                        if (!variables_.count(j) &&
                            (liveness.live(push, j) ||
                             (pop && liveness.live(pop, j))))
                            createVariable(j, true);
                    }
                });
            }
        });
        Visitor::run(code->entry, [&](Instruction* i) {
            if (allocator.needsAVariable(i) && liveness.count(i) &&
                !variables_.count(i))
                createVariable(i, false);
        });
    }

    std::unordered_map<BB*, int> blockInPushContext;
    blockInPushContext[code->entry] = 0;

    LoweringVisitor::run(code->entry, [&](BB* bb) {
        currentBB = bb;

        builder.SetInsertPoint(getBlock(bb));
        inPushContext = blockInPushContext.at(bb);

        if (LLVMDebugInfo()) {
            DI->emitLocation(builder, DI->getBBLoc(bb));
        }

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            currentInstr = it;
            auto i = *it;

            if (LLVMDebugInfo()) {
                DI->emitLocation(builder, DI->getInstLoc(i));

                if (variables_.count(i)) {
                    auto makeName = [](Instruction* i) {
                        std::stringstream n;
                        auto id = i->id();
                        char sep = '_';
                        n << "pir" << sep << id.bb() << sep << id.idx();
                        return n.str();
                    };
                    auto& v = variables_[i];
                    auto store = v.slot;
                    auto rep = Rep::Of(i);
                    auto ditype =
                        (rep == Rep::SEXP
                             ? DI->SEXPType
                             : (rep == Rep::i32
                                    ? DI->IntType
                                    : (rep == Rep::f64 ? DI->DoubleType
                                                       : DI->UnspecifiedType)));
                    assert(diVariables_.count(i) == 0);
                    diVariables_[i] = DIB->createAutoVariable(
                        DI->getScope(), makeName(i), DI->File,
                        builder.getCurrentDebugLocation().getLine(), ditype,
                        true);
                    if (store) {
                        if (rep == Rep::SEXP)
                            DIB->insertDeclare(
                                store, diVariables_[i], DIB->createExpression(),
                                builder.getCurrentDebugLocation(),
                                builder.GetInsertBlock());
                        else
                            DIB->insertDbgValueIntrinsic(
                                store, diVariables_[i], DIB->createExpression(),
                                builder.getCurrentDebugLocation(),
                                builder.GetInsertBlock());
                    }
                }
            }

            auto adjustRefcount = refcount.beforeUse.find(i);
            if (adjustRefcount != refcount.beforeUse.end()) {
                i->eachArg([&](Value* v) {
                    if (Rep::Of(v) != Rep::SEXP)
                        return;
                    if (auto j = Instruction::Cast(v->followCasts())) {
                        auto needed = adjustRefcount->second.find(j);
                        if (needed != adjustRefcount->second.end()) {
                            auto kind = needed->second;
                            if (kind == NeedsRefcountAdjustment::SetShared)
                                ensureShared(load(v));
                            else if (kind ==
                                     NeedsRefcountAdjustment::EnsureNamed)
                                ensureNamed(load(v));
                        }
                    }
                });
            }

            switch (i->tag) {
            case Tag::ExpandDots: {
                auto in = i->arg(0).val();
                if (!deadMove(in, i))
                    setVal(i, load(i->arg(0).val()));
                break;
            }

            case Tag::DotsList: {
                auto mk = DotsList::Cast(i);
                auto arglist = constant(R_NilValue, t::SEXP);
                if (mk->nargs()) {
                    std::stack<llvm::Value*> argsLoaded;
                    mk->eachElement([&](SEXP name, Value* v) {
                        argsLoaded.push(loadSxp(v));
                    });
                    mk->eachElementRev([&](SEXP name, Value* v) {
                        auto val = argsLoaded.top();
                        argsLoaded.pop();
                        incrementNamed(val);
                        arglist = call(
                            NativeBuiltins::get(NativeBuiltins::Id::consNr),
                            {val, arglist});
                        setTag(arglist, constant(name, t::SEXP), false);
                    });
                    setSexptype(arglist, DOTSXP);
                }
                setVal(i, arglist);
                break;
            }

            case Tag::PushContext: {
                compilePushContext(i);
                break;
            }

            case Tag::PopContext: {
                compilePopContext(i);
                break;
            }

            case Tag::CastType: {
                auto in = i->arg(0).val();
                if (!variables_.count(i))
                    break;
                setVal(i, load(in, i->type, Rep::Of(i)));
                break;
            }

            case Tag::PirCopy: {
                auto in = i->arg(0).val();
                if (!deadMove(in, i))
                    setVal(i, load(in, Rep::Of(i)));
                break;
            }

            case Tag::LdArg:
                // handled in load
                break;

            case Tag::LdFunctionEnv:
                setVal(i, paramEnv());
                break;

            case Tag::Invisible:
                setVisible(0);
                break;

            case Tag::Visible:
                setVisible(1);
                break;

            case Tag::Identical: {
                auto a = i->arg(0).val();
                auto b = i->arg(1).val();

                auto rep = Rep::Of(a) < Rep::Of(b) ? Rep::Of(b) : Rep::Of(a);
                auto ai = load(a, rep);
                auto bi = load(b, rep);
                if (Rep::Of(a) == Rep::SEXP && a->type.maybePromiseWrapped())
                    ai = depromise(ai, a->type);
                if (Rep::Of(b) == Rep::SEXP && b->type.maybePromiseWrapped())
                    bi = depromise(bi, b->type);

                // Not needed so far. Needs some care to ensure NA == NA holds
                assert(ai->getType() != t::Double &&
                       ai->getType() != t::Double);

                auto res = builder.CreateICmpEQ(ai, bi);

                if (rep == Rep::SEXP) {
                    if (a->type.maybeLazy()) {
                        res = builder.CreateAnd(
                            res, builder.CreateICmpNE(
                                     ai, constant(R_UnboundValue, Rep::SEXP)));
                    } else if (b->type.maybeLazy()) {
                        res = builder.CreateAnd(
                            res, builder.CreateICmpNE(
                                     bi, constant(R_UnboundValue, Rep::SEXP)));
                    }
                }

                // TODO: is it okay to ignore RType::builtin and RType::special
                // here?
                if (a->type.maybe(RType::closure) ||
                    b->type.maybe(RType::closure)) {
                    res = createSelect2(
                        res, [&]() { return builder.getTrue(); },
                        [&]() {
                            auto cls = builder.CreateAnd(
                                builder.CreateICmpEQ(sexptype(ai), c(CLOSXP)),
                                builder.CreateICmpEQ(sexptype(bi), c(CLOSXP)));
                            return createSelect2(
                                cls,
                                [&]() {
                                    return call(NativeBuiltins::get(
                                                    NativeBuiltins::Id::clsEq),
                                                {ai, bi});
                                },
                                [&]() { return builder.getFalse(); });
                        });
                }
                setVal(i, builder.CreateZExt(res, t::Int));
                break;
            }

            case Tag::CallSafeBuiltin: {
                auto b = CallSafeBuiltin::Cast(i);
                if (compileDotcall(
                        b, [&]() { return constant(b->builtinSexp, t::SEXP); },
                        [&](size_t i) { return R_NilValue; })) {
                    break;
                }
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });

                auto callTheBuiltin = [&]() -> llvm::Value* {
                    // Some "safe" builtins still look up functions in the base
                    // env
                    return callRBuiltin(b->builtinSexp, args, i->srcIdx,
                                        b->builtin,
                                        constant(R_BaseEnv, t::SEXP));
                };

                auto fixVisibility = [&]() {
                    if (!b->effects.contains(Effect::Visibility))
                        return;
                    int flag = getFlag(b->builtinId);
                    if (flag < 2)
                        setVisible(flag != 1);
                };

                // TODO: this should probably go somewhere else... This is
                // an inlined version of bitwise builtins
                if (Rep::Of(b) == Rep::i32) {
                    if (b->nargs() == 2) {
                        auto x = b->arg(0).val();
                        auto y = b->arg(1).val();
                        auto xRep = Rep::Of(x);
                        auto yRep = Rep::Of(y);

                        static auto bitwise = {
                            blt("bitwiseShiftL"), blt("bitwiseShiftR"),
                            blt("bitwiseAnd"),    blt("bitwiseOr"),
                            blt("bitwiseXor"),
                        };
                        auto found = std::find(bitwise.begin(), bitwise.end(),
                                               b->builtinId);
                        if (found != bitwise.end()) {
                            const static PirType num =
                                (PirType() | RType::integer | RType::logical |
                                 RType::real)
                                    .simpleScalar();

                            if (xRep == Rep::SEXP && x->type.isA(num))
                                xRep = Rep::f64;
                            if (yRep == Rep::SEXP && y->type.isA(num))
                                yRep = Rep::f64;

                            if (xRep != Rep::SEXP && yRep != Rep::SEXP) {

                                BasicBlock* isNaBr = nullptr;
                                auto done = BasicBlock::Create(
                                    PirJitLLVM::getContext(), "", fun);

                                auto res = phiBuilder(t::Int);

                                auto xInt = load(x, Rep::i32);
                                auto yInt = load(y, Rep::i32);

                                auto naCheck = [&](Value* v, llvm::Value* asInt,
                                                   Rep rep) {
                                    if (v->type.maybeNAOrNaN()) {
                                        if (rep == Rep::f64) {
                                            auto vv = load(v, rep);
                                            if (!isNaBr)
                                                isNaBr = BasicBlock::Create(
                                                    PirJitLLVM::getContext(),
                                                    "isNa", fun);
                                            nacheck(vv, v->type, isNaBr);
                                        } else {
                                            assert(rep == Rep::i32);
                                            if (!isNaBr)
                                                isNaBr = BasicBlock::Create(
                                                    PirJitLLVM::getContext(),
                                                    "isNa", fun);
                                            nacheck(asInt, v->type, isNaBr);
                                        }
                                    }
                                };
                                naCheck(x, xInt, xRep);
                                naCheck(y, yInt, yRep);

                                switch (found - bitwise.begin()) {
                                case 0: {
                                    if (!isNaBr)
                                        isNaBr = BasicBlock::Create(
                                            PirJitLLVM::getContext(), "isNa",
                                            fun);
                                    auto ok = BasicBlock::Create(
                                        PirJitLLVM::getContext(), "", fun);
                                    auto ofl =
                                        builder.CreateICmpSLT(yInt, c(0));
                                    builder.CreateCondBr(ofl, isNaBr, ok,
                                                         branchMostlyFalse);
                                    builder.SetInsertPoint(ok);

                                    ok = BasicBlock::Create(
                                        PirJitLLVM::getContext(), "", fun);
                                    ofl = builder.CreateICmpSGT(yInt, c(31));
                                    builder.CreateCondBr(ofl, isNaBr, ok,
                                                         branchMostlyFalse);
                                    builder.SetInsertPoint(ok);

                                    res.addInput(builder.CreateShl(xInt, yInt));
                                    break;
                                }
                                case 1: {
                                    if (!isNaBr)
                                        isNaBr = BasicBlock::Create(
                                            PirJitLLVM::getContext(), "isNa",
                                            fun);
                                    auto ok = BasicBlock::Create(
                                        PirJitLLVM::getContext(), "", fun);
                                    auto ofl =
                                        builder.CreateICmpSLT(yInt, c(0));
                                    builder.CreateCondBr(ofl, isNaBr, ok,
                                                         branchMostlyFalse);
                                    builder.SetInsertPoint(ok);

                                    ok = BasicBlock::Create(
                                        PirJitLLVM::getContext(), "", fun);
                                    ofl = builder.CreateICmpSGT(yInt, c(31));
                                    builder.CreateCondBr(ofl, isNaBr, ok,
                                                         branchMostlyFalse);
                                    builder.SetInsertPoint(ok);

                                    res.addInput(
                                        builder.CreateLShr(xInt, yInt));
                                    break;
                                }
                                case 2: {
                                    res.addInput(builder.CreateAnd(xInt, yInt));
                                    break;
                                }
                                case 3: {
                                    res.addInput(builder.CreateOr(xInt, yInt));
                                    break;
                                }
                                case 4: {
                                    res.addInput(builder.CreateXor(xInt, yInt));
                                    break;
                                }
                                }

                                builder.CreateBr(done);

                                if (isNaBr) {
                                    builder.SetInsertPoint(isNaBr);
                                    res.addInput(c(NA_INTEGER));
                                    builder.CreateBr(done);
                                }

                                builder.SetInsertPoint(done);
                                setVal(i, res());
                                fixVisibility();
                                break;
                            }
                        }
                    }
                }

                if (b->nargs() == 1) {
                    auto a = load(b->callArg(0).val());
                    auto irep = Rep::Of(b->arg(0).val());
                    auto itype = b->callArg(0).val()->type;
                    auto orep = Rep::Of(i);
                    bool done = true;

                    auto doTypetest = [&](int type) {
                        if (irep == Rep::SEXP) {
                            setVal(i, builder.CreateSelect(
                                          builder.CreateICmpEQ(sexptype(a),
                                                               c(type)),
                                          constant(R_TrueValue, orep),
                                          constant(R_FalseValue, orep)));

                        } else {
                            setVal(i, constant(R_FalseValue, orep));
                        }
                    };

                    switch (b->builtinId) {
                    case blt("dim"): {
                        if (!i->arg(0).val()->type.maybeObj()) {
                            if (irep == Rep::SEXP) {
                                setVal(
                                    i,
                                    call(NativeBuiltins::get(
                                             NativeBuiltins::Id::getAttrb),
                                         {a, constant(R_DimSymbol, t::SEXP)}));
                            } else {
                                setVal(i, constant(R_NilValue, orep));
                            }
                        } else {
                            done = false;
                        }
                        break;
                    }
                    case blt("length"):
                        if (irep == Rep::SEXP &&
                            !b->callArg(0).val()->type.isA(
                                PirType::anySimpleScalar())) {
                            auto callLengthBuiltin = [&]() {
                                return call(NativeBuiltins::get(
                                                NativeBuiltins::Id::xlength),
                                            {a});
                            };
                            llvm::Value* r;
                            if (vectorTypeSupport(b->callArg(0).val())) {
                                r = createSelect2(
                                    isAltrep(a), callLengthBuiltin,
                                    [&]() { return vectorLength(a); });
                            } else {
                                r = callLengthBuiltin();
                            }
                            if (orep == Rep::SEXP) {
                                r = createSelect2(
                                    builder.CreateICmpUGT(r, c(INT_MAX, 64)),
                                    [&]() {
                                        return boxReal(
                                            builder.CreateUIToFP(r, t::Double));
                                    },
                                    [&]() {
                                        return boxInt(
                                            builder.CreateTrunc(r, t::Int));
                                    });

                            } else if (orep == Rep::f64) {
                                r = builder.CreateUIToFP(r, t::Double);
                            } else {
                                assert(orep == Rep::i32);
                                r = builder.CreateTrunc(r, t::Int);
                            }
                            setVal(i, r);
                        } else {
                            setVal(i, constant(ScalarInteger(1), orep));
                        }
                        break;
                    case blt("names"): {
                        if (Rep::Of(b->callArg(0).val()) != Rep::SEXP) {
                            setVal(i, constant(R_NilValue, Rep::SEXP));
                        } else if (itype.isA(
                                       PirType::vecs().orAttribsOrObj())) {
                            if (!itype.maybeNotFastVecelt() ||
                                !itype.maybeHasAttrs()) {
                                setVal(i, constant(R_NilValue, t::SEXP));
                            } else {
                                auto res = phiBuilder(t::SEXP);
                                auto done = BasicBlock::Create(
                                    PirJitLLVM::getContext(), "", fun);
                                auto hasAttr = BasicBlock::Create(
                                    PirJitLLVM::getContext(), "", fun);
                                auto noAttr = BasicBlock::Create(
                                    PirJitLLVM::getContext(), "", fun);
                                auto mightHaveNames = builder.CreateICmpNE(
                                    attr(a), constant(R_NilValue, t::SEXP));
                                if (itype.maybeObj())
                                    mightHaveNames = builder.CreateOr(
                                        mightHaveNames, isObj(a));
                                builder.CreateCondBr(mightHaveNames, hasAttr,
                                                     noAttr);

                                builder.SetInsertPoint(hasAttr);
                                res.addInput(callTheBuiltin());
                                builder.CreateBr(done);

                                builder.SetInsertPoint(noAttr);
                                res.addInput(constant(R_NilValue, t::SEXP));
                                builder.CreateBr(done);

                                builder.SetInsertPoint(done);
                                setVal(i, res());
                            }
                        } else {
                            done = false;
                        }
                        break;
                    }
                    case blt("abs"): {
                        if (irep == orep && irep == Rep::i32) {
                            setVal(i, builder.CreateSelect(
                                          builder.CreateICmpSGE(a, c(0)), a,
                                          builder.CreateNeg(a)));

                        } else if (irep == orep && irep == Rep::f64) {
                            setVal(i, builder.CreateSelect(
                                          builder.CreateFCmpUGE(a, c(0.0)), a,
                                          builder.CreateFNeg(a)));

                        } else {
                            done = false;
                        }
                        break;
                    }
                    case blt("sqrt"): {
                        if (orep == Rep::f64 && irep == Rep::i32) {
                            a = convert(a, i->type);
                            setVal(i, builder.CreateIntrinsic(
                                          Intrinsic::sqrt, {t::Double}, {a}));
                        } else if (orep == irep && irep == Rep::f64) {
                            setVal(i, builder.CreateIntrinsic(
                                          Intrinsic::sqrt, {t::Double}, {a}));
                        } else if (irep == Rep::SEXP &&
                                   i->arg(0).val()->type.isA(
                                       PirType(RType::real))) {
                            auto l = vectorLength(a);
                            auto res = call(NativeBuiltins::get(
                                                NativeBuiltins::Id::makeVector),
                                            {c(REALSXP), l});

                            auto idx = phiBuilder(t::i64);
                            idx.addInput(c(0, 64));

                            auto loopH = BasicBlock::Create(
                                PirJitLLVM::getContext(), "sqrt-loop-hd", fun);
                            auto loopB = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);
                            auto loopE = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);

                            builder.CreateBr(loopH);
                            builder.SetInsertPoint(loopH);

                            auto idxPhi = idx(2);
                            builder.CreateCondBr(
                                builder.CreateICmpEQ(idxPhi, l), loopE, loopB);
                            builder.SetInsertPoint(loopB);

                            assignVector(
                                res, idxPhi,
                                builder.CreateIntrinsic(
                                    Intrinsic::sqrt, {t::Double},
                                    {accessVector(a, idxPhi,
                                                  i->arg(0).val()->type)}),
                                i->type);
                            idx.addInput(builder.CreateAdd(idxPhi, c(1, 64)));
                            builder.CreateBr(loopH);

                            builder.SetInsertPoint(loopE);
                            setVal(i, res);
                        } else {
                            done = false;
                        }
                        break;
                    }
                    case blt("sum"):
                    case blt("prod"): {
                        if (irep == Rep::i32 || irep == Rep::f64) {
                            setVal(i, box(a, itype));
                        } else if (orep == Rep::f64 || orep == Rep::i32) {
                            assert(irep == Rep::SEXP);
                            auto itype = b->callArg(0).val()->type;
                            if (itype.isA(PirType::intReal())) {
                                auto trg = b->builtinId == blt("sum")
                                               ? NativeBuiltins::get(
                                                     NativeBuiltins::Id::sumr)
                                               : NativeBuiltins::get(
                                                     NativeBuiltins::Id::prodr);
                                llvm::Value* res = call(trg, {a});
                                if (orep == Rep::i32)
                                    res = convert(res, i->type);
                                setVal(i, res);
                            } else {
                                done = false;
                            }
                        } else {
                            done = false;
                        }
                        break;
                    }
                    case blt("as.logical"):
                        if (irep == Rep::i32 && orep == Rep::i32) {
                            setVal(i,
                                   builder.CreateSelect(
                                       builder.CreateICmpEQ(a, c(NA_INTEGER)),
                                       constant(R_LogicalNAValue, orep),
                                       builder.CreateSelect(
                                           builder.CreateICmpEQ(a, c(0)),
                                           constant(R_FalseValue, orep),
                                           constant(R_TrueValue, orep))));

                        } else if (irep == Rep::f64 &&
                                   (orep == Rep::i32 || orep == Rep::f64)) {

                            setVal(i, builder.CreateSelect(
                                          builder.CreateFCmpUNE(a, a),
                                          constant(R_LogicalNAValue, orep),
                                          builder.CreateSelect(
                                              builder.CreateFCmpUEQ(a, c(0.0)),
                                              constant(R_FalseValue, orep),
                                              constant(R_TrueValue, orep))));

                        } else {
                            done = false;
                        }
                        break;
                    case blt("as.integer"):
                        if (irep == Rep::i32 && orep == Rep::i32) {
                            setVal(i, a);
                        } else if (irep == Rep::f64 && orep == Rep::i32) {
                            setVal(i, builder.CreateSelect(
                                          builder.CreateFCmpUNE(a, a),
                                          c(NA_INTEGER),
                                          builder.CreateFPToSI(a, t::Int)));

                        } else if (irep == Rep::f64 && orep == Rep::f64) {

                            setVal(i, createSelect2(
                                          builder.CreateFCmpUNE(a, a),
                                          [&]() { return a; },
                                          [&]() {
                                              return builder.CreateIntrinsic(
                                                  Intrinsic::floor,
                                                  {a->getType()}, {a});
                                          }));

                        } else if (irep == Rep::SEXP) {
                            auto isSimpleInt = builder.CreateAnd(
                                builder.CreateICmpEQ(
                                    attr(a), constant(R_NilValue, t::SEXP)),
                                builder.CreateICmpEQ(sexptype(a), c(INTSXP)));

                            setVal(i, createSelect2(
                                          isSimpleInt,
                                          [&]() { return convert(a, i->type); },
                                          [&]() {
                                              return convert(callTheBuiltin(),
                                                             i->type);
                                          }));

                        } else {
                            done = false;
                        }
                        break;
                    case blt("as.character"):
                        if (irep == Rep::SEXP) {
                            setVal(i, createSelect2(
                                          builder.CreateAnd(
                                              builder.CreateICmpEQ(
                                                  attr(a), constant(R_NilValue,
                                                                    t::SEXP)),
                                              builder.CreateICmpEQ(sexptype(a),
                                                                   c(STRSXP))),
                                          [&]() { return a; },
                                          [&]() { return callTheBuiltin(); }));
                        } else {
                            done = false;
                        }
                        break;
                    case blt("as.vector"):
                        if (irep == Rep::SEXP) {
                            setVal(i,
                                   createSelect2(
                                       builder.CreateAnd(
                                           builder.CreateNot(isObj(a)),
                                           isVector(a)),
                                       [&]() {
                                           return builder.CreateSelect(
                                               builder.CreateICmpEQ(
                                                   vectorLength(a), c(0, 64)),
                                               constant(R_NilValue, orep), a);
                                       },
                                       [&]() { return callTheBuiltin(); }));
                        } else {
                            setVal(i, box(a, itype));
                        }
                        break;
                    case blt("is.logical"):
                        if (b->arg(0).val()->type.isA(RType::logical)) {
                            // ensure that logicals represented as ints are
                            // handled.
                            setVal(i, constant(R_TrueValue, orep));
                        } else {
                            doTypetest(LGLSXP);
                        }
                        break;
                    case blt("is.complex"):
                        doTypetest(CPLXSXP);
                        break;
                    case blt("is.character"):
                        doTypetest(STRSXP);
                        break;
                    case blt("is.symbol"):
                        doTypetest(SYMSXP);
                        break;
                    case blt("is.expression"):
                        doTypetest(EXPRSXP);
                        break;
                    case blt("is.call"):
                        doTypetest(LANGSXP);
                        break;
                    case blt("is.function"): {
                        if (irep == Rep::SEXP) {
                            auto t = sexptype(a);
                            auto is = builder.CreateOr(
                                builder.CreateICmpEQ(t, c(CLOSXP)),
                                builder.CreateOr(
                                    builder.CreateICmpEQ(t, c(BUILTINSXP)),
                                    builder.CreateICmpEQ(t, c(SPECIALSXP))));
                            setVal(i, builder.CreateSelect(
                                          is, constant(R_TrueValue, orep),
                                          constant(R_FalseValue, orep)));

                        } else {
                            setVal(i, constant(R_FalseValue, orep));
                        }
                        break;
                    }
                    case blt("anyNA"):
                    case blt("is.na"):
                        if (irep == Rep::i32) {
                            setVal(i,
                                   builder.CreateSelect(
                                       builder.CreateICmpEQ(a, c(NA_INTEGER)),
                                       constant(R_TrueValue, orep),
                                       constant(R_FalseValue, orep)));
                        } else if (irep == Rep::f64) {
                            setVal(i, builder.CreateSelect(
                                          builder.CreateFCmpUNE(a, a),
                                          constant(R_TrueValue, orep),
                                          constant(R_FalseValue, orep)));
                        } else {
                            done = false;
                        }
                        break;
                    case blt("is.object"):
                        if (irep == Rep::SEXP) {
                            setVal(i, builder.CreateSelect(
                                          isObj(a), constant(R_TrueValue, orep),
                                          constant(R_FalseValue, orep)));
                        } else {
                            setVal(i, constant(R_FalseValue, orep));
                        }
                        break;
                    case blt("is.array"):
                        if (irep == Rep::SEXP) {
                            setVal(i,
                                   builder.CreateSelect(
                                       isArray(a), constant(R_TrueValue, orep),
                                       constant(R_FalseValue, orep)));
                        } else {
                            setVal(i, constant(R_FalseValue, orep));
                        }
                        break;
                    case blt("is.atomic"):
                        if (irep == Rep::SEXP) {
                            auto t = sexptype(a);
                            auto isatomic = builder.CreateOr(
                                builder.CreateICmpEQ(t, c(NILSXP)),
                                builder.CreateOr(
                                    builder.CreateICmpEQ(t, c(CHARSXP)),
                                    builder.CreateOr(
                                        builder.CreateICmpEQ(t, c(LGLSXP)),
                                        builder.CreateOr(
                                            builder.CreateICmpEQ(t, c(INTSXP)),
                                            builder.CreateOr(
                                                builder.CreateICmpEQ(
                                                    t, c(REALSXP)),
                                                builder.CreateOr(
                                                    builder.CreateICmpEQ(
                                                        t, c(CPLXSXP)),
                                                    builder.CreateOr(
                                                        builder.CreateICmpEQ(
                                                            t, c(STRSXP)),
                                                        builder.CreateICmpEQ(
                                                            t,
                                                            c(RAWSXP)))))))));
                            setVal(i, builder.CreateSelect(
                                          isatomic, constant(R_TrueValue, orep),

                                          constant(R_FalseValue, orep)));
                        } else {
                            setVal(i, constant(R_TrueValue, orep));
                        }
                        break;
                    case blt("bodyCode"): {
                        assert(irep == Rep::SEXP && orep == irep);
                        llvm::Value* res = nullptr;
                        if (i->arg(0).val()->type.isA(PirType::function())) {
                            res = closxpBody(a);
                        } else {
                            res = createSelect2(
                                builder.CreateICmpEQ(c(CLOSXP), sexptype(a)),
                                [&]() { return closxpBody(a); },
                                [&]() {
                                    return constant(R_NilValue, t::SEXP);
                                });
                        }
                        setVal(i, res);
                        break;
                    }
                    case blt("environment"):
                        if (!i->arg(0).val()->type.isA(PirType::function())) {
                            done = false;
                            break;
                        }
                        assert(irep == Rep::SEXP && orep == irep);
                        setVal(i, closxpEnv(a));
                        break;
                    default:
                        done = false;
                    };
                    if (done) {
                        fixVisibility();
                        break;
                    }
                }

                if (b->nargs() == 2) {
                    bool fastcase = false;
                    auto arep = Rep::Of(b->arg(0).val());
                    auto brep = Rep::Of(b->arg(1).val());
                    auto orep = Rep::Of(b);
                    auto aval = load(b->arg(0).val());
                    auto bval = load(b->arg(1).val());

                    switch (b->builtinId) {
                    case blt("vector"): {
                        auto l = b->arg(1).val();
                        if (l->type.isA(PirType::anySimpleScalar())) {
                            if (auto con = Const::Cast(b->arg(0).val())) {
                                if (TYPEOF(con->c()) == STRSXP &&
                                    XLENGTH(con->c()) == 1) {
                                    SEXPTYPE type =
                                        str2type(CHAR(STRING_ELT(con->c(), 0)));
                                    switch (type) {
                                    case LGLSXP:
                                    case INTSXP:
                                    case REALSXP:
                                    case CPLXSXP:
                                    case STRSXP:
                                    case EXPRSXP:
                                    case VECSXP:
                                    case RAWSXP:
                                        setVal(
                                            i,
                                            call(NativeBuiltins::get(
                                                     NativeBuiltins::Id::
                                                         makeVector),
                                                 {c(type),
                                                  Rep::Of(l) == Rep::f64
                                                      ? builder.CreateFPToUI(
                                                            load(l), t::i64)
                                                      : builder.CreateZExt(
                                                            load(l, Rep::i32),
                                                            t::i64)}));
                                        fastcase = true;
                                        break;
                                    default: {}
                                    }
                                }
                            }
                        }
                        break;
                    }
                    case blt("min"):
                    case blt("max"): {
                        bool isMin = b->builtinId == blt("min");
                        if (arep == Rep::i32 && brep == Rep::i32 &&
                            orep != Rep::f64) {
                            auto res = builder.CreateSelect(
                                isMin ? builder.CreateICmpSLT(bval, aval)
                                      : builder.CreateICmpSLT(aval, bval),
                                bval, aval);
                            if (orep == Rep::i32) {
                                setVal(i, res);
                            } else {
                                assert(orep == Rep::SEXP);
                                setVal(i, boxInt(res, false));
                            }
                            fastcase = true;
                        } else if (arep == Rep::f64 && brep == Rep::f64 &&
                                   orep != Rep::i32) {
                            auto res = builder.CreateSelect(
                                isMin ? builder.CreateFCmpUGT(bval, aval)
                                      : builder.CreateFCmpUGT(aval, bval),
                                aval, bval);
                            if (orep == Rep::f64) {
                                setVal(i, res);
                            } else {
                                assert(orep == Rep::SEXP);
                                setVal(i, boxReal(res, false));
                            }
                            fastcase = true;
                        }
                        break;
                    }
                    case blt("is.vector"): {
                        auto cnst = Const::Cast(b->arg(1).val());
                        if (!cnst)
                            break;

                        if (TYPEOF(cnst->c()) != STRSXP ||
                            LENGTH(cnst->c()) != 1)
                            break;

                        auto kind = STRING_ELT(cnst->c(), 0);
                        if (std::string("any") != CHAR(kind))
                            break;

                        if (arep == Rep::SEXP) {
                            llvm::Value* res;
                            auto isvec = isVector(aval);
                            auto v = b->arg(0).val();
                            if (!v->type.maybeHasAttrs()) {
                                res = builder.CreateSelect(
                                    isvec, constant(R_TrueValue, orep),
                                    constant(R_FalseValue, orep));
                            } else {
                                res = createSelect2(
                                    isvec,
                                    [&]() -> llvm::Value* {
                                        auto a = attr(aval);
                                        auto zero = builder.CreateICmpEQ(
                                            a, constant(R_NilValue, t::SEXP));
                                        return createSelect2(
                                            zero,
                                            [&]() -> llvm::Value* {
                                                return constant(R_TrueValue,
                                                                orep);
                                            },
                                            [&]() -> llvm::Value* {
                                                auto one = builder.CreateICmpEQ(
                                                    cdr(a), constant(R_NilValue,
                                                                     t::SEXP));
                                                auto names =
                                                    builder.CreateICmpEQ(
                                                        tag(a),
                                                        constant(R_NamesSymbol,
                                                                 t::SEXP));
                                                auto onlyNamesAtrrs =
                                                    builder.CreateAnd(one,
                                                                      names);
                                                return builder.CreateSelect(
                                                    onlyNamesAtrrs,
                                                    constant(R_TrueValue, orep),
                                                    constant(R_FalseValue,
                                                             orep));
                                            });
                                    },
                                    [&]() -> llvm::Value* {
                                        return constant(R_FalseValue, orep);
                                    });
                            }
                            setVal(i, res);
                        } else {
                            setVal(i, constant(R_TrueValue, orep));
                        }
                        fastcase = true;
                        break;
                    }
                    default:
                        break;
                    }
                    if (fastcase) {
                        fixVisibility();
                        break;
                    }
                }
                if (b->builtinId == blt("c") &&
                    (!b->type.maybeNotFastVecelt() ||
                     !b->type.maybeHasAttrs())) {
                    bool allScalar = true;
                    b->eachArg([&](Value* v) {
                        if (!v->type.isA(PirType::anySimpleScalar()))
                            allScalar = false;
                    });
                    if (allScalar) {
                        SEXPTYPE typ = 100;
                        if (b->type.isA(RType::real)) {
                            typ = REALSXP;
                        } else if (b->type.isA(RType::integer)) {
                            typ = INTSXP;
                        } else if (b->type.isA(RType::logical)) {
                            typ = LGLSXP;
                        }
                        if (typ != 100) {
                            auto res = call(NativeBuiltins::get(
                                                NativeBuiltins::Id::makeVector),
                                            {c(typ), c(b->nCallArgs(), 64)});
                            auto pos = 0;
                            b->eachCallArg([&](Value* v) {
                                assignVector(res, c(pos),
                                             convert(load(v), b->type.scalar()),
                                             b->type);
                                pos++;
                            });
                            setVal(i, res);
                            fixVisibility();
                            break;
                        }
                    }
                }

                if (b->builtinId == blt("list")) {
                    auto res = call(
                        NativeBuiltins::get(NativeBuiltins::Id::makeVector),
                        {c(VECSXP), c(b->nCallArgs(), 64)});
                    protectTemp(res);
                    auto pos = 0;
                    auto resT = PirType(RType::vec).notObject();

                    b->eachCallArg([&](Value* v) {
                        auto vn = loadSxp(v);
                        if (v->minReferenceCount() < 2) {
                            auto isnamed = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);
                            auto cont = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);
                            ensureNamed(vn);
                            builder.CreateCondBr(isNamed(vn), isnamed, cont);
                            builder.SetInsertPoint(isnamed);
                            ensureNamed(vn);
                            builder.CreateBr(cont);
                            builder.SetInsertPoint(cont);
                        }
                        assignVector(res, c(pos), vn, resT);
                        pos++;
                    });
                    setVal(i, res);
                    fixVisibility();
                    break;
                }

                setVal(i, callTheBuiltin());
                break;
            }

            case Tag::CallBuiltin: {
                auto b = CallBuiltin::Cast(i);

                // TODO: this is not sound... There are other ways to call
                // remove... What we should do instead is trap do_remove in gnur
                // and clear the cache!
                if (b->builtinId == blt("remove")) {
                    std::unordered_set<size_t> affected;
                    if (b->nargs() >= 2 &&
                        bindingsCache.count(b->arg(1).val())) {
                        for (const auto& b : bindingsCache[b->arg(1).val()])
                            affected.insert(b.second);
                    }
                    if (bindingsCache.count(b->env())) {
                        for (const auto& b : bindingsCache[b->env()])
                            affected.insert(b.second);
                    }
                    for (auto v : affected)
                        builder.CreateStore(
                            llvm::ConstantPointerNull::get(t::SEXP),
                            builder.CreateGEP(bindingsCacheBase, c(v)));
                }

                if (compileDotcall(
                        b, [&]() { return constant(b->builtinSexp, t::SEXP); },
                        [&](size_t i) { return R_NilValue; })) {
                    break;
                }
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, callRBuiltin(
                              b->builtinSexp, args, i->srcIdx, b->builtin,
                              b->hasEnv() ? loadSxp(b->env())
                                          : constant(R_BaseEnv, t::SEXP)));
                break;
            }

            case Tag::Call: {
                auto b = Call::Cast(i);

                if (compileDotcall(b, [&]() { return loadSxp(b->cls()); },
                                   [&](size_t i) { return R_NilValue; })) {
                    break;
                }

                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                Context asmpt = b->inferAvailableAssumptions();

                auto callId = ArglistOrder::NOT_REORDERED;
                if (b->isReordered())
                    callId = pushArgReordering(b->getArgOrderOrig());

                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(
                               NativeBuiltins::get(NativeBuiltins::Id::call),
                               {c(callId), paramCode(), c(b->srcIdx),
                                loadSxp(b->cls()), loadSxp(b->env()),
                                c(b->nCallArgs()), c(asmpt.toI())});
                       }));
                break;
            }

            case Tag::NamedCall: {
                auto b = NamedCall::Cast(i);
                if (compileDotcall(b, [&]() { return loadSxp(b->cls()); },
                                   [&](size_t i) { return b->names[i]; })) {
                    break;
                }
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                Context asmpt = b->inferAvailableAssumptions();

                std::vector<BC::PoolIdx> names;
                for (size_t i = 0; i < b->names.size(); ++i)
                    names.push_back(Pool::insert((b->names[i])));
                auto namesConst = c(names);
                auto namesStore = globalConst(namesConst);

                auto callId = ArglistOrder::NOT_REORDERED;
                if (b->isReordered())
                    callId = pushArgReordering(b->getArgOrderOrig());

                setVal(
                    i, withCallFrame(args, [&]() -> llvm::Value* {
                        return call(
                            NativeBuiltins::get(NativeBuiltins::Id::namedCall),
                            {
                                c(callId),
                                paramCode(),
                                c(b->srcIdx),
                                loadSxp(b->cls()),
                                loadSxp(b->env()),
                                c(b->nCallArgs()),
                                builder.CreateBitCast(namesStore, t::IntPtr),
                                c(asmpt.toI()),
                            });
                    }));
                break;
            }

            case Tag::StaticCall: {
                auto calli = StaticCall::Cast(i);
                calli->eachArg([](Value* v) { assert(!ExpandDots::Cast(v)); });
                auto target = calli->tryDispatch();
                auto bestTarget = calli->tryOptimisticDispatch();
                std::vector<Value*> args;
                calli->eachCallArg([&](Value* v) { args.push_back(v); });
                Context asmpt = calli->inferAvailableAssumptions();

                auto callId = ArglistOrder::NOT_REORDERED;
                if (calli->isReordered())
                    callId = pushArgReordering(calli->getArgOrderOrig());

                if (!target->owner()->hasOriginClosure()) {
                    setVal(
                        i, withCallFrame(args, [&]() -> llvm::Value* {
                            return call(
                                NativeBuiltins::get(NativeBuiltins::Id::call),
                                {c(callId), paramCode(), c(calli->srcIdx),
                                 loadSxp(calli->runtimeClosure()),
                                 loadSxp(calli->env()), c(calli->nCallArgs()),
                                 c(asmpt.toI())});
                        }));
                    break;
                }

                if (target == bestTarget) {
                    auto callee = target->owner()->rirClosure();
                    auto dt = DispatchTable::check(BODY(callee));
                    rir::Function* nativeTarget = nullptr;
                    for (size_t i = 0; i < dt->size(); i++) {
                        auto entry = dt->get(i);
                        if (entry->context() == target->context() &&
                            entry->signature().numArguments >= args.size()) {
                            nativeTarget = entry;
                        }
                    }
                    if (nativeTarget) {
                        assert(
                            asmpt.includes(Assumption::StaticallyArgmatched));
                        auto idx = Pool::makeSpace();
                        NativeBuiltins::targetCaches.push_back(idx);
                        Pool::patch(idx, nativeTarget->container());
                        auto missAsmptStore =
                            Rf_allocVector(RAWSXP, sizeof(Context));
                        auto missAsmptIdx = Pool::insert(missAsmptStore);
                        new (DATAPTR(missAsmptStore))
                            Context(nativeTarget->context() - asmpt);
                        assert(asmpt.smaller(nativeTarget->context()));
                        auto res = withCallFrame(args, [&]() {
                            return call(
                                NativeBuiltins::get(
                                    NativeBuiltins::Id::nativeCallTrampoline),
                                {
                                    c(callId),
                                    paramCode(),
                                    constant(callee, t::SEXP),
                                    c(idx),
                                    c(calli->srcIdx),
                                    loadSxp(calli->env()),
                                    c(args.size()),
                                    c(asmpt.toI()),
                                    c(missAsmptIdx),
                                });
                        });
                        setVal(i, res);
                        break;
                    }
                }

                assert(asmpt.includes(Assumption::StaticallyArgmatched));
                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(
                               NativeBuiltins::get(NativeBuiltins::Id::call),
                               {
                                   c(callId),
                                   paramCode(),
                                   c(calli->srcIdx),
                                   builder.CreateIntToPtr(
                                       c(calli->cls()->rirClosure()), t::SEXP),
                                   loadSxp(calli->env()),
                                   c(calli->nCallArgs()),
                                   c(asmpt.toI()),
                               });
                       }));
                break;
            }

            case Tag::Inc: {
                auto arg = i->arg(0).val();
                llvm::Value* res = nullptr;
                assert(Rep::Of(arg) == Rep::i32);
                res = load(arg, Rep::i32);
                res = builder.CreateAdd(res, c(1), "", true, true);
                setVal(i, res);
                break;
            }

            case Tag::Phi:
            case Tag::Nop:
                break;

            case Tag::ToForSeq: {
                auto a = i->arg(0).val();
                if (Rep::Of(a) != Rep::SEXP) {
                    setVal(i, load(a));
                    break;
                }
                llvm::Value* res =
                    call(NativeBuiltins::get(NativeBuiltins::Id::toForSeq),
                         {loadSxp(i->arg(0).val())});
                setVal(i, res);
                break;
            }

            case Tag::Branch: {
                auto br = Branch::Cast(i);
                auto cond = load(i->arg(0).val(), Rep::i32);
                cond = builder.CreateICmpNE(cond, c(0));

                auto t = bb->trueBranch();
                auto f = bb->falseBranch();
                MDNode* weight = nullptr;
                bool retrigger = (cls->optFunction->isOptimized() &&
                                  cls->optFunction->deoptCount() > 0) ||
                                 cls->isContinuation();
                bool chaos =
                    br->deoptTrigger && Parameter::DEOPT_CHAOS &&
                    (!retrigger || !Parameter::DEOPT_CHAOS_NO_RETRIGGER);

                if (t->isDeopt() || (t->isJmp() && t->next()->isDeopt())) {
                    if (chaos)
                        cond = builder.CreateOr(
                            cond,
                            call(NativeBuiltins::get(
                                     NativeBuiltins::Id::deoptChaosTrigger),
                                 {builder.getTrue()}));
                    weight = branchAlwaysFalse;
                } else if (f->isDeopt() ||
                           (f->isJmp() && f->next()->isDeopt())) {
                    if (chaos)
                        cond = builder.CreateAnd(
                            cond,
                            call(NativeBuiltins::get(
                                     NativeBuiltins::Id::deoptChaosTrigger),
                                 {builder.getFalse()}));
                    weight = branchAlwaysTrue;
                }
                builder.CreateCondBr(cond, getBlock(bb->trueBranch()),
                                     getBlock(bb->falseBranch()), weight);
                break;
            }

            case Tag::Deopt: {
                // TODO, this is copied from pir2rir... rather ugly
                DeoptMetadata* m = nullptr;
                auto deopt = Deopt::Cast(i);
                std::vector<Value*> args;
                {
                    std::vector<FrameState*> frames;

                    auto fs = deopt->frameState();
                    while (fs) {
                        frames.push_back(fs);
                        fs = fs->next();
                    }

                    size_t nframes = frames.size();
                    SEXP store =
                        Rf_allocVector(RAWSXP, sizeof(DeoptMetadata) +
                                                   nframes * sizeof(FrameInfo));
                    m = new (DATAPTR(store)) DeoptMetadata;
                    m->numFrames = nframes;

                    int frameNr = nframes - 1;
                    for (auto f = frames.rbegin(); f != frames.rend(); ++f) {
                        auto fs = *f;
                        for (size_t pos = 0; pos < fs->stackSize; pos++)
                            args.push_back(fs->arg(pos).val());
                        args.push_back(fs->env());
                        m->frames[frameNr--] = {fs->pc, fs->code, fs->stackSize,
                                                fs->inPromise};
                    }

                    target->addExtraPoolEntry(store);
                }

                withCallFrame(args, [&]() {
                    return call(NativeBuiltins::get(NativeBuiltins::Id::deopt),
                                {paramCode(), paramClosure(),
                                 convertToPointer(m, t::i8, true), paramArgs(),
                                 c(deopt->escapedEnv, 1),
                                 load(deopt->deoptReason()),
                                 loadSxp(deopt->deoptTrigger())});
                });
                builder.CreateUnreachable();
                break;
            }

            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(i);
                auto parent = loadSxp(mkenv->env());

                std::vector<BC::PoolIdx> names;
                for (size_t i = 0; i < mkenv->nLocals(); ++i) {
                    auto n = mkenv->varName[i];
                    if (mkenv->missing[i])
                        n = CONS_NR(n, R_NilValue);
                    names.push_back(Pool::insert(n));
                }
                auto namesConst = c(names);
                auto namesStore = globalConst(namesConst);

                if (mkenv->stub) {
                    auto env =
                        call(NativeBuiltins::get(
                                 NativeBuiltins::Id::createStubEnvironment),
                             {parent, c((int)mkenv->nLocals()),
                              builder.CreateBitCast(namesStore, t::IntPtr),
                              c(mkenv->context)});
                    protectTemp(env);
                    size_t pos = 0;
                    mkenv->eachLocalVar([&](SEXP name, Value* v, bool miss) {
                        auto vn = loadSxp(v);
                        envStubSet(env, pos, vn, mkenv->nLocals(), false);
                        if (miss) {
                            envStubSetMissing(env, pos);
                        } else {
                            if (v->type.maybeMissing() &&
                                vn->getType() == t::SEXP) {
                                auto isMissing = BasicBlock::Create(
                                    PirJitLLVM::getContext(), "", fun);
                                auto done = BasicBlock::Create(
                                    PirJitLLVM::getContext(), "", fun);
                                builder.CreateCondBr(
                                    builder.CreateICmpEQ(
                                        vn, constant(R_MissingArg, Rep::SEXP)),
                                    isMissing, done);
                                builder.SetInsertPoint(isMissing);
                                envStubSetMissing(env, pos);
                                builder.CreateBr(done);
                                builder.SetInsertPoint(done);
                            }
                        }
                        pos++;
                        incrementNamed(vn);
                    });
                    setVal(i, env);
                    break;
                }

                // Need to load these first, to avoid protect issues
                std::stack<llvm::Value*> args;
                mkenv->eachLocalVar([&](SEXP name, Value* v, bool miss) {
                    args.push(loadSxp(v));
                });

                auto arglist = constant(R_NilValue, t::SEXP);
                mkenv->eachLocalVarRev([&](SEXP name, Value* v, bool miss) {
                    if (miss) {
                        arglist = call(
                            NativeBuiltins::get(
                                NativeBuiltins::Id::createMissingBindingCell),
                            {args.top(), constant(name, t::SEXP), arglist});
                    } else {
                        arglist = call(
                            NativeBuiltins::get(
                                NativeBuiltins::Id::createBindingCell),
                            {args.top(), constant(name, t::SEXP), arglist});
                    }
                    args.pop();
                });

                setVal(i, call(NativeBuiltins::get(
                                   NativeBuiltins::Id::createEnvironment),
                               {parent, arglist, c(mkenv->context)}));

                if (bindingsCache.count(i))
                    for (auto b : bindingsCache.at(i))
                        builder.CreateStore(
                            llvm::ConstantPointerNull::get(t::SEXP),
                            builder.CreateGEP(bindingsCacheBase, c(b.second)));
                break;
            }

            case Tag::MaterializeEnv: {
                auto materialize = MaterializeEnv::Cast(i);
                setVal(i, call(NativeBuiltins::get(
                                   NativeBuiltins::Id::materializeEnvironment),
                               {loadSxp(materialize->env())}));
                break;
            }

            case Tag::Add:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 // TODO: Check NA
                                 auto res =
                                     builder.CreateAdd(a, b, "", false, true);
                                 return res;
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFAdd(a, b);
                             },
                             BinopKind::ADD);
                break;

            case Tag::Sub:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 // TODO: Check NA
                                 return builder.CreateSub(a, b, "", false,
                                                          true);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFSub(a, b);
                             },
                             BinopKind::SUB);
                break;

            case Tag::Mul:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 // TODO: Check NA
                                 return builder.CreateMul(a, b, "", false,
                                                          true);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFMul(a, b);
                             },
                             BinopKind::MUL);
                break;

            case Tag::Div:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 // TODO: Check NA
                                 return builder.CreateSDiv(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFDiv(a, b);
                             },
                             BinopKind::DIV);
                break;

            case Tag::Pow:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 // TODO: Check NA?
                                 return builder.CreateIntrinsic(
                                     Intrinsic::powi,
                                     {a->getType(), b->getType()}, {a, b});
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateBinaryIntrinsic(
                                     Intrinsic::pow, a, b);
                             },
                             BinopKind::POW);
                break;

            case Tag::Neq:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateICmpNE(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFCmpUNE(a, b);
                             },
                             BinopKind::NE);
                break;

            case Tag::Minus: {
                compileUnop(
                    i, [&](llvm::Value* a) { return builder.CreateNeg(a); },
                    [&](llvm::Value* a) { return builder.CreateFNeg(a); },
                    UnopKind::MINUS);
                break;
            }

            case Tag::Plus: {
                compileUnop(i, [&](llvm::Value* a) { return a; },
                            [&](llvm::Value* a) { return a; }, UnopKind::PLUS);
                break;
            }

            case Tag::Not: {
                auto resultRep = Rep::Of(i);
                auto argument = i->arg(0).val();
                auto argumentRep = Rep::Of(argument);
                if (argumentRep == Rep::SEXP) {
                    auto argumentNative = loadSxp(argument);

                    llvm::Value* res = nullptr;
                    if (i->hasEnv()) {
                        res = call(
                            NativeBuiltins::get(NativeBuiltins::Id::notEnv),
                            {argumentNative, loadSxp(i->env()), c(i->srcIdx)});
                    } else {
                        res =
                            call(NativeBuiltins::get(NativeBuiltins::Id::notOp),
                                 {argumentNative});
                    }
                    setVal(i, res);
                    break;
                }

                auto done =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                auto isNa =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                auto argumentNative = load(argument, argumentRep);

                nacheck(argumentNative, argument->type, isNa);

                auto res = phiBuilder(t::Int);

                if (argumentRep == Rep::f64) {
                    res.addInput(builder.CreateZExt(
                        builder.CreateFCmpUEQ(argumentNative, c(0.0)), t::Int));
                } else {
                    res.addInput(builder.CreateZExt(
                        builder.CreateICmpEQ(argumentNative, c(0)), t::Int));
                }
                builder.CreateBr(done);

                builder.SetInsertPoint(isNa);
                // Maybe we need to model R_LogicalNAValue?
                res.addInput(c(NA_INTEGER));
                builder.CreateBr(done);
                builder.SetInsertPoint(done);

                if (resultRep == Rep::SEXP) {
                    setVal(i, boxLgl(res()));
                } else {
                    setVal(i, res());
                }
                break;
            }

            case Tag::Eq:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateICmpEQ(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFCmpOEQ(a, b);
                             },
                             BinopKind::EQ);
                break;

            case Tag::Lte:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateICmpSLE(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFCmpOLE(a, b);
                             },
                             BinopKind::LTE);
                break;

            case Tag::Lt:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateICmpSLT(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFCmpOLT(a, b);
                             },
                             BinopKind::LT);
                break;

            case Tag::Gte:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateICmpSGE(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFCmpOGE(a, b);
                             },
                             BinopKind::GTE);
                break;

            case Tag::Gt:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateICmpSGT(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFCmpOGT(a, b);
                             },
                             BinopKind::GT);
                break;

            case Tag::LAnd:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 auto afalse = builder.CreateICmpEQ(a, c(0));
                                 auto bfalse = builder.CreateICmpEQ(b, c(0));
                                 return createSelect2(
                                     builder.CreateOr(afalse, bfalse),
                                     [&]() { return c(0); },
                                     [&]() {
                                         auto aNa = builder.CreateICmpEQ(
                                             a, c(NA_LOGICAL));
                                         auto bNa = builder.CreateICmpEQ(
                                             b, c(NA_LOGICAL));
                                         return createSelect2(
                                             builder.CreateOr(aNa, bNa),
                                             []() { return c(NA_LOGICAL); },
                                             []() { return c(1); });
                                     });
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 auto afalse = builder.CreateFCmpUEQ(a, c(0.0));
                                 auto bfalse = builder.CreateFCmpUEQ(b, c(0.0));
                                 return createSelect2(
                                     builder.CreateOr(afalse, bfalse),
                                     [&]() { return c(0); },
                                     [&]() {
                                         auto aNa = builder.CreateFCmpUNE(a, b);
                                         auto bNa = builder.CreateFCmpUNE(b, b);
                                         return createSelect2(
                                             builder.CreateOr(aNa, bNa),
                                             []() { return c(NA_LOGICAL); },
                                             []() { return c(1); });
                                     });
                             },
                             BinopKind::LAND, false);
                break;

            case Tag::LOr:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        auto afalse = builder.CreateICmpEQ(a, c(0));
                        auto bfalse = builder.CreateICmpEQ(b, c(0));
                        auto aNa = builder.CreateICmpEQ(a, c(NA_LOGICAL));
                        auto bNa = builder.CreateICmpEQ(b, c(NA_LOGICAL));
                        auto atrue = builder.CreateAnd(
                            builder.CreateNot(afalse), builder.CreateNot(aNa));
                        auto btrue = builder.CreateAnd(
                            builder.CreateNot(bfalse), builder.CreateNot(bNa));
                        return createSelect2(
                            builder.CreateOr(atrue, btrue),
                            [&]() { return c(1); },
                            [&]() {
                                return createSelect2(
                                    builder.CreateOr(aNa, bNa),
                                    []() { return c(NA_LOGICAL); },
                                    []() { return c(0); });
                            });
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        auto afalse = builder.CreateFCmpUEQ(a, c(0.0));
                        auto bfalse = builder.CreateFCmpUEQ(b, c(0.0));
                        auto aNa = builder.CreateFCmpUNE(a, b);
                        auto bNa = builder.CreateFCmpUNE(b, b);
                        auto atrue = builder.CreateAnd(
                            builder.CreateNot(afalse), builder.CreateNot(aNa));
                        auto btrue = builder.CreateAnd(
                            builder.CreateNot(bfalse), builder.CreateNot(bNa));
                        return createSelect2(
                            builder.CreateOr(atrue, btrue),
                            [&]() { return c(1); },
                            [&]() {
                                return createSelect2(
                                    builder.CreateOr(aNa, bNa),
                                    []() { return c(NA_LOGICAL); },
                                    []() { return c(0); });
                            });
                    },
                    BinopKind::LOR, false);
                break;

            case Tag::IDiv:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        auto isZero = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto notZero = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto cnt = BasicBlock::Create(PirJitLLVM::getContext(),
                                                      "", fun);
                        builder.CreateCondBr(builder.CreateICmpEQ(b, c(0)),
                                             isZero, notZero,
                                             branchMostlyFalse);

                        auto res = phiBuilder(t::Int);

                        builder.SetInsertPoint(isZero);
                        res.addInput(c(NA_INTEGER));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(notZero);
                        auto r = builder.CreateFDiv(
                            builder.CreateSIToFP(a, t::Double),
                            builder.CreateSIToFP(b, t::Double));
                        res.addInput(builder.CreateFPToSI(r, t::Int));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return res();
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        // from myfloor
                        auto q = builder.CreateFDiv(a, b);
                        auto isZero = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto notZero = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto cnt = BasicBlock::Create(PirJitLLVM::getContext(),
                                                      "", fun);
                        builder.CreateCondBr(builder.CreateFCmpUEQ(b, c(0.0)),
                                             isZero, notZero,
                                             branchMostlyFalse);

                        auto res = phiBuilder(t::Double);

                        builder.SetInsertPoint(isZero);
                        res.addInput(q);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(notZero);
                        auto fq = builder.CreateIntrinsic(Intrinsic::floor,
                                                          {t::Double}, {q});
                        auto tmp =
                            builder.CreateFSub(a, builder.CreateFMul(fq, b));
                        auto frem = builder.CreateIntrinsic(
                            Intrinsic::floor, {t::Double},
                            {builder.CreateFDiv(tmp, b)});
                        res.addInput(builder.CreateFAdd(fq, frem));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return res();
                    },
                    BinopKind::IDIV);
                break;

            case Tag::Mod: {
                auto myfmod = [&](llvm::Value* a, llvm::Value* b) {
                    // from myfmod
                    auto isZero =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto notZero =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto cnt =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto res = phiBuilder(t::Double);
                    builder.CreateCondBr(builder.CreateFCmpUEQ(b, c(0.0)),
                                         isZero, notZero, branchMostlyFalse);

                    builder.SetInsertPoint(isZero);
                    res.addInput(c(R_NaN));
                    builder.CreateBr(cnt);

                    builder.SetInsertPoint(notZero);
                    auto q = builder.CreateFDiv(a, b);
                    auto fq = builder.CreateIntrinsic(Intrinsic::floor,
                                                      {t::Double}, {q});

                    auto absq = builder.CreateIntrinsic(Intrinsic::fabs,
                                                        {t::Double}, {q});
                    auto finite = builder.CreateFCmpUNE(
                        absq, c((double)0x7FF0000000000000));
                    auto gt =
                        builder.CreateFCmpUGT(absq, c(1 / R_AccuracyInfo.eps));

                    auto warn =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto noWarn =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    builder.CreateCondBr(builder.CreateAnd(finite, gt), warn,
                                         noWarn, branchMostlyFalse);

                    builder.SetInsertPoint(warn);
                    auto msg = builder.CreateGlobalString(
                        "probable complete loss of accuracy in modulus");
                    call(NativeBuiltins::get(NativeBuiltins::Id::warn),
                         {builder.CreateInBoundsGEP(msg, {c(0), c(0)})});
                    builder.CreateBr(noWarn);

                    builder.SetInsertPoint(noWarn);
                    auto tmp = builder.CreateFSub(a, builder.CreateFMul(fq, b));
                    auto frem =
                        builder.CreateIntrinsic(Intrinsic::floor, {t::Double},
                                                {builder.CreateFDiv(tmp, b)});
                    res.addInput(
                        builder.CreateFSub(tmp, builder.CreateFMul(frem, b)));
                    builder.CreateBr(cnt);

                    builder.SetInsertPoint(cnt);
                    return res();
                };

                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        auto fast = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);
                        auto fast1 = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto slow = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);
                        auto cnt = BasicBlock::Create(PirJitLLVM::getContext(),
                                                      "", fun);
                        auto res = phiBuilder(t::Int);
                        builder.CreateCondBr(builder.CreateICmpSGE(a, c(0)),
                                             fast1, slow, branchMostlyTrue);

                        builder.SetInsertPoint(fast1);
                        builder.CreateCondBr(builder.CreateICmpSGT(b, c(0)),
                                             fast, slow, branchMostlyTrue);

                        builder.SetInsertPoint(fast);
                        res.addInput(builder.CreateSRem(a, b));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(slow);
                        res.addInput(builder.CreateFPToSI(
                            myfmod(builder.CreateSIToFP(a, t::Double),
                                   builder.CreateSIToFP(b, t::Double)),
                            t::Int));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return res();
                    },
                    myfmod, BinopKind::MOD);
                break;
            }

            case Tag::Colon: {
                assert(Rep::Of(i) == Rep::SEXP);
                auto a = i->arg(0).val();
                auto b = i->arg(1).val();
                llvm::Value* res;
                if (i->hasEnv()) {
                    auto e = loadSxp(i->env());
                    res =
                        call(NativeBuiltins::get(NativeBuiltins::Id::binopEnv),
                             {loadSxp(a), loadSxp(b), e, c(i->srcIdx),
                              c((int)BinopKind::COLON)});
                } else if (Rep::Of(a) == Rep::i32 && Rep::Of(b) == Rep::i32) {
                    res = call(NativeBuiltins::get(NativeBuiltins::Id::colon),
                               {load(a), load(b)});
                } else {
                    res = call(
                        NativeBuiltins::get(NativeBuiltins::Id::binop),
                        {loadSxp(a), loadSxp(b), c((int)BinopKind::COLON)});
                }
                setVal(i, res);
                break;
            }

            case Tag::Return: {
                auto res = loadSxp(Return::Cast(i)->arg<0>().val());
                exitBlocks.push_back(builder.GetInsertBlock());
                builder.CreateRet(res);
                break;
            }

            case Tag::NonLocalReturn: {
                call(NativeBuiltins::get(NativeBuiltins::Id::nonLocalReturn),
                     {loadSxp(i->arg(0).val()), loadSxp(i->env())});
                builder.CreateUnreachable();
                break;
            }

            case Tag::IsEnvStub: {
                auto arg = loadSxp(i->arg(0).val());
                auto env = MkEnv::Cast(i->env());

                auto isStub =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                auto isNotMaterialized =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                auto isNotStub =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                auto done =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                auto r = Rep::Of(i);
                auto res = phiBuilder(r.toLlvm());

                builder.CreateCondBr(isExternalsxp(arg, LAZY_ENVIRONMENT_MAGIC),
                                     isStub, isNotStub, branchAlwaysTrue);

                builder.SetInsertPoint(isStub);
                auto materialized = envStubGet(arg, -2, env->nLocals());
                builder.CreateCondBr(
                    builder.CreateICmpEQ(
                        materialized, llvm::ConstantPointerNull::get(t::SEXP)),
                    isNotMaterialized, isNotStub, branchAlwaysTrue);

                builder.SetInsertPoint(isNotMaterialized);
                res.addInput(constant(R_TrueValue, r));
                builder.CreateBr(done);

                builder.SetInsertPoint(isNotStub);
                res.addInput(constant(R_FalseValue, r));
                builder.CreateBr(done);

                builder.SetInsertPoint(done);

                setVal(i, res());
                break;
            }

            case Tag::MkCls: {
                auto mkFunction = MkCls::Cast(i);
                auto srcRef = constant(mkFunction->srcRef, t::SEXP);
                auto formals = constant(mkFunction->formals, t::SEXP);
                auto body =
                    constant(mkFunction->originalBody->container(), t::SEXP);
                assert(DispatchTable::check(
                    mkFunction->originalBody->container()));
                setVal(
                    i,
                    call(NativeBuiltins::get(NativeBuiltins::Id::createClosure),
                         {body, formals, loadSxp(mkFunction->env()), srcRef}));
                break;
            }

            case Tag::IsType: {
                assert(Rep::Of(i) == Rep::i32);

                auto t = IsType::Cast(i);
                auto arg = i->arg(0).val();
                if (Rep::Of(arg) == Rep::SEXP) {
                    auto a = loadSxp(arg);

                    // Some specialcases for the simple scalars which we want to
                    // be extra fast.
                    auto depromiseIfNeeded = [&]() {
                        if (t->typeTest.maybePromiseWrapped())
                            return depromise(a, arg->type);
                        return a;
                    };
                    if (t->typeTest.notPromiseWrapped()
                            .orNAOrNaN()
                            .orWrappedMissing() == PirType::simpleScalarInt()) {
                        a = depromiseIfNeeded();
                        auto res = isSimpleScalar(a, INTSXP);
                        if (!t->typeTest.maybeNAOrNaN())
                            res = createSelect2(
                                res,
                                [&]() {
                                    return builder.CreateICmpNE(unboxInt(a),
                                                                c(NA_INTEGER));
                                },
                                [&]() { return builder.getFalse(); });
                        setVal(i, builder.CreateZExt(res, t::Int));
                        break;
                    } else if (t->typeTest.notPromiseWrapped()
                                   .orNAOrNaN()
                                   .orWrappedMissing() ==
                               PirType::simpleScalarLogical()) {
                        a = depromiseIfNeeded();
                        auto res = isSimpleScalar(a, LGLSXP);
                        if (!t->typeTest.maybeNAOrNaN()) {
                            res = createSelect2(
                                res,
                                [&]() {
                                    return builder.CreateICmpNE(unboxLgl(a),
                                                                c(NA_LOGICAL));
                                },
                                [&]() { return builder.getFalse(); });
                        }
                        setVal(i, builder.CreateZExt(res, t::Int));
                        break;
                    } else if (t->typeTest.notPromiseWrapped()
                                   .orNAOrNaN()
                                   .orWrappedMissing() ==
                               PirType::simpleScalarReal()) {
                        a = depromiseIfNeeded();
                        auto res = isSimpleScalar(a, REALSXP);
                        if (!t->typeTest.maybeNAOrNaN())
                            res = createSelect2(
                                res,
                                [&]() {
                                    auto va = unboxReal(a);
                                    return builder.CreateFCmpUEQ(va, va);
                                },
                                [&]() { return builder.getFalse(); });
                        setVal(i, builder.CreateZExt(res, t::Int));
                        break;
                    }

                    if (!(arg->type.maybeNAOrNaN() <=
                          t->typeTest.maybeNAOrNaN())) {
                        arg->type.print(std::cout);
                        std::cout << " ";
                        t->typeTest.print(std::cout);
                        std::cout << "\n";
                    }

                    // NA checks can only be done on scalars!
                    assert(arg->type.maybeNAOrNaN() <=
                           t->typeTest.maybeNAOrNaN());

                    // Here we depromise the value. In case the promise is lazy,
                    // a will be R_UnboundValue, which is handled in the tests
                    // below. In case the type-test guards against wrapped
                    // missing values, we must test the promise content against
                    // missing. However this check must distinguish the promise
                    // case from the naked value case, because missingness of a
                    // type can change with forcing.
                    auto phi = phiBuilder(t::i1);
                    if (t->typeTest.maybePromiseWrapped())
                        a = depromise(
                            a, arg->type,
                            [&](llvm::Value* promisedVal) {
                                if (!t->typeTest.forced().maybeMissing() &&
                                    arg->type.forced().maybeMissing()) {
                                    phi.addInput(builder.CreateICmpNE(
                                        promisedVal,
                                        constant(R_MissingArg, t::SEXP)));
                                } else {
                                    phi.addInput(builder.getTrue());
                                }
                            },
                            [&]() {
                                if (!t->typeTest.maybeMissing() &&
                                    arg->type.maybeMissing()) {
                                    phi.addInput(builder.CreateICmpNE(
                                        a, constant(R_MissingArg, t::SEXP)));
                                } else {
                                    phi.addInput(builder.getTrue());
                                }
                            });
                    auto res = phi.initialized() ? phi() : builder.getTrue();

                    llvm::Value* res1;
                    if (t->typeTest.noAttribsOrObject().isA(
                            PirType(RType::logical).orPromiseWrapped())) {
                        res1 = builder.CreateICmpEQ(sexptype(a), c(LGLSXP));
                    } else if (t->typeTest.noAttribsOrObject().isA(
                                   PirType(RType::integer)
                                       .orPromiseWrapped())) {
                        res1 = builder.CreateICmpEQ(sexptype(a), c(INTSXP));
                    } else if (t->typeTest.noAttribsOrObject().isA(
                                   PirType(RType::real).orPromiseWrapped())) {
                        res1 = builder.CreateICmpEQ(sexptype(a), c(REALSXP));
                    } else if (t->typeTest.noAttribsOrObject().isA(
                                   PirType(RType::closure)
                                       .orPromiseWrapped())) {
                        res1 = builder.CreateICmpEQ(sexptype(a), c(CLOSXP));
                    } else if (t->typeTest.noAttribsOrObject().isA(
                                   PirType(RType::builtin)
                                       .orPromiseWrapped())) {
                        res1 = builder.CreateICmpEQ(sexptype(a), c(BUILTINSXP));
                    } else if (t->typeTest.noAttribsOrObject().isA(
                                   PirType(RType::special)
                                       .orPromiseWrapped())) {
                        res1 = builder.CreateICmpEQ(sexptype(a), c(SPECIALSXP));
                    } else {
                        assert(arg->type.notMissing()
                                   .notPromiseWrapped()
                                   .noAttribsOrObject()
                                   .isA(t->typeTest));
                        res1 = builder.CreateICmpNE(
                            a, constant(R_UnboundValue, t::SEXP));
                    }
                    if (res == builder.getTrue())
                        res = res1;
                    else
                        res = builder.CreateAnd(res, res1);

                    if (t->typeTest.isScalar() && !arg->type.isScalar()) {
                        assert(a->getType() == t::SEXP);
                        res = builder.CreateAnd(res, isScalar(a));
                    }
                    if (arg->type.maybeHasAttrs() &&
                        !t->typeTest.maybeHasAttrs()) {
                        res = builder.CreateAnd(
                            res, builder.CreateICmpEQ(
                                     attr(a), constant(R_NilValue, t::SEXP)));
                    }
                    if (arg->type.maybeNotFastVecelt() &&
                        !t->typeTest.maybeNotFastVecelt()) {
                        res = builder.CreateAnd(res, fastVeceltOkNative(a));
                    } else if (arg->type.maybeObj() &&
                               !t->typeTest.maybeObj()) {
                        res =
                            builder.CreateAnd(res, builder.CreateNot(isObj(a)));
                    }
                    setVal(i, builder.CreateZExt(res, t::Int));
                } else {
                    if (Rep::Of(arg) == Rep::f64 &&
                        arg->type.maybe(RType::real) &&
                        !t->typeTest.maybe(RType::real)) {
                        setVal(i, builder.CreateZExt(
                                      checkDoubleToInt(load(arg), arg->type),
                                      t::Int));
                    } else {
                        setVal(i, c(1));
                    }
                }
                break;
            }

            case Tag::Is: {
                assert(Rep::Of(i) == Rep::i32);
                auto is = Is::Cast(i);
                auto arg = i->arg(0).val();
                llvm::Value* res;
                if (Rep::Of(arg) == Rep::SEXP) {
                    auto argNative = loadSxp(arg);
                    switch (is->typecheck) {
                    case BC::RirTypecheck::isNILSXP:
                    case BC::RirTypecheck::isLGLSXP:
                    case BC::RirTypecheck::isREALSXP:
                    case BC::RirTypecheck::isSTRSXP:
                    case BC::RirTypecheck::isINTSXP:
                    case BC::RirTypecheck::isCPLXSXP:
                    case BC::RirTypecheck::isRAWSXP:
                    case BC::RirTypecheck::isEXPRSXP: {
                        auto typeNative = sexptype(argNative);
                        auto expectedTypeNative = c((SEXPTYPE)is->typecheck);
                        res = builder.CreateICmpEQ(typeNative,
                                                   expectedTypeNative);
                        break;
                    }
                    case BC::RirTypecheck::isVECSXP: {
                        auto typeNative = sexptype(argNative);
                        auto operandLhs =
                            builder.CreateICmpEQ(typeNative, c(VECSXP));
                        auto operandRhs =
                            builder.CreateICmpEQ(typeNative, c(LISTSXP));
                        res = builder.CreateOr(operandLhs, operandRhs);
                        break;
                    }

                    case BC::RirTypecheck::isLISTSXP: {
                        auto typeNative = sexptype(argNative);
                        auto operandLhs =
                            builder.CreateICmpEQ(typeNative, c(LISTSXP));
                        auto operandRhs =
                            builder.CreateICmpEQ(typeNative, c(NILSXP));
                        res = builder.CreateOr(operandLhs, operandRhs);
                        break;
                    }

                    case BC::RirTypecheck::isVector:
                    case BC::RirTypecheck::isNonObject:
                        // These are decomposed into smaller operations in
                        // rir2pir
                        assert(false);
                        res = builder.getFalse();
                        break;

                    case BC::RirTypecheck::isFactor:
                        if (Rep::Of(arg) != Rep::SEXP) {
                            res = builder.getFalse();
                        } else {
                            res = call(NativeBuiltins::get(
                                           NativeBuiltins::Id::isFactor),
                                       {loadSxp(arg)});
                        }
                        break;
                    }
                } else {
                    assert(i->type.isA(RType::integer) ||
                           i->type.isA(RType::logical) ||
                           i->type.isA(RType::real));
                    assert(Rep::Of(i) == Rep::i32 || Rep::Of(i) == Rep::f64);

                    bool matchInt =
                        (is->typecheck == BC::RirTypecheck::isINTSXP) &&
                        i->type.isA(RType::integer);
                    bool matchLgl =
                        (is->typecheck == BC::RirTypecheck::isLGLSXP) &&
                        i->type.isA(RType::logical);
                    bool matchReal =
                        (is->typecheck == BC::RirTypecheck::isREALSXP) &&
                        i->type.isA(RType::real);

                    res = (matchInt || matchLgl || matchReal)
                              ? builder.getTrue()
                              : builder.getFalse();
                }
                setVal(i, builder.CreateZExt(res, t::Int));
                break;
            }

            case Tag::AsSwitchIdx: {
                auto arg = i->arg(0).val();
                llvm::Value* res;
                auto rep = Rep::Of(i->arg(0).val());
                if (rep == Rep::i32) {
                    auto a = load(arg);
                    res = builder.CreateSelect(
                        builder.CreateICmpEQ(c(NA_INTEGER), a), c(-1), a);
                } else {
                    res = call(
                        NativeBuiltins::get(NativeBuiltins::Id::asSwitchIdx),
                        {loadSxp(arg)});
                }
                setVal(i, res);
                break;
            }

            case Tag::CheckTrueFalse: {
                assert(Rep::Of(i) == Rep::i32);

                auto arg = i->arg(0).val();
                llvm::Value* res;

                if (Rep::Of(arg) == Rep::SEXP) {
                    auto a = loadSxp(arg);
                    res = call(
                        NativeBuiltins::get(NativeBuiltins::Id::checkTrueFalse),
                        {a});
                } else {
                    auto r = Rep::Of(arg);

                    auto done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto isNa = BasicBlock::Create(PirJitLLVM::getContext(),
                                                   "asTestIsNa", fun);

                    if (r == Rep::f64) {
                        auto narg = load(arg, r);
                        nacheck(narg, arg->type, isNa);
                        res = builder.CreateFCmpUNE(c(0.0), narg);
                        builder.CreateBr(done);
                    } else {
                        auto narg = load(arg, Rep::i32);
                        nacheck(narg, arg->type, isNa);
                        res = builder.CreateICmpNE(c(0), narg);
                        builder.CreateBr(done);
                    }

                    builder.SetInsertPoint(isNa);
                    auto msg = builder.CreateGlobalString(
                        "missing value where TRUE/FALSE needed");
                    call(NativeBuiltins::get(NativeBuiltins::Id::error),
                         {builder.CreateInBoundsGEP(msg, {c(0), c(0)})});
                    builder.CreateUnreachable();

                    builder.SetInsertPoint(done);
                }
                setVal(i, builder.CreateZExt(res, t::Int));
                break;
            }

            case Tag::AsLogical: {
                auto arg = i->arg(0).val();

                auto r1 = Rep::Of(arg);
                auto r2 = Rep::Of(i);

                assert(r2 == Rep::i32);

                llvm::Value* res;
                if (r1 == Rep::SEXP) {
                    res = call(
                        NativeBuiltins::get(NativeBuiltins::Id::asLogicalBlt),
                        {loadSxp(arg)});
                } else if (r1 == Rep::f64) {
                    auto phi = phiBuilder(t::Int);
                    auto nin = load(arg);

                    auto done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto isNaBr = BasicBlock::Create(PirJitLLVM::getContext(),
                                                     "isNa", fun);
                    auto notNaBr =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    nacheck(nin, arg->type, isNaBr, notNaBr);

                    builder.SetInsertPoint(isNaBr);
                    phi.addInput(c(NA_INTEGER));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(notNaBr);
                    auto cnv =
                        builder.CreateSelect(builder.CreateFCmpOEQ(c(0.0), nin),
                                             constant(R_FalseValue, t::Int),
                                             constant(R_TrueValue, t::Int));
                    phi.addInput(cnv);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                    res = phi();
                } else {
                    assert(r1 == Rep::i32);
                    res = load(arg);
                    if (!arg->type.isA(RType::logical)) {
                        res = builder.CreateSelect(
                            builder.CreateICmpEQ(res, c(NA_INTEGER)),
                            c(NA_LOGICAL),
                            builder.CreateSelect(
                                builder.CreateICmpEQ(res, c(0)),
                                constant(R_FalseValue, t::Int),
                                constant(R_TrueValue, t::Int)));
                    }
                }

                setVal(i, res);
                break;
            }

            case Tag::Force: {
                auto f = Force::Cast(i);
                auto arg = f->arg<0>().val();
                if (!f->effects.includes(Effect::Force)) {
                    if (!arg->type.maybePromiseWrapped()) {
                        setVal(i, load(arg, Rep::Of(i)));
                    } else {
                        auto res = depromise(arg);
                        setVal(i, res);
#ifdef ENABLE_SLOWASSERT
                        insn_assert(builder.CreateICmpNE(
                                        constant(R_UnboundValue, t::SEXP), res),
                                    "Expected evaluated promise");
#endif
                    }
                } else {
                    setVal(i, force(i, loadSxp(arg)));
                }
                break;
            }

            case Tag::LdFun: {
                auto ld = LdFun::Cast(i);
                auto res =
                    call(NativeBuiltins::get(NativeBuiltins::Id::ldfun),
                         {constant(ld->varName, t::SEXP), loadSxp(ld->env())});
                setVal(i, res);
                setVisible(1);
                break;
            }

            case Tag::MkArg: {
                auto p = MkArg::Cast(i);
                auto id = promMap.at(p->prom());
                auto exp = loadPromise(paramCode(), id.first);
                // if the env of a promise is elided we need to put a dummy env,
                // to forcePromise complaining.
                if (p->hasEnv()) {
                    auto e = loadSxp(p->env());
                    if (p->isEager()) {
                        setVal(i,
                               call(NativeBuiltins::get(
                                        NativeBuiltins::Id::createPromiseEager),
                                    {exp, e, loadSxp(p->eagerArg())}));
                    } else {
                        setVal(i, call(NativeBuiltins::get(
                                           NativeBuiltins::Id::createPromise),
                                       {exp, e}));
                    }
                } else {
                    if (p->isEager()) {
                        setVal(i, call(NativeBuiltins::get(
                                           NativeBuiltins::Id::
                                               createPromiseNoEnvEager),
                                       {exp, loadSxp(p->eagerArg())}));
                    } else {
                        setVal(i,
                               call(NativeBuiltins::get(
                                        NativeBuiltins::Id::createPromiseNoEnv),
                                    {exp}));
                    }
                }
                break;
            }

            case Tag::UpdatePromise: {
                auto val = loadSxp(i->arg(1).val());
                ensureShared(val);
                setPromsxpValue(loadSxp(i->arg(0).val()), val);
                break;
            }

            case Tag::LdVarSuper: {
                auto ld = LdVarSuper::Cast(i);

                llvm::Value* env;
                if (auto mk = MkEnv::Cast(i->env()))
                    env = loadSxp(mk->env());
                else
                    env = envsxpEnclos(loadSxp(ld->env()));

                auto res = call(NativeBuiltins::get(NativeBuiltins::Id::ldvar),
                                {constant(ld->varName, t::SEXP), env});
                res->setName(CHAR(PRINTNAME(ld->varName)));

                checkMissing(res);
                checkUnbound(res);
                setVal(i, res);
                break;
            }

            case Tag::LdDots:
            case Tag::LdVar: {
                auto maybeLd = LdVar::Cast(i);
                auto varName = maybeLd ? maybeLd->varName : R_DotsSymbol;

                auto env = MkEnv::Cast(i->env());
                if (LdFunctionEnv::Cast(i->env()))
                    env = myPromenv;

                bool maybeUnbound = true;
                llvm::Value* res;
                if (env && env->stub) {
                    auto e = loadSxp(env);
                    res = envStubGet(e, env->indexOf(varName), env->nLocals());
                    if (env->argNamed(varName).val() !=
                        UnboundValue::instance()) {
                        maybeUnbound = false;
                    } else {
                        res = createSelect2(
                            builder.CreateICmpEQ(
                                res, constant(R_UnboundValue, t::SEXP)),
                            // if unsassigned in the stub, fall through
                            [&]() {
                                return call(
                                    NativeBuiltins::get(
                                        NativeBuiltins::Id::ldvar),
                                    {constant(varName, t::SEXP),
                                     envStubGet(e, -1, env->nLocals())});
                            },
                            [&]() { return res; });
                    }
                } else if (bindingsCache.count(i->env())) {
                    auto phi = phiBuilder(t::SEXP);
                    auto offset = bindingsCache.at(i->env()).at(varName);

                    auto cachePtr =
                        builder.CreateGEP(bindingsCacheBase, c(offset));
                    llvm::Value* cache = builder.CreateLoad(cachePtr);

                    auto hit1 =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto hit2 =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto miss =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    // TODO: check active binding
                    builder.CreateCondBr(
                        builder.CreateICmpULE(
                            builder.CreatePtrToInt(cache, t::i64),
                            c(NativeBuiltins::bindingsCacheFails)),
                        miss, hit1, branchMostlyFalse);
                    builder.SetInsertPoint(hit1);
                    auto val = car(cache);
                    builder.CreateCondBr(
                        builder.CreateICmpEQ(val,
                                             constant(R_UnboundValue, t::SEXP)),
                        miss, hit2, branchMostlyFalse);
                    builder.SetInsertPoint(hit2);
                    ensureNamed(val);
                    phi.addInput(val);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(miss);
                    llvm::Value* res0 = call(
                        NativeBuiltins::get(NativeBuiltins::Id::ldvarCacheMiss),
                        {constant(varName, t::SEXP), loadSxp(i->env()),
                         cachePtr});
                    if (needsLdVarForUpdate.count(i))
                        res0 = cloneIfShared(res0);
                    phi.addInput(res0);
                    builder.CreateBr(done);
                    builder.SetInsertPoint(done);
                    res = phi();
                } else if (i->env() == Env::global()) {
                    res = call(
                        NativeBuiltins::get(NativeBuiltins::Id::ldvarGlobal),
                        {constant(varName, t::SEXP)});
                } else {
                    if (needsLdVarForUpdate.count(i)) {
                        res = call(
                            NativeBuiltins::get(
                                NativeBuiltins::Id::ldvarForUpdate),
                            {constant(varName, t::SEXP), loadSxp(i->env())});
                    } else {
                        res = nullptr;
                        if (auto e = Env::Cast(i->env())) {
                            if (e->rho == R_BaseNamespace ||
                                e->rho == R_BaseEnv) {
                                auto sym = constant(varName, t::SEXP);
                                res = symsxpValue(sym);
                                res = createSelect2(
                                    builder.CreateICmpNE(
                                        res, constant(R_UnboundValue, t::SEXP)),
                                    [&]() { return res; },
                                    [&]() {
                                        return call(
                                            NativeBuiltins::get(
                                                NativeBuiltins::Id::
                                                    ldvarGlobal),
                                            {constant(varName, t::SEXP)});
                                    });
                            }
                        }
                        if (!res) {
                            res = call(
                                NativeBuiltins::get(NativeBuiltins::Id::ldvar),
                                {constant(varName, t::SEXP),
                                 loadSxp(i->env())});
                        }
                    }
                }

                res->setName(CHAR(PRINTNAME(varName)));

                if (maybeLd) {
                    checkMissing(res);
                    if (maybeUnbound)
                        checkUnbound(res);
                }
                setVal(i, res);
                break;
            }

            case Tag::Extract1_1D: {
                auto extract = Extract1_1D::Cast(i);
                auto vector = loadSxp(extract->vec());

                bool fastcase = !extract->vec()->type.maybe(RType::vec) &&
                                vectorTypeSupport(extract->vec()) &&
                                extract->type.unboxable() &&
                                extract->idx()->type.isA(
                                    PirType::intReal().notObject().scalar());
                BasicBlock* done;
                auto res = phiBuilder(Rep::Of(i).toLlvm());

                if (fastcase) {
                    auto fallback =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    llvm::Value* vector = load(extract->vec());

                    if (Rep::Of(extract->vec()) == Rep::SEXP) {
                        auto hit2 = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);
                        builder.CreateCondBr(isAltrep(vector), fallback, hit2,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit2);

                        if (extract->vec()->type.maybeNotFastVecelt()) {
                            auto hit3 = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);
                            builder.CreateCondBr(fastVeceltOkNative(vector),
                                                 hit3, fallback,
                                                 branchMostlyTrue);
                            builder.SetInsertPoint(hit3);
                        }
                    }

                    llvm::Value* index =
                        computeAndCheckIndex(extract->idx(), vector, fallback);
                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);
                    res.addInput(convert(res0, i->type));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto env = constant(R_NilValue, t::SEXP);
                if (extract->hasEnv())
                    env = loadSxp(extract->env());
                auto idx = loadSxp(extract->idx());
                auto res0 =
                    call(NativeBuiltins::get(NativeBuiltins::Id::extract11),
                         {vector, idx, env, c(extract->srcIdx)});

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }

                setVal(i, res());
                break;
            }

            case Tag::Extract1_2D: {
                auto extract = Extract1_2D::Cast(i);

                bool fastcase = !extract->vec()->type.maybe(RType::vec) &&
                                extract->type.unboxable() &&
                                vectorTypeSupport(extract->vec()) &&
                                extract->idx1()->type.isA(
                                    PirType::intReal().notObject().scalar()) &&
                                extract->idx2()->type.isA(
                                    PirType::intReal().notObject().scalar());

                BasicBlock* done;
                auto res = phiBuilder(Rep::Of(i).toLlvm());

                if (fastcase) {
                    auto fallback =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    llvm::Value* vector = load(extract->vec());

                    if (Rep::Of(extract->vec()) == Rep::SEXP) {
                        auto hit2 = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);
                        builder.CreateCondBr(isAltrep(vector), fallback, hit2,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit2);

                        if (extract->vec()->type.maybeNotFastVecelt()) {
                            auto hit3 = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);
                            builder.CreateCondBr(fastVeceltOkNative(vector),
                                                 hit3, fallback,
                                                 branchMostlyTrue);
                            builder.SetInsertPoint(hit3);
                        }
                    }

                    auto ncol = builder.CreateZExt(
                        call(NativeBuiltins::get(
                                 NativeBuiltins::Id::matrixNcols),
                             {vector}),
                        t::i64);
                    auto nrow = builder.CreateZExt(
                        call(NativeBuiltins::get(
                                 NativeBuiltins::Id::matrixNrows),
                             {vector}),
                        t::i64);
                    llvm::Value* index1 = computeAndCheckIndex(
                        extract->idx1(), vector, fallback, nrow);
                    llvm::Value* index2 = computeAndCheckIndex(
                        extract->idx2(), vector, fallback, ncol);

                    llvm::Value* index =
                        builder.CreateMul(nrow, index2, "", true, true);
                    index = builder.CreateAdd(index, index1, "", true, true);

                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);

                    res.addInput(convert(res0, i->type));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto vector = loadSxp(extract->vec());
                auto idx1 = loadSxp(extract->idx1());
                auto idx2 = loadSxp(extract->idx2());
                auto res0 =
                    call(NativeBuiltins::get(NativeBuiltins::Id::extract12),
                         {vector, idx1, idx2, loadSxp(extract->env()),
                          c(extract->srcIdx)});

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::Extract2_1D: {
                auto extract = Extract2_1D::Cast(i);
                // TODO: Extend a fastPath for generic vectors.
                bool fastcase = vectorTypeSupport(extract->vec()) &&
                                extract->idx()->type.isA(
                                    PirType::intReal().notObject().scalar());

                BasicBlock* done;
                auto res = phiBuilder(Rep::Of(i).toLlvm());

                if (fastcase) {
                    auto fallback =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    llvm::Value* vector = load(extract->vec());

                    if (Rep::Of(extract->vec()) == Rep::SEXP) {
                        auto hit2 = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);

                        builder.CreateCondBr(isAltrep(vector), fallback, hit2,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit2);
                    }

                    llvm::Value* index =
                        computeAndCheckIndex(extract->idx(), vector, fallback);
                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);
                    res.addInput(convert(res0, i->type));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto irep = Rep::Of(extract->idx());
                llvm::Value* res0;

                if (irep != Rep::SEXP) {
                    NativeBuiltin getter;
                    if (irep == Rep::i32) {
                        getter =
                            NativeBuiltins::get(NativeBuiltins::Id::extract21i);
                    } else {
                        assert(irep == Rep::f64);
                        getter =
                            NativeBuiltins::get(NativeBuiltins::Id::extract21r);
                    }
                    auto vector = loadSxp(extract->vec());
                    res0 = call(getter,
                                {vector, load(extract->idx()),
                                 loadSxp(extract->env()), c(extract->srcIdx)});
                } else {
                    auto vector = loadSxp(extract->vec());
                    auto idx = loadSxp(extract->idx());
                    res0 =
                        call(NativeBuiltins::get(NativeBuiltins::Id::extract21),
                             {vector, idx, loadSxp(extract->env()),
                              c(extract->srcIdx)});
                }

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::Extract1_3D: {
                auto extract = Extract1_3D::Cast(i);
                auto vector = loadSxp(extract->vec());
                auto idx1 = loadSxp(extract->idx1());
                auto idx2 = loadSxp(extract->idx2());
                auto idx3 = loadSxp(extract->idx3());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto env = constant(R_NilValue, t::SEXP);
                if (extract->hasEnv())
                    env = loadSxp(extract->env());

                auto res =
                    call(NativeBuiltins::get(NativeBuiltins::Id::extract13),
                         {vector, idx1, idx2, idx3, env, c(extract->srcIdx)});
                setVal(i, res);

                break;
            }

            case Tag::Extract2_2D: {
                auto extract = Extract2_2D::Cast(i);

                bool fastcase = vectorTypeSupport(extract->vec()) &&
                                extract->idx1()->type.isA(
                                    PirType::intReal().notObject().scalar()) &&
                                extract->idx2()->type.isA(
                                    PirType::intReal().notObject().scalar());

                BasicBlock* done;
                auto res = phiBuilder(Rep::Of(i).toLlvm());

                if (fastcase) {
                    auto fallback =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto hit2 =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    llvm::Value* vector = load(extract->vec());

                    if (Rep::Of(extract->vec()) == Rep::SEXP) {
                        builder.CreateCondBr(isAltrep(vector), fallback, hit2,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit2);
                    }

                    auto ncol = builder.CreateZExt(
                        call(NativeBuiltins::get(
                                 NativeBuiltins::Id::matrixNcols),
                             {vector}),
                        t::i64);
                    auto nrow = builder.CreateZExt(
                        call(NativeBuiltins::get(
                                 NativeBuiltins::Id::matrixNrows),
                             {vector}),
                        t::i64);
                    llvm::Value* index1 = computeAndCheckIndex(
                        extract->idx1(), vector, fallback, nrow);
                    llvm::Value* index2 = computeAndCheckIndex(
                        extract->idx2(), vector, fallback, ncol);

                    llvm::Value* index =
                        builder.CreateMul(nrow, index2, "", true, true);
                    index = builder.CreateAdd(index, index1, "", true, true);

                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);

                    res.addInput(convert(res0, i->type));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto irep = Rep::Of(extract->idx1());
                llvm::Value* res0;

                if (irep != Rep::SEXP && Rep::Of(extract->idx2()) == irep) {
                    NativeBuiltin getter;
                    if (irep == Rep::i32) {
                        getter = NativeBuiltins::get(
                            NativeBuiltins::Id::extract22ii);
                    } else {
                        assert(irep == Rep::f64);
                        getter = NativeBuiltins::get(
                            NativeBuiltins::Id::extract22rr);
                    }

                    auto vector = loadSxp(extract->vec());
                    res0 = call(getter,
                                {vector, load(extract->idx1()),
                                 load(extract->idx2()), loadSxp(extract->env()),
                                 c(extract->srcIdx)});
                } else {

                    auto vector = loadSxp(extract->vec());
                    auto idx1 = loadSxp(extract->idx1());
                    auto idx2 = loadSxp(extract->idx2());
                    res0 =
                        call(NativeBuiltins::get(NativeBuiltins::Id::extract22),
                             {vector, idx1, idx2, loadSxp(extract->env()),
                              c(extract->srcIdx)});
                }

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::Subassign1_3D: {
                auto subAssign = Subassign1_3D::Cast(i);
                auto vector = loadSxp(subAssign->vec());
                auto val = loadSxp(subAssign->val());
                auto idx1 = loadSxp(subAssign->idx1());
                auto idx2 = loadSxp(subAssign->idx2());
                auto idx3 = loadSxp(subAssign->idx3());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto res =
                    call(NativeBuiltins::get(NativeBuiltins::Id::subassign13),
                         {vector, idx1, idx2, idx3, val,
                          loadSxp(subAssign->env()), c(subAssign->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::Subassign1_2D: {
                auto subAssign = Subassign1_2D::Cast(i);
                auto vector = loadSxp(subAssign->vec());
                auto val = loadSxp(subAssign->val());
                auto idx1 = loadSxp(subAssign->idx1());
                auto idx2 = loadSxp(subAssign->idx2());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto res =
                    call(NativeBuiltins::get(NativeBuiltins::Id::subassign12),
                         {vector, idx1, idx2, val, loadSxp(subAssign->env()),
                          c(subAssign->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::Subassign2_2D: {
                auto subAssign = Subassign2_2D::Cast(i);

                auto idx1Type = subAssign->idx1()->type;
                auto idx2Type = subAssign->idx2()->type;
                auto valType = subAssign->val()->type;
                auto vecType = subAssign->vec()->type;

                BasicBlock* done = nullptr;
                auto res = phiBuilder(Rep::Of(i).toLlvm());

                // Missing cases: store int into double matrix / store double
                // into int matrix
                auto fastcase =
                    idx1Type.isA(PirType::intReal().notObject().scalar()) &&
                    idx2Type.isA(PirType::intReal().notObject().scalar()) &&
                    valType.isScalar() && !vecType.maybeObj() &&
                    ((vecType.isA(PirType(RType::integer).orFastVecelt()) &&
                      valType.isA(RType::integer)) ||
                     (vecType.isA(PirType(RType::real).orFastVecelt()) &&
                      valType.isA(RType::real)));

                // Conversion from scalar to vector. eg. `a = 1; a[10] = 2`
                if (Rep::Of(subAssign->vec()) != Rep::SEXP &&
                    Rep::Of(i) == Rep::SEXP)
                    fastcase = false;

                if (fastcase) {
                    auto fallback =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    llvm::Value* vector = load(subAssign->vec());

                    auto ncol = builder.CreateZExt(
                        call(NativeBuiltins::get(
                                 NativeBuiltins::Id::matrixNcols),
                             {vector}),
                        t::i64);
                    auto nrow = builder.CreateZExt(
                        call(NativeBuiltins::get(
                                 NativeBuiltins::Id::matrixNrows),
                             {vector}),
                        t::i64);
                    llvm::Value* index1 = computeAndCheckIndex(
                        subAssign->idx1(), vector, fallback, nrow);
                    llvm::Value* index2 = computeAndCheckIndex(
                        subAssign->idx2(), vector, fallback, ncol);

                    auto val = load(subAssign->val());
                    if (Rep::Of(i) == Rep::SEXP) {
                        llvm::Value* index =
                            builder.CreateMul(nrow, index2, "", true, true);
                        index =
                            builder.CreateAdd(index, index1, "", true, true);

                        auto cont = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);
                        auto update = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto unchanged = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto cur = accessVector(vector, index, vecType);
                        auto same = Rep::Of(subAssign->val()) == Rep::f64
                                        ? builder.CreateFCmpUEQ(val, cur)
                                        : builder.CreateICmpEQ(val, cur);
                        auto res2 = phiBuilder(Rep::Of(i).toLlvm());
                        builder.CreateCondBr(same, unchanged, update);

                        builder.SetInsertPoint(unchanged);
                        res2.addInput(convert(vector, i->type));
                        builder.CreateBr(cont);

                        builder.SetInsertPoint(update);
                        auto cv = cloneIfShared(vector);
                        assignVector(cv, index, val, vecType);
                        res2.addInput(convert(cv, i->type));
                        builder.CreateBr(cont);

                        builder.SetInsertPoint(cont);
                        res.addInput(res2());
                    } else {
                        res.addInput(convert(val, i->type));
                    }

                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto idx1 = loadSxp(subAssign->idx1());
                auto idx2 = loadSxp(subAssign->idx2());

                llvm::Value* assign = nullptr;
                auto irep = Rep::Of(subAssign->idx1());
                auto vrep = Rep::Of(subAssign->val());
                // TODO: support unboxed logicals stored into e.g. vectors
                bool noConfusion = vrep != Rep::i32 ||
                                   subAssign->val()->type.isA(RType::integer);
                if (noConfusion && Rep::Of(subAssign->idx2()) == irep &&
                    irep != Rep::SEXP && vrep != Rep::SEXP &&
                    subAssign->val()->type.isA(subAssign->vec()->type)) {
                    NativeBuiltin setter;
                    if (irep == Rep::i32 && vrep == Rep::i32)
                        setter = NativeBuiltins::get(
                            NativeBuiltins::Id::subassign22iii);
                    else if (irep == Rep::f64 && vrep == Rep::i32)
                        setter = NativeBuiltins::get(
                            NativeBuiltins::Id::subassign22rri);
                    else if (irep == Rep::i32 && vrep == Rep::f64)
                        setter = NativeBuiltins::get(
                            NativeBuiltins::Id::subassign22iir);
                    else {
                        assert(irep == Rep::f64 && vrep == Rep::f64);
                        setter = NativeBuiltins::get(
                            NativeBuiltins::Id::subassign22rrr);
                    }

                    assign = call(
                        setter,
                        {loadSxp(subAssign->vec()), load(subAssign->idx1()),
                         load(subAssign->idx2()), load(subAssign->val()),
                         loadSxp(subAssign->env()), c(subAssign->srcIdx)});
                } else {
                    assign = call(
                        NativeBuiltins::get(NativeBuiltins::Id::subassign22),
                        {loadSxp(subAssign->vec()), idx1, idx2,
                         loadSxp(subAssign->val()), loadSxp(subAssign->env()),
                         c(subAssign->srcIdx)});
                }

                res.addInput(assign);
                if (fastcase) {
                    builder.CreateBr(done);
                    builder.SetInsertPoint(done);
                }
                setVal(i, res());

                break;
            }

            case Tag::Subassign1_1D: {
                auto subAssign = Subassign1_1D::Cast(i);

                // TODO: Extend a fastPath for generic vectors.
                // TODO: Support type conversions
                auto vecType = subAssign->vec()->type;
                auto valType = subAssign->val()->type;
                auto idxType = subAssign->idx()->type;

                BasicBlock* done = nullptr;
                auto resultRep = Rep::Of(i);
                auto res = phiBuilder(resultRep.toLlvm());

                // Missing cases: store int into double vect / store double into
                // int vect
                bool fastcase =
                    idxType.isA(PirType::intReal().notObject().scalar()) &&
                    valType.isScalar() && !vecType.maybeObj() &&
                    (vecType.isA(PirType(RType::vec).orFastVecelt()) ||
                     (vecType.isA(PirType(RType::integer).orFastVecelt()) &&
                      valType.isA(RType::integer)) ||
                     (vecType.isA(PirType(RType::real).orFastVecelt()) &&
                      valType.isA(RType::real)));
                // Conversion from scalar to vector. eg. `a = 1; a[10] = 2`
                if (Rep::Of(subAssign->vec()) != Rep::SEXP &&
                    Rep::Of(i) == Rep::SEXP)
                    fastcase = false;

                if (fastcase) {
                    auto fallback =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    llvm::Value* vector = load(subAssign->vec());
                    if (Rep::Of(subAssign->vec()) == Rep::SEXP) {
                        auto hit1 = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);
                        builder.CreateCondBr(isAltrep(vector), fallback, hit1,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit1);

                        if (vecType.maybeNotFastVecelt()) {
                            auto hit2 = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);
                            builder.CreateCondBr(fastVeceltOkNative(vector),
                                                 hit2, fallback,
                                                 branchMostlyTrue);
                            builder.SetInsertPoint(hit2);
                        }

                        vector = cloneIfShared(vector);
                    }

                    llvm::Value* index = computeAndCheckIndex(subAssign->idx(),
                                                              vector, fallback);

                    auto val = load(subAssign->val());
                    if (Rep::Of(i) == Rep::SEXP) {
                        assignVector(vector, index, val,
                                     subAssign->vec()->type);
                        res.addInput(convert(vector, i->type));
                    } else {
                        res.addInput(convert(val, i->type));
                    }

                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                llvm::Value* res0 =
                    call(NativeBuiltins::get(NativeBuiltins::Id::subassign11),
                         {loadSxp(subAssign->vec()), loadSxp(subAssign->idx()),
                          loadSxp(subAssign->val()), loadSxp(subAssign->env()),
                          c(subAssign->srcIdx)});

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::SetVecElt: {
                auto setVecElt = SetVecElt::Cast(i);
                auto vec = loadSxp(setVecElt->vec());
                auto val = loadSxp(setVecElt->val());
                auto idx = loadSxp(setVecElt->idx());

                auto res =
                    call(NativeBuiltins::get(NativeBuiltins::Id::setVecElt),
                         {vec, idx, val});
                setVal(i, res);
                break;
            }

            case Tag::Subassign2_1D: {
                auto subAssign = Subassign2_1D::Cast(i);

                // TODO: Extend a fastPath for generic vectors.
                // TODO: Support type conversions
                auto vecType = subAssign->vec()->type;
                auto valType = subAssign->val()->type;
                auto idxType = subAssign->idx()->type;

                BasicBlock* done = nullptr;
                auto resultRep = Rep::Of(i);
                auto res = phiBuilder(resultRep.toLlvm());

                // Missing cases: store int into double vect / store double into
                // int vect
                bool fastcase =
                    idxType.isA(PirType::intRealLgl().notObject().scalar()) &&
                    valType.isScalar() && !vecType.maybeObj() &&
                    ((vecType.isA(PirType(RType::integer).orFastVecelt()) &&
                      valType.isA(RType::integer)) ||
                     (vecType.isA(PirType(RType::real).orFastVecelt()) &&
                      valType.isA(RType::real)));
                // Conversion from scalar to vector. eg. `a = 1; a[10] = 2`
                if (Rep::Of(subAssign->vec()) != Rep::SEXP &&
                    Rep::Of(i) == Rep::SEXP)
                    fastcase = false;

                if (fastcase) {
                    auto fallback =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    llvm::Value* vector = load(subAssign->vec());
                    if (Rep::Of(subAssign->vec()) == Rep::SEXP) {
                        auto hit1 = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);
                        builder.CreateCondBr(isAltrep(vector), fallback, hit1,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit1);
                    }

                    llvm::Value* index = computeAndCheckIndex(subAssign->idx(),
                                                              vector, fallback);

                    auto val = load(subAssign->val());
                    if (Rep::Of(i) == Rep::SEXP) {

                        auto cont = BasicBlock::Create(PirJitLLVM::getContext(),
                                                       "", fun);
                        auto update = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto unchanged = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto cur = accessVector(vector, index, vecType);
                        auto same = Rep::Of(subAssign->val()) == Rep::f64
                                        ? builder.CreateFCmpUEQ(val, cur)
                                        : builder.CreateICmpEQ(val, cur);
                        auto res2 = phiBuilder(Rep::Of(i).toLlvm());
                        builder.CreateCondBr(same, unchanged, update);

                        builder.SetInsertPoint(unchanged);
                        res2.addInput(convert(vector, i->type));
                        builder.CreateBr(cont);

                        builder.SetInsertPoint(update);
                        auto cv = cloneIfShared(vector);
                        assignVector(cv, index, val, vecType);
                        res2.addInput(convert(cv, i->type));
                        builder.CreateBr(cont);

                        builder.SetInsertPoint(cont);
                        res.addInput(res2());
                    } else {
                        res.addInput(convert(val, i->type));
                    }

                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                llvm::Value* res0 = nullptr;
                auto irep = Rep::Of(subAssign->idx());
                auto vrep = Rep::Of(subAssign->val());
                // TODO: support unboxed logicals stored into e.g. vectors
                bool noConfusion = vrep != Rep::i32 ||
                                   subAssign->val()->type.isA(RType::integer);
                if (noConfusion && irep != Rep::SEXP && vrep != Rep::SEXP &&
                    subAssign->val()->type.isA(subAssign->vec()->type)) {
                    NativeBuiltin setter;
                    if (irep == Rep::i32 && vrep == Rep::i32)
                        setter = NativeBuiltins::get(
                            NativeBuiltins::Id::subassign21ii);
                    else if (irep == Rep::f64 && vrep == Rep::i32)
                        setter = NativeBuiltins::get(
                            NativeBuiltins::Id::subassign21ri);
                    else if (irep == Rep::i32 && vrep == Rep::f64)
                        setter = NativeBuiltins::get(
                            NativeBuiltins::Id::subassign21ir);
                    else {
                        assert(irep == Rep::f64 && vrep == Rep::f64);
                        setter = NativeBuiltins::get(
                            NativeBuiltins::Id::subassign21rr);
                    }

                    res0 =
                        call(setter,
                             {loadSxp(subAssign->vec()), load(subAssign->idx()),
                              load(subAssign->val()), loadSxp(subAssign->env()),
                              c(subAssign->srcIdx)});
                } else {
                    res0 = call(
                        NativeBuiltins::get(NativeBuiltins::Id::subassign21),
                        {loadSxp(subAssign->vec()), loadSxp(subAssign->idx()),
                         loadSxp(subAssign->val()), loadSxp(subAssign->env()),
                         c(subAssign->srcIdx)});
                }

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::StVar: {
                auto st = StVar::Cast(i);
                auto environment = MkEnv::Cast(st->env());
                if (LdFunctionEnv::Cast(st->env()))
                    environment = myPromenv;

                if (environment && environment->stub) {
                    auto idx = environment->indexOf(st->varName);
                    auto e = loadSxp(environment);
                    BasicBlock* done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto cur = envStubGet(e, idx, environment->nLocals());

                    if (Rep::Of(st->val()) != Rep::SEXP) {
                        auto fastcase = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto fallback = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);

                        auto expected =
                            Rep::Of(st->val()) == Rep::i32 ? INTSXP : REALSXP;
                        auto reuse =
                            builder.CreateAnd(isSimpleScalar(cur, expected),
                                              builder.CreateNot(shared(cur)));
                        builder.CreateCondBr(reuse, fastcase, fallback,
                                             branchMostlyTrue);

                        builder.SetInsertPoint(fastcase);
                        auto store =
                            vectorPositionPtr(cur, c(0), st->val()->type);
                        builder.CreateStore(load(st->val()), store);
                        builder.CreateBr(done);

                        builder.SetInsertPoint(fallback);
                    }

                    auto val = loadSxp(st->val());
                    if (Rep::Of(st->val()) == Rep::SEXP) {
                        if (!st->isStArg) {
                            auto same = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);
                            auto different = BasicBlock::Create(
                                PirJitLLVM::getContext(), "", fun);
                            builder.CreateCondBr(builder.CreateICmpEQ(val, cur),
                                                 same, different);

                            builder.SetInsertPoint(same);
                            ensureNamed(val);
                            envStubSetNotMissing(e, idx);
                            builder.CreateBr(done);

                            builder.SetInsertPoint(different);
                        }
                        incrementNamed(val);
                        envStubSet(e, idx, val, environment->nLocals(),
                                   !st->isStArg);
                    } else {
                        ensureNamed(val);
                        envStubSet(e, idx, val, environment->nLocals(),
                                   !st->isStArg);
                    }

                    builder.CreateBr(done);
                    builder.SetInsertPoint(done);
                    break;
                }

                auto pirVal = st->arg<0>().val();
                bool integerValueCase = Rep::Of(pirVal) == Rep::i32 &&
                                        pirVal->type.isA(RType::integer);
                bool realValueCase = Rep::Of(pirVal) == Rep::f64 &&
                                     pirVal->type.isA(RType::real);
                auto setter = NativeBuiltins::get(NativeBuiltins::Id::stvar);
                if (st->isStArg)
                    setter = NativeBuiltins::get(NativeBuiltins::Id::starg);
                if (!st->isStArg && integerValueCase)
                    setter = NativeBuiltins::get(NativeBuiltins::Id::stvari);
                if (!st->isStArg && realValueCase)
                    setter = NativeBuiltins::get(NativeBuiltins::Id::stvarr);
                bool unboxed =
                    setter.llvmSignature->getFunctionParamType(1) != t::SEXP;

                if (bindingsCache.count(environment)) {
                    auto offset = bindingsCache.at(environment).at(st->varName);
                    auto cachePtr =
                        builder.CreateGEP(bindingsCacheBase, c(offset));
                    llvm::Value* cache = builder.CreateLoad(cachePtr);

                    auto hit1 =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto hit2 =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto hit3 =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto identical =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto miss =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                    auto done =
                        BasicBlock::Create(PirJitLLVM::getContext(), "", fun);

                    builder.CreateCondBr(
                        builder.CreateICmpULE(
                            builder.CreatePtrToInt(cache, t::i64),
                            c(NativeBuiltins::bindingsCacheFails)),
                        miss, hit1, branchMostlyFalse);

                    builder.SetInsertPoint(hit1);
                    auto val = car(cache);
                    builder.CreateCondBr(
                        builder.CreateICmpEQ(val,
                                             constant(R_UnboundValue, t::SEXP)),
                        miss, hit2, branchMostlyFalse);

                    builder.SetInsertPoint(hit2);

                    llvm::Value* newVal = nullptr;
                    if (integerValueCase || realValueCase) {
                        auto hitUnbox = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto hitUnbox2 = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto fallbackUnbox = BasicBlock::Create(
                            PirJitLLVM::getContext(), "", fun);
                        auto storeType =
                            integerValueCase ? RType::integer : RType::real;
                        auto isScalarType = isSimpleScalar(
                            val, integerValueCase ? INTSXP : REALSXP);
                        auto notShared = builder.CreateNot(shared(val));
                        builder.CreateCondBr(
                            builder.CreateAnd(isScalarType, notShared),
                            hitUnbox, fallbackUnbox);

                        builder.SetInsertPoint(hitUnbox);
                        auto newValNative = load(pirVal);
                        auto oldVal = accessVector(val, c(0), storeType);
                        auto same =
                            integerValueCase
                                ? builder.CreateICmpEQ(newValNative, oldVal)
                                : builder.CreateFCmpUEQ(newValNative, oldVal);
                        builder.CreateCondBr(same, identical, hitUnbox2);

                        builder.SetInsertPoint(hitUnbox2);
                        assignVector(val, c(0), newValNative, storeType);
                        builder.CreateBr(done);

                        builder.SetInsertPoint(fallbackUnbox);
                        newVal = loadSxp(pirVal);
                        builder.CreateBr(hit3);
                    } else {
                        newVal = loadSxp(pirVal);
                        builder.CreateCondBr(builder.CreateICmpEQ(val, newVal),
                                             identical, hit3,
                                             branchMostlyFalse);
                    }

                    builder.SetInsertPoint(hit3);
                    incrementNamed(newVal);
                    assert(cache->getType() == t::SEXP);
                    assert(newVal->getType() == t::SEXP);
                    setCar(cache, newVal);
                    // Missingness needs not be updated here, because the cache
                    // hit on the second store, i.e. after a slow-case starg
                    // builtin, which already updated missingness
                    builder.CreateBr(done);

                    builder.SetInsertPoint(identical);
                    // In the fast case (where the value is not updated) we
                    // still need to ensure it is named.
                    ensureNamed(val);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(miss);
                    llvm::Value* theValue =
                        unboxed ? load(pirVal) : loadSxp(pirVal);
                    call(setter, {constant(st->varName, t::SEXP), theValue,
                                  loadSxp(st->env())});
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);

                } else {
                    llvm::Value* theValue =
                        unboxed ? load(pirVal) : loadSxp(pirVal);
                    call(setter, {constant(st->varName, t::SEXP), theValue,
                                  loadSxp(st->env())});
                }
                break;
            }

            case Tag::StVarSuper: {
                auto st = StVarSuper::Cast(i);
                auto environment = MkEnv::Cast(st->env());
                if (environment) {
                    auto parent = MkEnv::Cast(environment->lexicalEnv());
                    if (environment->stub || (parent && parent->stub)) {
                        call(
                            NativeBuiltins::get(NativeBuiltins::Id::stvarSuper),
                            {constant(st->varName, t::SEXP),
                             loadSxp(st->arg<0>().val()), loadSxp(st->env())});
                        break;
                    }
                }

                // In case we statically knew the parent PIR already converted
                // super assigns to standard stores
                call(NativeBuiltins::get(NativeBuiltins::Id::defvar),
                     {constant(st->varName, t::SEXP),
                      loadSxp(st->arg<0>().val()), loadSxp(st->env())});
                break;
            }

            case Tag::Missing: {
                assert(Rep::Of(i) == Rep::i32);
                auto missing = Missing::Cast(i);
                setVal(i,
                       call(NativeBuiltins::get(NativeBuiltins::Id::isMissing),
                            {constant(missing->varName, t::SEXP),
                             loadSxp(i->env())}));
                break;
            }

            case Tag::ChkMissing: {
                auto arg = i->arg(0).val();
                if (Rep::Of(arg) == Rep::SEXP)
                    checkMissing(loadSxp(arg));
                setVal(i, load(arg, arg->type.notMissing(), Rep::Of(i)));
                break;
            }

            case Tag::ChkFunction: {
                auto arg = loadSxp(i->arg(0).val());
                call(NativeBuiltins::get(NativeBuiltins::Id::chkfun),
                     {constant(Rf_install(ChkFunction::Cast(i)->name().c_str()),
                               t::SEXP),
                      arg});
                setVal(i, arg);
                break;
            }

            case Tag::ColonInputEffects: {
                auto a = i->arg(0).val();
                auto b = i->arg(1).val();
                if (Rep::Of(a) == Rep::SEXP || Rep::Of(b) == Rep::SEXP) {
                    setVal(i, call(NativeBuiltins::get(
                                       NativeBuiltins::Id::colonInputEffects),
                                   {loadSxp(a), loadSxp(b), c(i->srcIdx)}));
                    break;
                }

                // Native version of colonInputEffects
                auto checkRhs = [&]() -> llvm::Value* {
                    if (Rep::Of(b) == Rep::f64) {
                        auto ld = builder.CreateFPToSI(load(b), t::i64);
                        return builder.CreateICmpNE(ld, c(INT_MAX, 64));
                    }
                    assert(Rep::Of(b) == Rep::i32);
                    return builder.CreateICmpNE(load(b), c(INT_MAX));
                };

                auto sequenceIsReal =
                    Rep::Of(a) == Rep::f64
                        ? builder.CreateNot(checkDoubleToInt(load(a), a->type))
                        : builder.getFalse();

                auto res = createSelect2(
                    sequenceIsReal,
                    [&]() -> llvm::Value* {
                        // If the lhs is truly real, then the sequence is
                        // real and we always go into fastcase
                        return builder.getTrue();
                    },
                    [&]() -> llvm::Value* {
                        auto sequenceIsAmbiguous =
                            Rep::Of(a) == Rep::f64
                                ? builder.CreateNot(
                                      checkDoubleToInt(load(b), b->type))
                                : builder.getFalse();

                        return createSelect2(
                            sequenceIsAmbiguous,
                            [&]() -> llvm::Value* {
                                // If the lhs is integer and the rhs is
                                // real we don't support it as fastcase
                                return builder.getFalse();
                            },
                            // This is the case where both sides are int-ish,
                            // we need to check for overflow here.
                            checkRhs);
                    });

                setVal(i, builder.CreateZExt(res, t::i32));
                break;
            }

            case Tag::ColonCastLhs: {
                auto a = i->arg(0).val();
                if (Rep::Of(a) == Rep::SEXP || Rep::Of(i) == Rep::SEXP) {
                    setVal(i, call(NativeBuiltins::get(
                                       NativeBuiltins::Id::colonCastLhs),
                                   {loadSxp(a)}));
                    break;
                }
                auto ld = load(a);

                auto naBr =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                auto contBr =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                nacheck(ld, a->type, naBr, contBr);

                builder.SetInsertPoint(naBr);
                auto msg = builder.CreateGlobalString("NA/NaN argument");
                call(NativeBuiltins::get(NativeBuiltins::Id::error),
                     {builder.CreateInBoundsGEP(msg, {c(0), c(0)})});
                builder.CreateUnreachable();

                builder.SetInsertPoint(contBr);
                setVal(i, convert(ld, i->type));
                break;
            }

            case Tag::ColonCastRhs: {
                auto a = i->arg(0).val();
                auto b = i->arg(1).val();
                if (Rep::Of(a) == Rep::SEXP || Rep::Of(b) == Rep::SEXP ||
                    Rep::Of(i) == Rep::SEXP) {
                    setVal(i, call(NativeBuiltins::get(
                                       NativeBuiltins::Id::colonCastRhs),
                                   {loadSxp(a), loadSxp(b)}));
                    break;
                }

                auto ldb = load(b);

                auto naBr =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                auto contBr =
                    BasicBlock::Create(PirJitLLVM::getContext(), "", fun);
                nacheck(ldb, b->type, naBr, contBr);

                builder.SetInsertPoint(naBr);
                auto msg = builder.CreateGlobalString("NA/NaN argument");
                call(NativeBuiltins::get(NativeBuiltins::Id::error),
                     {builder.CreateInBoundsGEP(msg, {c(0), c(0)})});
                builder.CreateUnreachable();

                builder.SetInsertPoint(contBr);

                // This is such a mess, but unfortunately a more or less literal
                // translation of the corresponding bytecode...

                if (ldb->getType() != t::Double)
                    ldb = builder.CreateSIToFP(ldb, t::Double);
                auto lda = load(a);
                if (lda->getType() != t::Double)
                    lda = builder.CreateSIToFP(lda, t::Double);

                auto increasing = builder.CreateFCmpOLE(lda, ldb);
                auto upwards = [&]() {
                    return builder.CreateFAdd(
                        lda, builder.CreateFAdd(
                                 builder.CreateIntrinsic(
                                     Intrinsic::floor, {ldb->getType()},
                                     {builder.CreateFSub(ldb, lda)}),
                                 c(1.0)));
                };
                auto downwards = [&]() {
                    return builder.CreateFSub(
                        builder.CreateFSub(
                            lda, builder.CreateIntrinsic(
                                     Intrinsic::floor, {ldb->getType()},
                                     {builder.CreateFSub(lda, ldb)})),
                        c(1.0));
                };

                auto res = createSelect2(increasing, upwards, downwards);
                setVal(i, convert(res, i->type));
                break;
            }

            case Tag::Names:
                setVal(i, call(NativeBuiltins::get(NativeBuiltins::Id::names),
                               {loadSxp(i->arg(0).val())}));
                break;

            case Tag::SetNames:
                setVal(
                    i,
                    call(NativeBuiltins::get(NativeBuiltins::Id::setNames),
                         {loadSxp(i->arg(0).val()), loadSxp(i->arg(1).val())}));
                break;

            case Tag::Length: {
                assert(Rep::Of(i) == Rep::i32);

                auto a = loadSxp(i->arg(0).val());
                auto callLengthBuiltin = [&]() {
                    return call(NativeBuiltins::get(NativeBuiltins::Id::length),
                                {a});
                };
                llvm::Value* r;
                if (vectorTypeSupport(i->arg(0).val())) {
                    r = createSelect2(isAltrep(a), callLengthBuiltin, [&]() {
                        return builder.CreateTrunc(vectorLength(a), t::Int);
                    });
                } else {
                    r = callLengthBuiltin();
                }
                setVal(i, r);
                break;
            }

            case Tag::FrameState:
                break;

            case Tag::Unreachable:
                builder.CreateUnreachable();
                break;

            case Tag::Int3:
                assert(false);
                break;

            case Tag::_UNUSED_:
                assert(false && "Invalid instruction tag");
                break;

            case Tag::Checkpoint:
            case Tag::Assume:
                assert(false && "Expected scheduled deopt");
                break;

#define V(Value) case Tag::Value:
                COMPILER_VALUES(V)
#undef V
                assert(false && "Values should not occur in instructions");
                break;
            }

            // Here we directly access the variable to bypass liveness
            // checks when loading the variable. This is ok, since this is
            // the current instruction and we have already written to it...
            assert(*currentInstr == i);
            assert(!variables_.count(i) || variables_.at(i).initialized);
            ++currentInstr;
            if (!Phi::Cast(i))
                ensureNamedIfNeeded(i);

            // For OSR-in try to collect more typefeedback for the part of the
            // code that was not yet executed.
            if (cls->isContinuation() && Rep::Of(i) == Rep::SEXP &&
                variables_.count(i) &&
                !cls->isContinuation()->continuationContext->asDeoptContext()) {
                if (i->hasTypeFeedback() &&
                    i->typeFeedback().feedbackOrigin.pc()) {
                    call(NativeBuiltins::get(
                             NativeBuiltins::Id::recordTypefeedback),
                         {c((void*)i->typeFeedback().feedbackOrigin.pc()),
                          c((void*)i->typeFeedback().feedbackOrigin.srcCode()),
                          load(i)});
                }
                if (i->hasCallFeedback()) {
                    assert(i->callFeedback().feedbackOrigin.pc());
                    call(NativeBuiltins::get(
                             NativeBuiltins::Id::recordTypefeedback),
                         {c((void*)i->callFeedback().feedbackOrigin.pc()),
                          c((void*)i->callFeedback().feedbackOrigin.srcCode()),
                          load(i)});
                }
            }

            if (Parameter::RIR_CHECK_PIR_TYPES > 0 && !i->type.isVoid() &&
                (variables_.count(i) || LdArg::Cast(i))) {
                if (Rep::Of(i) == Rep::SEXP || LdArg::Cast(i)) {
                    if (i->type != RType::expandedDots &&
                        i->type != NativeType::context && !CastType::Cast(i)) {
                        static std::vector<std::string> leaky;
                        const char* msg = nullptr;
                        static const char* defaultMsg = "";
                        if (Parameter::RIR_CHECK_PIR_TYPES > 1) {
                            std::stringstream str;
                            i->printRecursive(str, 4);
                            str << cls->context() << "\n";
                            leaky.push_back(str.str());
                            msg = leaky.back().c_str();
                        } else {
                            msg = defaultMsg;
                        }
                        call(NativeBuiltins::get(NativeBuiltins::Id::checkType),
                             {loadSxp(i), c((unsigned long)i->type.serialize()),
                              convertToPointer(msg, t::i8, true)});
                    }
                }
#ifdef ENABLE_SLOWASSERT
                if (i->type.isA(PirType::test())) {
                    auto ok =
                        builder.CreateOr(builder.CreateICmpEQ(load(i), c(0)),
                                         builder.CreateICmpEQ(load(i), c(1)));
                    insn_assert(ok, "Variable of type test has invalid range");
                }
#endif
            }
        }

        // Copy of phi input values
        for (auto i : *bb) {
            if (phis.count(i)) {
                auto phi = phis.at(i);
                if (deadMove(i, phi))
                    continue;
                auto r = Rep::Of(phi->type);
                auto inpv = load(i, r);
                ensureNamedIfNeeded(phi, inpv);
                if (LLVMDebugInfo() && diVariables_.count(phi)) {
                    DIB->insertDbgValueIntrinsic(
                        inpv, diVariables_[phi], DIB->createExpression(),
                        builder.getCurrentDebugLocation(),
                        builder.GetInsertBlock());
                }
                updateVariable(phi, inpv);
            }
        }

        // Clear the temp-protected space on the stack after every
        // instruction to catch GC errors early
        static bool CLEAR_TEMPS = getenv("PIR_TEST_CLEAR_TEMPS") &&
                                  *getenv("PIR_TEST_CLEAR_TEMPS") == '1';
        if (CLEAR_TEMPS && numTemps > 0 &&
            (builder.GetInsertBlock()->empty() ||
             !builder.GetInsertBlock()->back().isTerminator())) {
            auto pos = builder.CreateGEP(basepointer, c(numLocals, 32));
            builder.CreateMemSet(pos, c(0, 8), c(numTemps, 32),
                                 MaybeAlign(alignof(R_bcstack_t)));
        }
        numTemps = 0;

        if (bb->isJmp())
            builder.CreateBr(getBlock(bb->next()));

        for (auto suc : bb->successors())
            blockInPushContext[suc] = inPushContext;
    });

    // Delayed insertion of the branch, so we can still easily add instructions
    // to the entry block while compiling
    builder.SetInsertPoint(entryBlock);
    int sz = numLocals + maxTemps;
    if (sz > 0) {
        if (LLVMDebugInfo())
            DI->clearLocation(builder);
        incStack(sz, true);
    }
    builder.CreateBr(getBlock(code->entry));

    for (auto bb : exitBlocks) {
        auto pos = bb->end();
        pos--;
        builder.SetInsertPoint(bb, pos);
        if (LLVMDebugInfo())
            DI->clearLocation(builder);
        decStack(sz + additionalStackSlots);
    }

    if (RuntimeProfiler::enabled()) {
        std::unordered_set<rir::Code*> codes;
        std::unordered_map<size_t, const pir::TypeFeedback&> variableMapping;
#ifdef DEBUG_REGISTER_MAP
    std::unordered_set<size_t> usedSlots;
#endif
    for (auto& var : variables_) {
        auto i = var.first;
        if (Rep::Of(i) != Rep::SEXP)
            continue;
        if (!i->typeFeedback().feedbackOrigin.pc())
            continue;
        if (!var.second.initialized)
            continue;
        if (var.second.stackSlot < PirTypeFeedback::MAX_SLOT_IDX) {
            codes.insert(i->typeFeedback().feedbackOrigin.srcCode());
            variableMapping.emplace(var.second.stackSlot, i->typeFeedback());
#ifdef DEBUG_REGISTER_MAP
            assert(!usedSlots.count(var.second.stackSlot));
            usedSlots.insert(var.second.stackSlot);
#endif
        }
        if (variableMapping.size() == PirTypeFeedback::MAX_SLOT_IDX)
            break;
    }
    if (!variableMapping.empty()) {
        pirTypeFeedback = PirTypeFeedback::New(codes, variableMapping);
        p_(pirTypeFeedback->container());
#ifdef DEBUG_REGISTER_MAP
        for (auto m : variableMapping) {
            auto origin = registerMap->getOriginOfSlot(m.first);
            assert(origin == m.second.second);
        }
#endif
    }
    }
}

size_t Parameter::DEOPT_CHAOS =
    getenv("PIR_DEOPT_CHAOS") ? atoi(getenv("PIR_DEOPT_CHAOS")) : 0;
bool Parameter::DEOPT_CHAOS_NO_RETRIGGER =
    getenv("PIR_DEOPT_CHAOS_NO_RETRIGGER")
        ? atoi(getenv("PIR_DEOPT_CHAOS_NO_RETRIGGER"))
        : 0;
int Parameter::DEOPT_CHAOS_SEED =
    getenv("PIR_DEOPT_CHAOS_SEED") ? atoi(getenv("PIR_DEOPT_CHAOS_SEED")) : 42;

} // namespace pir
} // namespace rir
