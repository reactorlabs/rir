#include "jit_llvm.h"

#include "types_llvm.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"

#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "builtins.h"
#include "compiler/analysis/liveness.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include "interpreter/LazyEnvironment.h"
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

LLVMContext& C = rir::pir::JitLLVM::C;

extern "C" size_t R_NSize;
extern "C" size_t R_NodesInUse;

struct Representation {
    enum Type {
        Bottom,
        Integer,
        Real,
        Sexp,
    };
    Representation() : t(Bottom) {}
    // cppcheck-suppress noExplicitConstructor
    Representation(Type t) : t(t) {}
    explicit Representation(llvm::Type* jt) {
        if (jt == t::Void)
            t = Bottom;
        else if (jt == t::Int)
            t = Integer;
        else if (jt == t::Double)
            t = Real;
        else if (jt == t::SEXP)
            t = Sexp;
        else {
            jt->print(outs());
            outs() << "\n";
            assert(false);
        }
    }
    Type t;
    operator llvm::Type*() {
        switch (t) {
        case Representation::Bottom:
            return t::Void;
        case Representation::Integer:
            return t::Int;
        case Representation::Real:
            return t::Double;
        case Representation::Sexp:
            return t::SEXP;
        }
        assert(false);
        return nullptr;
    }
    bool merge(const Representation& other) {
        if (t < other.t) {
            t = other.t;
            return true;
        }
        return false;
    }
    bool operator==(const Representation& other) const { return t == other.t; }
    bool operator!=(const Representation& other) const {
        return !(*this == other);
    }
};

static Representation representationOf(PirType t) {
    // Combined types like integer|real cannot be unbox, since we do not know
    // how to re-box again.
    if (t.isA(NativeType::test))
        return Representation::Integer;
    if (t.isA(PirType(RType::logical).scalar().notObject()))
        return Representation::Integer;
    if (t.isA(PirType(RType::integer).scalar().notObject()))
        return Representation::Integer;
    if (t.isA(PirType(RType::real).scalar().notObject()))
        return Representation::Real;
    return Representation::Sexp;
}

static Representation representationOf(Value* v) {
    return representationOf(v->type);
}

class LowerFunctionLLVM {

    ClosureVersion* cls;
    Code* code;
    const std::unordered_map<Promise*, unsigned>& promMap;
    const std::unordered_set<Instruction*>& needsEnsureNamed;
    IRBuilder<> builder;
    MDBuilder MDB;
    CFG cfg;
    LivenessIntervals liveness;
    size_t numLocals;
    llvm::Value* basepointer = nullptr;
    llvm::Value* constantpool = nullptr;

  public:
    llvm::Function* fun;

    LowerFunctionLLVM(const std::string& name, ClosureVersion* cls, Code* code,
                      const std::unordered_map<Promise*, unsigned>& promMap,
                      const std::unordered_set<Instruction*>& needsEnsureNamed)
        : cls(cls), code(code), promMap(promMap),
          needsEnsureNamed(needsEnsureNamed), builder(C), MDB(C), cfg(code),
          liveness(code->nextBBId, cfg), numLocals(liveness.maxLive) {

        std::vector<Type*> args(
            {t::voidPtr, t::voidPtr, t::stackCellPtr, t::SEXP, t::SEXP});
        FunctionType* signature = FunctionType::get(t::SEXP, args, false);

        fun = JitLLVM::declare(name, signature);
        // prevent Wunused
        this->cls->size();
        this->promMap.size();
    }

    static llvm::Value* convertToPointer(void* what, Type* ty = t::voidPtr) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(C, llvm::APInt(64, (std::uint64_t)what)),
            ty);
    }
    static llvm::Value* convertToPointer(SEXP what) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(C, llvm::APInt(64, (std::uint64_t)what)),
            t::SEXP);
    }

    std::unordered_map<Value*, llvm::Value*> valueMap;

    llvm::Value* constant(SEXP co, llvm::Type* needed);
    llvm::Value* nodestackPtr();
    llvm::Value* stack(int i);
    void stack(const std::vector<llvm::Value*>& args);
    void setLocal(size_t i, llvm::Value* v);
    void incStack(int i, bool zero);
    void decStack(int i);
    llvm::Value* withCallFrame(Instruction* i, const std::vector<Value*>& args,
                               const std::function<llvm::Value*()>& theCall);
    llvm::Value* load(Instruction* pos, Value* v, Representation r);
    llvm::Value* load(Instruction* pos, Value* v);
    llvm::Value* loadSxp(Instruction* pos, Value* v);
    llvm::Value* loadSame(Instruction* pos, Value* v);
    llvm::Value* load(Instruction* pos, Value* val, PirType type,
                      Representation needed);
    llvm::Value* dataPtr(llvm::Value* v);
    llvm::Value* unboxIntLgl(llvm::Value* v);
    llvm::Value* unboxInt(llvm::Value* v);
    llvm::Value* unboxLgl(llvm::Value* v);
    llvm::Value* unboxReal(llvm::Value* v);
    llvm::Value* unboxRealIntLgl(llvm::Value* v);

    void writeBarrier(llvm::Value* x, llvm::Value* y, std::function<void()> no,
                      std::function<void()> yes);
    llvm::Value* envStubGet(llvm::Value* x, int i);
    void envStubSet(llvm::Value* x, int i, llvm::Value* y);

    void setVisible(int i);

    std::array<std::string, 5> argNames = {
        {"code", "ctx", "args", "env", "closure"}};
    std::vector<llvm::Value*> args;
    llvm::Value* paramCode() { return args[0]; }
    llvm::Value* paramCtx() { return args[1]; }
    llvm::Value* paramArgs() { return args[2]; }
    llvm::Value* paramEnv() { return args[3]; }
    llvm::Value* paramClosure() { return args[4]; }

    llvm::Value* c(void* i) {
        return llvm::ConstantInt::get(C, APInt(64, (intptr_t)i));
    }

    llvm::Value* c(unsigned long i, int bs = 64) {
        return llvm::ConstantInt::get(C, APInt(bs, i));
    }

    llvm::Value* c(long i, int bs = 64) {
        return llvm::ConstantInt::get(C, APInt(bs, i));
    }

    llvm::Value* c(unsigned i, int bs = 32) {
        return llvm::ConstantInt::get(C, APInt(bs, i));
    }

    llvm::Value* c(int i, int bs = 32) {
        return llvm::ConstantInt::get(C, APInt(bs, i));
    }

    llvm::Value* c(double d) {
        return llvm::ConstantFP::get(C, llvm::APFloat(d));
    }

    llvm::Value* argument(int i);
    void setVal(Instruction* i, llvm::Value* val);

    llvm::Value* isExternalsxp(llvm::Value* v, uint32_t magic);
    void checkSexptype(llvm::Value* v, const std::vector<SEXPTYPE>& types);
    void checkIsSexp(llvm::Value* v, const std::string& msg = "");
    llvm::Value* sexptype(llvm::Value* v);
    llvm::Value* tag(llvm::Value* v);
    llvm::Value* car(llvm::Value* v);
    void setCar(llvm::Value* x, llvm::Value* y);
    llvm::Value* isObj(llvm::Value*);
    llvm::Value* sxpinfoPtr(llvm::Value*);

    void ensureNamed(llvm::Value* v);
    void incrementNamed(llvm::Value* v);
    void nacheck(llvm::Value* v, BasicBlock* isNa, BasicBlock* notNa = nullptr);
    void checkMissing(llvm::Value* v);
    void checkUnbound(llvm::Value* v);
    llvm::Value* call(const NativeBuiltin& builtin,
                      const std::vector<llvm::Value*>& args);
    llvm::Value* box(Instruction* pos, llvm::Value* v, PirType t);
    llvm::Value* boxInt(Instruction* pos, llvm::Value* v);
    llvm::Value* boxReal(Instruction* pos, llvm::Value* v);
    llvm::Value* boxLgl(Instruction* pos, llvm::Value* v);
    void gcSafepoint(Instruction* i, size_t required, bool protectArgs);
    llvm::Value* depromise(llvm::Value* arg);
    void insn_assert(llvm::Value* v, const char* msg);

    llvm::Value* force(Instruction* i, llvm::Value* arg);

    void compileBinop(
        Instruction* i,
        std::function<llvm::Value*(llvm::Value*, llvm::Value*)> intInsert,
        std::function<llvm::Value*(llvm::Value*, llvm::Value*)> fpInsert,
        BinopKind kind);
    void compileRelop(
        Instruction* i,
        std::function<llvm::Value*(llvm::Value*, llvm::Value*)> intInsert,
        std::function<llvm::Value*(llvm::Value*, llvm::Value*)> fpInsert,
        BinopKind kind);

    bool success = true;
    bool tryCompile();
};

void LowerFunctionLLVM::setVisible(int i) {
    builder.CreateStore(c(i), convertToPointer(&R_Visible, t::IntPtr));
}

llvm::Value* LowerFunctionLLVM::force(Instruction* i, llvm::Value* arg) {

    auto isProm = BasicBlock::Create(C, "isProm", fun);
    auto needsEval = BasicBlock::Create(C, "needsEval", fun);
    auto ok = BasicBlock::Create(C, "ok", fun);

    auto res = builder.CreateAlloca(t::SEXP);
    builder.CreateStore(arg, res);

    checkIsSexp(arg, "force argument");

    auto type = sexptype(arg);
    auto tt = builder.CreateICmpEQ(type, c(PROMSXP));

    builder.CreateCondBr(tt, isProm, ok);

    builder.SetInsertPoint(isProm);
    auto val = car(arg);
    checkIsSexp(arg, "prval");
    builder.CreateStore(val, res);

    auto tv = builder.CreateICmpEQ(val, constant(R_UnboundValue, t::SEXP));
    builder.CreateCondBr(tv, needsEval, ok);

    builder.SetInsertPoint(needsEval);
    gcSafepoint(i, -1, true);
    auto evaled = call(NativeBuiltins::forcePromise, {arg});
    checkIsSexp(evaled, "force result");
    builder.CreateStore(evaled, res);
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
    return builder.CreateLoad(res);
}

void LowerFunctionLLVM::insn_assert(llvm::Value* v, const char* msg) {
    auto nok = BasicBlock::Create(C, "assertFail", fun);
    auto ok = BasicBlock::Create(C, "assertOk", fun);

    auto br = builder.CreateCondBr(v, ok, nok);
    MDNode* WeightNode = MDB.createBranchWeights(1, 0);
    br->setMetadata(LLVMContext::MD_prof, WeightNode);

    builder.SetInsertPoint(nok);
    call(NativeBuiltins::assertFail, {convertToPointer((void*)msg)});
    builder.CreateRet(builder.CreateIntToPtr(c(nullptr), t::SEXP));

    builder.SetInsertPoint(ok);
}

llvm::Value* LowerFunctionLLVM::constant(SEXP co, llvm::Type* needed) {
    static std::unordered_set<SEXP> eternal = {R_TrueValue,  R_NilValue,
                                               R_FalseValue, R_UnboundValue,
                                               R_MissingArg, R_GlobalEnv};
    if (needed == t::Int) {
        assert(Rf_length(co) == 1);
        if (TYPEOF(co) == INTSXP)
            return llvm::ConstantInt::get(C, llvm::APInt(32, INTEGER(co)[0]));
        if (TYPEOF(co) == REALSXP) {
            return llvm::ConstantInt::get(C, llvm::APInt(32, (int)REAL(co)[0]));
        }
        if (TYPEOF(co) == LGLSXP)
            return llvm::ConstantInt::get(C, llvm::APInt(32, LOGICAL(co)[0]));
    }

    if (needed == t::Double) {
        assert(Rf_length(co) == 1);
        if (TYPEOF(co) == INTSXP)
            return llvm::ConstantFP::get(C,
                                         llvm::APFloat((double)INTEGER(co)[0]));
        if (TYPEOF(co) == REALSXP)
            return llvm::ConstantFP::get(C, llvm::APFloat(REAL(co)[0]));
    }

    assert(needed == t::SEXP);
    if (TYPEOF(co) == SYMSXP || eternal.count(co))
        return convertToPointer(co);

    auto i = Pool::insert(co);
    llvm::Value* pos = builder.CreateLoad(constantpool);
    pos = builder.CreateBitCast(dataPtr(pos), PointerType::get(t::SEXP, 0));
    pos = builder.CreateGEP(pos, c(i));
    return builder.CreateLoad(pos);
}

llvm::Value* LowerFunctionLLVM::nodestackPtr() {
    auto ptrptr = convertToPointer(&R_BCNodeStackTop,
                                   PointerType::get(t::stackCellPtr, 0));
    return builder.CreateLoad(ptrptr);
}

llvm::Value* LowerFunctionLLVM::stack(int i) {
    auto offset = -(i + 1);
    auto pos = builder.CreateGEP(nodestackPtr(), {c(offset), c(1)});
    return builder.CreateLoad(t::SEXP, pos);
}

void LowerFunctionLLVM::stack(const std::vector<llvm::Value*>& args) {
    auto stackptr = nodestackPtr();
    for (auto arg = args.rbegin(); arg != args.rend(); arg++) {
        stackptr = builder.CreateGEP(stackptr, c(-1));
        // set type tag to 0
        auto tagS = builder.CreateGEP(stackptr, {c(0), c(0)});
        builder.CreateStore(c(0), tagS);
        // store the value
        auto valS = builder.CreateGEP(stackptr, {c(0), c(1)});
        builder.CreateStore(*arg, valS);
    }
}

void LowerFunctionLLVM::setLocal(size_t i, llvm::Value* v) {
    assert(i < numLocals);
    assert(v->getType() == t::SEXP);
    auto pos = builder.CreateGEP(basepointer, {c(i), c(1)});
    builder.CreateStore(v, pos);
}

void LowerFunctionLLVM::incStack(int i, bool zero) {
    if (i == 0)
        return;
    auto cur = nodestackPtr();
    auto offset = sizeof(R_bcstack_t) * i;
    if (zero)
        builder.CreateMemSet(cur, c(0, 8), offset, 1, true);
    auto up = builder.CreateGEP(cur, c(i));
    auto ptrptr = convertToPointer(&R_BCNodeStackTop,
                                   PointerType::get(t::stackCellPtr, 0));
    builder.CreateStore(up, ptrptr);
}

void LowerFunctionLLVM::decStack(int i) {
    if (i == 0)
        return;
    auto cur = nodestackPtr();
    auto up = builder.CreateGEP(cur, c(-i));
    auto ptrptr = convertToPointer(&R_BCNodeStackTop,
                                   PointerType::get(t::stackCellPtr, 0));
    builder.CreateStore(up, ptrptr);
}

llvm::Value*
LowerFunctionLLVM::withCallFrame(Instruction* i,
                                 const std::vector<Value*>& args,
                                 const std::function<llvm::Value*()>& theCall) {
    gcSafepoint(i, -1, false);
    auto nargs = args.size();
    incStack(nargs, false);
    std::vector<llvm::Value*> jitArgs;
    for (auto& arg : args)
        jitArgs.push_back(load(i, arg, Representation::Sexp));
    stack(jitArgs);
    auto res = theCall();
    decStack(nargs);
    return res;
}

llvm::Value* LowerFunctionLLVM::load(Instruction* pos, Value* v,
                                     Representation r) {
    return load(pos, v, v->type, r);
}

llvm::Value* LowerFunctionLLVM::load(Instruction* pos, Value* v) {
    return load(pos, v, v->type, representationOf(v));
}
llvm::Value* LowerFunctionLLVM::loadSxp(Instruction* pos, Value* v) {
    return load(pos, v, Representation::Sexp);
}
llvm::Value* LowerFunctionLLVM::loadSame(Instruction* pos, Value* v) {
    return load(pos, v, representationOf(pos));
}

llvm::Value* LowerFunctionLLVM::load(Instruction* pos, Value* val, PirType type,
                                     Representation needed) {
    llvm::Value* res;

    if (auto cast = CastType::Cast(val)) {
        auto arg = cast->arg(0).val();
        return load(pos, arg, cast->type, needed);
    }

    if (valueMap.count(val))
        res = valueMap.at(val);
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
            pos->print(std::cout);
            std::cout << "\n";
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
            res = boxInt(pos, res);
        else if (type.isA(PirType() | RType::logical))
            res = boxLgl(pos, res);
        else if (type.isA(NativeType::test))
            res = boxLgl(pos, res);
        else if (type.isA(RType::real)) {
            res = boxReal(pos, res);
        } else {
            std::cout << "Failed to convert int/float to " << type << "\n";
            pos->print(std::cout);
            std::cout << "\n";
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
        std::cout << " for the instruction ";
        pos->print(std::cout, true);
        std::cout << " in the representation " << needed << "\n";
        assert(false);
    }

    return res;
}

llvm::Value* LowerFunctionLLVM::dataPtr(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto pos = builder.CreateBitCast(v, t::VECTOR_SEXPREC_ptr);
    return builder.CreateGEP(pos, c(1));
}

llvm::Value* LowerFunctionLLVM::unboxIntLgl(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    checkSexptype(v, {LGLSXP, INTSXP});
    auto pos = builder.CreateBitCast(dataPtr(v), t::IntPtr);
    return builder.CreateLoad(pos);
}
llvm::Value* LowerFunctionLLVM::unboxInt(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    checkSexptype(v, {INTSXP});
    auto pos = builder.CreateBitCast(dataPtr(v), t::IntPtr);
    return builder.CreateLoad(pos);
}
llvm::Value* LowerFunctionLLVM::unboxLgl(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    checkSexptype(v, {LGLSXP});
    auto pos = builder.CreateBitCast(dataPtr(v), t::IntPtr);
    return builder.CreateLoad(pos);
}
llvm::Value* LowerFunctionLLVM::unboxReal(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    checkSexptype(v, {REALSXP});
    auto pos = builder.CreateBitCast(dataPtr(v), t::DoublePtr);
    auto res = builder.CreateLoad(pos);
    return res;
}
llvm::Value* LowerFunctionLLVM::unboxRealIntLgl(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto done = BasicBlock::Create(C, "", fun);
    auto isReal = BasicBlock::Create(C, "isReal", fun);
    auto notReal = BasicBlock::Create(C, "notReal", fun);

    auto res = builder.CreateAlloca(t::Double);

    auto type = sexptype(v);
    auto tt = builder.CreateICmpEQ(type, c(REALSXP));
    builder.CreateCondBr(tt, isReal, notReal);

    builder.SetInsertPoint(notReal);

    auto intres = unboxIntLgl(v);

    auto isNaBr = BasicBlock::Create(C, "isNa", fun);
    nacheck(intres, isNaBr);

    builder.CreateStore(builder.CreateSIToFP(intres, t::Double), res);
    builder.CreateBr(done);

    builder.SetInsertPoint(isNaBr);
    builder.CreateStore(c(NAN), res);
    builder.CreateBr(done);

    builder.SetInsertPoint(isReal);
    builder.CreateStore(unboxReal(v), res);
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
    return builder.CreateLoad(res);
}

llvm::Value* LowerFunctionLLVM::argument(int i) {
    auto pos = builder.CreateGEP(paramArgs(), c(i));
    pos = builder.CreateGEP(pos, {c(0), c(1)});
    return builder.CreateLoad(t::SEXP, pos);
}

void LowerFunctionLLVM::setVal(Instruction* i, llvm::Value* val) {
    assert(!valueMap.count(i));
    if (val->getType() == t::SEXP && representationOf(i) == t::Int)
        val = unboxIntLgl(val);
    if (val->getType() == t::SEXP && representationOf(i) == t::Double)
        val = unboxRealIntLgl(val);
    if (i->producesRirResult() && representationOf(i) != val->getType()) {
        val->getType()->print(outs());
        outs() << "\n";
        i->print(std::cout);
        std::cout << "\nWanted a " << representationOf(i) << " but got a "
                  << Representation(val->getType()) << "\n";
        std::cout << "\n";
        assert(false);
    }
    if (!val->hasName())
        val->setName(i->getRef());
    valueMap[i] = val;
}

llvm::Value* LowerFunctionLLVM::isExternalsxp(llvm::Value* v, uint32_t magic) {
    assert(v->getType() == t::SEXP);
    auto isExternalsxp = builder.CreateICmpEQ(c(EXTERNALSXP), sexptype(v));
    auto es = builder.CreateBitCast(dataPtr(v),
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

llvm::Value* LowerFunctionLLVM::sexptype(llvm::Value* v) {
    auto sxpinfo = builder.CreateLoad(sxpinfoPtr(v));
    auto t = builder.CreateAnd(sxpinfo, c(MAX_NUM_SEXPTYPE - 1, 64));
    return builder.CreateTrunc(t, t::Int);
}

llvm::Value* LowerFunctionLLVM::tag(llvm::Value* v) {
    auto pos = builder.CreateGEP(v, {c(0), c(4), c(2)});
    return builder.CreateLoad(pos);
}

void LowerFunctionLLVM::setCar(llvm::Value* x, llvm::Value* y) {
    writeBarrier(x, y,
                 [&]() {
                     auto xx = builder.CreateGEP(x, {c(0), c(4), c(0)});
                     builder.CreateStore(y, xx);
                 },
                 [&]() {
                     call(NativeBuiltins::setCar, {x, y});
                 });
}

llvm::Value* LowerFunctionLLVM::car(llvm::Value* v) {
    v = builder.CreateGEP(v, {c(0), c(4), c(0)});
    return builder.CreateLoad(v);
}

void LowerFunctionLLVM::ensureNamed(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = builder.CreateBitCast(sxpinfoPtr(v), t::i64ptr);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    auto named = builder.CreateOr(sxpinfo, c(0x10000000ul));
    auto isNamed = builder.CreateICmpEQ(sxpinfo, named);

    auto notNamed = BasicBlock::Create(C, "notNamed", fun);
    auto ok = BasicBlock::Create(C, "", fun);

    builder.CreateCondBr(isNamed, ok, notNamed);
    builder.SetInsertPoint(notNamed);
    builder.CreateStore(named, sxpinfoP);
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
};

void LowerFunctionLLVM::incrementNamed(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto sxpinfoP = sxpinfoPtr(v);
    auto sxpinfo = builder.CreateLoad(sxpinfoP);

    static auto namedMask = ((unsigned long)pow(2, NAMED_BITS) - 1);
    static auto namedNegMask = ~(namedMask << 32);

    auto named = builder.CreateLShr(sxpinfo, c(32, 64));
    named = builder.CreateAnd(named, c(namedMask));

    auto isNamedMax = builder.CreateICmpEQ(named, c(NAMEDMAX, 64));

    auto incrementBr = BasicBlock::Create(C, "", fun);
    auto done = BasicBlock::Create(C, "", fun);

    builder.CreateCondBr(isNamedMax, done, incrementBr);

    builder.SetInsertPoint(incrementBr);
    auto newNamed = builder.CreateAdd(named, c(1, 64));
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
    llvm::Instruction* br;
    if (v->getType() == t::Double) {
        auto isNotNa = builder.CreateFCmpOEQ(v, v);
        br = builder.CreateCondBr(isNotNa, notNa, isNa);
    } else {
        auto isNotNa = builder.CreateICmpNE(v, c(NA_INTEGER));
        br = builder.CreateCondBr(isNotNa, notNa, isNa);
    }
    MDNode* WeightNode = MDB.createBranchWeights(1, 0);
    br->setMetadata(LLVMContext::MD_prof, WeightNode);
    builder.SetInsertPoint(notNa);
}

void LowerFunctionLLVM::checkMissing(llvm::Value* v) {
    assert(v->getType() == t::SEXP);
    auto ok = BasicBlock::Create(C, "", fun);
    auto nok = BasicBlock::Create(C, "", fun);
    auto t = builder.CreateICmpEQ(v, constant(R_MissingArg, t::SEXP));
    auto br = builder.CreateCondBr(t, nok, ok);
    MDNode* WeightNode = MDB.createBranchWeights(0, 1);
    br->setMetadata(LLVMContext::MD_prof, WeightNode);

    builder.SetInsertPoint(nok);
    call(NativeBuiltins::error, {});
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
}

void LowerFunctionLLVM::checkUnbound(llvm::Value* v) {
    auto ok = BasicBlock::Create(C, "", fun);
    auto nok = BasicBlock::Create(C, "", fun);
    auto t = builder.CreateICmpEQ(v, constant(R_UnboundValue, t::SEXP));
    auto br = builder.CreateCondBr(t, nok, ok);
    MDNode* WeightNode = MDB.createBranchWeights(0, 1);
    br->setMetadata(LLVMContext::MD_prof, WeightNode);

    builder.SetInsertPoint(nok);
    call(NativeBuiltins::error, {});
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
}

llvm::Value* LowerFunctionLLVM::call(const NativeBuiltin& builtin,
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

llvm::Value* LowerFunctionLLVM::box(Instruction* pos, llvm::Value* v,
                                    PirType t) {
    if (t.isA(PirType(RType::integer).notObject()))
        return boxInt(pos, v);
    if (t.isA(PirType(RType::logical).notObject()))
        return boxLgl(pos, v);
    if (t.isA(PirType(RType::real).notObject()))
        return boxReal(pos, v);
    assert(false);
    return nullptr;
}
llvm::Value* LowerFunctionLLVM::boxInt(Instruction* pos, llvm::Value* v) {
    gcSafepoint(pos, 1, true);
    if (v->getType() == t::Int)
        return call(NativeBuiltins::newInt, {v});
    assert(v->getType() == t::Double);
    return call(NativeBuiltins::newIntFromReal, {v});
}
llvm::Value* LowerFunctionLLVM::boxReal(Instruction* pos, llvm::Value* v) {
    gcSafepoint(pos, 1, true);
    if (v->getType() == t::Double)
        return call(NativeBuiltins::newReal, {v});
    assert(v->getType() == t::Int);
    return call(NativeBuiltins::newRealFromInt, {v});
}
llvm::Value* LowerFunctionLLVM::boxLgl(Instruction* pos, llvm::Value* v) {
    gcSafepoint(pos, 1, true);
    if (v->getType() == t::Int)
        return call(NativeBuiltins::newLgl, {v});
    assert(v->getType() == t::Double);
    return call(NativeBuiltins::newLglFromReal, {v});
}

void LowerFunctionLLVM::gcSafepoint(Instruction* i, size_t required,
                                    bool protectArgs) {
    auto ok = BasicBlock::Create(C, "", fun);

    if (required != (size_t)-1) {
        auto use_ptr = convertToPointer(&R_NodesInUse, t::i64ptr);
        auto size_ptr = convertToPointer(&R_NSize, t::i64ptr);
        auto use = builder.CreateLoad(use_ptr);
        auto size = builder.CreateLoad(size_ptr);
        auto req = builder.CreateAdd(use, c(required));
        auto t = builder.CreateICmpULT(req, size);
        auto nok = BasicBlock::Create(C, "", fun);
        auto br = builder.CreateCondBr(t, ok, nok);

        MDNode* WeightNode = MDB.createBranchWeights(1, 0);
        br->setMetadata(LLVMContext::MD_prof, WeightNode);
        builder.SetInsertPoint(nok);
    }

    // Store every live variable into a local variable slot from R
    size_t pos = 0;
    for (auto& v : valueMap) {
        auto test = Instruction::Cast(v.first);
        if (!test)
            continue;

        bool isArg = false;
        if (protectArgs) {
            i->eachArg([&](Value* a) { isArg = isArg || a == test; });
        }

        if (i != test && (isArg || liveness.live(i, v.first))) {
            if (v.second->getType() == t::SEXP)
                setLocal(pos++, v.second);
        }
    }

    builder.CreateBr(ok);
    builder.SetInsertPoint(ok);
}

llvm::Value* LowerFunctionLLVM::depromise(llvm::Value* arg) {

    auto isProm = BasicBlock::Create(C, "isProm", fun);
    auto ok = BasicBlock::Create(C, "ok", fun);

    auto res = builder.CreateAlloca(t::SEXP);
    builder.CreateStore(arg, res);

    auto type = sexptype(arg);
    auto tt = builder.CreateICmpEQ(type, c(PROMSXP));

    builder.CreateCondBr(tt, isProm, ok);

    builder.SetInsertPoint(isProm);
    auto val = car(arg);
    builder.CreateStore(val, res);
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
    return builder.CreateLoad(res);
}

void LowerFunctionLLVM::compileRelop(
    Instruction* i,
    std::function<llvm::Value*(llvm::Value*, llvm::Value*)> intInsert,
    std::function<llvm::Value*(llvm::Value*, llvm::Value*)> fpInsert,
    BinopKind kind) {
    auto rep = representationOf(i);
    auto lhs = i->arg(0).val();
    auto rhs = i->arg(1).val();
    auto lhsRep = representationOf(lhs);
    auto rhsRep = representationOf(rhs);
    if (lhsRep == Representation::Sexp || rhsRep == Representation::Sexp) {
        auto a = loadSxp(i, lhs);
        auto b = loadSxp(i, rhs);

        llvm::Value* res;
        gcSafepoint(i, -1, true);
        if (i->hasEnv()) {
            auto e = loadSxp(i, i->env());
            res = call(NativeBuiltins::binopEnv,
                       {a, b, e, c(i->srcIdx), c((int)kind)});
        } else {
            res = call(NativeBuiltins::binop, {a, b, c((int)kind)});
        }
        if (rep == Representation::Integer)
            setVal(i, unboxIntLgl(res));
        else
            setVal(i, res);
        return;
    }

    BasicBlock* isNaBr = BasicBlock::Create(C, "isNa", fun);
    auto done = BasicBlock::Create(C, "", fun);

    llvm::Value* res = builder.CreateAlloca(t::Int);
    auto a = load(i, lhs, lhsRep);
    auto b = load(i, rhs, rhsRep);

    nacheck(a, isNaBr);
    nacheck(b, isNaBr);

    if (a->getType() == t::Int && b->getType() == t::Int) {
        builder.CreateStore(builder.CreateZExt(intInsert(a, b), t::Int), res);
    } else {
        if (a->getType() == t::Int)
            a = builder.CreateSIToFP(a, t::Double);
        if (b->getType() == t::Int)
            b = builder.CreateSIToFP(b, t::Double);
        builder.CreateStore(builder.CreateZExt(fpInsert(a, b), t::Int), res);
    }

    builder.CreateBr(done);

    builder.SetInsertPoint(isNaBr);
    builder.CreateStore(c(NA_INTEGER), res);
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
    res = builder.CreateLoad(res);
    if (rep == Representation::Sexp)
        setVal(i, boxLgl(i, res));
    else
        setVal(i, res);
};

void LowerFunctionLLVM::compileBinop(
    Instruction* i,
    std::function<llvm::Value*(llvm::Value*, llvm::Value*)> intInsert,
    std::function<llvm::Value*(llvm::Value*, llvm::Value*)> fpInsert,
    BinopKind kind) {
    auto rep = representationOf(i);
    auto lhs = i->arg(0).val();
    auto rhs = i->arg(1).val();
    auto lhsRep = representationOf(lhs);
    auto rhsRep = representationOf(rhs);

    if (lhsRep == Representation::Sexp || rhsRep == Representation::Sexp) {
        auto a = loadSxp(i, i->arg(0).val());
        auto b = loadSxp(i, i->arg(1).val());

        llvm::Value* res = nullptr;
        gcSafepoint(i, -1, true);
        if (i->hasEnv()) {
            auto e = loadSxp(i, i->env());
            res = call(NativeBuiltins::binopEnv,
                       {a, b, e, c(i->srcIdx), c((int)kind)});
        } else {
            res = call(NativeBuiltins::binop, {a, b, c((int)kind)});
        }

        if (rep == Representation::Integer)
            setVal(i, unboxIntLgl(res));
        else if (rep == Representation::Real)
            setVal(i, unboxRealIntLgl(res));
        else
            setVal(i, res);
        return;
    }

    BasicBlock* isNaBr = nullptr;
    auto done = BasicBlock::Create(C, "", fun);

    auto r = (lhsRep == Representation::Real || rhsRep == Representation::Real)
                 ? t::Double
                 : t::Int;

    llvm::Value* res = builder.CreateAlloca(r);

    auto a = load(i, lhs, lhsRep);
    auto b = load(i, rhs, rhsRep);

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
        builder.CreateStore(intInsert(a, b), res);
    } else {
        if (a->getType() == t::Int)
            a = builder.CreateSIToFP(a, t::Double);
        if (b->getType() == t::Int)
            b = builder.CreateSIToFP(b, t::Double);
        builder.CreateStore(fpInsert(a, b), res);
    }
    builder.CreateBr(done);

    if (lhsRep == Representation::Integer ||
        rhsRep == Representation::Integer) {
        if (isNaBr) {
            builder.SetInsertPoint(isNaBr);

            if (r == t::Int)
                builder.CreateStore(c(NA_INTEGER), res);
            else
                builder.CreateStore(c((double)NAN), res);

            builder.CreateBr(done);
        }
    }

    builder.SetInsertPoint(done);
    res = builder.CreateLoad(res);
    if (rep == Representation::Sexp) {
        setVal(i, box(i, res, lhs->type.mergeWithConversion(rhs->type)));
    } else {
        setVal(i, res);
    }
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
    builder.CreateCondBr(olderGen, needsBarrier, noBarrier);

    builder.SetInsertPoint(noBarrier);
    no();
    builder.CreateBr(done);

    builder.SetInsertPoint(needsBarrier);
    yes();
    builder.CreateBr(done);

    builder.SetInsertPoint(done);
};

llvm::Value* LowerFunctionLLVM::envStubGet(llvm::Value* x, int i) {
    // We could use externalsxpGetEntry, but this is faster
    assert(x->getType() == t::SEXP);
#ifdef ENABLE_SLOWASSERT
    insn_assert(isExternalsxp(x, LAZY_ENVIRONMENT_MAGIC),
                "envStubGet on something which is not an env stub");
#endif
    auto le = builder.CreateBitCast(dataPtr(x),
                                    PointerType::get(t::LazyEnvironment, 0));
    auto payload =
        builder.CreateBitCast(builder.CreateGEP(le, c(1)), t::SEXP_ptr);
    auto pos = builder.CreateGEP(payload, c(i + LazyEnvironment::ArgOffset));
    return builder.CreateLoad(pos);
}

void LowerFunctionLLVM::envStubSet(llvm::Value* x, int i, llvm::Value* y) {
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
                dataPtr(x), PointerType::get(t::LazyEnvironment, 0));
            auto payload =
                builder.CreateBitCast(builder.CreateGEP(le, c(1)), t::SEXP_ptr);
            auto pos =
                builder.CreateGEP(payload, c(i + LazyEnvironment::ArgOffset));
            builder.CreateStore(y, pos);
        },
        [&]() {
            call(NativeBuiltins::externalsxpSetEntry,
                 {{x, c(i + LazyEnvironment::ArgOffset), y}});
        });
}

llvm::Value* LowerFunctionLLVM::isObj(llvm::Value* v) {
    checkIsSexp(v, "in IsObj");
    auto sxpinfo = builder.CreateLoad(sxpinfoPtr(v));
    return builder.CreateICmpNE(
        c(0, 64),
        builder.CreateAnd(sxpinfo, c((unsigned long)(1ul << (TYPE_BITS + 1)))));
};

bool LowerFunctionLLVM::tryCompile() {
    std::unordered_map<BB*, BasicBlock*> blockMapping_;
    auto getBlock = [&](BB* bb) {
        auto b = blockMapping_.find(bb);
        if (b != blockMapping_.end()) {
            return b->second;
        }
        std::stringstream ss;
        ss << "BB" << bb->id;
        return blockMapping_[bb] = BasicBlock::Create(C, ss.str(), fun);
    };
    builder.SetInsertPoint(getBlock(code->entry));

    std::unordered_map<Value*, std::unordered_map<SEXP, size_t>> bindingsCache;
    llvm::Value* bindingsCacheBase;
    {
        SmallSet<std::pair<Value*, SEXP>> bindings;
        Visitor::run(code->entry, [&](Instruction* i) {
            SEXP varName = nullptr;
            if (auto l = LdVar::Cast(i))
                varName = l->varName;
            else if (auto l = StVar::Cast(i))
                varName = l->varName;

            if (varName) {
                auto e = MkEnv::Cast(i->env());
                if (e && !e->stub) {
                    bindings.insert(std::pair<Value*, SEXP>(i->env(), varName));
                }
            }
        });
        size_t idx = 0;
        for (auto& b : bindings) {
            bindingsCache[b.first][b.second] = idx++;
        }
        bindingsCacheBase = builder.CreateAlloca(t::SEXP, 0, c(idx));
    }

    basepointer = nodestackPtr();
    std::unordered_map<Instruction*, llvm::Value*> phis;
    Visitor::run(code->entry, [&](BB* bb) {
        for (auto i : *bb) {
            if (auto phi = Phi::Cast(i)) {
                auto val = builder.CreateAlloca(representationOf(i));
                phis[i] = val;
                phi->eachArg([&](BB*, Value* v) {
                    auto i = Instruction::Cast(v);
                    assert(i);
                    phis[i] = val;
                });
            }
        }
    });

    auto arg = fun->arg_begin();
    for (size_t i = 0; i < argNames.size(); ++i) {
        args.push_back(arg);
        args.back()->setName(argNames[i]);
        arg++;
    }

    constantpool =
        builder.CreateBitCast(paramCtx(), PointerType::get(t::SEXP, 0));
    constantpool = builder.CreateGEP(constantpool, c(1));

    if (numLocals > 0)
        incStack(numLocals, true);

    LoweringVisitor::run(code->entry, [&](BB* bb) {
        if (!success)
            return;

        builder.SetInsertPoint(getBlock(bb));

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto i = *it;
            if (!success)
                return;

            switch (i->tag) {
            case Tag::PirCopy: {
                auto c = PirCopy::Cast(i);
                auto in = c->arg<0>().val();
                if (Phi::Cast(in))
                    setVal(i, loadSame(i, in));
                break;
            }
            case Tag::Phi:
                setVal(i, builder.CreateLoad(phis.at(i)));
                break;

            case Tag::LdArg:
                setVal(i, argument(LdArg::Cast(i)->id));
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
                auto a = depromise(load(i, i->arg(0).val()));
                auto b = depromise(load(i, i->arg(1).val()));
                setVal(i,
                       builder.CreateZExt(builder.CreateICmpEQ(a, b), t::Int));
                break;
            }

            case Tag::CallSafeBuiltin: {
                auto b = CallSafeBuiltin::Cast(i);
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });

                // TODO: this should probably go somewhere else... This is
                // an inlined version of bitwise builtins
                if (representationOf(b) == Representation::Integer) {
                    if (b->nargs() == 2) {
                        auto x = b->arg(0).val();
                        auto y = b->arg(1).val();
                        auto xRep = representationOf(x);
                        auto yRep = representationOf(y);

                        static auto bitwise = {
                            findBuiltin("bitwiseShiftL"),
                            findBuiltin("bitwiseShiftR"),
                            findBuiltin("bitwiseAnd"),
                            findBuiltin("bitwiseOr"),
                            findBuiltin("bitwiseXor"),
                        };
                        auto found = std::find(bitwise.begin(), bitwise.end(),
                                               b->builtinId);
                        if (found != bitwise.end()) {
                            const static PirType num =
                                (PirType() | RType::integer | RType::logical |
                                 RType::real)
                                    .notObject()
                                    .scalar();

                            if (xRep == Representation::Sexp &&
                                x->type.isA(num))
                                xRep = Representation::Real;
                            if (yRep == Representation::Sexp &&
                                y->type.isA(num))
                                yRep = Representation::Real;

                            if (xRep != Representation::Sexp &&
                                yRep != Representation::Sexp) {

                                BasicBlock* isNaBr = nullptr;
                                auto done = BasicBlock::Create(C, "", fun);
                                auto res = builder.CreateAlloca(t::Int);

                                auto xInt = load(i, x, Representation::Integer);
                                auto yInt = load(i, y, Representation::Integer);

                                auto naCheck = [&](Value* v, llvm::Value* asInt,
                                                   Representation rep) {
                                    if (rep == Representation::Real) {
                                        auto vv = load(i, v, rep);
                                        if (!isNaBr)
                                            isNaBr = BasicBlock::Create(
                                                C, "isNa", fun);
                                        nacheck(vv, isNaBr);
                                    } else {
                                        assert(rep == Representation::Integer);
                                        if (!isNaBr)
                                            isNaBr = BasicBlock::Create(
                                                C, "isNa", fun);
                                        nacheck(asInt, isNaBr);
                                    }
                                };
                                naCheck(x, xInt, xRep);
                                naCheck(y, yInt, yRep);

                                switch (found - bitwise.begin()) {
                                case 0: {
                                    if (!isNaBr)
                                        isNaBr =
                                            BasicBlock::Create(C, "isNa", fun);
                                    auto ok = BasicBlock::Create(C, "", fun);
                                    auto ofl =
                                        builder.CreateICmpSLT(yInt, c(0));
                                    auto br =
                                        builder.CreateCondBr(ofl, isNaBr, ok);
                                    MDNode* WeightNode =
                                        MDB.createBranchWeights(0, 1);
                                    br->setMetadata(LLVMContext::MD_prof,
                                                    WeightNode);
                                    builder.SetInsertPoint(ok);

                                    ok = BasicBlock::Create(C, "", fun);
                                    ofl = builder.CreateICmpSGT(yInt, c(31));
                                    br = builder.CreateCondBr(ofl, isNaBr, ok);
                                    WeightNode = MDB.createBranchWeights(0, 1);
                                    br->setMetadata(LLVMContext::MD_prof,
                                                    WeightNode);
                                    builder.SetInsertPoint(ok);

                                    auto res0 = builder.CreateShl(xInt, yInt);
                                    builder.CreateStore(res0, res);
                                    break;
                                }
                                case 1: {
                                    if (!isNaBr)
                                        isNaBr =
                                            BasicBlock::Create(C, "isNa", fun);
                                    auto ok = BasicBlock::Create(C, "", fun);
                                    auto ofl =
                                        builder.CreateICmpSLT(yInt, c(0));
                                    auto br =
                                        builder.CreateCondBr(ofl, isNaBr, ok);
                                    MDNode* WeightNode =
                                        MDB.createBranchWeights(0, 1);
                                    br->setMetadata(LLVMContext::MD_prof,
                                                    WeightNode);
                                    builder.SetInsertPoint(ok);

                                    ok = BasicBlock::Create(C, "", fun);
                                    ofl = builder.CreateICmpSGT(yInt, c(31));
                                    br = builder.CreateCondBr(ofl, isNaBr, ok);
                                    WeightNode = MDB.createBranchWeights(0, 1);
                                    br->setMetadata(LLVMContext::MD_prof,
                                                    WeightNode);
                                    builder.SetInsertPoint(ok);

                                    auto res0 = builder.CreateLShr(xInt, yInt);
                                    builder.CreateStore(res0, res);
                                    break;
                                }
                                case 2: {
                                    auto res0 = builder.CreateAnd(xInt, yInt);
                                    builder.CreateStore(res0, res);
                                    break;
                                }
                                case 3: {
                                    auto res0 = builder.CreateOr(xInt, yInt);
                                    builder.CreateStore(res0, res);
                                    break;
                                }
                                case 4: {
                                    auto res0 = builder.CreateXor(xInt, yInt);
                                    builder.CreateStore(res0, res);
                                    break;
                                }
                                }

                                builder.CreateBr(done);

                                if (isNaBr) {
                                    builder.SetInsertPoint(isNaBr);
                                    builder.CreateStore(c(NA_INTEGER), res);
                                    builder.CreateBr(done);
                                }

                                builder.SetInsertPoint(done);
                                setVal(i, builder.CreateLoad(res));
                                break;
                            }
                        }
                    }
                }

                setVal(i, withCallFrame(i, args, [&]() -> llvm::Value* {
                           return call(
                               NativeBuiltins::callBuiltin,
                               {
                                   paramCode(),
                                   c(b->srcIdx),
                                   constant(b->blt, t::SEXP),
                                   constant(symbol::delayedEnv, t::SEXP),
                                   c(b->nCallArgs()),
                                   paramCtx(),
                               });
                       }));
                break;
            }

            case Tag::CallBuiltin: {
                auto b = CallBuiltin::Cast(i);
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, withCallFrame(i, args, [&]() -> llvm::Value* {
                           return call(NativeBuiltins::callBuiltin,
                                       {
                                           paramCode(),
                                           c(b->srcIdx),
                                           constant(b->blt, t::SEXP),
                                           loadSxp(i, b->env()),
                                           c(b->nCallArgs()),
                                           paramCtx(),
                                       });
                       }));
                break;
            }

            case Tag::Call: {
                auto b = Call::Cast(i);
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, withCallFrame(i, args, [&]() -> llvm::Value* {
                           return call(NativeBuiltins::call,
                                       {
                                           paramCode(),
                                           c(b->srcIdx),
                                           loadSxp(i, b->cls()),
                                           loadSxp(i, b->env()),
                                           c(b->nCallArgs()),
                                           paramCtx(),
                                       });
                       }));
                break;
            }

            case Tag::AsInt: {
                auto arg = i->arg(0).val();
                auto asint = AsInt::Cast(i);
                llvm::Value* res = nullptr;
                if (representationOf(arg) == Representation::Integer) {
                    res = load(i, arg, Representation::Integer);
                } else if (representationOf(arg) == Representation::Real) {
                    auto a = load(i, arg, Representation::Real);
                    if (asint->ceil) {
                        res = builder.CreateIntrinsic(Intrinsic::ceil,
                                                      {a->getType()}, {a});
                    } else {
                        res = builder.CreateIntrinsic(Intrinsic::floor,
                                                      {a->getType()}, {a});
                    }
                    res = builder.CreateFPToSI(res, t::Int);
                } else {
                    // TODO!!
                    success = false;
                    break;
                }
                setVal(i, res);
                break;
            }

            case Tag::Inc: {
                auto arg = i->arg(0).val();
                llvm::Value* res = nullptr;
                if (representationOf(arg) == Representation::Integer) {
                    res = load(i, arg, Representation::Integer);
                    res = builder.CreateAdd(res, c(1));
                } else {
                    success = false;
                }
                setVal(i, res);
                break;
            }

            case Tag::Dec: {
                auto arg = i->arg(0).val();
                llvm::Value* res = nullptr;
                if (representationOf(arg) == Representation::Integer) {
                    res = load(i, arg, Representation::Integer);
                    res = builder.CreateSub(res, c(1));
                } else {
                    success = false;
                }
                setVal(i, res);
                break;
            }

            case Tag::LdConst:
            case Tag::Nop:
                break;

            case Tag::Branch: {
                auto cond = load(i, i->arg(0).val(), Representation::Integer);
                cond = builder.CreateICmpNE(cond, c(0));
                auto br = builder.CreateCondBr(cond, getBlock(bb->trueBranch()),
                                               getBlock(bb->falseBranch()));
                if (bb->trueBranch()->isDeopt()) {
                    MDNode* WeightNode = MDB.createBranchWeights(0, 1);
                    br->setMetadata(LLVMContext::MD_prof, WeightNode);
                } else if (bb->falseBranch()->isDeopt()) {
                    MDNode* WeightNode = MDB.createBranchWeights(1, 0);
                    br->setMetadata(LLVMContext::MD_prof, WeightNode);
                }
                break;
            }

            case Tag::ScheduledDeopt: {
                // TODO, this is copied from pir_2_rir... rather ugly
                DeoptMetadata* m = nullptr;
                {
                    auto deopt = ScheduledDeopt::Cast(i);
                    size_t nframes = deopt->frames.size();
                    SEXP store =
                        Rf_allocVector(RAWSXP, sizeof(DeoptMetadata) +
                                                   nframes * sizeof(FrameInfo));
                    m = new (DATAPTR(store)) DeoptMetadata;
                    m->numFrames = nframes;

                    size_t i = 0;
                    // Frames in the ScheduledDeopt are in pir argument
                    // order (from left to right). On the other hand frames
                    // in the rir deopt_ instruction are in stack order,
                    // from tos down.
                    for (auto fi = deopt->frames.rbegin();
                         fi != deopt->frames.rend(); fi++)
                        m->frames[i++] = *fi;
                    Pool::insert(store);
                }

                std::vector<Value*> args;
                i->eachArg([&](Value* v) { args.push_back(v); });
                withCallFrame(i, args, [&]() {
                    return call(NativeBuiltins::deopt,
                                {paramCode(), paramClosure(),
                                 convertToPointer(m), paramArgs()});
                });
                builder.CreateRet(builder.CreateIntToPtr(c(nullptr), t::SEXP));
                break;
            }

            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(i);
                auto parent = loadSxp(i, mkenv->env());

                static std::vector<std::vector<Immediate>> mkEnvStubNames;
                mkEnvStubNames.push_back({});
                auto& namesBuffer = mkEnvStubNames.back();
                for (size_t i = 0; i < mkenv->nLocals(); ++i)
                    namesBuffer.push_back(Pool::insert(mkenv->varName[i]));

                if (mkenv->stub) {
                    gcSafepoint(i, -1, true);
                    auto env =
                        call(NativeBuiltins::createStubEnvironment,
                             {parent, c((int)mkenv->nLocals()),
                              convertToPointer(namesBuffer.data(), t::voidPtr),
                              c(mkenv->context)});
                    size_t pos = 0;
                    mkenv->eachLocalVar([&](SEXP name, Value* v) {
                        envStubSet(env, pos++, loadSxp(i, v));
                    });
                    setVal(i, env);
                    break;
                }

                gcSafepoint(i, mkenv->nargs() + 1, true);
                auto arglist = constant(R_NilValue, t::SEXP);
                mkenv->eachLocalVarRev([&](SEXP name, Value* v) {
                    if (v == MissingArg::instance()) {
                        arglist = call(NativeBuiltins::createMissingBindingCell,
                                       {constant(name, t::SEXP), arglist});
                    } else {
                        arglist = call(
                            NativeBuiltins::createBindingCell,
                            {loadSxp(i, v), constant(name, t::SEXP), arglist});
                    }
                });

                setVal(i, call(NativeBuiltins::createEnvironment,
                               {parent, arglist, c(mkenv->context)}));

                if (bindingsCache.count(i))
                    for (auto b : bindingsCache.at(i))
                        builder.CreateStore(
                            convertToPointer(nullptr, t::SEXP),
                            builder.CreateGEP(bindingsCacheBase, c(b.second)));
                break;
            }

            case Tag::Add:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateAdd(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFAdd(a, b);
                             },
                             BinopKind::ADD);
                break;
            case Tag::Sub:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateSub(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFSub(a, b);
                             },
                             BinopKind::SUB);
                break;
            case Tag::Mul:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateMul(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFMul(a, b);
                             },
                             BinopKind::MUL);
                break;
            case Tag::Div:
                compileBinop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateSDiv(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFDiv(a, b);
                             },
                             BinopKind::DIV);
                break;

            case Tag::Neq:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateICmpNE(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateFCmpONE(a, b);
                             },
                             BinopKind::NE);
                break;
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
                                 return builder.CreateAnd(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 a = builder.CreateZExt(
                                     builder.CreateFCmpONE(a, c(0.0)), t::Int);
                                 b = builder.CreateZExt(
                                     builder.CreateFCmpONE(b, c(0.0)), t::Int);
                                 return builder.CreateAnd(a, b);
                             },
                             BinopKind::LAND);
                break;
            case Tag::LOr:
                compileRelop(i,
                             [&](llvm::Value* a, llvm::Value* b) {
                                 return builder.CreateOr(a, b);
                             },
                             [&](llvm::Value* a, llvm::Value* b) {
                                 a = builder.CreateZExt(
                                     builder.CreateFCmpONE(a, c(0.0)), t::Int);
                                 b = builder.CreateZExt(
                                     builder.CreateFCmpONE(b, c(0.0)), t::Int);
                                 return builder.CreateOr(a, b);
                             },
                             BinopKind::LOR);
                break;

            case Tag::CastType: {
                // Scheduled on use
                break;
            }

            case Tag::Return: {
                auto res = loadSxp(i, Return::Cast(i)->arg<0>().val());
                if (numLocals > 0) {
                    decStack(numLocals);
                }
                builder.CreateRet(res);
                break;
            }

            case Tag::IsEnvStub: {
                auto arg = loadSxp(i, i->arg(0).val());

                auto isStub = BasicBlock::Create(C, "", fun);
                auto isNotMaterialized = BasicBlock::Create(C, "", fun);
                auto isNotStub = BasicBlock::Create(C, "", fun);
                auto done = BasicBlock::Create(C, "", fun);

                auto r = representationOf(i);
                llvm::Value* res = builder.CreateAlloca(r);

                auto br = builder.CreateCondBr(
                    isExternalsxp(arg, LAZY_ENVIRONMENT_MAGIC), isStub,
                    isNotStub);
                MDNode* WeightNode = MDB.createBranchWeights(1, 0);
                br->setMetadata(LLVMContext::MD_prof, WeightNode);

                builder.SetInsertPoint(isStub);
                auto materialized = envStubGet(arg, -2);
                br = builder.CreateCondBr(
                    builder.CreateICmpEQ(materialized,
                                         convertToPointer(nullptr, t::SEXP)),
                    isNotMaterialized, isNotStub);
                WeightNode = MDB.createBranchWeights(1, 0);
                br->setMetadata(LLVMContext::MD_prof, WeightNode);

                builder.SetInsertPoint(isNotMaterialized);
                builder.CreateStore(constant(R_TrueValue, r), res);
                builder.CreateBr(done);

                builder.SetInsertPoint(isNotStub);
                builder.CreateStore(constant(R_FalseValue, r), res);
                builder.CreateBr(done);

                builder.SetInsertPoint(done);
                res = builder.CreateLoad(res);

                setVal(i, res);
                break;
            }

            case Tag::MkFunCls: {
                auto mkFunction = MkFunCls::Cast(i);
                auto closure = mkFunction->cls;
                auto srcRef = constant(closure->srcRef(), t::SEXP);
                auto formals = constant(closure->formals().original(), t::SEXP);
                auto body =
                    constant(mkFunction->originalBody->container(), t::SEXP);
                assert(DispatchTable::check(
                    mkFunction->originalBody->container()));
                gcSafepoint(i, 1, true);
                setVal(i, call(NativeBuiltins::createClosure,
                               {body, formals, loadSxp(i, mkFunction->env()),
                                srcRef}));
                break;
            }

            case Tag::IsType: {
                if (representationOf(i) != Representation::Integer) {
                    success = false;
                    break;
                }

                auto t = IsType::Cast(i);
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp) {
                    llvm::Value* res;
                    auto a = loadSxp(i, arg);
                    if (t->typeTest.isA(RType::integer)) {
                        res = builder.CreateICmpEQ(sexptype(a), c(INTSXP));
                    } else if (t->typeTest.isA(PirType(RType::integer)
                                                   .orPromiseWrapped())) {
                        a = depromise(a);
                        builder.CreateICmpEQ(sexptype(a), c(INTSXP));
                    } else if (t->typeTest.isA(RType::real)) {
                        res = builder.CreateICmpEQ(sexptype(a), c(REALSXP));
                    } else if (t->typeTest.isA(
                                   PirType(RType::real).orPromiseWrapped())) {
                        a = depromise(a);
                        res = builder.CreateICmpEQ(sexptype(a), c(REALSXP));
                    } else {
                        t->print(std::cerr, true);
                        assert(false);
                    }
                    if (t->typeTest.isScalar()) {
                        assert(a->getType() == t::SEXP);
                        auto va =
                            builder.CreateBitCast(a, t::VECTOR_SEXPREC_ptr);
                        auto lp = builder.CreateGEP(va, {c(0), c(4), c(0)});
                        auto l = builder.CreateLoad(lp);
                        auto lt = builder.CreateICmpEQ(l, c(1, 64));
                        res = builder.CreateAnd(res, lt);
                    }
                    setVal(i, builder.CreateZExt(res, t::Int));
                } else {
                    setVal(i, c(1));
                }
                break;
            }

            case Tag::IsObject: {
                if (representationOf(i) != Representation::Integer) {
                    success = false;
                    break;
                }

                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp) {
                    auto a = loadSxp(i, arg);
                    if (arg->type.maybePromiseWrapped())
                        a = depromise(a);
                    setVal(i, builder.CreateZExt(isObj(a), t::Int));
                } else {
                    setVal(i, c((int)0));
                }
                break;
            }

            case Tag::AsTest: {
                assert(representationOf(i) == Representation::Integer);

                auto arg = i->arg(0).val();
                if (auto lgl = AsLogical::Cast(arg))
                    arg = lgl->arg(0).val();

                if (representationOf(arg) == Representation::Sexp) {
                    auto a = loadSxp(i, arg);
                    gcSafepoint(i, -1, true);
                    setVal(i, call(NativeBuiltins::asTest, {a}));
                    break;
                }

                auto r = representationOf(arg);

                auto done = BasicBlock::Create(C, "", fun);
                auto isNa = BasicBlock::Create(C, "asTestIsNa", fun);

                if (r == Representation::Real) {
                    auto narg = load(i, arg, r);
                    auto isNotNa = builder.CreateFCmpOEQ(narg, narg);
                    narg = builder.CreateFPToSI(narg, t::Int);
                    setVal(i, narg);
                    auto br = builder.CreateCondBr(isNotNa, done, isNa);
                    MDNode* WeightNode = MDB.createBranchWeights(1, 0);
                    br->setMetadata(LLVMContext::MD_prof, WeightNode);
                } else {
                    auto narg = load(i, arg, Representation::Integer);
                    auto isNotNa = builder.CreateICmpNE(narg, c(NA_INTEGER));
                    setVal(i, narg);
                    auto br = builder.CreateCondBr(isNotNa, done, isNa);
                    MDNode* WeightNode = MDB.createBranchWeights(1, 0);
                    br->setMetadata(LLVMContext::MD_prof, WeightNode);
                }

                builder.SetInsertPoint(isNa);
                call(NativeBuiltins::error, {});
                builder.CreateRet(builder.CreateIntToPtr(c(nullptr), t::SEXP));

                builder.SetInsertPoint(done);
                break;
            }

            case Tag::AsLogical: {
                auto arg = i->arg(0).val();

                auto r1 = representationOf(arg);
                auto r2 = representationOf(i);

                assert(r2 == Representation::Integer);

                llvm::Value* res;
                if (r1 == Representation::Sexp) {
                    res = call(NativeBuiltins::asLogicalBlt, {loadSxp(i, arg)});
                } else if (r1 == Representation::Real) {
                    res = builder.CreateAlloca(t::Int);
                    auto in = load(i, arg, Representation::Integer);
                    auto nin = load(i, arg, Representation::Real);
                    builder.CreateStore(in, res);

                    auto done = BasicBlock::Create(C, "", fun);
                    auto isNaBr = BasicBlock::Create(C, "AsLogicalNa", fun);
                    nacheck(nin, isNaBr, done);

                    builder.SetInsertPoint(isNaBr);
                    builder.CreateStore(c(NA_INTEGER), res);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                    res = builder.CreateLoad(res);
                } else {
                    assert(r1 == Representation::Integer);
                    res = load(i, arg, Representation::Integer);
                }

                setVal(i, res);
                break;
            }

            case Tag::Force: {
                auto f = Force::Cast(i);
                auto arg = loadSxp(i, f->arg<0>().val());
                if (!f->effects.includes(Effect::Force)) {
                    auto res = depromise(arg);
                    setVal(i, res);
#ifdef ENABLE_SLOWASSERT
                    insn_assert(builder.CreateICmpNE(
                                    constant(R_UnboundValue, t::SEXP), res),
                                "Expected evaluated promise");
#endif
                } else {
                    setVal(i, force(i, arg));
                }
                break;
            }

            case Tag::LdFun: {
                auto ld = LdFun::Cast(i);
                gcSafepoint(i, -1, false);
                auto res =
                    call(NativeBuiltins::ldfun, {constant(ld->varName, t::SEXP),
                                                 loadSxp(i, ld->env())});
                // TODO..
                checkMissing(res);
                checkUnbound(res);
                setVal(i, res);
                setVisible(1);
                break;
            }

            case Tag::MkArg: {
                auto p = MkArg::Cast(i);
                gcSafepoint(i, 1, true);
                setVal(i,
                       call(NativeBuiltins::createPromise,
                            {paramCode(), c(promMap.at(p->prom())),
                             loadSxp(i, p->env()), loadSxp(i, p->eagerArg())}));
                break;
            }

            case Tag::LdVar: {
                auto ld = LdVar::Cast(i);

                auto env = MkEnv::Cast(ld->env());
                if (env && env->stub) {
                    setVal(i, envStubGet(loadSxp(i, env),
                                         env->indexOf(ld->varName)));
                    break;
                }

                llvm::Value* res = nullptr;
                if (bindingsCache.count(i->env())) {
                    res = builder.CreateAlloca(t::SEXP);
                    auto offset = bindingsCache.at(i->env()).at(ld->varName);

                    auto cachePtr =
                        builder.CreateGEP(bindingsCacheBase, c(offset));
                    llvm::Value* cache = builder.CreateLoad(cachePtr);

                    auto hit1 = BasicBlock::Create(C, "", fun);
                    auto hit2 = BasicBlock::Create(C, "", fun);
                    auto miss = BasicBlock::Create(C, "", fun);
                    auto done = BasicBlock::Create(C, "", fun);

                    builder.CreateCondBr(
                        builder.CreateICmpULE(
                            builder.CreatePtrToInt(cache, t::i64), c(1, 64)),
                        miss, hit1);
                    builder.SetInsertPoint(hit1);
                    auto val = car(cache);
                    builder.CreateCondBr(
                        builder.CreateICmpEQ(val,
                                             constant(R_UnboundValue, t::SEXP)),
                        miss, hit2);
                    builder.SetInsertPoint(hit2);
                    builder.CreateStore(val, res);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(miss);
                    auto res0 = call(NativeBuiltins::ldvarCacheMiss,
                                     {constant(ld->varName, t::SEXP),
                                      loadSxp(i, ld->env()), cachePtr});
                    builder.CreateStore(res0, res);
                    builder.CreateBr(done);
                    builder.SetInsertPoint(done);
                    res = builder.CreateLoad(res);
                } else {
                    res = call(NativeBuiltins::ldvar,
                               {constant(ld->varName, t::SEXP),
                                loadSxp(i, ld->env())});
                }
                res->setName(CHAR(PRINTNAME(ld->varName)));

                checkMissing(res);
                checkUnbound(res);
                setVal(i, res);
                break;
            }

            case Tag::Extract1_1D: {
                auto extract = Extract1_1D::Cast(i);
                auto vector = loadSxp(i, extract->vec());
                auto idx = loadSxp(i, extract->idx());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto env = constant(R_NilValue, t::SEXP);
                if (extract->hasEnv())
                    env = loadSxp(i, extract->env());

                gcSafepoint(i, -1, true);
                auto res = call(NativeBuiltins::extract11,
                                {vector, idx, env, c(extract->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::Extract2_1D: {
                auto extract = Extract2_1D::Cast(i);
                auto vector = loadSxp(i, extract->vec());
                auto idx = loadSxp(i, extract->idx());

                auto env = constant(R_NilValue, t::SEXP);
                if (extract->hasEnv())
                    env = loadSxp(i, extract->env());

                gcSafepoint(i, -1, true);
                auto res = call(NativeBuiltins::extract21,
                                {vector, idx, env, c(extract->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::StVar: {
                auto st = StVar::Cast(i);
                auto environment = MkEnv::Cast(st->env());

                if (environment && environment->stub) {
                    envStubSet(loadSxp(i, environment),
                               environment->indexOf(st->varName),
                               loadSxp(i, st->val()));
                    break;
                }

                auto setter = NativeBuiltins::stvar;
                if (st->isStArg)
                    setter = NativeBuiltins::starg;

                if (bindingsCache.count(environment)) {
                    auto offset = bindingsCache.at(environment).at(st->varName);
                    auto cachePtr =
                        builder.CreateGEP(bindingsCacheBase, c(offset));
                    llvm::Value* cache = builder.CreateLoad(cachePtr);

                    auto hit1 = BasicBlock::Create(C, "", fun);
                    auto hit2 = BasicBlock::Create(C, "", fun);
                    auto hit3 = BasicBlock::Create(C, "", fun);
                    auto identical = BasicBlock::Create(C, "", fun);
                    auto miss = BasicBlock::Create(C, "", fun);
                    auto done = BasicBlock::Create(C, "", fun);

                    auto newVal = loadSxp(i, st->arg<0>().val());

                    builder.CreateCondBr(
                        builder.CreateICmpULE(
                            builder.CreatePtrToInt(cache, t::i64), c(1, 64)),
                        miss, hit1);

                    builder.SetInsertPoint(hit1);
                    auto val = car(cache);
                    builder.CreateCondBr(
                        builder.CreateICmpEQ(val,
                                             constant(R_UnboundValue, t::SEXP)),
                        miss, hit2);

                    builder.SetInsertPoint(hit2);
                    builder.CreateCondBr(builder.CreateICmpEQ(val, newVal),
                                         identical, hit3);

                    builder.SetInsertPoint(hit3);
                    incrementNamed(newVal);
                    assert(cache->getType() == t::SEXP);
                    assert(newVal->getType() == t::SEXP);
                    setCar(cache, newVal);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(identical);
                    // In the fast case (where the value is not updated) we
                    // still need to ensure it is named.
                    ensureNamed(val);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(miss);
                    gcSafepoint(i, 1, true);
                    call(setter, {constant(st->varName, t::SEXP), newVal,
                                  loadSxp(i, st->env())});
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);

                } else {
                    gcSafepoint(i, 1, true);
                    call(setter, {constant(st->varName, t::SEXP),
                                  loadSxp(i, st->arg<0>().val()),
                                  loadSxp(i, st->env())});
                }
                break;
            }

            case Tag::StVarSuper: {
                auto st = StVarSuper::Cast(i);
                auto environment = MkEnv::Cast(st->env());
                if (environment) {
                    auto parent = MkEnv::Cast(environment->lexicalEnv());
                    if (environment->stub || (parent && parent->stub)) {
                        success = false;
                        break;
                    }
                }

                // In case we statically knew the parent PIR already converted
                // super assigns to standard stores
                gcSafepoint(i, 1, true);
                call(NativeBuiltins::defvar,
                     {constant(st->varName, t::SEXP),
                      loadSxp(i, st->arg<0>().val()), loadSxp(i, st->env())});
                break;
            }

            case Tag::ChkMissing: {
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp)
                    checkMissing(loadSxp(i, arg));
                setVal(i, loadSame(i, arg));
                break;
            }

            default:
                success = false;
                break;
            }

            if (!success) {
                // std::cerr << "Can't compile ";
                // i->print(std::cerr, true);
                // std::cerr << "\n";
            }

            if (!success)
                return;

            if (phis.count(i)) {
                auto r = Representation(
                    phis.at(i)->getType()->getPointerElementType());
                if (PirCopy::Cast(i)) {
                    builder.CreateStore(load(i, i->arg(0).val(), r),
                                        phis.at(i));
                } else {
                    builder.CreateStore(load(i, i, r), phis.at(i));
                }
            }

            if (representationOf(i) == Representation::Sexp &&
                needsEnsureNamed.count(i)) {
                ensureNamed(loadSxp(i, i));
            }
        }

        if (bb->isJmp()) {
            builder.CreateBr(getBlock(bb->next()));
        }
    });

    if (success) {
        // outs() << "Compiled " << fun->getName() << "\n";
        // fun->dump();
    }
    return success;
}

} // namespace pir
} // namespace rir

#include "lower_llvm.h"

namespace rir {
namespace pir {

void* LowerLLVM::tryCompile(
    ClosureVersion* cls, Code* code,
    const std::unordered_map<Promise*, unsigned>& m,
    const std::unordered_set<Instruction*>& needsEnsureNamed) {

    JitLLVM::createModule();
    auto mangledName = JitLLVM::mangle(cls->name());
    LowerFunctionLLVM funCompiler(mangledName, cls, code, m, needsEnsureNamed);
    if (!funCompiler.tryCompile())
        return nullptr;

    return JitLLVM::tryCompile(funCompiler.fun);
}

} // namespace pir
} // namespace rir
