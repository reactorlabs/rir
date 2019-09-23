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
    const std::unordered_set<Instruction*>& needsSetShared;
    bool refcountAnalysisOverflow;
    IRBuilder<> builder;
    MDBuilder MDB;
    CFG cfg;
    LivenessIntervals liveness;
    size_t numLocals;
    size_t numTemps;
    constexpr static size_t MAX_TEMPS = 4;
    llvm::Value* basepointer = nullptr;
    llvm::Value* constantpool = nullptr;
    BasicBlock* entryBlock = nullptr;
    int inPushContext = 0;
    std::unordered_set<Value*> escapesInlineContext;

    struct ContextData {
        llvm::AllocaInst* rcntxt;
        llvm::AllocaInst* result;
        llvm::BasicBlock* popContextTarget;
        std::unordered_map<Instruction*, size_t> savedSexpPos;
    };
    std::unordered_map<Value*, ContextData> contexts;

    std::unordered_map<Value*, std::unordered_map<SEXP, size_t>> bindingsCache;
    llvm::Value* bindingsCacheBase = nullptr;

  public:
    llvm::Function* fun;

    LowerFunctionLLVM(const std::string& name, ClosureVersion* cls, Code* code,
                      const std::unordered_map<Promise*, unsigned>& promMap,
                      const std::unordered_set<Instruction*>& needsEnsureNamed,
                      const std::unordered_set<Instruction*>& needsSetShared,
                      bool refcountAnalysisOverflow)
        : cls(cls), code(code), promMap(promMap),
          needsEnsureNamed(needsEnsureNamed), needsSetShared(needsSetShared),
          refcountAnalysisOverflow(refcountAnalysisOverflow), builder(C),
          MDB(C), cfg(code), liveness(code->nextBBId, cfg), numLocals(0),
          numTemps(0) {

        fun = JitLLVM::declare(name, t::nativeFunction);
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

    struct Variable {
        enum Kind {
            MutableLocalRVariable,
            ImmutableLocalRVariable,
            MutablePrimitive,
            ImmutablePrimitive,
        };
        Kind kind;

        static Variable MutableRVariable(Instruction* i, size_t pos,
                                         IRBuilder<>& builder,
                                         llvm::Value* basepointer) {
            auto v = RVariable(i, pos, builder, basepointer);
            v.kind = MutableLocalRVariable;
            return v;
        }

        static Variable RVariable(Instruction* i, size_t pos,
                                  IRBuilder<>& builder,
                                  llvm::Value* basepointer) {
            assert(i->producesRirResult());
            assert(!LdConst::Cast(i));
            assert(!CastType::Cast(i));
            assert(representationOf(i) == Representation::Sexp);
            auto ptr = builder.CreateGEP(basepointer, {c(pos), c(1)});
            ptr->setName(i->getRef());
            return {ImmutableLocalRVariable, ptr, false};
        }

        static Variable Mutable(Instruction* i, AllocaInst* location) {
            assert(i->producesRirResult());
            auto r = representationOf(i);
            assert(r != Representation::Sexp);
            location->setName(i->getRef());
            return {MutablePrimitive, location, false};
        }

        static Variable Immutable(Instruction* i) {
            assert(i->producesRirResult());
            auto r = representationOf(i);
            assert(r != Representation::Sexp);
            return {ImmutablePrimitive, nullptr, false};
        }

        llvm::Value* get(IRBuilder<>& builder) {
            assert(initialized);
            assert(slot);
            switch (kind) {
            case ImmutableLocalRVariable:
            case MutableLocalRVariable:
            case MutablePrimitive:
                return builder.CreateLoad(slot);
            case ImmutablePrimitive:
                return slot;
            }
            assert(false);
            return nullptr;
        }

        void update(IRBuilder<>& builder, llvm::Value* val,
                    bool volatile_ = false) {
            initialized = true;
            switch (kind) {
            case MutableLocalRVariable:
            case MutablePrimitive:
                assert(slot);
                builder.CreateStore(val, slot, volatile_);
                break;
            case ImmutableLocalRVariable:
            case ImmutablePrimitive:
                assert(false);
                break;
            }
        }

        void set(IRBuilder<>& builder, llvm::Value* val,
                 bool volatile_ = false) {
            assert(!initialized);
            initialized = true;
            switch (kind) {
            case ImmutableLocalRVariable:
            case MutableLocalRVariable:
            case MutablePrimitive:
                assert(slot);
                builder.CreateStore(val, slot, volatile_);
                break;
            case ImmutablePrimitive:
                slot = val;
                break;
            }
        }

        llvm::Value* slot;
        bool initialized;
    };

    std::unordered_map<Instruction*, Variable> variables;

    llvm::Value* constant(SEXP co, llvm::Type* needed);
    llvm::Value* nodestackPtr();
    llvm::Value* stack(int i);
    void stack(const std::vector<llvm::Value*>& args);
    void setLocal(size_t i, llvm::Value* v);
    llvm::Value* getLocal(size_t i);
    void incStack(int i, bool zero);
    void decStack(int i);
    llvm::Value* withCallFrame(const std::vector<Value*>& args,
                               const std::function<llvm::Value*()>& theCall,
                               bool pop = true);
    llvm::Value* load(Value* v, Representation r);
    llvm::Value* load(Value* v);
    llvm::Value* loadSxp(Value* v);
    llvm::Value* load(Value* val, PirType type, Representation needed);
    llvm::Value* dataPtr(llvm::Value* v, bool enableAsserts = true);
    llvm::Value* accessVector(llvm::Value* vector, llvm::Value* position,
                              PirType type);
    llvm::Value* assignVector(llvm::Value* vector, llvm::Value* position,
                              llvm::Value* value, PirType type);

    llvm::Value* unboxIntLgl(llvm::Value* v);
    llvm::Value* unboxInt(llvm::Value* v);
    llvm::Value* unboxLgl(llvm::Value* v);
    llvm::Value* unboxReal(llvm::Value* v);
    llvm::Value* unboxRealIntLgl(llvm::Value* v);

    void writeBarrier(llvm::Value* x, llvm::Value* y, std::function<void()> no,
                      std::function<void()> yes);
    llvm::Value* envStubGet(llvm::Value* x, int i, size_t size);
    void envStubSet(llvm::Value* x, int i, llvm::Value* y, size_t size,
                    bool setNotMissing);

    void setVisible(int i);

    std::array<std::string, 4> argNames = {{"code", "args", "env", "closure"}};
    std::vector<llvm::Value*> args;
    llvm::Value* paramCode() { return args[0]; }
    llvm::Value* paramArgs() { return args[1]; }
    llvm::Value* paramEnv() { return args[2]; }
    llvm::Value* paramClosure() { return args[3]; }

    static llvm::Constant* c(void* i) {
        return llvm::ConstantInt::get(C, APInt(64, (intptr_t)i));
    }

    static llvm::Constant* c(unsigned long i, int bs = 64) {
        return llvm::ConstantInt::get(C, APInt(bs, i));
    }

    static llvm::Constant* c(long i, int bs = 64) {
        return llvm::ConstantInt::get(C, APInt(bs, i));
    }

    static llvm::Constant* c(unsigned int i, int bs = 32) {
        return llvm::ConstantInt::get(C, APInt(bs, i));
    }

    static llvm::Constant* c(int i, int bs = 32) {
        return llvm::ConstantInt::get(C, APInt(bs, i));
    }

    static llvm::Constant* c(double d) {
        return llvm::ConstantFP::get(C, llvm::APFloat(d));
    }

    static llvm::Constant* c(const std::vector<unsigned int>& array) {
        std::vector<llvm::Constant*> init;
        for (const auto& e : array)
            init.push_back(c(e));
        auto ty = llvm::ArrayType::get(t::Int, array.size());
        return llvm::ConstantArray::get(ty, init);
    }

    static llvm::Value* globalConst(llvm::Constant* init,
                                    llvm::Type* ty = nullptr) {
        if (!ty)
            ty = init->getType();
        return new llvm::GlobalVariable(JitLLVM::module(), ty, true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init);
    };

    llvm::AllocaInst* topAlloca(llvm::Type* t, size_t len = 1);

    llvm::Value* argument(int i);
    llvm::Value* convert(llvm::Value* val, PirType to, bool protect = true);
    void setVal(Instruction* i, llvm::Value* val);

    llvm::Value* isExternalsxp(llvm::Value* v, uint32_t magic);
    void checkSexptype(llvm::Value* v, const std::vector<SEXPTYPE>& types);
    void checkIsSexp(llvm::Value* v, const std::string& msg = "");
    void setSexptype(llvm::Value* v, int t);
    llvm::Value* sexptype(llvm::Value* v);
    llvm::Value* attr(llvm::Value* v);
    llvm::Value* vectorLength(llvm::Value* v);
    llvm::Value* tag(llvm::Value* v);
    llvm::Value* car(llvm::Value* v);
    llvm::Value* cdr(llvm::Value* v);
    void setCar(llvm::Value* x, llvm::Value* y, bool writeBarrier = true);
    void setCdr(llvm::Value* x, llvm::Value* y, bool writeBarrier = true);
    void setTag(llvm::Value* x, llvm::Value* y, bool writeBarrier = true);
    llvm::Value* isObj(llvm::Value*);
    llvm::Value* isAltrep(llvm::Value*);
    llvm::Value* sxpinfoPtr(llvm::Value*);

    void protectTemp(llvm::Value* v);

    void ensureNamed(llvm::Value* v);
    llvm::Value* shared(llvm::Value* v);
    void assertNamed(llvm::Value* v);
    void ensureShared(llvm::Value* v);
    void incrementNamed(llvm::Value* v, int max = NAMEDMAX);
    void nacheck(llvm::Value* v, BasicBlock* isNa, BasicBlock* notNa = nullptr);
    void checkMissing(llvm::Value* v);
    void checkUnbound(llvm::Value* v);
    llvm::Value* call(const NativeBuiltin& builtin,
                      const std::vector<llvm::Value*>& args);
    llvm::Value* box(llvm::Value* v, PirType t, bool protect = true);
    llvm::Value* boxInt(llvm::Value* v, bool protect = true);
    llvm::Value* boxReal(llvm::Value* v, bool protect = true);
    llvm::Value* boxLgl(llvm::Value* v, bool protect = true);
    llvm::Value* boxTst(llvm::Value* v, bool protect = true);
    llvm::Value* depromise(llvm::Value* arg);
    void insn_assert(llvm::Value* v, const char* msg);

    llvm::Value* force(Instruction* i, llvm::Value* arg);

    llvm::Value* computeAndCheckIndex(Value* index, llvm::Value* vector,
                                      BasicBlock* fallback,
                                      BasicBlock* hit = nullptr);
    bool compileDotcall(Instruction* i,
                        const std::function<llvm::Value*()>& callee,
                        const std::function<SEXP(size_t)>& names);

    void compilePushContext(Instruction* i);
    void compilePopContext(Instruction* i);

    void compileBinop(
        Instruction* i,
        const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>&
            intInsert,
        const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& fpInsert,
        BinopKind kind) {
        compileBinop(i, i->arg(0).val(), i->arg(1).val(), intInsert, fpInsert,
                     kind);
    }
    void compileBinop(
        Instruction* i, Value* lhs, Value* rhs,
        const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>&
            intInsert,
        const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& fpInsert,
        BinopKind kind);
    void compileUnop(Instruction* i,
                     const std::function<llvm::Value*(llvm::Value*)>& intInsert,
                     const std::function<llvm::Value*(llvm::Value*)>& fpInsert,
                     UnopKind kind) {
        compileUnop(i, i->arg(0).val(), intInsert, fpInsert, kind);
    }
    void compileUnop(Instruction* i, Value* lhs,
                     const std::function<llvm::Value*(llvm::Value*)>& intInsert,
                     const std::function<llvm::Value*(llvm::Value*)>& fpInsert,
                     UnopKind kind);
    void compileRelop(
        Instruction* i,
        const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>&
            intInsert,
        const std::function<llvm::Value*(llvm::Value*, llvm::Value*)>& fpInsert,
        BinopKind kind);

    bool success = true;
    bool tryCompile();

  private:
    llvm::Value* vectorPositionPtr(llvm::Value* vector, llvm::Value* position,
                                   PirType type);
};

void LowerFunctionLLVM::setVisible(int i) {
    builder.CreateStore(c(i), convertToPointer(&R_Visible, t::IntPtr));
}

llvm::Value* LowerFunctionLLVM::force(Instruction* i, llvm::Value* arg) {

    auto isProm = BasicBlock::Create(C, "isProm", fun);
    auto needsEval = BasicBlock::Create(C, "needsEval", fun);
    auto ok = BasicBlock::Create(C, "ok", fun);

    llvm::Value* res = builder.CreateAlloca(t::SEXP);
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
    auto evaled = call(NativeBuiltins::forcePromise, {arg});
    checkIsSexp(evaled, "force result");
    builder.CreateStore(evaled, res);
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
    res = builder.CreateLoad(res);
#ifdef ENABLE_SLOWASSERT
    insn_assert(builder.CreateICmpNE(sexptype(res), c(PROMSXP)),
                "Force returned promise");
#endif
    return res;
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
    pos = builder.CreateBitCast(dataPtr(pos, false),
                                PointerType::get(t::SEXP, 0));
    pos = builder.CreateGEP(pos, c(i));
    auto res = builder.CreateLoad(pos);
    return res;
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
        return load(arg, cast->type, needed);
    }

    auto vali = Instruction::Cast(val);
    if (vali && variables.count(vali))
        res = variables.at(vali).get(builder);
    else if (val == Env::elided())
        // Some "safe" builtins still look up functions in the global env
        res = constant(R_GlobalEnv, needed);
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
                                                     BasicBlock* hit) {
    if (!hit)
        hit = BasicBlock::Create(C, "", fun);
    llvm::Value* nativeIndex = nullptr;
    if (representationOf(index) == Representation::Sexp) {
        auto vectorizedIndex = loadSxp(index);
        nativeIndex = accessVector(vectorizedIndex, c(0), index->type);
        if (index->type.isA(PirType(RType::real))) {
            nativeIndex = builder.CreateTrunc(nativeIndex, t::i64);
        } else {
            nativeIndex = builder.CreateZExt(nativeIndex, t::i64);
        }
        nacheck(nativeIndex, fallback);
    } else if (representationOf(index) == Representation::Real) {
        nativeIndex =
            builder.CreateFPToUI(load(index, Representation::Real), t::i64);
    } else {
        nativeIndex =
            builder.CreateZExt(load(index, Representation::Integer), t::i64);
    }

    nativeIndex = builder.CreateSub(nativeIndex, c(1ul));
    auto ty = vector->getType();
    assert(ty == t::SEXP || ty == t::Int || ty == t::Double);
    auto veclength = (ty == t::SEXP) ? vectorLength(vector) : c(1ul);
    auto indexOverRange = builder.CreateICmpUGE(nativeIndex, veclength);
    auto indexUnderRange = builder.CreateICmpULT(nativeIndex, c(0ul));
    builder.CreateCondBr(builder.CreateOr(indexOverRange, indexUnderRange),
                         fallback, hit);
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
                    auto v = var.get(builder);
                    ensureShared(v);
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
    } else {
        nativeType = t::SEXP_ptr;
        assert(false);
    }
    auto pos = builder.CreateBitCast(dataPtr(vector), nativeType);
    return builder.CreateGEP(pos, position);
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
    builder.CreateStore(c(R_NaN), res);
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

AllocaInst* LowerFunctionLLVM::topAlloca(llvm::Type* t, size_t len) {
    auto cur = builder.GetInsertBlock();
    builder.SetInsertPoint(entryBlock);
    auto res = builder.CreateAlloca(t, 0, c(len));
    builder.SetInsertPoint(cur);
    return res;
}

llvm::Value* LowerFunctionLLVM::convert(llvm::Value* val, PirType toType,
                                        bool protect) {
    auto to = representationOf(toType);
    auto from = val->getType();
    if (from == to)
        return val;

    if (from == t::SEXP && to == t::Int)
        return unboxIntLgl(val);
    if (from == t::SEXP && to == t::Double)
        return unboxRealIntLgl(val);
    if (from == t::SEXP && to != t::SEXP)
        return box(val, toType, protect);

    std::cout << "\nFailed to convert a " << val->getType() << " to " << toType
              << "\n";
    assert(false);
    return nullptr;
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
    auto named = builder.CreateLShr(sxpinfo, c(32, 64));
    named = builder.CreateAnd(named, c(namedMask));
    return builder.CreateICmpUGT(named, c(1, 64));
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
        assert(v->getType() == t::Int);
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
llvm::Value* LowerFunctionLLVM::boxReal(llvm::Value* v, bool potect) {
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
    auto ok = BasicBlock::Create(C, "ok", fun);

    llvm::Value* res = builder.CreateAlloca(t::SEXP);
    builder.CreateStore(arg, res);

    auto type = sexptype(arg);
    auto tt = builder.CreateICmpEQ(type, c(PROMSXP));

    builder.CreateCondBr(tt, isProm, ok);

    builder.SetInsertPoint(isProm);
    auto val = car(arg);
    builder.CreateStore(val, res);
    builder.CreateBr(ok);

    builder.SetInsertPoint(ok);
    res = builder.CreateLoad(res);
#ifdef ENABLE_SLOWASSERT
    insn_assert(builder.CreateICmpNE(sexptype(res), c(PROMSXP)),
                "Depromise returned promise");
#endif
    return res;
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

    llvm::Value* res = builder.CreateAlloca(t::Int);
    auto a = load(lhs, lhsRep);
    auto b = load(rhs, rhsRep);

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
    if (rep == Representation::Sexp) {
        setVal(i, boxLgl(res, false));
    } else {
        setVal(i, res);
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

    llvm::Value* res = builder.CreateAlloca(r);

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
                builder.CreateStore(c((double)R_NaN), res);

            builder.CreateBr(done);
        }
    }

    builder.SetInsertPoint(done);
    res = builder.CreateLoad(res);
    if (rep == Representation::Sexp) {
        setVal(i, box(res, lhs->type.mergeWithConversion(rhs->type), false));
    } else {
        setVal(i, res);
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

    llvm::Value* res = builder.CreateAlloca(r);

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
        builder.CreateStore(intInsert(a), res);
    } else {
        builder.CreateStore(fpInsert(a), res);
    }
    builder.CreateBr(done);

    if (argRep == Representation::Integer) {
        if (isNaBr) {
            builder.SetInsertPoint(isNaBr);

            if (r == t::Int)
                builder.CreateStore(c(NA_INTEGER), res);
            else
                builder.CreateStore(c((double)R_NaN), res);

            builder.CreateBr(done);
        }
    }

    builder.SetInsertPoint(done);
    res = builder.CreateLoad(res);
    setVal(i, res);
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

    setVal(i, withCallFrame(
                  args,
                  [&]() -> llvm::Value* {
                      return call(
                          NativeBuiltins::dotsCall,
                          {
                              paramCode(),
                              c(i->srcIdx),
                              callee(),
                              loadSxp(i->hasEnv() ? i->env() : Env::elided()),
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

llvm::Value* LowerFunctionLLVM::isAltrep(llvm::Value* v) {
    checkIsSexp(v, "in is altrep");
    auto sxpinfo = builder.CreateLoad(sxpinfoPtr(v));
    return builder.CreateICmpNE(
        c(0, 64),
        builder.CreateAnd(sxpinfo, c((unsigned long)(1ul << (TYPE_BITS + 2)))));
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
    entryBlock = BasicBlock::Create(C, "", fun);
    builder.SetInsertPoint(entryBlock);

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

    std::unordered_map<Instruction*, Instruction*> phis;
    {
        basepointer = nodestackPtr();
        auto needsVariable = [](Instruction* v) {
            return v->producesRirResult() && !LdConst::Cast(v) &&
                   !CastType::Cast(v);
        };
        auto createVariable = [&](Instruction* i, bool mut) {
            if (representationOf(i) == Representation::Sexp) {
                if (mut)
                    variables[i] = Variable::MutableRVariable(
                        i, numLocals++, builder, basepointer);
                else
                    variables[i] = Variable::RVariable(i, numLocals++, builder,
                                                       basepointer);
            } else {
                if (mut)
                    variables[i] =
                        Variable::Mutable(i, topAlloca(representationOf(i)));
                else
                    variables[i] = Variable::Immutable(i);
            }
        };

        auto arg = fun->arg_begin();
        for (size_t i = 0; i < argNames.size(); ++i) {
            args.push_back(arg);
            args.back()->setName(argNames[i]);
            arg++;
        }

        constantpool = builder.CreateIntToPtr(c(globalContext()), t::SEXP_ptr);
        constantpool = builder.CreateGEP(constantpool, c(1));

        Visitor::run(code->entry, [&](BB* bb) {
            for (auto i : *bb) {
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

        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto pop = PopContext::Cast(i)) {
                auto res = pop->result();
                auto push = pop->push();
                auto resStore = builder.CreateAlloca(representationOf(res));
                auto rcntxt = builder.CreateAlloca(t::RCNTXT);
                contexts[push] = {rcntxt, resStore,
                                  BasicBlock::Create(C, "", fun)};

                // Everything which is live at the Push context needs to be
                // mutable, to be able to restore on restart
                Visitor::run(code->entry, [&](Instruction* j) {
                    if (needsVariable(j)) {
                        if (representationOf(j) == t::SEXP &&
                            liveness.live(push, j)) {
                            contexts[push].savedSexpPos[j] = numLocals++;
                        }
                        if (!liveness.live(push, j) && liveness.live(pop, j))
                            escapesInlineContext.insert(j);
                        if (!variables.count(j) &&
                            (liveness.live(push, j) || liveness.live(pop, j)))
                            createVariable(j, true);
                    }
                });
            }
        });
        Visitor::run(code->entry, [&](Instruction* i) {
            if (needsVariable(i) && !variables.count(i))
                createVariable(i, false);
        });
    }

    numLocals += MAX_TEMPS;
    if (numLocals > 0)
        incStack(numLocals, true);

    std::unordered_map<BB*, int> blockInPushContext;
    blockInPushContext[code->entry] = 0;

    LoweringVisitor::run(code->entry, [&](BB* bb) {
        if (!success)
            return;

        builder.SetInsertPoint(getBlock(bb));
        inPushContext = blockInPushContext.at(bb);

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto i = *it;
            if (!success)
                return;

            switch (i->tag) {
            case Tag::ExpandDots:
                // handled in calls
                break;

            case Tag::DotsList: {
                auto mk = DotsList::Cast(i);
                auto arglist = constant(R_NilValue, t::SEXP);
                mk->eachElementRev([&](SEXP name, Value* v) {
                    auto val = loadSxp(v);
                    incrementNamed(val);
                    arglist = call(NativeBuiltins::consNr, {val, arglist});
                    setTag(arglist, constant(name, t::SEXP), false);
                });
                setSexptype(arglist, DOTSXP);
                setVal(i, arglist);
                break;
            }

            case Tag::RecordDeoptReason: {
                auto rec = RecordDeoptReason::Cast(i);
                auto reason = builder.CreateAlloca(t::DeoptReason);
                builder.CreateStore(c(rec->reason.reason, 32),
                                    builder.CreateGEP(reason, {c(0), c(0)}));
                builder.CreateStore(convertToPointer(rec->reason.srcCode),
                                    builder.CreateGEP(reason, {c(0), c(1)}));
                builder.CreateStore(c(rec->reason.originOffset),
                                    builder.CreateGEP(reason, {c(0), c(2)}));
                call(NativeBuiltins::recordDeopt,
                     {loadSxp(rec->arg<0>().val()), reason});
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

            case Tag::PirCopy: {
                auto c = PirCopy::Cast(i);
                auto in = c->arg<0>().val();
                if (Phi::Cast(in))
                    setVal(i, load(in, representationOf(i)));
                break;
            }

            case Tag::Phi:
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
                auto a = depromise(load(i->arg(0).val()));
                auto b = depromise(load(i->arg(1).val()));
                setVal(i,
                       builder.CreateZExt(builder.CreateICmpEQ(a, b), t::Int));
                break;
            }

            case Tag::CallSafeBuiltin: {
                auto b = CallSafeBuiltin::Cast(i);
                if (compileDotcall(b,
                                   [&]() { return constant(b->blt, t::SEXP); },
                                   [&](size_t i) { return R_NilValue; })) {
                    break;
                }
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

                                auto xInt = load(x, Representation::Integer);
                                auto yInt = load(y, Representation::Integer);

                                auto naCheck = [&](Value* v, llvm::Value* asInt,
                                                   Representation rep) {
                                    if (rep == Representation::Real) {
                                        auto vv = load(v, rep);
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

                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(NativeBuiltins::callBuiltin,
                                       {
                                           paramCode(),
                                           c(b->srcIdx),
                                           constant(b->blt, t::SEXP),
                                           load(Env::elided()),
                                           c(b->nCallArgs()),
                                       });
                       }));
                break;
            }

            case Tag::CallBuiltin: {
                auto b = CallBuiltin::Cast(i);
                if (compileDotcall(b,
                                   [&]() { return constant(b->blt, t::SEXP); },
                                   [&](size_t i) { return R_NilValue; })) {
                    break;
                }
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(NativeBuiltins::callBuiltin,
                                       {
                                           paramCode(),
                                           c(b->srcIdx),
                                           constant(b->blt, t::SEXP),
                                           loadSxp(b->env()),
                                           c(b->nCallArgs()),
                                       });
                       }));
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
                Assumptions asmpt = b->inferAvailableAssumptions();
                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(NativeBuiltins::call,
                                       {paramCode(), c(b->srcIdx),
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
                Assumptions asmpt = b->inferAvailableAssumptions();

                std::vector<BC::PoolIdx> names;
                for (size_t i = 0; i < b->names.size(); ++i)
                    names.push_back(Pool::insert((b->names[i])));
                auto namesConst = c(names);
                auto namesStore = globalConst(namesConst);

                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(
                               NativeBuiltins::namedCall,
                               {
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

                if (target == bestTarget) {
                    auto callee = target->owner()->rirClosure();
                    auto dt = DispatchTable::check(BODY(callee));
                    assert(cls);
                    rir::Function* nativeTarget = nullptr;
                    for (size_t i = 0; i < dt->size(); i++) {
                        auto entry = dt->get(i);
                        if (entry->signature().assumptions ==
                                target->assumptions() &&
                            entry->signature().numArguments >= args.size()) {
                            nativeTarget = entry;
                        }
                    }
                    if (nativeTarget && nativeTarget->body()->nativeCode) {
                        Assumptions asmpt = calli->inferAvailableAssumptions();
                        assert(
                            asmpt.includes(Assumption::StaticallyArgmatched));
                        auto res = withCallFrame(args, [&]() {
                            return call(NativeBuiltins::nativeCallTrampoline,
                                        {
                                            constant(callee, t::SEXP),
                                            builder.CreateIntToPtr(
                                                c(nativeTarget), t::voidPtr),
                                            c(calli->srcIdx),
                                            loadSxp(calli->env()),
                                            c(args.size()),
                                            c(asmpt.toI()),
                                        });
                        });
                        setVal(i, res);
                        break;
                    }
                }

                Assumptions asmpt = calli->inferAvailableAssumptions();
                assert(asmpt.includes(Assumption::StaticallyArgmatched));
                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(
                               NativeBuiltins::call,
                               {
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

            case Tag::AsInt: {
                auto arg = i->arg(0).val();
                auto asint = AsInt::Cast(i);
                llvm::Value* res = nullptr;
                if (representationOf(arg) == Representation::Integer) {
                    res = load(arg, Representation::Integer);
                } else if (representationOf(arg) == Representation::Real) {
                    auto a = load(arg, Representation::Real);
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
                    res = load(arg, Representation::Integer);
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
                    res = load(arg, Representation::Integer);
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

            case Tag::ForSeqSize: {
                llvm::Value* res = call(NativeBuiltins::forSeqSize,
                                        {loadSxp(i->arg(0).val())});
                if (representationOf(i) == Representation::Real)
                    res = builder.CreateSIToFP(res, t::Double);
                else if (representationOf(i) == Representation::Sexp)
                    res = boxInt(res);
                setVal(i, res);
                break;
            }

            case Tag::Branch: {
                auto cond = load(i->arg(0).val(), Representation::Integer);
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
                withCallFrame(args, [&]() {
                    return call(NativeBuiltins::deopt,
                                {paramCode(), paramClosure(),
                                 convertToPointer(m), paramArgs()});
                });
                builder.CreateRet(builder.CreateIntToPtr(c(nullptr), t::SEXP));
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
                        call(NativeBuiltins::createStubEnvironment,
                             {parent, c((int)mkenv->nLocals()),
                              builder.CreateBitCast(namesStore, t::IntPtr),
                              c(mkenv->context)});
                    size_t pos = 0;
                    mkenv->eachLocalVar([&](SEXP name, Value* v, bool miss) {
                        envStubSet(env, pos++, loadSxp(v), mkenv->nLocals(),
                                   false);
                    });
                    setVal(i, env);
                    break;
                }

                auto arglist = constant(R_NilValue, t::SEXP);
                mkenv->eachLocalVarRev([&](SEXP name, Value* v, bool miss) {
                    if (miss) {
                        arglist = call(
                            NativeBuiltins::createMissingBindingCell,
                            {loadSxp(v), constant(name, t::SEXP), arglist});
                    } else {
                        arglist = call(
                            NativeBuiltins::createBindingCell,
                            {loadSxp(v), constant(name, t::SEXP), arglist});
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
            case Tag::Pow:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateIntrinsic(
                            Intrinsic::powi, {a->getType(), b->getType()},
                            {a, b});
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateIntrinsic(
                            Intrinsic::pow, {a->getType(), b->getType()},
                            {a, b});
                    },
                    BinopKind::POW);
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
                auto resultRep = representationOf(i);
                auto argument = i->arg(0).val();
                auto argumentRep = representationOf(argument);
                if (argumentRep == Representation::Sexp) {
                    auto argumentNative = loadSxp(argument);

                    llvm::Value* res = nullptr;
                    if (i->hasEnv()) {
                        res = call(
                            NativeBuiltins::notEnv,
                            {argumentNative, loadSxp(i->env()), c(i->srcIdx)});
                    } else {
                        res = call(NativeBuiltins::notOp, {argumentNative});
                    }
                    setVal(i, res);
                    break;
                }

                auto done = BasicBlock::Create(C, "", fun);
                auto isNa = BasicBlock::Create(C, "", fun);

                auto res = builder.CreateAlloca(t::Int);
                auto argumentNative = load(argument, argumentRep);

                nacheck(argumentNative, isNa);

                builder.CreateStore(
                    builder.CreateZExt(
                        builder.CreateICmpEQ(argumentNative, c(0)), t::Int),
                    res);
                builder.CreateBr(done);

                builder.SetInsertPoint(isNa);
                // Maybe we need to model R_LogicalNAValue?
                builder.CreateStore(c(NA_INTEGER), res);
                builder.CreateBr(done);
                builder.SetInsertPoint(done);

                auto result = builder.CreateLoad(res);
                if (resultRep == Representation::Sexp) {
                    setVal(i, boxLgl(result, true));
                } else {
                    setVal(i, result);
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
                                 a = builder.CreateZExt(
                                     builder.CreateICmpNE(a, c(0)), t::Int);
                                 b = builder.CreateZExt(
                                     builder.CreateICmpNE(b, c(0)), t::Int);
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
            case Tag::IDiv:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        auto isZero = BasicBlock::Create(C, "", fun);
                        auto notZero = BasicBlock::Create(C, "", fun);
                        auto cnt = BasicBlock::Create(C, "", fun);
                        llvm::Value* res = builder.CreateAlloca(t::Int);
                        builder.CreateCondBr(builder.CreateICmpEQ(b, c(0)),
                                             isZero, notZero);

                        builder.SetInsertPoint(isZero);
                        builder.CreateStore(c(NA_INTEGER), res);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(notZero);
                        auto r = builder.CreateFDiv(
                            builder.CreateSIToFP(a, t::Double),
                            builder.CreateSIToFP(b, t::Double));
                        builder.CreateStore(builder.CreateFPToSI(r, t::Int),
                                            res);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return builder.CreateLoad(res);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        // from myfloor
                        auto q = builder.CreateFDiv(a, b);
                        auto isZero = BasicBlock::Create(C, "", fun);
                        auto notZero = BasicBlock::Create(C, "", fun);
                        auto cnt = BasicBlock::Create(C, "", fun);
                        llvm::Value* res = builder.CreateAlloca(t::Double);
                        builder.CreateCondBr(builder.CreateFCmpOEQ(b, c(0.0)),
                                             isZero, notZero);

                        builder.SetInsertPoint(isZero);
                        builder.CreateStore(q, res);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(notZero);
                        auto fq = builder.CreateIntrinsic(Intrinsic::floor,
                                                          {t::Double}, {q});
                        auto tmp =
                            builder.CreateFSub(a, builder.CreateFMul(fq, b));
                        auto frem = builder.CreateIntrinsic(
                            Intrinsic::floor, {t::Double},
                            {builder.CreateFDiv(tmp, b)});
                        builder.CreateStore(builder.CreateFAdd(fq, frem), res);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return builder.CreateLoad(res);
                    },
                    BinopKind::IDIV);
                break;
            case Tag::Mod: {
                auto myfmod =
                    [&](llvm::Value* a, llvm::Value* b) {
                        // from myfmod
                        auto isZero = BasicBlock::Create(C, "", fun);
                        auto notZero = BasicBlock::Create(C, "", fun);
                        auto cnt = BasicBlock::Create(C, "", fun);
                        llvm::Value* res = builder.CreateAlloca(t::Double);
                        builder.CreateCondBr(builder.CreateFCmpOEQ(b, c(0.0)),
                                             isZero, notZero);

                        builder.SetInsertPoint(isZero);
                        builder.CreateStore(c(R_NaN), res);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(notZero);
                        auto q = builder.CreateFDiv(a, b);
                        auto fq = builder.CreateIntrinsic(Intrinsic::floor,
                                                          {t::Double}, {q});

                        auto absq = builder.CreateIntrinsic(Intrinsic::fabs,
                                                            {t::Double}, {q});
                        auto finite = builder.CreateFCmpONE(
                            absq, c((double)0x7FF0000000000000));
                        auto gt = builder.CreateFCmpOGT(
                            absq, c(1 / R_AccuracyInfo.eps));

                        auto warn = BasicBlock::Create(C, "", fun);
                        auto noWarn = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(builder.CreateAnd(finite, gt),
                                             warn, noWarn);

                        builder.SetInsertPoint(warn);
                        auto msg = builder.CreateGlobalString(
                            "probable complete loss of accuracy in modulus");
                        call(NativeBuiltins::warn,
                             {builder.CreateBitCast(msg, t::voidPtr)});
                        builder.CreateBr(noWarn);

                        builder.SetInsertPoint(noWarn);
                        auto tmp =
                            builder.CreateFSub(a, builder.CreateFMul(fq, b));
                        auto frem = builder.CreateIntrinsic(
                            Intrinsic::floor, {t::Double},
                            {builder.CreateFDiv(tmp, b)});
                        builder.CreateStore(
                            builder.CreateFSub(tmp,
                                               builder.CreateFMul(frem, b)),
                            res);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return builder.CreateLoad(res);
                    };

                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        auto fast = BasicBlock::Create(C, "", fun);
                        auto fast1 = BasicBlock::Create(C, "", fun);
                        auto slow = BasicBlock::Create(C, "", fun);
                        auto cnt = BasicBlock::Create(C, "", fun);
                        llvm::Value* res = builder.CreateAlloca(t::Int);
                        builder.CreateCondBr(builder.CreateICmpSGE(a, c(0)),
                                             fast1, slow);

                        builder.SetInsertPoint(fast1);
                        builder.CreateCondBr(builder.CreateICmpSGT(b, c(0)),
                                             fast, slow);

                        builder.SetInsertPoint(fast);
                        builder.CreateStore(builder.CreateSRem(a, b), res);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(slow);
                        builder.CreateStore(
                            builder.CreateFPToSI(
                                myfmod(builder.CreateSIToFP(a, t::Double),
                                       builder.CreateSIToFP(b, t::Double)),
                                t::Int),
                            res);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return builder.CreateLoad(res);
                    },
                    myfmod, BinopKind::MOD);
                break;
            }
            case Tag::Colon: {
                assert(representationOf(i) == t::SEXP);
                auto a = loadSxp(i->arg(0).val());
                auto b = loadSxp(i->arg(1).val());
                auto e = loadSxp(i->env());
                auto res =
                    call(NativeBuiltins::binopEnv,
                         {a, b, e, c(i->srcIdx), c((int)BinopKind::COLON)});
                setVal(i, res);
                break;
            }

            case Tag::CastType: {
                // Scheduled on use
                break;
            }

            case Tag::Return: {
                auto res = loadSxp(Return::Cast(i)->arg<0>().val());
                if (numLocals > 0)
                    decStack(numLocals);
                builder.CreateRet(res);
                break;
            }

            case Tag::IsEnvStub: {
                auto arg = loadSxp(i->arg(0).val());
                auto env = MkEnv::Cast(i->env());

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
                auto materialized = envStubGet(arg, -2, env->nLocals());
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
                setVal(i, call(NativeBuiltins::createClosure,
                               {body, formals, loadSxp(mkFunction->env()),
                                srcRef}));
                break;
            }

            case Tag::MkCls: {
                auto mk = MkCls::Cast(i);
                auto formals = loadSxp(mk->arg(0).val());
                auto body = loadSxp(mk->arg(1).val());
                auto srcRef = loadSxp(mk->arg(2).val());
                auto env = loadSxp(mk->arg(3).val());
                setVal(i, call(NativeBuiltins::createClosure,
                               {body, formals, env, srcRef}));
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
                    llvm::Value* res = nullptr;
                    auto a = loadSxp(arg);
                    if (t->typeTest.isA(RType::integer)) {
                        res = builder.CreateICmpEQ(sexptype(a), c(INTSXP));
                    } else if (t->typeTest.isA(PirType(RType::integer)
                                                   .orPromiseWrapped())) {
                        a = depromise(a);
                        res = builder.CreateICmpEQ(sexptype(a), c(INTSXP));
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

            case Tag::Is: {
                assert(representationOf(i) == Representation::Integer);
                auto is = Is::Cast(i);
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp) {
                    auto argNative = loadSxp(arg);
                    auto expectedTypeNative = c(is->sexpTag);
                    llvm::Value* res = nullptr;
                    auto typeNative = sexptype(argNative);
                    switch (is->sexpTag) {
                    case NILSXP:
                    case LGLSXP:
                    case REALSXP:
                        res = builder.CreateICmpEQ(typeNative,
                                                   expectedTypeNative);
                        break;

                    case VECSXP: {
                        auto operandLhs =
                            builder.CreateICmpEQ(typeNative, c(VECSXP));
                        auto operandRhs =
                            builder.CreateICmpEQ(typeNative, c(LISTSXP));
                        res = builder.CreateOr(operandLhs, operandRhs);
                        break;
                    }

                    case LISTSXP: {
                        auto operandLhs =
                            builder.CreateICmpEQ(typeNative, c(LISTSXP));
                        auto operandRhs =
                            builder.CreateICmpEQ(typeNative, c(NILSXP));
                        res = builder.CreateOr(operandLhs, operandRhs);
                        break;
                    }

                    default:
                        assert(false);
                        success = false;
                        break;
                    }

                    setVal(i, builder.CreateZExt(res, t::Int));

                } else {
                    // How do we implement the fast path? Because in native
                    // representations we may have lost the real representation
                    success = false;
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
                    auto a = loadSxp(arg);
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
                    auto a = loadSxp(arg);
                    setVal(i, call(NativeBuiltins::asTest, {a}));
                    break;
                }

                auto r = representationOf(arg);

                auto done = BasicBlock::Create(C, "", fun);
                auto isNa = BasicBlock::Create(C, "asTestIsNa", fun);

                if (r == Representation::Real) {
                    auto narg = load(arg, r);
                    auto isNotNa = builder.CreateFCmpOEQ(narg, narg);
                    narg = builder.CreateFPToSI(narg, t::Int);
                    setVal(i, narg);
                    auto br = builder.CreateCondBr(isNotNa, done, isNa);
                    MDNode* WeightNode = MDB.createBranchWeights(1, 0);
                    br->setMetadata(LLVMContext::MD_prof, WeightNode);
                } else {
                    auto narg = load(arg, Representation::Integer);
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

                llvm::Value* res = nullptr;
                if (r1 == Representation::Sexp) {
                    res = call(NativeBuiltins::asLogicalBlt, {loadSxp(arg)});
                } else if (r1 == Representation::Real) {
                    res = builder.CreateAlloca(t::Int);
                    auto in = load(arg, Representation::Integer);
                    auto nin = load(arg, Representation::Real);
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
                    res = load(arg, Representation::Integer);
                }

                setVal(i, res);
                break;
            }

            case Tag::Force: {
                auto f = Force::Cast(i);
                auto arg = loadSxp(f->arg<0>().val());
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
                auto res =
                    call(NativeBuiltins::ldfun,
                         {constant(ld->varName, t::SEXP), loadSxp(ld->env())});
                setVal(i, res);
                setVisible(1);
                break;
            }

            case Tag::MkArg: {
                auto p = MkArg::Cast(i);
                setVal(i, call(NativeBuiltins::createPromise,
                               {paramCode(), c(promMap.at(p->prom())),
                                loadSxp(p->env()), loadSxp(p->eagerArg())}));
                break;
            }

            case Tag::UpdatePromise: {
                setCar(loadSxp(i->arg(0).val()), loadSxp(i->arg(1).val()));
                break;
            }

            case Tag::LdVarSuper: {
                auto ld = LdVarSuper::Cast(i);

                auto env = cdr(loadSxp(ld->env()));

                auto res = call(NativeBuiltins::ldvar,
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
                if (env && env->stub) {
                    setVal(i, envStubGet(loadSxp(env), env->indexOf(varName),
                                         env->nLocals()));
                    break;
                }

                llvm::Value* res = nullptr;
                if (bindingsCache.count(i->env())) {
                    res = builder.CreateAlloca(t::SEXP);
                    auto offset = bindingsCache.at(i->env()).at(varName);

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
                    ensureNamed(val);
                    builder.CreateStore(val, res);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(miss);
                    auto res0 = call(NativeBuiltins::ldvarCacheMiss,
                                     {constant(varName, t::SEXP),
                                      loadSxp(i->env()), cachePtr});
                    builder.CreateStore(res0, res);
                    builder.CreateBr(done);
                    builder.SetInsertPoint(done);
                    res = builder.CreateLoad(res);
                } else {
                    res = call(NativeBuiltins::ldvar,
                               {constant(varName, t::SEXP), loadSxp(i->env())});
                }
                res->setName(CHAR(PRINTNAME(varName)));

                if (maybeLd) {
                    checkMissing(res);
                    checkUnbound(res);
                }
                setVal(i, res);
                break;
            }

            case Tag::Extract1_1D: {
                auto extract = Extract1_1D::Cast(i);
                auto vector = loadSxp(extract->vec());
                auto idx = loadSxp(extract->idx());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto env = constant(R_NilValue, t::SEXP);
                if (extract->hasEnv())
                    env = loadSxp(extract->env());

                auto res = call(NativeBuiltins::extract11,
                                {vector, idx, env, c(extract->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::Extract2_1D: {
                auto extract = Extract2_1D::Cast(i);
                // TODO: Extend a fastPath for generic vectors.
                if (extract->vec()->type.isA(PirType::num().notObject()) &&
                    extract->idx()->type.isScalar()) {
                    auto fallback = BasicBlock::Create(C, "", fun);
                    auto hit = BasicBlock::Create(C, "", fun);
                    auto hit2 = BasicBlock::Create(C, "", fun);
                    auto done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(extract->vec());
                    llvm::Value* res =
                        builder.CreateAlloca(representationOf(i));

                    if (extract->vec()->type.maybeObj()) {
                        auto rNil = constant(R_NilValue, t::SEXP);
                        auto vectorhasAttr =
                            builder.CreateICmpEQ(attr(vector), rNil);
                        builder.CreateCondBr(vectorhasAttr, hit, fallback);
                        builder.SetInsertPoint(hit);
                    }

                    if (representationOf(extract->vec()) == t::SEXP) {
                        builder.CreateCondBr(isAltrep(vector), fallback, hit2);
                        builder.SetInsertPoint(hit2);
                    }

                    llvm::Value* index =
                        computeAndCheckIndex(extract->idx(), vector, fallback);
                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);
                    builder.CreateStore(convert(res0, i->type), res);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                    auto env = (extract->hasEnv())
                                   ? loadSxp(extract->env())
                                   : constant(R_NilValue, t::SEXP);
                    res0 =
                        call(NativeBuiltins::extract21,
                             {loadSxp(extract->vec()), loadSxp(extract->idx()),
                              env, c(extract->srcIdx)});
                    builder.CreateStore(convert(res0, i->type), res);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                    setVal(i, builder.CreateLoad(res));
                } else {
                    auto env = (extract->hasEnv())
                                   ? loadSxp(extract->env())
                                   : constant(R_NilValue, t::SEXP);
                    setVal(i, call(NativeBuiltins::extract21,
                                   {loadSxp(extract->vec()),
                                    loadSxp(extract->idx()), env,
                                    c(extract->srcIdx)}));
                }
                break;
            }

            case Tag::Subassign1_1D: {
                auto subAssign = Subassign1_1D::Cast(i);
                auto vector = loadSxp(subAssign->vector());
                auto val = loadSxp(subAssign->val());
                auto idx = loadSxp(subAssign->idx());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto env = constant(R_NilValue, t::SEXP);
                if (subAssign->hasEnv())
                    env = loadSxp(subAssign->env());
                auto res = call(NativeBuiltins::subassign11,
                                {vector, idx, val, env, c(subAssign->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::Subassign2_1D: {
                auto subAssign = Subassign2_1D::Cast(i);
                auto idx = loadSxp(subAssign->idx());

                // TODO: Extend a fastPath for generic vectors.
                // TODO: Support type conversions
                auto vecType = subAssign->vector()->type;
                auto valType = subAssign->val()->type;
                auto idxType = subAssign->idx()->type;
                if (idxType.isA(PirType::num().notObject().scalar()) &&
                    valType.isScalar() &&
                    ((vecType.isA(RType::integer) &&
                      valType.isA(RType::integer)) ||
                     (vecType.isA(RType::real) && valType.isA(RType::real)))) {
                    auto resultRep = representationOf(i);
                    auto fallback = BasicBlock::Create(C, "", fun);
                    auto hit = BasicBlock::Create(C, "", fun);
                    auto done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(subAssign->vector());
                    llvm::Value* res = builder.CreateAlloca(resultRep);
                    if (representationOf(subAssign->vector()) == t::SEXP) {
                        builder.CreateCondBr(shared(vector), fallback, hit);
                        builder.SetInsertPoint(hit);
                    }

                    llvm::Value* index = computeAndCheckIndex(subAssign->idx(),
                                                              vector, fallback);

                    auto val = load(subAssign->val());
                    if (i->type.isScalar()) {
                        builder.CreateStore(convert(val, i->type), res);
                    } else {
                        assignVector(vector, index, val,
                                     subAssign->vector()->type);
                        builder.CreateStore(convert(vector, i->type), res);
                    }

                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                    auto env = (subAssign->hasEnv())
                                   ? loadSxp(subAssign->env())
                                   : constant(R_NilValue, t::SEXP);
                    auto res0 = call(NativeBuiltins::subassign21,
                                     {loadSxp(subAssign->vector()), idx,
                                      loadSxp(subAssign->val()), env,
                                      c(subAssign->srcIdx)});
                    builder.CreateStore(convert(res0, i->type), res);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                    setVal(i, builder.CreateLoad(res));
                } else {
                    auto env = (subAssign->hasEnv())
                                   ? loadSxp(subAssign->env())
                                   : constant(R_NilValue, t::SEXP);
                    setVal(i, call(NativeBuiltins::subassign21,
                                   {loadSxp(subAssign->vector()), idx,
                                    loadSxp(subAssign->val()), env,
                                    c(subAssign->srcIdx)}));
                }
                break;
            }

            case Tag::StVar: {
                auto st = StVar::Cast(i);
                auto environment = MkEnv::Cast(st->env());

                if (environment && environment->stub) {
                    auto val = loadSxp(st->val());
                    incrementNamed(val);
                    envStubSet(loadSxp(environment),
                               environment->indexOf(st->varName), val,
                               environment->nLocals(), !st->isStArg);
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

                    auto newVal = loadSxp(st->arg<0>().val());

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
                    call(setter, {constant(st->varName, t::SEXP), newVal,
                                  loadSxp(st->env())});
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);

                } else {
                    call(setter,
                         {constant(st->varName, t::SEXP),
                          loadSxp(st->arg<0>().val()), loadSxp(st->env())});
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
                call(NativeBuiltins::defvar,
                     {constant(st->varName, t::SEXP),
                      loadSxp(st->arg<0>().val()), loadSxp(st->env())});
                break;
            }

            case Tag::Missing: {
                assert(representationOf(i) == Representation::Integer);
                auto missing = Missing::Cast(i);
                setVal(i, call(NativeBuiltins::isMissing,
                               {constant(missing->varName, t::SEXP),
                                loadSxp(i->env())}));
                break;
            }

            case Tag::ChkMissing: {
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp)
                    checkMissing(loadSxp(arg));
                setVal(i, load(arg, representationOf(i)));
                break;
            }

            case Tag::ChkClosure: {
                auto arg = loadSxp(i->arg(0).val());
                call(
                    NativeBuiltins::chkfun,
                    {constant(Rf_install(ChkClosure::Cast(i)->name()), t::SEXP),
                     arg});
                setVal(i, arg);
                break;
            }

            case Tag::Length: {
                llvm::Value* l;
                if (representationOf(i) == Representation::Sexp) {
                    l = vectorLength(loadSxp(i->arg(0).val()));
                } else if (representationOf(i) == Representation::Real) {
                    l = c((double)1);
                } else {
                    assert(representationOf(i) == Representation::Integer);
                    l = c(1);
                }
                setVal(i, l);
                break;
            }

            case Tag::Int3:
            case Tag::PrintInvocation:
            case Tag::Extract2_2D:
            case Tag::Extract1_2D:
            case Tag::Subassign1_2D:
            case Tag::Subassign2_2D:
                success = false;
                break;

            case Tag::_UNUSED_:
                assert(false && "Invalid instruction tag");
                success = false;
                break;

            case Tag::FrameState:
            case Tag::Checkpoint:
            case Tag::Assume:
            case Tag::Deopt:
                assert(false && "Expected scheduled deopt");
                success = false;
                break;

            case Tag::True:
            case Tag::False:
            case Tag::NaLogical:
            case Tag::Tombstone:
            case Tag::MissingArg:
            case Tag::UnboundValue:
            case Tag::Env:
            case Tag::Nil:
                assert(false && "Values should not occur in instructions");
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
                auto phi = phis.at(i);
                auto r = representationOf(phi);
                auto inp =
                    PirCopy::Cast(i) ? load(i->arg(0).val(), r) : load(i, r);
                variables.at(phi).update(builder, inp);
            }

            if (variables.count(i) && variables.at(i).initialized &&
                representationOf(i) == t::SEXP) {
                if (i->minReferenceCount() < 2 && needsSetShared.count(i))
                    ensureShared(loadSxp(i));
                else if (i->minReferenceCount() < 1 &&
                         (refcountAnalysisOverflow ||
                          needsEnsureNamed.count(i)))
                    ensureNamed(loadSxp(i));
            }

            numTemps = 0;
        }

        if (bb->isJmp())
            builder.CreateBr(getBlock(bb->next()));

        if (bb->next0)
            blockInPushContext[bb->next0] = inPushContext;
        if (bb->next1)
            blockInPushContext[bb->next1] = inPushContext;
    });

    // Delayed insertion of the branch, so we can still easily add instructions
    // to the entry block while compiling
    builder.SetInsertPoint(entryBlock);
    builder.CreateBr(getBlock(code->entry));

    if (success) {
        // outs() << "Compiled " << fun->getName() << "\n";
        // fun->dump();
        // code->printCode(std::cout, true, true);
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
    const std::unordered_set<Instruction*>& needsEnsureNamed,
    const std::unordered_set<Instruction*>& needsSetShared,
    bool refcountAnalysisOverflow) {

    JitLLVM::createModule();
    auto mangledName = JitLLVM::mangle(cls->name());
    LowerFunctionLLVM funCompiler(mangledName, cls, code, m, needsEnsureNamed,
                                  needsSetShared, refcountAnalysisOverflow);
    if (!funCompiler.tryCompile())
        return nullptr;

    return JitLLVM::tryCompile(funCompiler.fun);
}

} // namespace pir
} // namespace rir
