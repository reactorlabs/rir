#ifndef PIR_COMPILER_LOWER_FUNCTION_LLVM_H
#define PIR_COMPILER_LOWER_FUNCTION_LLVM_H

#include "jit_llvm.h"
#include "lower_llvm.h"
#include "types_llvm.h"

#include "compiler/pir/pir.h"

#include "R/Protect.h"
#include "runtime/Code.h"

#include "compiler/analysis/liveness.h"
#include "compiler/analysis/reference_count.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/MDBuilder.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace rir {
namespace pir {

struct Representation;
class LowerFunctionLLVM {

    ClosureVersion* cls;
    Code* code;
    BB::Instrs::iterator currentInstr;
    BB* currentBB = nullptr;
    const PromMap& promMap;
    const NeedsRefcountAdjustment& refcount;
    const std::unordered_set<Instruction*>& needsLdVarForUpdate;
    llvm::IRBuilder<> builder;
    llvm::MDBuilder MDB;
    LivenessIntervals liveness;
    LogStream& log;
    size_t numLocals;
    size_t numTemps;
    constexpr static size_t MAX_TEMPS = 4;
    llvm::Value* basepointer = nullptr;
    llvm::Value* constantpool = nullptr;
    llvm::BasicBlock* entryBlock = nullptr;
    int inPushContext = 0;
    std::unordered_set<Value*> escapesInlineContext;

    struct ContextData {
        llvm::AllocaInst* rcntxt;
        llvm::AllocaInst* result;
        llvm::BasicBlock* popContextTarget;
        std::unordered_map<Instruction*, size_t> savedSexpPos;
    };
    std::unordered_map<Value*, ContextData> contexts;

    std::vector<ArglistOrder::CallArglistOrder> argReordering;

    std::unordered_map<Value*, std::unordered_map<SEXP, size_t>> bindingsCache;
    llvm::Value* bindingsCacheBase = nullptr;

    llvm::MDNode* branchAlwaysTrue;
    llvm::MDNode* branchAlwaysFalse;
    llvm::MDNode* branchMostlyTrue;
    llvm::MDNode* branchMostlyFalse;

    Protect p_;

  public:
    PirTypeFeedback* pirTypeFeedback = nullptr;
    llvm::Function* fun;
    MkEnv* myPromenv = nullptr;

    LowerFunctionLLVM(
        const std::string& name, ClosureVersion* cls, Code* code,
        const PromMap& promMap, const NeedsRefcountAdjustment& refcount,
        const std::unordered_set<Instruction*>& needsLdVarForUpdate,
        LogStream& log)
        : cls(cls), code(code), promMap(promMap), refcount(refcount),
          needsLdVarForUpdate(needsLdVarForUpdate), builder(JitLLVM::C),
          MDB(JitLLVM::C), liveness(code, code->nextBBId), log(log),
          numLocals(0), numTemps(0),
          branchAlwaysTrue(MDB.createBranchWeights(100000000, 1)),
          branchAlwaysFalse(MDB.createBranchWeights(1, 100000000)),
          branchMostlyTrue(MDB.createBranchWeights(1000, 1)),
          branchMostlyFalse(MDB.createBranchWeights(1, 1000)) {
        fun = JitLLVM::declare(cls, name, t::nativeFunction);
        // prevent Wunused
        this->cls->size();
        this->promMap.size();
        auto p = promMap.find(code);
        if (p != promMap.end()) {
            auto mk = MkEnv::Cast(p->second.second->env());
            myPromenv = mk;
        }
    }

    static llvm::Constant* convertToPointer(const void* what,
                                            llvm::Type* ty = t::voidPtr) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(JitLLVM::C,
                                   llvm::APInt(64, (std::uint64_t)what)),
            ty);
    }
    static llvm::Constant* convertToPointer(SEXP what) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(JitLLVM::C,
                                   llvm::APInt(64, (std::uint64_t)what)),
            t::SEXP);
    }

    struct Variable {
        bool deadMove(const Variable& other) const;

        enum Kind {
            MutableLocalRVariable,
            ImmutableLocalRVariable,
            MutablePrimitive,
            ImmutablePrimitive,
        };
        Kind kind;

        static Variable MutableRVariable(Instruction* i, size_t pos,
                                         llvm::IRBuilder<>& builder,
                                         llvm::Value* basepointer);
        static Variable RVariable(Instruction* i, size_t pos,
                                  llvm::IRBuilder<>& builder,
                                  llvm::Value* basepointer);
        static Variable Mutable(Instruction* i, llvm::AllocaInst* location);
        static Variable Immutable(Instruction* i);
        llvm::Value* get(llvm::IRBuilder<>& builder);
        void update(llvm::IRBuilder<>& builder, llvm::Value* val,
                    bool volatile_ = false);
        void set(llvm::IRBuilder<>& builder, llvm::Value* val,
                 bool volatile_ = false);

        llvm::Value* slot;
        bool initialized;
        size_t stackSlot;
    };

    class PhiBuilder {
        std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> inputs;

        llvm::Type* type;
        llvm::IRBuilder<>& builder;
        bool created = false;

      public:
        PhiBuilder(llvm::IRBuilder<>& builder, llvm::Type* type)
            : type(type), builder(builder) {}

        void addInput(llvm::Value* v);
        void addInput(llvm::Value* v, llvm::BasicBlock* b) {
            assert(!created);

            assert(v->getType() == type);
            inputs.push_back({v, b});
        }

        llvm::Value* operator()();

        ~PhiBuilder() { assert(created && "dangling PhiBuilder"); }
    };

    PhiBuilder phiBuilder(llvm::Type* type) {
        return PhiBuilder(builder, type);
    }

    std::unordered_map<Instruction*, Variable> variables_;
    void setVariable(Instruction* variable, llvm::Value* val,
                     bool volatile_ = false) {
        // silently drop dead variables...
        if (!liveness.count(variable))
            return;
        assert(liveness.live(currentInstr, variable));
        variables_.at(variable).set(builder, val, volatile_);
    }
    void updateVariable(Instruction* variable, llvm::Value* val) {
        // silently drop dead variables...
        if (!liveness.count(variable))
            return;

        if (auto phi = Phi::Cast(variable)) {
            bool isNext = false;
            for (auto n : currentBB->successors())
                if (n == phi->bb())
                    isNext = true;
            if (!isNext) {
                currentBB->owner->printCode(std::cout, true, true);
                phi->printRecursive(std::cout, 2);
                (*currentInstr)->printRef(std::cout);
                std::cout << "\n";
            }
            assert(isNext);
        } else {
            assert(liveness.live(currentInstr, variable));
        }
        variables_.at(variable).update(builder, val);
    }
    llvm::Value* getVariable(Instruction* variable) {
        assert(liveness.count(variable));

        if (Phi::Cast(variable)) {
            assert(variable->bb() == currentBB);
        } else {
            if (currentInstr == currentBB->begin())
                assert(liveness.liveAtBBEntry(currentBB, variable));
            else
                assert(liveness.live(currentInstr - 1, variable));
        }
        return variables_.at(variable).get(builder);
    }

    llvm::Value* constant(SEXP co, llvm::Type* needed);
    llvm::Value* nodestackPtr();
    llvm::Value* nodestackPtrAddr = nullptr;
    llvm::Value* stack(int i);
    void stack(const std::vector<llvm::Value*>& args);
    void setLocal(size_t i, llvm::Value* v);
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
    llvm::Value* unboxRealIntLgl(llvm::Value* v, PirType toType);

    void writeBarrier(llvm::Value* x, llvm::Value* y, std::function<void()> no,
                      std::function<void()> yes);
    llvm::Value* envStubGet(llvm::Value* x, int i, size_t size);
    llvm::Value* loadPromise(llvm::Value* code, int i);
    void envStubSet(llvm::Value* x, int i, llvm::Value* y, size_t size,
                    bool setNotMissing);
    void envStubSetNotMissing(llvm::Value* x, int i);
    void envStubSetMissing(llvm::Value* x, int i);

    void setVisible(int i);

    std::array<std::string, 4> argNames = {{"code", "args", "env", "closure"}};
    std::vector<llvm::Value*> args;
    llvm::Value* paramCode() { return args[0]; }
    llvm::Value* paramArgs() { return args[1]; }
    llvm::Value* paramEnv() { return args[2]; }
    llvm::Value* paramClosure() { return args[3]; }

    static llvm::Constant* c(void* i) {
        return llvm::ConstantInt::get(JitLLVM::C, llvm::APInt(64, (intptr_t)i));
    }

    static llvm::Constant* c(unsigned long i, int bs = 64) {
        return llvm::ConstantInt::get(JitLLVM::C, llvm::APInt(bs, i));
    }

    static llvm::Constant* c(long i, int bs = 64) {
        return llvm::ConstantInt::get(JitLLVM::C, llvm::APInt(bs, i));
    }

    static llvm::Constant* c(unsigned int i, int bs = 32) {
        return llvm::ConstantInt::get(JitLLVM::C, llvm::APInt(bs, i));
    }

    static llvm::Constant* c(int i, int bs = 32) {
        return llvm::ConstantInt::get(JitLLVM::C, llvm::APInt(bs, i));
    }

    static llvm::Constant* c(double d) {
        return llvm::ConstantFP::get(JitLLVM::C, llvm::APFloat(d));
    }

    static llvm::Constant* c(const std::vector<unsigned int>& array) {
        std::vector<llvm::Constant*> init;
        for (const auto& e : array)
            init.push_back(c(e));
        auto ty = llvm::ArrayType::get(t::Int, array.size());
        return llvm::ConstantArray::get(ty, init);
    }

    static llvm::Value* globalConst(llvm::Constant* init,
                                    llvm::Type* ty = nullptr);
    llvm::AllocaInst* topAlloca(llvm::Type* t, size_t len = 1);

    llvm::Value* argument(int i);
    llvm::Value* convert(llvm::Value* val, PirType to, bool protect = true);
    void setVal(Instruction* i, llvm::Value* val);

    llvm::Value* isExternalsxp(llvm::Value* v, uint32_t magic);
    void checkSexptype(llvm::Value* v, const std::vector<SEXPTYPE>& types);
    void checkIsSexp(llvm::Value* v, const std::string& msg = "");
    void setSexptype(llvm::Value* v, int t);
    llvm::Value* isVector(llvm::Value* v);
    llvm::Value* isArray(llvm::Value* v);
    llvm::Value* isMatrix(llvm::Value* v);
    llvm::Value* sexptype(llvm::Value* v);
    llvm::Value* attr(llvm::Value* v);
    llvm::Value* vectorLength(llvm::Value* v);
    llvm::Value* isScalar(llvm::Value* v);
    llvm::Value* isSimpleScalar(llvm::Value* v, SEXPTYPE);
    llvm::Value* tag(llvm::Value* v);
    llvm::Value* car(llvm::Value* v);
    llvm::Value* cdr(llvm::Value* v);
    void setCar(llvm::Value* x, llvm::Value* y, bool writeBarrier = true);
    void setCdr(llvm::Value* x, llvm::Value* y, bool writeBarrier = true);
    void setTag(llvm::Value* x, llvm::Value* y, bool writeBarrier = true);
    llvm::Value* isObj(llvm::Value*);
    llvm::Value* fastVeceltOkNative(llvm::Value*);
    llvm::Value* isAltrep(llvm::Value*);
    llvm::Value* sxpinfoPtr(llvm::Value*);

    llvm::Value* container(llvm::Value*);

    void protectTemp(llvm::Value* v);

    bool deadMove(Value* a, Instruction* bi) {
        auto ai = Instruction::Cast(a);
        auto av = variables_.find(ai);
        if (av == variables_.end())
            return false;
        auto bv = variables_.find(bi);
        if (bv == variables_.end())
            return false;
        auto dead = av->second.deadMove(bv->second);
        if (dead)
            bv->second.initialized = true;
        return dead;
    }

    llvm::Value* cloneIfShared(llvm::Value*);
    void ensureNamed(llvm::Value* v);
    void ensureNamedIfNeeded(Instruction* i, llvm::Value* val = nullptr);
    llvm::Value* shared(llvm::Value* v);
    void assertNamed(llvm::Value* v);
    void ensureShared(llvm::Value* v);
    void incrementNamed(llvm::Value* v, int max = NAMEDMAX);
    // We explicitly require the type of the argument to ensure we use non-NA
    // info. If the type is not NA, this will not actually emit a check
    void nacheck(llvm::Value* v, PirType type, llvm::BasicBlock* isNa,
                 llvm::BasicBlock* notNa = nullptr);
    void checkMissing(llvm::Value* v);
    void checkUnbound(llvm::Value* v);

    llvm::Value* checkDoubleToInt(llvm::Value*);

    llvm::CallInst* call(const NativeBuiltin& builtin,
                         const std::vector<llvm::Value*>& args);
    llvm::Value* callRBuiltin(SEXP builtin, const std::vector<Value*>& args,
                              int srcIdx, CCODE, llvm::Value* env);

    llvm::Value* box(llvm::Value* v, PirType t, bool protect = true);
    llvm::Value* boxInt(llvm::Value* v, bool protect = true);
    llvm::Value* boxReal(llvm::Value* v, bool protect = true);
    llvm::Value* boxLgl(llvm::Value* v, bool protect = true);
    llvm::Value* boxTst(llvm::Value* v, bool protect = true);
    void insn_assert(llvm::Value* v, const char* msg, llvm::Value* p = nullptr);
    llvm::Value* depromise(llvm::Value* arg, const PirType& t);
    llvm::Value* depromise(Value* v);

    llvm::Value* force(Instruction* i, llvm::Value* arg);

    llvm::Value* computeAndCheckIndex(Value* index, llvm::Value* vector,
                                      llvm::BasicBlock* fallback,
                                      llvm::Value* max = nullptr);
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

    void compile();

    llvm::Value* createSelect2(llvm::Value* cond,
                               std::function<llvm::Value*()> trueValueAction,
                               std::function<llvm::Value*()> falseValueAction);

    bool hasArgReordering() const { return !argReordering.empty(); }
    std::vector<ArglistOrder::CallArglistOrder> const&
    getArgReordering() const {
        return argReordering;
    }
    ArglistOrder::CallId
    pushArgReordering(ArglistOrder::CallArglistOrder const& reordering) {
        auto res = argReordering.size();
        argReordering.push_back(reordering);
        return res;
    }

  private:
    bool vectorTypeSupport(Value* v);
    llvm::Value* vectorPositionPtr(llvm::Value* vector, llvm::Value* position,
                                   PirType type);
};

} // namespace pir
} // namespace rir

#endif
