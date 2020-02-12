#ifndef PIR_COMPILER_LOWER_FUNCTION_H
#define PIR_COMPILER_LOWER_FUNCTION_H

#include "llvm_imports.h"

#include "builtins.h"
#include "compiler/analysis/liveness.h"
#include "compiler/analysis/reference_count.h"
#include "compiler/pir/pir.h"
#include "constants.h"
#include "phi_builder.h"
#include "representation.h"
#include "runtime/Code.h"
#include "variable.h"
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace rir {
namespace pir {

class LowerFunction {

    ClosureVersion* cls;
    Code* code;
    Instruction* currentInstr = nullptr;
    const std::unordered_map<Promise*, unsigned>& promMap;
    const NeedsRefcountAdjustment& refcount;
    const std::unordered_set<Instruction*>& needsLdVarForUpdate;
    IRBuilder<> builder;
    MDBuilder MDB;
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

    MDNode* branchAlwaysTrue;
    MDNode* branchAlwaysFalse;
    MDNode* branchMostlyTrue;
    MDNode* branchMostlyFalse;

  public:
    llvm::Function* fun;

    LowerFunction(const std::string& name, ClosureVersion* cls, Code* code,
                  const std::unordered_map<Promise*, unsigned>& promMap,
                  const NeedsRefcountAdjustment& refcount,
                  const std::unordered_set<Instruction*>& needsLdVarForUpdate)
        : cls(cls), code(code), promMap(promMap), refcount(refcount),
          needsLdVarForUpdate(needsLdVarForUpdate), builder(Jit::C),
          MDB(Jit::C), liveness(code, code->nextBBId), numLocals(0),
          numTemps(0), branchAlwaysTrue(MDB.createBranchWeights(100000000, 1)),
          branchAlwaysFalse(MDB.createBranchWeights(1, 100000000)),
          branchMostlyTrue(MDB.createBranchWeights(1000, 1)),
          branchMostlyFalse(MDB.createBranchWeights(1, 1000)) {

        fun = Jit::declare(cls, name, t::nativeFunction);
        // prevent Wunused
        this->cls->size();
        this->promMap.size();
    }

    static llvm::Constant* convertToPointer(void* what, Type* ty = t::voidPtr) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(Jit::C,
                                   llvm::APInt(64, (std::uint64_t)what)),
            ty);
    }
    static llvm::Constant* convertToPointer(SEXP what) {
        return llvm::ConstantExpr::getCast(
            llvm::Instruction::IntToPtr,
            llvm::ConstantInt::get(Jit::C,
                                   llvm::APInt(64, (std::uint64_t)what)),
            t::SEXP);
    }

    PhiBuilder phiBuilder(llvm::Type* type) {
        return PhiBuilder(builder, type);
    }

    std::unordered_map<Instruction*, Variable> variables;

    llvm::Value* constantSexp(SEXP co);
    llvm::Value* constant(SEXP co, llvm::Type* needed);
    llvm::Value* nodestackPtr();
    llvm::Value* nodestackPtrAddr = nullptr;
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

    llvm::Value* unbox(llvm::Value* v, Representation to);
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
    void envStubSetNotMissing(llvm::Value* x, int i);

    void setVisible(int i);

    std::array<std::string, 4> argNames = {{"code", "args", "env", "closure"}};
    std::vector<llvm::Value*> args;
    llvm::Value* paramCode() { return args[0]; }
    llvm::Value* paramArgs() { return args[1]; }
    llvm::Value* paramEnv() { return args[2]; }
    llvm::Value* paramClosure() { return args[3]; }

    llvm::AllocaInst* topAlloca(llvm::Type* t, size_t len = 1);

    llvm::Value* argument(int i);
    llvm::Value* intToDouble(llvm::Value* val);
    llvm::Value* doubleToInt(llvm::Value* val);
    llvm::Value* convert(llvm::Value* val, PirType to, bool protect = true);
    void setVal(Instruction* i, llvm::Value* val);

    llvm::Value* isExternalsxp(llvm::Value* v, uint32_t magic);
    void checkSexptype(llvm::Value* v, const std::vector<SEXPTYPE>& types);
    void checkIsSexp(llvm::Value* v, const std::string& msg = "");
    void setSexptype(llvm::Value* v, int t);
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

    void protectTemp(llvm::Value* v);

    void ensureNamed(llvm::Value* v);
    llvm::Value* shared(llvm::Value* v);
    void assertNamed(llvm::Value* v);
    void ensureShared(llvm::Value* v);
    void incrementNamed(llvm::Value* v, int max = NAMEDMAX);
    void nacheck(llvm::Value* v, BasicBlock* isNa, BasicBlock* notNa = nullptr);
    void checkMissing(llvm::Value* v);
    void checkUnbound(llvm::Value* v);

    llvm::CallInst* call(const NativeBuiltin& builtin,
                         const std::vector<llvm::Value*>& args);
    llvm::Value* callRBuiltin(SEXP builtin, const std::vector<Value*>& args,
                              int srcIdx, CCODE, llvm::Value* env);

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

    bool success = true;
    bool tryCompile();

    bool tryInlineBuiltin(CallSafeBuiltin* builtin,
                          std::function<llvm::Value*()> callTheBuiltin);

  private:
    bool vectorTypeSupport(Value* v);
    llvm::Value* vectorPositionPtr(llvm::Value* vector, llvm::Value* position,
                                   PirType type);
};

} // namespace pir
} // namespace rir

#endif
