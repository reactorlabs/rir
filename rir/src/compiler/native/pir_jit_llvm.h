#ifndef RIR_COMPILER_PIR_JIT_LLVM_H
#define RIR_COMPILER_PIR_JIT_LLVM_H

#include "compiler/log/stream_logger.h"
#include "compiler/native/builtins.h"
#include "compiler/pir/bb.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/instruction.h"
#include "compiler/pir/pir.h"
#include "compiler/pir/promise.h"
#include "compiler/util/visitor.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include <iomanip>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

namespace rir {

struct Code;

namespace pir {

bool LLVMDebugInfo();

struct NeedsRefcountAdjustment;
using PromMap = std::unordered_map<Code*, std::pair<unsigned, MkArg*>>;

// This class serves as an interface to the LLVM backend. When we first
// request PIR code to be lowered to native, LLVM is initialized. There
// is a global LLJIT instance with two JITDylibs: the main one serves
// to store code Modules (each corresponding to a PIR Module), the builtins
// one just provides definitions of the statically compiled symbols and their
// addresses for PIR builtins.
class PirJitLLVM {
  public:
    explicit PirJitLLVM(const std::string& name);
    PirJitLLVM(const PirJitLLVM&) = delete;
    PirJitLLVM(PirJitLLVM&&) = delete;
    ~PirJitLLVM();

    void compile(rir::Code* target, Code* code, const PromMap& m,
                 const NeedsRefcountAdjustment& refcount,
                 const std::unordered_set<Instruction*>& needsLdVarForUpdate,
                 ClosureStreamLogger& log);

    using GetModule = std::function<llvm::Module&()>;
    using GetFunction = std::function<llvm::Function*(Code*)>;
    using GetBuiltin = std::function<llvm::Function*(const NativeBuiltin&)>;
    using Declare = std::function<llvm::Function*(Code*, const std::string&,
                                                  llvm::FunctionType*)>;

    static llvm::LLVMContext& getContext();

  private:
    std::string name;

    // Initialized on the first call to compile
    std::unique_ptr<llvm::Module> M;

    // Directory of all functions and builtins
    std::unordered_map<Code*, llvm::Function*> funs;

    // We prepend `rsh_` to all user functions, as a mechanism to
    // differentiate them from builtins. We also append `.N` to all
    // definitions in module N. Builtins will be declared in the module with
    // their original names (Note: LLVM might still rename things in the
    // same module to make the names unique)
    static std::string makeName(Code* c) {
        std::stringstream ss;
        ss << "rsh_";
        c->printName(ss);
        ss << "." << nModules;
        return ss.str();
    }

    std::unordered_map<Code*, std::pair<rir::Code*, std::string>> jitFixup;
    void finalizeAndFixup();

    static size_t nModules;
    static void initializeLLVM();
    static bool initialized;

    // Support for debugging pir in gdb
  public:
    static std::string makeDbgFileName(const std::string& base) {
        std::stringstream ss;
        ss << base << "." << std::setfill('0') << std::setw(3) << nModules;
        return ss.str();
    }
    struct DebugInfo {
        DebugInfo(const DebugInfo&) = delete;
        DebugInfo(const std::string& folder, const std::string& name)
            : CU(nullptr), File(nullptr), Folder(folder),
              FileName(makeDbgFileName(name)), VoidPtrType(nullptr),
              SEXPRECType(nullptr), SEXPType(nullptr), NativeCodeType(nullptr),
              line(1) {}

        llvm::DICompileUnit* CU;
        llvm::DIFile* File;

        std::string Folder;
        std::string FileName;

        std::vector<llvm::DIScope*> LexicalBlocks;
        llvm::DIScope* getScope();

        llvm::DIType* VoidPtrType;
        llvm::DIType* getVoidPtrType(llvm::DIBuilder*);
        llvm::DIType* SEXPRECType;
        llvm::DIType* getSEXPRECType(llvm::DIBuilder*);
        llvm::DIType* SEXPType;
        llvm::DIType* getSEXPType(llvm::DIBuilder*);
        llvm::DISubroutineType* NativeCodeType;
        llvm::DISubroutineType* getNativeCodeType(llvm::DIBuilder*);
        llvm::DIType* getInstrType(llvm::DIBuilder*, PirType);

        std::unique_ptr<FileLogStream> log;
        size_t line;
        std::unordered_map<Code*, size_t> codeLoc;
        std::unordered_map<BB*, size_t> BBLoc;
        std::unordered_map<Instruction*, size_t> instLoc;
        void addCode(Code* c);
        size_t getCodeLoc(Code* c) const { return codeLoc.at(c); }
        size_t getBBLoc(BB* bb) const { return BBLoc.at(bb); }
        size_t getInstLoc(Instruction* i) const { return instLoc.at(i); }
        void emitLocation(llvm::IRBuilder<>&, size_t);
    };
    std::unique_ptr<DebugInfo> DI;
    std::unique_ptr<llvm::DIBuilder> DIB;
};

} // namespace pir
} // namespace rir

#endif
