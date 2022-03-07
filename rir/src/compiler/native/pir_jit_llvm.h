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

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
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
    static std::unique_ptr<llvm::orc::LLJIT> JIT;
    explicit PirJitLLVM(const std::string& name);
    PirJitLLVM(const PirJitLLVM&) = delete;
    PirJitLLVM(PirJitLLVM&&) = delete;
    ~PirJitLLVM();

    void compile(rir::Code* target, ClosureVersion* closure, Code* code,
                 const PromMap& m, const NeedsRefcountAdjustment& refcount,
                 const std::unordered_set<Instruction*>& needsLdVarForUpdate,
                 ClosureStreamLogger& log);

    void deserializeAndAddModule(
      SEXP fNames, SEXP fSrc,
      SEXP fArg,
      size_t hast, Context context,
      rir::FunctionSignature fs,
      std::string bcPath, std::string poolPath, std::string startingHandle, std::string promiseData,
      size_t & cPoolEntriesSize, size_t & srcPoolEntriesSize, size_t & ePoolEntriesSize
      );

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

    // We prepend `rshN_` to all user functions, as a mechanism to
    // differentiate them from builtins. `N` denotes that the definition
    // belongs to module N. Builtins will be declared in the module with
    // their original names (Note: LLVM might still rename things in the
    // same module to make the names unique)
    static std::string makeName(Code* c) {
        std::stringstream ss;
        ss << "rsh" << nModules << "_" << c;
        return ss.str().substr(0, rir::Code::MAX_CODE_HANDLE_LENGTH - 6);
    }

    std::unordered_map<Code*, std::pair<rir::Code*, llvm::StringRef>> jitFixup;
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
              FileName(makeDbgFileName(name)), line(1) {}

        llvm::DICompileUnit* CU;
        llvm::DIFile* File;

        std::string Folder;
        std::string FileName;

        std::vector<llvm::DIScope*> LexicalBlocks;

        llvm::DIType* UnspecifiedType = nullptr;
        llvm::DIType* VoidType = nullptr;
        llvm::DIType* IntType = nullptr;
        llvm::DIType* UIntType = nullptr;
        llvm::DIType* DoubleType = nullptr;
        llvm::DIType* VoidPtrType = nullptr;
        llvm::DIType* SEXPRECType = nullptr;
        llvm::DIType* SEXPType = nullptr;
        llvm::DISubroutineType* NativeCodeType = nullptr;

        std::unique_ptr<FileLogStream> log;
        size_t line;
        std::unordered_map<Code*, size_t> codeLoc;
        std::unordered_map<BB*, size_t> BBLoc;
        std::unordered_map<Instruction*, size_t> instLoc;

        void initializeTypes(llvm::DIBuilder*);
        llvm::DIScope* getScope();
        void addCode(Code* c);
        size_t getCodeLoc(Code* c) const { return codeLoc.at(c); }
        size_t getBBLoc(BB* bb) const { return BBLoc.at(bb); }
        size_t getInstLoc(Instruction* i) const { return instLoc.at(i); }
        void emitLocation(llvm::IRBuilder<>&, size_t);
        void clearLocation(llvm::IRBuilder<>&);
    };
    std::unique_ptr<DebugInfo> DI;
    std::unique_ptr<llvm::DIBuilder> DIB;
};

} // namespace pir
} // namespace rir

#endif
