#ifndef RIR_COMPILER_PIR_JIT_LLVM_H
#define RIR_COMPILER_PIR_JIT_LLVM_H

#include "compiler/log/stream_logger.h"
#include "compiler/native/builtins.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/pir.h"
#include "compiler/pir/promise.h"

#include "llvm/IR/Module.h"

#include <memory>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

namespace rir {

struct Code;

namespace pir {

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
    PirJitLLVM() {
        if (!initialized)
            initializeLLVM();
    }

    // We have to wait to query LLVM for native code addresses until all Code's
    // (including promises) are added to the Module. Hence, in the destructor,
    // we need to fixup all the native pointers.
    ~PirJitLLVM() {
        if (M) {
            finalizeAndFixup();
            nModules++;
        }
    }

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
    // Initialized on the first call to compile
    std::unique_ptr<llvm::Module> M;

    // Directory of all functions and builtins
    std::unordered_map<Code*, llvm::Function*> funs;
    std::unordered_map<std::string, std::pair<llvm::Function*, void*>> builtins;

    // We prepend `rsh_` to all user functions, as a mechanism to
    // differentiate them from builtins. We also append `.N` to all
    // definitions in module N. Builtins will be declared in the module with
    // their original names (Note: LLVM might still rename things in the
    // same module to make the names unique)
    static std::string makeName(Code* c) {
        std::stringstream ss;
        ss << "rsh_";
        if (auto cls = ClosureVersion::Cast(c)) {
            ss << cls->name();
        } else if (auto p = Promise::Cast(c)) {
            ss << p->owner->name() << "_" << *p;
        } else {
            assert(false);
        }
        ss << "." << nModules;
        return ss.str();
    }

    std::unordered_map<Code*, std::pair<rir::Code*, std::string>> jitFixup;
    void finalizeAndFixup();

    static size_t nModules;
    static void initializeLLVM();
    static bool initialized;
};

} // namespace pir
} // namespace rir

#endif
