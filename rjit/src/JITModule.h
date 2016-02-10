#ifndef JIT_MODULE_H
#define JIT_MODULE_H

#include "llvm.h"

#include "RDefs.h"

#include <unordered_map>

class JITModule : public llvm::Module {
  public:
    JITModule(const std::string& name, llvm::LLVMContext& ctx)
        : llvm::Module(name, ctx) {}

    SEXP getNativeSXP(SEXP formals, SEXP ast, std::vector<SEXP> const& objects,
                      llvm::Function* f);

    void finalizeNativeSEXPs(llvm::ExecutionEngine* engine);

    SEXP constPool(llvm::Function* f);
    SEXP formals(llvm::Function* f);

  private:
    /** List of relocations to be done when compiling.

      When a function is compiled, it is first translated to bitcode and a
      native SXP is created for it using nullptr for the native code. The
      function's SXP is added to the list of relocations here. When the
      compilation is done, the module is finalized and all SEXPs in the
      relocation lists are patched so that they point to correct native
      functions.
      */
    std::unordered_map<llvm::Function*, SEXP> relocations;
    std::unordered_map<llvm::Function*, SEXP> formals_;
};

#endif
