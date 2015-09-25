#ifndef JIT_SYMBOL_RESOLVER
#define JIT_SYMBOL_RESOLVER

#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"

namespace rjit {

class JITSymbolResolver : public llvm::RuntimeDyld::SymbolResolver {
  public:
    static JITSymbolResolver singleton;

    void* getSymbolAddress(const std::string& name) const;

    llvm::RuntimeDyld::SymbolInfo findSymbol(const std::string& name) override;

    llvm::RuntimeDyld::SymbolInfo
    findSymbolInLogicalDylib(const std::string& name) override;
};
}

#endif
