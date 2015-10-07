#ifndef GC_STRATEGY
#define GC_STRATEGY

#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Value.h"

namespace rjit {

class JITStatepointGC : public llvm::GCStrategy {
  public:
    JITStatepointGC();
    llvm::Optional<bool>
    isGCManagedPointer(const llvm::Value* V) const override;
    static bool isGCManaged(const llvm::Value* V);

    static const llvm::StringRef name() { return "rjit"; }
};
}

#endif
