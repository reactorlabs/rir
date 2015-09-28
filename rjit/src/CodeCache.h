#ifndef CODE_CACHE_H
#define CODE_CACHE_H

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <unordered_map>
#include <functional>

namespace rjit {

class CodeCache {
  public:
    static llvm::Function* get(std::string name,
                               std::function<llvm::Function*()> function,
                               llvm::Module* m);

  private:
    static std::unordered_map<std::string, llvm::Function*> cache;
};
}

#endif
