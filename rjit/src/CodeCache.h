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
    static void setAddress(std::string name, uint64_t addr);
    static uint64_t getAddress(std::string name);
    static bool missingAddress(std::string name);
    static bool contains(std::string name) { return cache.count(name); }

  private:
    static std::unordered_map<std::string,
                              std::pair<llvm::FunctionType*, uint64_t>> cache;
};
}

#endif
