#ifndef MEMORY_MANAGER_H
#define MEMORY_MANAGER_H

#include "llvm_includes.h"

#include <unordered_map>

class MemoryManager : public llvm::SectionMemoryManager {
  MemoryManager(const MemoryManager&) LLVM_DELETED_FUNCTION;
  void operator=(const MemoryManager&) LLVM_DELETED_FUNCTION;

  std::unordered_map<std::string, uint64_t> symbols;
  
public:
  MemoryManager() : llvm::SectionMemoryManager() {};

  void addSymbol(std::string name, uint64_t location) {
      symbols[name] = location;
  }

  uint64_t getSymbolAddress(const std::string &Name) override;

  static MemoryManager manager;
};

#endif
