#include "memory_manager.h"

MemoryManager MemoryManager::manager;

uint64_t MemoryManager::getSymbolAddress(const std::string &name) {
    if (symbols.count(name)) return symbols.at(name);

    return SectionMemoryManager::getSymbolAddress(name);
}
