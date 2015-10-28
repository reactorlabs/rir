#include "CodeCache.h"

using namespace llvm;

namespace rjit {

std::unordered_map<std::string, std::pair<llvm::FunctionType*, uint64_t>>
    CodeCache::cache;

void CodeCache::setAddress(std::string name, uint64_t addr) {
    assert(cache.count(name));
    assert(std::get<1>(cache.at(name)) == 0);
    std::get<1>(cache.at(name)) = addr;
}
uint64_t CodeCache::getAddress(std::string name) {
    if (!cache.count(name))
        return 0;
    auto res = std::get<1>(cache.at(name));
    assert(res);
    return res;
}
bool CodeCache::missingAddress(std::string name) {
    return cache.count(name) && !std::get<1>(cache.at(name));
}

llvm::Function* CodeCache::get(std::string name,
                               std::function<llvm::Function*()> function,
                               llvm::Module* m) {
    auto here = m->getFunction(name);
    if (here)
        return here;

    if (cache.count(name)) {
        auto ty = std::get<0>(cache.at(name));
        return Function::Create(ty, GlobalValue::ExternalLinkage, name, m);
    }

    auto f = function();
    cache[name] = std::pair<FunctionType*, uint64_t>(f->getFunctionType(), 0);
    return f;
}
}
