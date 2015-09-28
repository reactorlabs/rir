#include "CodeCache.h"

using namespace llvm;

namespace rjit {

std::unordered_map<std::string, Function*> CodeCache::cache;

llvm::Function* CodeCache::get(std::string name,
                               std::function<llvm::Function*()> function,
                               llvm::Module* m) {
    auto here = m->getFunction(name);
    if (here)
        return here;

    if (cache.count(name)) {
        auto f = cache.at(name);
        return Function::Create(f->getFunctionType(),
                                GlobalValue::ExternalLinkage, name, m);
    }

    auto f = function();
    cache[name] = f;
    return f;
}
}
