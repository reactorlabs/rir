#include "JITCompileLayer.h"

#include "CodeCache.h"
#include "JITSymbolResolver.h"
#include "llvm/Support/DynamicLibrary.h"
#include "Runtime.h"
#include <iostream>

using namespace llvm;

namespace rjit {

JITSymbolResolver JITSymbolResolver::singleton;

void* JITSymbolResolver::getSymbolAddress(const std::string& name) const {
    assert(false);
}

RuntimeDyld::SymbolInfo JITSymbolResolver::findSymbol(const std::string& name) {
    // Unmangle mach-o symbols
    int st = (name[0] == '_') ? 1 : 0;
    auto unmangled = name.c_str() + st;

    uint64_t res = 0;

    // Add your global symbols that you want to use in jitted functions
    do {

#define check(sym)                                                             \
    if (name.compare(st, sizeof(#sym) - 1, #sym) == 0) {                       \
        res = (uint64_t)&sym;                                                  \
        break;                                                                 \
    }
        check(patchIC);
        check(compileIC);

    } while (false);

    // Look for jited functions with that name
    if (!res)
        res = CodeCache::getAddress(unmangled);

    // Look for symbols in the current process with that name
    if (!res)
        res =
            (uint64_t)sys::DynamicLibrary::SearchForAddressOfSymbol(unmangled);

    assert(res && "Could not resolve a symbol");
    return RuntimeDyld::SymbolInfo(res, JITSymbolFlags::Exported);
}

RuntimeDyld::SymbolInfo
JITSymbolResolver::findSymbolInLogicalDylib(const std::string& name) {
    // TODO what is this used for anyways?
    assert(false);
}
}
