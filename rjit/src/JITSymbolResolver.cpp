#include "JITCompileLayer.h"

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
    int st = (name[0] == '_') ? 1 : 0;
    uint64_t res = 0;

    do {

#define check(sym)                                                             \
    if (name.compare(st, sizeof(#sym) - 1, #sym) == 0) {                       \
        res = (uint64_t)&sym;                                                  \
        break;                                                                 \
    }
        check(patchIC);
        check(compileIC);

    } while (false);

    if (!res)
        res = JITCompileLayer::getSymbol(name);
    if (!res)
        res = (uint64_t)sys::DynamicLibrary::SearchForAddressOfSymbol(
            name.c_str() + st);

    assert(res);
    return RuntimeDyld::SymbolInfo(res, JITSymbolFlags::Exported);
}

RuntimeDyld::SymbolInfo
JITSymbolResolver::findSymbolInLogicalDylib(const std::string& name) {
    assert(false);
}
}
