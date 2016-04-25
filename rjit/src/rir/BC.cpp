#include "BC.h"

#include "Pool.h"
#include <iostream>

#include "RIntlns.h"

namespace rjit {
namespace rir {

void Code::print() {
    BC* pc = bc;

    auto immediate = [&pc]() {
        Pool::idx i = *(Pool::idx*)pc;
        pc = (BC*)((uintptr_t)pc + sizeof(Pool::idx));
        return Pool::instance().get(i);
    };

    while ((uintptr_t)pc < (uintptr_t)bc + size) {
        switch (*pc++) {
        case BC::invalid:
            assert(false);
            break;
        case BC::push:
            std::cout << "push ";
            Rf_PrintValue(immediate());
            break;
        case BC::getfun:
            std::cout << "getfun\n";
            break;
        case BC::getvar:
            std::cout << "getvar\n";
            break;
        case BC::call:
            std::cout << "call\n";
            break;
        case BC::mkprom:
            std::cout << "mkprom\n";
            break;
        case BC::mkclosure:
            std::cout << "mkclosure\n";
            break;
        }
    }
}
}
}
