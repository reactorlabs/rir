#include "BC.h"

#include "Pool.h"
#include <iostream>

#include "RIntlns.h"

namespace rjit {
namespace rir {

void Code::print() {
    BC_t* pc = bc;

    auto immediatei = [&pc]() {
        immediate_t i = *(immediate_t*)pc;
        pc = (BC_t*)((uintptr_t)pc + sizeof(immediate_t));
        return i;
    };

    auto immediate = [&pc]() {
        immediate_t i = *(immediate_t*)pc;
        pc = (BC_t*)((uintptr_t)pc + sizeof(immediate_t));
        return Pool::instance().get(i);
    };

    while ((uintptr_t)pc < (uintptr_t)bc + size) {
        switch ((BC_t)*pc++) {
        case BC_t::invalid:
            assert(false);
            break;
        case BC_t::push:
            std::cout << "push ";
            Rf_PrintValue(immediate());
            break;
        case BC_t::getfun:
            std::cout << "getfun " << CHAR(PRINTNAME((immediate()))) << "\n";
            break;
        case BC_t::getvar:
            std::cout << "getvar " << CHAR(PRINTNAME((immediate()))) << "\n";
            break;
        case BC_t::call:
            std::cout << "call\n";
            break;
        case BC_t::mkprom:
            std::cout << "mkprom " << immediatei() << "\n";
            break;
        case BC_t::mkclosure:
            std::cout << "mkclosure " << immediatei() << "\n";
            break;
        }
    }
}
}
}
