#include "BC.h"

#include "Pool.h"
#include <iostream>

#include "CodeStream.h"
#include "RIntlns.h"

namespace rjit {
namespace rir {

void BC::write(CodeStream& cs) const {
    cs.insert(bc);
    switch (bc) {
    case BC_t::push:
    case BC_t::getfun:
    case BC_t::getvar:
        cs.insert(immediate.pool);
        return;
    case BC_t::call:
        return;
    case BC_t::mkprom:
    case BC_t::mkclosure:
        cs.insert(immediate.fun);
        return;
    case BC_t::call_name:
    case BC_t::invalid:
    case BC_t::num_of:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() { return Pool::instance().get(immediate.pool); }

fun_idx_t BC::immediateFunIdx() { return immediate.fun; }

void Code::print() {
    BC_t* pc = bc;

    while ((uintptr_t)pc < (uintptr_t)bc + size) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
        case BC_t::invalid:
        case BC_t::num_of:
        case BC_t::call_name:
            assert(false);
            break;
        case BC_t::push:
            std::cout << "push ";
            Rf_PrintValue(bc.immediateConst());
            break;
        case BC_t::getfun:
            std::cout << "getfun " << CHAR(PRINTNAME((bc.immediateConst())))
                      << "\n";
            break;
        case BC_t::getvar:
            std::cout << "getvar " << CHAR(PRINTNAME((bc.immediateConst())))
                      << "\n";
            break;
        case BC_t::call:
            std::cout << "call\n";
            break;
        case BC_t::mkprom:
            std::cout << "mkprom " << bc.immediateFunIdx() << "\n";
            break;
        case BC_t::mkclosure:
            std::cout << "mkclosure " << bc.immediateFunIdx() << "\n";
            break;
        }
    }
}
}
}
