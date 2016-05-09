#include "BC.h"

#include "Pool.h"
#include <iostream>

#include "CodeStream.h"
#include "RIntlns.h"
#include "../RList.h"

namespace rjit {
namespace rir {

void BC::write(CodeStream& cs) const {
    cs.insert(bc);
    switch (bc) {
    case BC_t::push:
    case BC_t::getfun:
    case BC_t::getvar:
    case BC_t::call_name:
        cs.insert(immediate.pool);
        return;
    case BC_t::load_arg:
    case BC_t::call:
        cs.insert(immediate.numArgs);
        return;
    case BC_t::mkprom:
    case BC_t::mkclosure:
        cs.insert(immediate.fun);
        return;
    case BC_t::ret:
    case BC_t::force:
    case BC_t::pop:
    case BC_t::get_ast:
        return;
    case BC_t::invalid:
    case BC_t::num_of:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() { return Pool::instance().get(immediate.pool); }

void Code::print() {
    BC_t* pc = bc;

    std::cout << "-------------------\n";

    while ((uintptr_t)pc < (uintptr_t)bc + size) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
        case BC_t::invalid:
        case BC_t::num_of:
            assert(false);
            break;
        case BC_t::call_name:
            std::cout << "call_name ";
            for (auto n : RVector(bc.immediateConst())) {
                std::cout << CHAR(PRINTNAME(n)) << " ";
            }
            std::cout << "\n";
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
        case BC_t::force:
            std::cout << "force\n";
            break;
        case BC_t::pop:
            std::cout << "pop\n";
            break;
        case BC_t::ret:
            std::cout << "ret\n";
            break;
        case BC_t::call:
            std::cout << "call " << bc.immediateNumArgs() << "\n";
            break;
        case BC_t::get_ast:
            std::cout << "get_ast\n";
            break;
        case BC_t::load_arg:
            std::cout << "load_arg " << bc.immediateNumArgs() << "\n";
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
