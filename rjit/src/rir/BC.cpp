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
    case BC_t::check_special:
        cs.insert(immediate.pool);
        return;

    case BC_t::call:
        cs.insert(immediate.call_args);
        break;

    case BC_t::load_arg:
        cs.insert(immediate.numArgs);
        return;

    case BC_t::mkprom:
        cs.insert(immediate.fun);
        return;

    case BC_t::jmp:
    case BC_t::jmp_true:
    case BC_t::jmp_false:
        cs.patchpoint(immediate.offset);
        return;

    case BC_t::pushi:
        cs.insert(immediate.i);
        return;

    case BC_t::mkclosure:
    case BC_t::ret:
    case BC_t::force:
    case BC_t::force_all:
    case BC_t::pop:
    case BC_t::get_ast:
    case BC_t::setvar:
    case BC_t::to_bool:
    case BC_t::numargi:
    case BC_t::lti:
    case BC_t::eqi:
    case BC_t::dupi:
    case BC_t::dup:
    case BC_t::inci:
    case BC_t::load_argi:
    case BC_t::add:
    case BC_t::sub:
    case BC_t::lt:
        return;

    case BC_t::invalid:
    case BC_t::num_of:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() { return Pool::instance().get(immediate.pool); }
SEXP BC::immediateCallArgs() {
    return Pool::instance().get(immediate.call_args.args);
}
SEXP BC::immediateCallNames() {
    return immediate.call_args.names
               ? Pool::instance().get(immediate.call_args.names)
               : nullptr;
}

void Code::print() {
    BC_t* pc = bc;

    std::cout << "-------------------\n";

    while ((uintptr_t)pc < (uintptr_t)bc + size) {
        BC bc = BC::advance(&pc);
        bc.print();
    }
}

void BC::print() {
    switch (bc) {
    case BC_t::invalid:
    case BC_t::num_of:
        assert(false);
        break;
    case BC_t::call:
        std::cout << "call ";
        for (int i = 0; i < Rf_length(immediateCallArgs()); ++i) {
            std::cout << INTEGER(immediateCallArgs())[i] << " ";
        }
        if (immediateCallNames())
            for (auto n : RVector(immediateCallNames())) {
                std::cout << (n == R_NilValue ? "_" : CHAR(PRINTNAME(n)))
                          << " ";
            }
        std::cout << "\n";
        break;
    case BC_t::push:
        std::cout << "push ";
        Rf_PrintValue(immediateConst());
        break;
    case BC_t::check_special:
        std::cout << "check_special " << CHAR(PRINTNAME((immediateConst())))
                  << "\n";
        break;
    case BC_t::getfun:
        std::cout << "getfun " << CHAR(PRINTNAME((immediateConst()))) << "\n";
        break;
    case BC_t::getvar:
        std::cout << "getvar " << CHAR(PRINTNAME((immediateConst()))) << "\n";
        break;
    case BC_t::force_all:
        std::cout << "force_all\n";
        break;
    case BC_t::force:
        std::cout << "force\n";
        break;
    case BC_t::pop:
        std::cout << "pop\n";
        break;
    case BC_t::setvar:
        std::cout << "setvar\n";
        break;
    case BC_t::lti:
        std::cout << "lti\n";
        break;
    case BC_t::eqi:
        std::cout << "eqi\n";
        break;
    case BC_t::ret:
        std::cout << "ret\n";
        break;
    case BC_t::dup:
        std::cout << "dup\n";
        break;
    case BC_t::dupi:
        std::cout << "dupi\n";
        break;
    case BC_t::inci:
        std::cout << "inci\n";
        break;
    case BC_t::load_argi:
        std::cout << "load_argi\n";
        break;
    case BC_t::pushi:
        std::cout << "pushi " << immediate.i << "\n";
        break;
    case BC_t::get_ast:
        std::cout << "get_ast\n";
        break;
    case BC_t::to_bool:
        std::cout << "to_bool\n";
        break;
    case BC_t::numargi:
        std::cout << "numargi\n";
        break;
    case BC_t::load_arg:
        std::cout << "load_arg " << immediate.numArgs << "\n";
        break;
    case BC_t::mkprom:
        std::cout << "mkprom " << immediate.fun << "\n";
        break;
    case BC_t::mkclosure:
        std::cout << "mkclosure " << immediate.fun << "\n";
        break;
    case BC_t::jmp_true:
        std::cout << "jmp_true " << immediate.offset << "\n";
        break;
    case BC_t::jmp_false:
        std::cout << "jmp_false " << immediate.offset << "\n";
        break;
    case BC_t::jmp:
        std::cout << "jmp " << immediate.offset << "\n";
        break;
    case BC_t::add:
        std::cout << "add\n";
        break;
    case BC_t::sub:
        std::cout << "sub\n";
        break;
    case BC_t::lt:
        std::cout << "lt\n";
        break;
    }
}

const BC BC::call(std::vector<fun_idx_t> args, std::vector<SEXP> names) {
    assert(args.size() == names.size());
    assert(args.size() < MAX_NUM_ARGS);

    Protect p;
    SEXP a = Rf_allocVector(INTSXP, args.size());
    p(a);
    for (size_t i = 0; i < args.size(); ++i) {
        INTEGER(a)[i] = args[i];
    }

    bool hasNames = false;
    for (auto n : names) {
        if (n != R_NilValue) {
            hasNames = true;
            break;
        }
    }

    SEXP n;
    if (hasNames) {
        n = Rf_allocVector(VECSXP, names.size());
        p(n);
        for (size_t i = 0; i < args.size(); ++i) {
            SET_VECTOR_ELT(n, i, names[i]);
        }
        call_args_t args_ = {Pool::instance().insert(a),
                             Pool::instance().insert(n)};
        immediate_t i;
        i.call_args = args_;
        return BC(BC_t::call, i);
    }

    call_args_t args_ = {Pool::instance().insert(a), 0};
    immediate_t i;
    i.call_args = args_;
    return BC(BC_t::call, i);
}
}
}
