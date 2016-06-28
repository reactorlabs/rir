#include "BC.h"

#include "Pool.h"
#include <iostream>
#include <iomanip>

#include "../RList.h"
#include "CodeStream.h"
#include "RIntlns.h"

namespace rjit {
namespace rir {

void BC::write(CodeStream& cs) const {
    cs.insert(bc);
    switch (bc) {
    case BC_t::push_:
    case BC_t::ldfun_:
    case BC_t::ldvar_:
    case BC_t::isspecial_:
        cs.insert(immediate.pool);
        return;

    case BC_t::call_:
        cs.insert(immediate.call_args);
        break;

    case BC_t::pusharg_:
        cs.insert(immediate.numArgs);
        return;

    case BC_t::promise_:
        cs.insert(immediate.fun);
        return;

    case BC_t::br_:
    case BC_t::brtrue_:
    case BC_t::brfalse_:
        cs.patchpoint(immediate.offset);
        return;

    case BC_t::pushi_:
        cs.insert(immediate.i);
        return;

    case BC_t::close_:
    case BC_t::ret_:
    case BC_t::force_:
    case BC_t::DEPRECATED_FORCE_ALL:
    case BC_t::pop_:
    case BC_t::asast_:
    case BC_t::stvar_:
    case BC_t::asbool_:
    case BC_t::NUMARGI_DEPRECATED:
    case BC_t::lti_:
    case BC_t::eqi_:
    case BC_t::dupi_:
    case BC_t::dup_:
    case BC_t::inci_:
    case BC_t::push_argi_:
    case BC_t::add_:
    case BC_t::sub_:
    case BC_t::lt_:
    case BC_t::isfun_:
        return;

    case BC_t::invalid_:
    case BC_t::num_of:
    case BC_t::label:
        assert(false);
        return;
    }
}

SEXP BC::immediateConst() { return Pool::get(immediate.pool); }
fun_idx_t* BC::immediateCallArgs() {
    return (fun_idx_t*)INTEGER(Pool::get(immediate.call_args.args));
}
num_args_t BC::immediateCallNargs() {
    size_t nargs =
        Rf_length(Pool::get(immediate.call_args.args)) / sizeof(fun_idx_t);
    assert(nargs < MAX_NUM_ARGS);
    return (num_args_t)nargs;
}
SEXP BC::immediateCallNames() {
    return immediate.call_args.names ? Pool::get(immediate.call_args.names)
                                     : nullptr;
}

void CodeHandle::print() {
    BC_t* pc = (BC_t*)data();

    while ((uintptr_t)pc < (uintptr_t)endData()) {
        std::cout << std::setw(3) << ((uintptr_t)pc - (uintptr_t)data()) << " ";
        BC bc = BC::advance(&pc);
        bc.print();
    }
}

void BC::print() {
    if (bc != BC_t::label)
        std::cout << "   ";

    switch (bc) {
    case BC_t::invalid_:
    case BC_t::num_of:
        assert(false);
        break;
    case BC_t::call_: {
        std::cout << "call ";
        fun_idx_t* args = immediateCallArgs();
        num_args_t nargs = immediateCallNargs();
        for (unsigned i = 0; i < nargs; ++i) {
            std::cout << args[i] << " ";
        }
        if (immediateCallNames())
            for (auto n : RVector(immediateCallNames())) {
                std::cout << (n == R_NilValue ? "_" : CHAR(PRINTNAME(n)))
                          << " ";
            }
        std::cout << "\n";
        break;
    }
    case BC_t::push_:
        std::cout << "push ";
        Rf_PrintValue(immediateConst());
        break;
    case BC_t::isspecial_:
        std::cout << "check_primitive " << CHAR(PRINTNAME((immediateConst())))
                  << "\n";
        break;
    case BC_t::ldfun_:
        std::cout << "getfun " << CHAR(PRINTNAME((immediateConst()))) << "\n";
        break;
    case BC_t::ldvar_:
        std::cout << "getvar " << CHAR(PRINTNAME((immediateConst()))) << "\n";
        break;
    case BC_t::DEPRECATED_FORCE_ALL:
        std::cout << "force_all\n";
        break;
    case BC_t::force_:
        std::cout << "force\n";
        break;
    case BC_t::pop_:
        std::cout << "pop\n";
        break;
    case BC_t::stvar_:
        std::cout << "setvar\n";
        break;
    case BC_t::lti_:
        std::cout << "lti\n";
        break;
    case BC_t::eqi_:
        std::cout << "eqi\n";
        break;
    case BC_t::ret_:
        std::cout << "ret\n";
        break;
    case BC_t::dup_:
        std::cout << "dup\n";
        break;
    case BC_t::dupi_:
        std::cout << "dupi\n";
        break;
    case BC_t::inci_:
        std::cout << "inci\n";
        break;
    case BC_t::push_argi_:
        std::cout << "load_argi\n";
        break;
    case BC_t::pushi_:
        std::cout << "pushi " << immediate.i << "\n";
        break;
    case BC_t::asast_:
        std::cout << "get_ast\n";
        break;
    case BC_t::asbool_:
        std::cout << "to_bool\n";
        break;
    case BC_t::NUMARGI_DEPRECATED:
        std::cout << "numargi\n";
        break;
    case BC_t::pusharg_:
        std::cout << "load_arg " << immediate.numArgs << "\n";
        break;
    case BC_t::promise_:
        std::cout << "mkprom " << immediate.fun << "\n";
        break;
    case BC_t::close_:
        std::cout << "mkclosure " << immediate.fun << "\n";
        break;
    case BC_t::brtrue_:
        std::cout << "jmp_true " << immediate.offset << "\n";
        break;
    case BC_t::brfalse_:
        std::cout << "jmp_false " << immediate.offset << "\n";
        break;
    case BC_t::br_:
        std::cout << "jmp " << immediate.offset << "\n";
        break;
    case BC_t::add_:
        std::cout << "add\n";
        break;
    case BC_t::sub_:
        std::cout << "sub\n";
        break;
    case BC_t::lt_:
        std::cout << "lt\n";
        break;
    case BC_t::isfun_:
        std::cout << "check_function" << std::endl;
        break;
    case BC_t::label:
        std::cout << immediate.offset << ":\n";
    }
}

const BC BC::call(std::vector<fun_idx_t> args, std::vector<SEXP> names) {
    assert(args.size() == names.size());
    assert(args.size() < MAX_NUM_ARGS);

    Protect p;
    SEXP a = Rf_allocVector(INTSXP, sizeof(fun_idx_t) * args.size());
    p(a);

    fun_idx_t* argsArray = (fun_idx_t*)INTEGER(a);
    for (size_t i = 0; i < args.size(); ++i) {
        argsArray[i] = args[i];
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
        call_args_t args_ = {Pool::insert(a), Pool::insert(n)};
        immediate_t i;
        i.call_args = args_;
        return BC(BC_t::call_, i);
    }

    call_args_t args_ = {Pool::insert(a), 0};
    immediate_t i;
    i.call_args = args_;
    return BC(BC_t::call_, i);
}
}
}
