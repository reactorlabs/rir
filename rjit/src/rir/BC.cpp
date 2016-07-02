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
    case BC_t::ldddvar_:
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
    case BC_t::pop_:
    case BC_t::asast_:
    case BC_t::stvar_:
    case BC_t::asbool_:
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
    SEXP c = Pool::get(immediate.call_args.args);
    assert(TYPEOF(c) == INTSXP);
    return (fun_idx_t*)INTEGER(c);
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

void BC::print() {
    if (bc != BC_t::label)
        Rprintf("   ");
    Rprintf("%s ", name(bc));
    switch (bc) {
    case BC_t::invalid_:
    case BC_t::num_of:
        assert(false);
        break;
    case BC_t::call_: {
        fun_idx_t* args = immediateCallArgs();
        num_args_t nargs = immediateCallNargs();
        Rprintf("[");
        for (unsigned i = 0; i < nargs; ++i) {
            Rprintf(" %x", args[i]);
        }
        Rprintf("] ");
        SEXP names = immediateCallNames();
        if (names != R_NilValue) {
            Rprintf("[");
            for (auto n : RVector(names)) {
                Rprintf(" %s", (n == R_NilValue ? "_" : CHAR(PRINTNAME(n))));
            }
            Rprintf("]");
        }
        break;
    }
    case BC_t::push_:
        Rprintf(" %u # ", immediate.pool);
        Rf_PrintValue(immediateConst());
        return;
    case BC_t::isspecial_:
    case BC_t::ldfun_:
    case BC_t::ldvar_:
    case BC_t::ldddvar_:
        Rprintf(" %u # %s", immediate.pool, CHAR(PRINTNAME((immediateConst()))));
        break;
    case BC_t::pushi_:
        Rprintf(" %i", immediate.i);
        break;
    case BC_t::force_:
    case BC_t::pop_:
    case BC_t::stvar_:
    case BC_t::lti_:
    case BC_t::eqi_:
    case BC_t::ret_:
    case BC_t::dup_:
    case BC_t::dupi_:
    case BC_t::inci_:
    case BC_t::push_argi_:
    case BC_t::asast_:
    case BC_t::asbool_:
    case BC_t::add_:
    case BC_t::sub_:
    case BC_t::lt_:
    case BC_t::isfun_:
        break;
    case BC_t::pusharg_:
        Rprintf(" %u", immediate.numArgs);
        break;
    case BC_t::promise_:
    case BC_t::close_:
        Rprintf(" %s", immediate.fun);
        break;
    case BC_t::brtrue_:
    case BC_t::brfalse_:
    case BC_t::br_:
    case BC_t::label:
        Rprintf(" %x", immediate.offset);
        break;
    }
    Rprintf("\n");
}

BC BC::call(std::vector<fun_idx_t> args, std::vector<SEXP> names) {
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

    call_args_t args_ = {Pool::insert(a), Pool::insert(R_NilValue)};
    immediate_t i;
    i.call_args = args_;
    return BC(BC_t::call_, i);
}
}
}
