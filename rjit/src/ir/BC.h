#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstddef>
#include <cstdint>
#include <map>

#include "utils/Pool.h"
#include "R/RDefs.h"

#include "BC_inc.h"

#include "R/Protect.h"

namespace rir {

namespace {

BC::immediate_t decodeImmediate(BC_t bc, BC_t* pc) {
    BC::immediate_t immediate = {0};
    switch (bc) {
    case BC_t::push_:
    case BC_t::ldfun_:
    case BC_t::ldvar_:
    case BC_t::ldddvar_:
    case BC_t::isspecial_:
        immediate.pool = *(pool_idx_t*)pc;
        break;
    case BC_t::call_:
        immediate.call_args = *(call_args_t*)pc;
        break;
    case BC_t::pusharg_:
        immediate.numArgs = *(num_args_t*)pc;
        break;
    case BC_t::promise_:
        immediate.fun = *(fun_idx_t*)pc;
        break;
    case BC_t::br_:
    case BC_t::brtrue_:
    case BC_t::brfalse_:
    case BC_t::label:
        immediate.offset = *(jmp_t*)pc;
        break;
    case BC_t::pushi_:
        immediate.i = *(int*)pc;
        break;
    case BC_t::close_:
    case BC_t::ret_:
    case BC_t::pop_:
    case BC_t::force_:
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
    case BC_t::invisible_:
        break;
    case BC_t::invalid_:
    case BC_t::num_of:
        assert(false);
        break;
    }
    return immediate;
}
}

BC BC::advance(BC_t** pc) {
    BC_t bc = **pc;
    BC cur(bc, decodeImmediate(bc, (*pc) + 1));
    *pc = (BC_t*)((uintptr_t)(*pc) + cur.size());
    return cur;
}

BC BC::decode(BC_t* pc) {
    BC_t bc = *pc;
    BC cur(bc, decodeImmediate(bc, pc + 1));
    return cur;
}

class CodeStream;

BC BC::ret() { return BC(BC_t::ret_); }
BC BC::force() { return BC(BC_t::force_); }
BC BC::pop() { return BC(BC_t::pop_); }
BC BC::push(SEXP constant) {
    immediate_t i;
    i.pool = Pool::insert(constant);
    return BC(BC_t::push_, i);
}
BC BC::push(double constant) {
    immediate_t i;
    i.pool = Pool::getNum(constant);
    return BC(BC_t::push_, i);
}
BC BC::ldfun(SEXP sym) {
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::ldfun_, i);
}
BC BC::ldddvar(SEXP sym) {
    assert(DDVAL(sym));
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::ldddvar_, i);
}
BC BC::ldvar(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::ldvar_, i);
}
BC BC::isspecial(SEXP sym) {
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::isspecial_, i);
}
BC BC::promise(fun_idx_t prom) { return BC(BC_t::promise_, {prom}); }
BC BC::pusharg(num_args_t arg) { return BC(BC_t::pusharg_, {arg}); }
BC BC::asast() { return BC(BC_t::asast_); }
BC BC::stvar() { return BC(BC_t::stvar_); }
BC BC::lti() { return BC(BC_t::lti_); }
BC BC::eqi() { return BC(BC_t::eqi_); }
BC BC::asbool() { return BC(BC_t::asbool_); }

BC BC::isfun() { return BC(BC_t::isfun_); }

BC BC::label(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::label, i);
}
BC BC::br(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::br_, i);
}
BC BC::brtrue(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::brtrue_, i);
}
BC BC::brfalse(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::brfalse_, i);
}
BC BC::dupi() { return BC(BC_t::dupi_); }
BC BC::dup() { return BC(BC_t::dup_); }
BC BC::inci() { return BC(BC_t::inci_); }
BC BC::push_argi() { return BC(BC_t::push_argi_); }
BC BC::pushi(int i) {
    immediate_t im;
    im.i = i;
    return BC(BC_t::pushi_, im);
}
BC BC::close() { return BC(BC_t::close_); }
BC BC::add() { return BC(BC_t::add_); }
BC BC::sub() { return BC(BC_t::sub_); }
BC BC::lt() { return BC(BC_t::lt_); }
BC BC::invisible() { return BC(BC_t::invisible_); }

} // rir

#endif
