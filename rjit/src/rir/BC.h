#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstddef>
#include <cstdint>
#include <map>

#include "Pool.h"
#include "RDefs.h"

#include "BC_inc.h"

#include "../Protect.h"

namespace rjit {
namespace rir {

namespace {

BC::immediate_t decodeImmediate(BC_t bc, BC_t* pc) {
    BC::immediate_t immediate = {0};
    switch (bc) {
    case BC_t::push_:
    case BC_t::ldfun_:
    case BC_t::ldvar_:
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
    case BC_t::DEPRECATED_FORCE_ALL:
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

class CodeStream;

// list of individual bytecode sizes
static size_t immediate_size[] = {
    (size_t)-1,          // invalid
    sizeof(pool_idx_t),  // push
    sizeof(pool_idx_t),  // getfun
    sizeof(pool_idx_t),  // getvar
    sizeof(call_args_t), // call
    sizeof(fun_idx_t),   // mkprom
    0,                   // mkclosure
    0,                   // ret
    0,                   // force
    0,                   // pop
    sizeof(num_args_t),  // load_arg
    0,                   // get_ast
    0,                   // setvar
    0,                   // numargi
    0,                   // to_bool
    sizeof(jmp_t),       // jmp_true
    sizeof(jmp_t),       // jmp_false
    sizeof(jmp_t),       // jmp
    0,                   // lti
    0,                   // eqi
    0,                   // force_all
    sizeof(int),         // pushi
    0,                   // dupi
    0,                   // load_argi
    0,                   // inci
    0,                   // dup
    0,                   // add
    0,                   // sub
    0,                   // lt
    sizeof(pool_idx_t),  // check_primitive
    0,                   // check_function
    sizeof(jmp_t),       // label
};
static_assert(sizeof(immediate_size) / sizeof(size_t) == (unsigned)BC_t::num_of,
              "Please add an entry for the size of the bytecode");

template <typename T>
T BC::readImmediate(BC_t** pc) {
    T res = *(T*)*pc;
    *pc = (BC_t*)((uintptr_t)*pc + sizeof(T));
    return res;
}

BC_t BC::readBC(BC_t** pc) {
    BC_t bc = **pc;
    *pc += 1;
    return bc;
}

bool BC::isJmp() {
    return bc == BC_t::br_ || bc == BC_t::brtrue_ || bc == BC_t::brfalse_;
}

size_t BC::size() const { return sizeof(BC_t) + immediate_size[(size_t)bc]; }

const BC BC::ret() { return BC(BC_t::ret_); }
const BC BC::force() { return BC(BC_t::force_); }
const BC BC::force_all() { return BC(BC_t::DEPRECATED_FORCE_ALL); }
const BC BC::pop() { return BC(BC_t::pop_); }
const BC BC::push(SEXP constant) {
    immediate_t i;
    i.pool = Pool::instance().insert(constant);
    return BC(BC_t::push_, i);
}
const BC BC::push(double constant) {
    immediate_t i;
    i.pool = Pool::instance().getNum(constant);
    return BC(BC_t::push_, i);
}
const BC BC::getfun(SEXP sym) {
    immediate_t i;
    i.pool = Pool::instance().insert(sym);
    return BC(BC_t::ldfun_, i);
}
const BC BC::getvar(SEXP sym) {
    immediate_t i;
    i.pool = Pool::instance().insert(sym);
    return BC(BC_t::ldvar_, i);
}
const BC BC::check_primitive(SEXP sym) {
    immediate_t i;
    i.pool = Pool::instance().insert(sym);
    return BC(BC_t::isspecial_, i);
}
const BC BC::mkprom(fun_idx_t prom) { return BC(BC_t::promise_, {prom}); }
const BC BC::load_arg(num_args_t arg) { return BC(BC_t::pusharg_, {arg}); }
const BC BC::get_ast() { return BC(BC_t::asast_); }
const BC BC::setvar() { return BC(BC_t::stvar_); }
const BC BC::lti() { return BC(BC_t::lti_); }
const BC BC::eqi() { return BC(BC_t::eqi_); }
const BC BC::numargi() { return BC(BC_t::NUMARGI_DEPRECATED); }
const BC BC::to_bool() { return BC(BC_t::asbool_); }

BC BC::check_function() { return BC(BC_t::isfun_); }

const BC BC::label(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::label, i);
}
const BC BC::jmp(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::br_, i);
}
const BC BC::jmp_true(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::brtrue_, i);
}
const BC BC::jmp_false(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::brfalse_, i);
}
const BC BC::dupi() { return BC(BC_t::dupi_); }
const BC BC::dup() { return BC(BC_t::dup_); }
const BC BC::inci() { return BC(BC_t::inci_); }
const BC BC::load_argi() { return BC(BC_t::push_argi_); }
const BC BC::pushi(int i) {
    immediate_t im;
    im.i = i;
    return BC(BC_t::pushi_, im);
}
const BC BC::mkclosure() { return BC(BC_t::close_); }
const BC BC::add() { return BC(BC_t::add_); }
const BC BC::sub() { return BC(BC_t::sub_); }
const BC BC::lt() { return BC(BC_t::lt_); }

} // rir
} // rjit

#endif
