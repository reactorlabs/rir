#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstdint>
#include <cstddef>
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
    case BC_t::push:
    case BC_t::getfun:
    case BC_t::getvar:
    case BC_t::check_special:
        immediate.pool = *(pool_idx_t*)pc;
        break;
    case BC_t::call:
        immediate.call_args = *(call_args_t*)pc;
        break;
    case BC_t::load_arg:
        immediate.numArgs = *(num_args_t*)pc;
        break;
    case BC_t::mkprom:
        immediate.fun = *(fun_idx_t*)pc;
        break;
    case BC_t::jmp:
    case BC_t::jmp_true:
    case BC_t::jmp_false:
        immediate.offset = *(jmp_t*)pc;
        break;
    case BC_t::pushi:
        immediate.i = *(int*)pc;
        break;
    case BC_t::mkclosure:
    case BC_t::ret:
    case BC_t::pop:
    case BC_t::force:
    case BC_t::force_all:
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
        break;
    case BC_t::invalid:
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
static size_t immediate_size[(size_t)BC_t::num_of] = {
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
    sizeof(pool_idx_t),  // check_special
};

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

size_t BC::size() const { return sizeof(BC_t) + immediate_size[(size_t)bc]; }

const BC BC::ret() { return BC(BC_t::ret); }
const BC BC::force() { return BC(BC_t::force); }
const BC BC::force_all() { return BC(BC_t::force_all); }
const BC BC::pop() { return BC(BC_t::pop); }
const BC BC::push(SEXP constant) {
    immediate_t i;
    i.pool = Pool::instance().insert(constant);
    return BC(BC_t::push, i);
}
const BC BC::getfun(SEXP sym) {
    immediate_t i;
    i.pool = Pool::instance().insert(sym);
    return BC(BC_t::getfun, i);
}
const BC BC::getvar(SEXP sym) {
    immediate_t i;
    i.pool = Pool::instance().insert(sym);
    return BC(BC_t::getvar, i);
}
const BC BC::check_special(SEXP sym) {
    immediate_t i;
    i.pool = Pool::instance().insert(sym);
    return BC(BC_t::check_special, i);
}
const BC BC::mkprom(fun_idx_t prom) { return BC(BC_t::mkprom, {prom}); }
const BC BC::load_arg(num_args_t arg) { return BC(BC_t::load_arg, {arg}); }
const BC BC::get_ast() { return BC(BC_t::get_ast); }
const BC BC::setvar() { return BC(BC_t::setvar); }
const BC BC::lti() { return BC(BC_t::lti); }
const BC BC::eqi() { return BC(BC_t::eqi); }
const BC BC::numargi() { return BC(BC_t::numargi); }
const BC BC::to_bool() { return BC(BC_t::to_bool); }
const BC BC::jmp(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::jmp, i);
}
const BC BC::jmp_true(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::jmp_true, i);
}
const BC BC::jmp_false(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::jmp_false, i);
}
const BC BC::dupi() { return BC(BC_t::dupi); }
const BC BC::dup() { return BC(BC_t::dup); }
const BC BC::inci() { return BC(BC_t::inci); }
const BC BC::load_argi() { return BC(BC_t::load_argi); }
const BC BC::pushi(int i) {
    immediate_t im;
    im.i = i;
    return BC(BC_t::pushi, im);
}
const BC BC::mkclosure() { return BC(BC_t::mkclosure); }
const BC BC::add() { return BC(BC_t::add); }
const BC BC::sub() { return BC(BC_t::sub); }
const BC BC::lt() { return BC(BC_t::lt); }

} // rir
} // rjit

#endif
