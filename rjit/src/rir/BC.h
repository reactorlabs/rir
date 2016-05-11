#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstdint>
#include <cstddef>

#include "Pool.h"
#include "RDefs.h"

#include "BC_inc.h"

namespace rjit {
namespace rir {

namespace {

immediate_t readImmediate(BC_t bc, BC_t* pc) {
    immediate_t immediate = {0};
    switch (bc) {
    case BC_t::push:
    case BC_t::getfun:
    case BC_t::getvar:
    case BC_t::call_name:
        immediate.pool = *(pool_idx_t*)pc;
        break;
    case BC_t::call:
    case BC_t::load_arg:
        immediate.numArgs = *(num_args_t*)pc;
        break;
    case BC_t::mkprom:
    case BC_t::mkclosure:
        immediate.fun = *(fun_idx_t*)pc;
        break;
    case BC_t::jmp:
    case BC_t::jmp_true:
    case BC_t::jmp_false:
        immediate.offset = *(jmp_t*)pc;
        break;
    case BC_t::ret:
    case BC_t::pop:
    case BC_t::force:
    case BC_t::get_ast:
    case BC_t::setvar:
    case BC_t::to_bool:
    case BC_t::numarg:
    case BC_t::lt:
    case BC_t::eq:
        break;
    case BC_t::invalid:
    case BC_t::num_of:
        assert(false);
        break;
    }
    return immediate;
}
}

class CodeStream;

static size_t immediate_size[(size_t)BC_t::num_of] = {
    (size_t)-1,         // invalid
    sizeof(pool_idx_t), // push
    sizeof(pool_idx_t), // getfun
    sizeof(pool_idx_t), // getvar
    sizeof(num_args_t), // call
    sizeof(pool_idx_t), // call_name
    sizeof(fun_idx_t),  // mkprom
    sizeof(fun_idx_t),  // mkclosure
    0,                  // ret
    0,                  // force
    0,                  // pop
    sizeof(num_args_t), // load_arg
    0,                  // get_ast
    0,                  // setvar
    0,                  // numarg
    0,                  // to_bool
    sizeof(jmp_t),      // jmp_true
    sizeof(jmp_t),      // jmp_false
    sizeof(jmp_t),      // jmp
    0,                  // lt
    0,                  // eq
};

const BC BC::read(BC_t* pc) {
    BC_t bc = *pc;
    return BC(bc, readImmediate(bc, pc + 1));
}

const BC BC::advance(BC_t** pc) {
    BC_t bc = **pc;
    BC cur(bc, readImmediate(bc, (*pc) + 1));
    *pc = (BC_t*)((uintptr_t)(*pc) + cur.size());
    return cur;
}

size_t BC::size() const { return sizeof(BC_t) + immediate_size[(size_t)bc]; }

const BC BC::ret() { return BC(BC_t::ret); }
const BC BC::force() { return BC(BC_t::force); }
const BC BC::pop() { return BC(BC_t::pop); }
const BC BC::call(num_args_t numArgs) { return BC(BC_t::call, {numArgs}); }
const BC BC::call_name(SEXP names) {
    return BC(BC_t::call_name, {Pool::instance().insert(names)});
}
const BC BC::push(SEXP constant) {
    return BC(BC_t::push, {Pool::instance().insert(constant)});
}
const BC BC::getfun(SEXP sym) {
    return BC(BC_t::getfun, {Pool::instance().insert(sym)});
}
const BC BC::getvar(SEXP sym) {
    return BC(BC_t::getvar, {Pool::instance().insert(sym)});
}
const BC BC::mkprom(fun_idx_t prom) { return BC(BC_t::mkprom, {prom}); }
const BC BC::load_arg(num_args_t arg) { return BC(BC_t::load_arg, {arg}); }
const BC BC::get_ast() { return BC(BC_t::get_ast); }
const BC BC::setvar() { return BC(BC_t::setvar); }
const BC BC::lt() { return BC(BC_t::lt); }
const BC BC::eq() { return BC(BC_t::eq); }
const BC BC::numarg() { return BC(BC_t::numarg); }
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

class Code {
  public:
    size_t size;
    BC_t* bc;

    Code(size_t size, BC_t* bc) : size(size), bc(bc){};
    ~Code() { delete bc; }

    void print();

    BC_t* end() { return (BC_t*)((uintptr_t)bc + size); }
};

} // rir
} // rjit

#endif
