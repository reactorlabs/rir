#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstdint>
#include <cstddef>
#include <map>

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
    case BC_t::call_builtin:
    case BC_t::call_special:
        immediate.prim = *(primitive_t*)pc;
        break;
    case BC_t::pushi:
        immediate.i = *(int*)pc;
        break;
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

class CodeStream;

static size_t immediate_size[(size_t)BC_t::num_of] = {
    (size_t)-1,          // invalid
    sizeof(pool_idx_t),  // push
    sizeof(pool_idx_t),  // getfun
    sizeof(pool_idx_t),  // getvar
    sizeof(num_args_t),  // call
    sizeof(pool_idx_t),  // call_name
    sizeof(fun_idx_t),   // mkprom
    sizeof(fun_idx_t),   // mkclosure
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
    sizeof(primitive_t), // call_builtin
    sizeof(primitive_t), // call_special
    0,                   // force_all
    sizeof(int),         // pushi
    0,                   // dupi
    0,                   // load_argi
    0,                   // inci
    0,                   // dup
    0,                   // add
    0,                   // sub
    0,                   // lt
};

const BC BC::read(BC_t* pc) {
    BC_t bc = *pc;
    return BC(bc, readImmediate(bc, pc + 1));
}

BC_t* BC::rewind(BC_t* pc, BC cur) {
    return (BC_t*)((uintptr_t)pc - cur.size());
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
const BC BC::force_all() { return BC(BC_t::force_all); }
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
const BC BC::call_special(primitive_t prim) {
    immediate_t i;
    i.prim = prim;
    return BC(BC_t::call_special, i);
}
const BC BC::call_builtin(primitive_t prim) {
    immediate_t i;
    i.prim = prim;
    return BC(BC_t::call_builtin, i);
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

class AstMap {
    size_t size;
    unsigned* pos;
    SEXP* ast;

  public:
    AstMap(std::map<unsigned, SEXP>& astMap) {
        size = astMap.size();
        pos = new unsigned[size];
        ast = new SEXP[size];
        unsigned i = 0;
        for (auto e : astMap) {
            pos[i] = e.first;
            ast[i] = e.second;
            i++;
        }
    }

    ~AstMap() {
        delete pos;
        delete ast;
    }

    SEXP at(unsigned p) {
        size_t f = 0;

        while (f < size && pos[f] < p)
            f++;

        assert(pos[f] == p);

        return ast[f];
    }
};

class Code {
  public:
    size_t size;
    BC_t* bc;
    SEXP ast;
    AstMap astMap;

    Code(size_t size, BC_t* bc, SEXP ast, std::map<unsigned, SEXP>& astMap)
        : size(size), bc(bc), ast(ast), astMap(astMap){};
    ~Code() { delete bc; }

    void print();

    BC_t* end() { return (BC_t*)((uintptr_t)bc + size); }

    SEXP getAst(BC_t* pc) { return astMap.at((uintptr_t)pc - (uintptr_t)bc); }
};

} // rir
} // rjit

#endif
