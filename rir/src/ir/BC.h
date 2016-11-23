#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstddef>
#include <cstdint>
#include <map>

#include "utils/Pool.h"
#include "R/r.h"

#include "BC_inc.h"

#include "R/Protect.h"

#include "interpreter/runtime.h"

namespace rir {

namespace {

BC::immediate_t decodeImmediate(BC_t bc, BC_t* pc) {
    BC::immediate_t immediate = {{0}};
    switch (bc) {
    case BC_t::push_:
    case BC_t::ldfun_:
    case BC_t::ldarg_:
    case BC_t::ldvar_:
    case BC_t::ldlval_:
    case BC_t::ldddvar_:
    case BC_t::stvar_:
    case BC_t::missing_:
    case BC_t::subassign2_:
        immediate.pool = *(pool_idx_t*)pc;
        break;
    case BC_t::dispatch_stack_:
    case BC_t::call_:
    case BC_t::dispatch_:
    case BC_t::call_stack_:
    case BC_t::static_call_stack_:
        immediate.call_args = *(CallArgs*)pc;
        break;
    case BC_t::guard_local_:
    case BC_t::guard_arg_:
        immediate.guard_local_args = *(GuardLocalArgs*)pc;
        break;
    case BC_t::guard_fun_:
        immediate.guard_fun_args = *(GuardFunArgs*)pc;
        break;
    case BC_t::promise_:
    case BC_t::push_code_:
        immediate.fun = *(fun_idx_t*)pc;
        break;
    case BC_t::br_:
    case BC_t::brtrue_:
    case BC_t::brobj_:
    case BC_t::brfalse_:
    case BC_t::label:
    case BC_t::beginloop_:
        immediate.offset = *(jmp_t*)pc;
        break;
    case BC_t::pick_:
    case BC_t::pull_:
    case BC_t::is_:
    case BC_t::put_:
    case BC_t::alloc_:
        immediate.i = *(uint32_t*)pc;
        break;
    case BC_t::test_bounds_:
    case BC_t::extract1_:
    case BC_t::subset1_:
    case BC_t::extract2_:
    case BC_t::subset2_:
    case BC_t::close_:
    case BC_t::ret_:
    case BC_t::pop_:
    case BC_t::force_:
    case BC_t::asast_:
    case BC_t::asbool_:
    case BC_t::dup_:
    case BC_t::dup2_:
    case BC_t::swap_:
    case BC_t::int3_:
    case BC_t::uniq_:
    case BC_t::aslogical_:
    case BC_t::lgl_and_:
    case BC_t::lgl_or_:
    case BC_t::inc_:
    case BC_t::add_:
    case BC_t::mul_:
    case BC_t::div_:
    case BC_t::idiv_:
    case BC_t::mod_:
    case BC_t::pow_:
    case BC_t::seq_:
    case BC_t::sub_:
    case BC_t::lt_:
    case BC_t::return_:
    case BC_t::isfun_:
    case BC_t::invisible_:
    case BC_t::visible_:
    case BC_t::endcontext_:
    case BC_t::subassign_:
    case BC_t::length_:
    case BC_t::names_:
    case BC_t::set_names_:
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
BC BC::return_() { return BC(BC_t::return_); }
BC BC::force() { return BC(BC_t::force_); }
BC BC::pop() { return BC(BC_t::pop_); }
BC BC::push(SEXP constant) {
    assert(TYPEOF(constant) != PROMSXP);
//    assert(!isValidFunctionSEXP(constant));
    assert(!isValidCodeObject(constant));
    immediate_t i;
    i.pool = Pool::insert(constant);
    return BC(BC_t::push_, i);
}
BC BC::push(double constant) {
    immediate_t i;
    i.pool = Pool::getNum(constant);
    return BC(BC_t::push_, i);
}
BC BC::push(int constant) {
    immediate_t i;
    i.pool = Pool::getInt(constant);
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
BC BC::ldlval(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::ldlval_, i);
}
BC BC::ldvar(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::ldvar_, i);
}
BC BC::ldarg(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::ldarg_, i);
}
BC BC::guardArg(SEXP sym) {
    immediate_t i;
    i.guard_local_args = {Pool::insert(sym), 123};
    return BC(BC_t::guard_arg_, i);
}
BC BC::guardLocal(SEXP sym) {
    immediate_t i;
    i.guard_local_args = {Pool::insert(sym), 123};
    return BC(BC_t::guard_local_, i);
}
BC BC::guardName(SEXP sym, SEXP expected) {
    immediate_t i;
    i.guard_fun_args = {Pool::insert(sym), Pool::insert(expected), 123};
    return BC(BC_t::guard_fun_, i);
}
BC BC::guardNamePrimitive(SEXP sym) {
    immediate_t i;
    assert(TYPEOF(sym) == SYMSXP);
    SEXP prim = CDR(sym);
    assert(TYPEOF(prim) == SPECIALSXP || TYPEOF(prim) == BUILTINSXP);
    i.guard_fun_args = {Pool::insert(sym), Pool::insert(prim), 123};
    return BC(BC_t::guard_fun_, i);
}
BC BC::push_code(fun_idx_t prom) {
    immediate_t i;
    i.fun = prom;
    return BC(BC_t::push_code_, i);
}
BC BC::promise(fun_idx_t prom) {
    immediate_t i;
    i.fun = prom;
    return BC(BC_t::promise_, i);
}
BC BC::asast() { return BC(BC_t::asast_); }
BC BC::missing(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::missing_, i);
}
BC BC::stvar(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::stvar_, i);
}
BC BC::subassign() { return BC(BC_t::subassign_); }
BC BC::subassign2(SEXP sym) {
    assert(sym == R_NilValue ||
           (TYPEOF(sym) == SYMSXP && strlen(CHAR(PRINTNAME(sym)))));
    immediate_t i;
    i.pool = Pool::insert(sym);
    return BC(BC_t::subassign2_, i);
}
BC BC::seq() { return BC(BC_t::seq_); }
BC BC::asbool() { return BC(BC_t::asbool_); }

BC BC::length() { return BC(BC_t::length_); }
BC BC::names() { return BC(BC_t::names_); }
BC BC::setNames() { return BC(BC_t::set_names_); }
BC BC::alloc(int type) {
    immediate_t i;
    i.i = type;
    return BC(BC_t::alloc_, i);
}

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
BC BC::brobj(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::brobj_, i);
}
BC BC::beginloop(jmp_t j) {
    immediate_t i;
    i.offset = j;
    return BC(BC_t::beginloop_, i);
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
BC BC::endcontext() { return BC(BC_t::endcontext_); }
BC BC::dup() { return BC(BC_t::dup_); }
BC BC::inc() { return BC(BC_t::inc_); }
BC BC::close() { return BC(BC_t::close_); }
BC BC::dup2() { return BC(BC_t::dup2_); }
BC BC::testBounds() { return BC(BC_t::test_bounds_); }
BC BC::add() { return BC(BC_t::add_); }
BC BC::mul() { return BC(BC_t::mul_); }
BC BC::div() { return BC(BC_t::div_); }
BC BC::idiv() { return BC(BC_t::idiv_); }
BC BC::mod() { return BC(BC_t::mod_); }
BC BC::pow() { return BC(BC_t::pow_); }
BC BC::sub() { return BC(BC_t::sub_); }
BC BC::lt() { return BC(BC_t::lt_); }
BC BC::invisible() { return BC(BC_t::invisible_); }
BC BC::visible() { return BC(BC_t::visible_); }
BC BC::extract1() { return BC(BC_t::extract1_); }
BC BC::subset1() { return BC(BC_t::subset1_); }
BC BC::extract2() { return BC(BC_t::extract2_); }
BC BC::subset2() { return BC(BC_t::subset2_); }
BC BC::swap() { return BC(BC_t::swap_); }
BC BC::int3() { return BC(BC_t::int3_); }
BC BC::uniq() { return BC(BC_t::uniq_); }
BC BC::asLogical() { return BC(BC_t::aslogical_); }
BC BC::lglAnd() { return BC(BC_t::lgl_and_); }
BC BC::lglOr() { return BC(BC_t::lgl_or_); }
BC BC::pull(uint32_t i) {
    immediate_t im;
    im.i = i;
    return BC(BC_t::pull_, im);
}
BC BC::pick(uint32_t i) {
    immediate_t im;
    im.i = i;
    return BC(BC_t::pick_, im);
}
BC BC::is(uint32_t i) {
    immediate_t im;
    im.i = i;
    return BC(BC_t::is_, im);
}
BC BC::put(uint32_t i) {
    immediate_t im;
    im.i = i;
    return BC(BC_t::put_, im);
}

} // rir

#endif
