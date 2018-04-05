#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstddef>
#include <cstdint>
#include <map>

#include "R/r.h"
#include "utils/Pool.h"

#include "BC_inc.h"

#include "R/Protect.h"

#include "interpreter/runtime.h"

namespace rir {

class CodeStream;

BC BC::nop() { return BC(Opcode::nop_); }
BC BC::makeEnv() { return BC(Opcode::make_env_); }
BC BC::getEnv() { return BC(Opcode::get_env_); }
BC BC::setEnv() { return BC(Opcode::set_env_); }
BC BC::ret() { return BC(Opcode::ret_); }
BC BC::return_() { return BC(Opcode::return_); }
BC BC::force() { return BC(Opcode::force_); }
BC BC::pop() { return BC(Opcode::pop_); }
BC BC::push(SEXP constant) {
    assert(TYPEOF(constant) != PROMSXP);
//    assert(!isValidFunctionSEXP(constant));
    assert(!isValidCodeObject(constant));
    ImmediateT i;
    i.pool = Pool::insert(constant);
    return BC(Opcode::push_, i);
}
BC BC::push(double constant) {
    ImmediateT i;
    i.pool = Pool::getNum(constant);
    return BC(Opcode::push_, i);
}
BC BC::push(int constant) {
    ImmediateT i;
    i.pool = Pool::getInt(constant);
    return BC(Opcode::push_, i);
}
BC BC::ldfun(SEXP sym) {
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldfun_, i);
}
BC BC::ldddvar(SEXP sym) {
    assert(DDVAL(sym));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldddvar_, i);
}
BC BC::ldlval(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldlval_, i);
}
BC BC::ldvar(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldvar_, i);
}
BC BC::ldvar2(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldvar2_, i);
}
BC BC::ldloc(uint32_t offset) {
    ImmediateT im;
    im.loc = offset;
    return BC(Opcode::ldloc_, im);
}
BC BC::stloc(uint32_t offset) {
    ImmediateT im;
    im.loc = offset;
    return BC(Opcode::stloc_, im);
}
BC BC::copyloc(uint32_t target, uint32_t source) {
    ImmediateT im;
    im.loc_cpy.target = target;
    im.loc_cpy.source = source;
    return BC(Opcode::copyloc_, im);
}
BC BC::ldarg(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldarg_, i);
}
BC BC::guardEnv(uint32_t id) {
    ImmediateT i;
    i.guard_id = id;
    return BC(Opcode::guard_env_, i);
}
BC BC::guardName(SEXP sym, SEXP expected) {
    ImmediateT i;
    i.guard_fun_args = {Pool::insert(sym), Pool::insert(expected),
                        NO_DEOPT_INFO};
    return BC(Opcode::guard_fun_, i);
}
BC BC::guardNamePrimitive(SEXP sym) {
    ImmediateT i;
    assert(TYPEOF(sym) == SYMSXP);
    SEXP prim = CDR(sym);
    assert(TYPEOF(prim) == SPECIALSXP || TYPEOF(prim) == BUILTINSXP);
    i.guard_fun_args = {Pool::insert(sym), Pool::insert(prim), NO_DEOPT_INFO};
    return BC(Opcode::guard_fun_, i);
}
BC BC::push_code(FunIdxT prom) {
    ImmediateT i;
    i.fun = prom;
    return BC(Opcode::push_code_, i);
}
BC BC::promise(FunIdxT prom) {
    ImmediateT i;
    i.fun = prom;
    return BC(Opcode::promise_, i);
}
BC BC::asast() { return BC(Opcode::asast_); }
BC BC::missing(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::missing_, i);
}
BC BC::stvar(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::stvar_, i);
}
BC BC::stvar2(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::stvar2_, i);
}
BC BC::subassign1() { return BC(Opcode::subassign1_); }
BC BC::subassign2(SEXP sym) {
    assert(sym == R_NilValue ||
           (TYPEOF(sym) == SYMSXP && strlen(CHAR(PRINTNAME(sym)))));
    ImmediateT i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::subassign2_, i);
}
BC BC::seq() { return BC(Opcode::seq_); }
BC BC::colon() { return BC(Opcode::colon_); }
BC BC::asbool() { return BC(Opcode::asbool_); }

BC BC::length() { return BC(Opcode::length_); }
BC BC::names() { return BC(Opcode::names_); }
BC BC::setNames() { return BC(Opcode::set_names_); }
BC BC::alloc(int type) {
    ImmediateT i;
    i.i = type;
    return BC(Opcode::alloc_, i);
}

BC BC::isfun() { return BC(Opcode::isfun_); }

BC BC::label(JmpT j) {
    ImmediateT i;
    i.offset = j;
    return BC(Opcode::label, i);
}
BC BC::br(JmpT j) {
    ImmediateT i;
    i.offset = j;
    return BC(Opcode::br_, i);
}
BC BC::brobj(JmpT j) {
    ImmediateT i;
    i.offset = j;
    return BC(Opcode::brobj_, i);
}
BC BC::beginloop(JmpT j) {
    ImmediateT i;
    i.offset = j;
    return BC(Opcode::beginloop_, i);
}
BC BC::brtrue(JmpT j) {
    ImmediateT i;
    i.offset = j;
    return BC(Opcode::brtrue_, i);
}
BC BC::brfalse(JmpT j) {
    ImmediateT i;
    i.offset = j;
    return BC(Opcode::brfalse_, i);
}
BC BC::endcontext() { return BC(Opcode::endcontext_); }
BC BC::dup() { return BC(Opcode::dup_); }
BC BC::inc() { return BC(Opcode::inc_); }
BC BC::close() { return BC(Opcode::close_); }
BC BC::dup2() { return BC(Opcode::dup2_); }
BC BC::forSeqSize() { return BC(Opcode::for_seq_size_); }
BC BC::add() { return BC(Opcode::add_); }
BC BC::mul() { return BC(Opcode::mul_); }
BC BC::div() { return BC(Opcode::div_); }
BC BC::idiv() { return BC(Opcode::idiv_); }
BC BC::mod() { return BC(Opcode::mod_); }
BC BC::pow() { return BC(Opcode::pow_); }
BC BC::sub() { return BC(Opcode::sub_); }
BC BC::uplus() { return BC(Opcode::uplus_); }
BC BC::uminus() { return BC(Opcode::uminus_); }
BC BC::Not() { return BC(Opcode::not_); }
BC BC::lt() { return BC(Opcode::lt_); }
BC BC::gt() { return BC(Opcode::gt_); }
BC BC::le() { return BC(Opcode::le_); }
BC BC::ge() { return BC(Opcode::ge_); }
BC BC::eq() { return BC(Opcode::eq_); }
BC BC::ne() { return BC(Opcode::ne_); }
BC BC::invisible() { return BC(Opcode::invisible_); }
BC BC::visible() { return BC(Opcode::visible_); }
BC BC::extract1_1() { return BC(Opcode::extract1_1_); }
BC BC::extract1_2() { return BC(Opcode::extract1_2_); }
BC BC::extract2_1() { return BC(Opcode::extract2_1_); }
BC BC::extract2_2() { return BC(Opcode::extract2_2_); }
BC BC::swap() { return BC(Opcode::swap_); }
BC BC::int3() { return BC(Opcode::int3_); }
BC BC::makeUnique() { return BC(Opcode::make_unique_); }
BC BC::setShared() { return BC(Opcode::set_shared_); }
BC BC::asLogical() { return BC(Opcode::aslogical_); }
BC BC::lglAnd() { return BC(Opcode::lgl_and_); }
BC BC::lglOr() { return BC(Opcode::lgl_or_); }
BC BC::pull(uint32_t i) {
    ImmediateT im;
    im.i = i;
    return BC(Opcode::pull_, im);
}
BC BC::pick(uint32_t i) {
    ImmediateT im;
    im.i = i;
    return BC(Opcode::pick_, im);
}
BC BC::is(uint32_t i) {
    ImmediateT im;
    im.i = i;
    return BC(Opcode::is_, im);
}
BC BC::put(uint32_t i) {
    ImmediateT im;
    im.i = i;
    return BC(Opcode::put_, im);
}

} // rir

#endif
