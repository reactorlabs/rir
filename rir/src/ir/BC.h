#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstddef>
#include <cstdint>
#include <map>

#include "BC_inc.h"
#include "R/Protect.h"
#include "R/r.h"
#include "interpreter/instance.h"
#include "ir/Deoptimization.h"
#include "runtime/Code.h"
#include "runtime/Function.h"
#include "utils/Pool.h"

namespace rir {

class CodeStream;

#define V(NESTED, name, name_)\
BC BC::name() { return BC(Opcode::name_##_); }
BC_NOARGS(V, _)
#undef V
BC BC::recordCall() { return BC(Opcode::record_call_); }
BC BC::recordType() { return BC(Opcode::record_type_); }
BC BC::recordDeopt(const DeoptReason& reason) {
    ImmediateArguments i;
    i.deoptReason = reason;
    return BC(Opcode::record_deopt_, i);
}

BC BC::popn(unsigned n) {
    ImmediateArguments i;
    i.i = n;
    return BC(Opcode::popn_, i);
}
BC BC::push(SEXP constant) {
    assert(TYPEOF(constant) != PROMSXP);
    assert(!Code::check(constant));
    ImmediateArguments i;
    i.pool = Pool::insert(constant);
    return BC(Opcode::push_, i);
}
BC BC::push(double constant) {
    ImmediateArguments i;
    i.pool = Pool::getNum(constant);
    return BC(Opcode::push_, i);
}
BC BC::push(int constant) {
    ImmediateArguments i;
    i.pool = Pool::getInt(constant);
    return BC(Opcode::push_, i);
}
BC BC::push_from_pool(PoolIdx idx) {
    ImmediateArguments i;
    i.pool = idx;
    return BC(Opcode::push_, i);
}

BC BC::ldfun(SEXP sym) {
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldfun_, i);
}
BC BC::ldddvar(SEXP sym) {
    assert(DDVAL(sym));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldddvar_, i);
}
BC BC::stvarStubbed(unsigned pos) {
    ImmediateArguments i;
    i.i = pos;
    return BC(Opcode::stvar_stubbed_, i);
}
BC BC::ldvarNoForceStubbed(unsigned pos) {
    ImmediateArguments i;
    i.i = pos;
    return BC(Opcode::ldvar_noforce_stubbed_, i);
}
BC BC::ldvar(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldvar_, i);
}
BC BC::ldvarCached(SEXP sym, uint32_t cacheSlot) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.poolAndCache.poolIndex = Pool::insert(sym);
    i.poolAndCache.cacheIndex = cacheSlot;
    return BC(Opcode::ldvar_cached_, i);
}
BC BC::ldvarForUpdateCached(SEXP sym, uint32_t cacheSlot) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.poolAndCache.poolIndex = Pool::insert(sym);
    i.poolAndCache.cacheIndex = cacheSlot;
    return BC(Opcode::ldvar_for_update_cache_, i);
}
BC BC::ldvarForUpdate(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldvar_for_update_, i);
}
BC BC::ldvarNoForce(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldvar_noforce_, i);
}
BC BC::ldvarNoForceCached(SEXP sym, uint32_t cacheSlot) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.poolAndCache.poolIndex = Pool::insert(sym);
    i.poolAndCache.cacheIndex = cacheSlot;
    return BC(Opcode::ldvar_noforce_cached_, i);
}
BC BC::ldvarSuper(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldvar_super_, i);
}
BC BC::ldvarNoForceSuper(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::ldvar_noforce_super_, i);
}
BC BC::ldarg(uint32_t offset) {
    ImmediateArguments i;
    i.arg_idx = offset;
    return BC(Opcode::ldarg_, i);
}
BC BC::ldloc(uint32_t offset) {
    ImmediateArguments im;
    im.loc = offset;
    return BC(Opcode::ldloc_, im);
}
BC BC::stloc(uint32_t offset) {
    ImmediateArguments im;
    im.loc = offset;
    return BC(Opcode::stloc_, im);
}
BC BC::copyloc(uint32_t target, uint32_t source) {
    ImmediateArguments im;
    im.loc_cpy.target = target;
    im.loc_cpy.source = source;
    return BC(Opcode::movloc_, im);
}
BC BC::guardName(SEXP sym, SEXP expected) {
    ImmediateArguments i;
    i.guard_fun_args = {Pool::insert(sym), Pool::insert(expected),
                        NO_DEOPT_INFO};
    return BC(Opcode::guard_fun_, i);
}
BC BC::guardNamePrimitive(SEXP sym) {
    ImmediateArguments i;
    assert(TYPEOF(sym) == SYMSXP);
    SEXP prim = CDR(sym);
    assert(TYPEOF(prim) == SPECIALSXP || TYPEOF(prim) == BUILTINSXP);
    i.guard_fun_args = {Pool::insert(sym), Pool::insert(prim), NO_DEOPT_INFO};
    return BC(Opcode::guard_fun_, i);
}
BC BC::push_code(FunIdx prom) {
    ImmediateArguments i;
    i.fun = prom;
    return BC(Opcode::push_code_, i);
}
BC BC::mkEagerPromise(FunIdx prom) {
    ImmediateArguments i;
    i.fun = prom;
    return BC(Opcode::mk_eager_promise_, i);
}
BC BC::mkPromise(FunIdx prom) {
    ImmediateArguments i;
    i.fun = prom;
    return BC(Opcode::mk_promise_, i);
}
BC BC::missing(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::missing_, i);
}
BC BC::starg(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::starg_, i);
}
BC BC::stargCached(SEXP sym, uint32_t cacheSlot) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.poolAndCache.poolIndex = Pool::insert(sym);
    i.poolAndCache.cacheIndex = cacheSlot;
    return BC(Opcode::starg_cached_, i);
}
BC BC::stvar(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::stvar_, i);
}
BC BC::stvarCached(SEXP sym, uint32_t cacheSlot) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.poolAndCache.poolIndex = Pool::insert(sym);
    i.poolAndCache.cacheIndex = cacheSlot;
    return BC(Opcode::stvar_cached_, i);
}
BC BC::stvarSuper(SEXP sym) {
    assert(TYPEOF(sym) == SYMSXP);
    assert(strlen(CHAR(PRINTNAME(sym))));
    ImmediateArguments i;
    i.pool = Pool::insert(sym);
    return BC(Opcode::stvar_super_, i);
}
BC BC::alloc(int type) {
    ImmediateArguments i;
    i.i = type;
    return BC(Opcode::alloc_, i);
}
BC BC::br(Jmp j) {
    ImmediateArguments i;
    i.offset = j;
    return BC(Opcode::br_, i);
}
BC BC::pushContext(Jmp j) {
    ImmediateArguments i;
    i.offset = j;
    return BC(Opcode::push_context_, i);
}
BC BC::beginloop(Jmp j) {
    ImmediateArguments i;
    i.offset = j;
    return BC(Opcode::beginloop_, i);
}
BC BC::brtrue(Jmp j) {
    ImmediateArguments i;
    i.offset = j;
    return BC(Opcode::brtrue_, i);
}
BC BC::brfalse(Jmp j) {
    ImmediateArguments i;
    i.offset = j;
    return BC(Opcode::brfalse_, i);
}
BC BC::pull(uint32_t i) {
    ImmediateArguments im;
    im.i = i;
    return BC(Opcode::pull_, im);
}
BC BC::pick(uint32_t i) {
    ImmediateArguments im;
    im.i = i;
    return BC(Opcode::pick_, im);
}
BC BC::is(uint32_t i) {
    assert(i < MAX_NUM_SEXPTYPE && "Invalid SEXPTYPE in is_");
    ImmediateArguments im;
    im.i = i;
    return BC(Opcode::is_, im);
}
BC BC::isType(TypeChecks i) {
    assert(static_cast<uint32_t>(i) > MAX_NUM_SEXPTYPE &&
           "Invalid type in istype_");
    ImmediateArguments im;
    im.i = static_cast<uint32_t>(i);
    return BC(Opcode::istype_, im);
}
BC BC::put(uint32_t i) {
    ImmediateArguments im;
    im.i = i;
    return BC(Opcode::put_, im);
}
BC BC::call(size_t nargs, SEXP ast, const Assumptions& given) {
    ImmediateArguments im;
    im.callFixedArgs.nargs = nargs;
    im.callFixedArgs.ast = Pool::insert(ast);
    im.callFixedArgs.given = given;
    return BC(Opcode::call_, im);
}
BC BC::callDots(size_t nargs, const std::vector<SEXP>& names, SEXP ast,
                const Assumptions& given) {
    ImmediateArguments im;
    im.callFixedArgs.nargs = nargs;
    im.callFixedArgs.ast = Pool::insert(ast);
    im.callFixedArgs.given = given;
    std::vector<PoolIdx> nameIdxs;
    for (auto n : names)
        nameIdxs.push_back(Pool::insert(n));
    BC cur(Opcode::call_dots_, im);
    cur.callExtra().callArgumentNames = nameIdxs;
    return cur;
}
BC BC::call(size_t nargs, const std::vector<SEXP>& names, SEXP ast,
            const Assumptions& given) {
    ImmediateArguments im;
    im.callFixedArgs.nargs = nargs;
    im.callFixedArgs.ast = Pool::insert(ast);
    im.callFixedArgs.given = given;
    std::vector<PoolIdx> nameIdxs;
    for (auto n : names)
        nameIdxs.push_back(Pool::insert(n));
    BC cur(Opcode::named_call_, im);
    cur.callExtra().callArgumentNames = nameIdxs;
    return cur;
}
BC BC::staticCall(size_t nargs, SEXP ast, SEXP targetClosure,
                  SEXP targetVersion, const Assumptions& given) {
    assert(!targetVersion || Function::unpack(targetVersion));
    assert(TYPEOF(targetClosure) == CLOSXP);
    auto target =
        targetVersion ? Pool::insert(targetVersion) : Pool::makeSpace();
    ImmediateArguments im;
    im.staticCallFixedArgs.nargs = nargs;
    im.staticCallFixedArgs.ast = Pool::insert(ast);
    im.staticCallFixedArgs.targetClosure = Pool::insert(targetClosure);
    im.staticCallFixedArgs.versionHint = target;
    im.staticCallFixedArgs.given = given;
    return BC(Opcode::static_call_, im);
}
BC BC::callBuiltin(size_t nargs, SEXP ast, SEXP builtin) {
    assert(TYPEOF(builtin) == BUILTINSXP);
    ImmediateArguments im;
    im.callBuiltinFixedArgs.nargs = nargs;
    im.callBuiltinFixedArgs.ast = Pool::insert(ast);
    im.callBuiltinFixedArgs.builtin = Pool::insert(builtin);
    return BC(Opcode::call_builtin_, im);
}

BC BC::mkEnv(const std::vector<SEXP>& names, SignedImmediate contextPos,
             bool stub) {
    ImmediateArguments im;
    im.mkEnvFixedArgs.nargs = names.size();
    im.mkEnvFixedArgs.context = contextPos;
    std::vector<PoolIdx> nameIdxs;
    for (auto n : names)
        nameIdxs.push_back(Pool::insert(n));
    BC cur;
    if (stub)
        cur = BC(Opcode::mk_stub_env_, im);
    else
        cur = BC(Opcode::mk_env_, im);
    cur.mkEnvExtra().names = nameIdxs;
    return cur;
}

BC BC::clearBindingCache(CacheIdx start, unsigned size) {
    ImmediateArguments im;
    im.cacheIdx.start = start;
    im.cacheIdx.size = size;
    return BC(Opcode::clear_binding_cache_, im);
}

BC BC::deopt(SEXP deoptMetadata) {
    ImmediateArguments i;
    i.pool = Pool::insert(deoptMetadata);
    return BC(Opcode::deopt_, i);
}

BC BC::assertType(pir::PirType typ, SignedImmediate instr) {
    ImmediateArguments i;
    i.assertTypeArgs.setPirType(typ);
    i.assertTypeArgs.instr = instr;
    return BC(Opcode::assert_type_, i);
}

BC BC::checkGlobalCache() {
    ImmediateArguments i;
    i.cacheVersion = 0;
    return BC(Opcode::check_global_cache_, i);
}

} // namespace rir

#endif
