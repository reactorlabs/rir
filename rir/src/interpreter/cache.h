#ifndef RIR_INTERPRETER_CACHE_H
#define RIR_INTERPRETER_CACHE_H

#include "instance.h"

namespace rir {

// must be a power of 2 for modulus using & CACHE_MASK to work
#define MAX_CACHE_SIZE 128
#define CACHE_MASK (MAX_CACHE_SIZE - 1)
#define ACTIVE_BINDING_MASK (1 << 15)
#define BINDING_LOCK_MASK (1 << 14)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)

static RIR_INLINE Immediate cacheIndex(Immediate index, bool smallCache) {
    Immediate cidx;
    if (smallCache)
        cidx = index;
    else
        cidx = index & CACHE_MASK;
    return cidx;
}

#ifdef CACHE_ON_R_STACK
// TODO: Create a version with a cache on the R_stack instead of the C stack
#else
typedef struct {
    SEXP loc;
    Immediate idx;
} BindingCache;
typedef BindingCache Cache;

static RIR_INLINE void clearCache(Cache* cache, size_t cacheSize) {
    memset(cache, 0, sizeof(Cache) * cacheSize);
}

static RIR_INLINE SEXP cachedGetBindingCell(SEXP env, Immediate poolIdx,
                                            Immediate cacheIdx,
                                            InterpreterInstance* ctx,
                                            Cache* cache,
                                            bool smallCache) {

    Immediate cidx = cacheIndex(cacheIdx, smallCache);

    if (cache[cidx].idx == cacheIdx)
        return cache[cidx].loc;

    return NULL;
}

static RIR_INLINE void cachedSetBindingCell(Immediate cacheIdx, Cache* cache,
                                            bool smallCache, R_varloc_t loc) {
    Immediate cidx = cacheIndex(cacheIdx, smallCache);
    cache[cidx].loc = loc.cell;
    cache[cidx].idx = cacheIdx;
}

static RIR_INLINE SEXP getCellFromCache(SEXP env, Immediate poolIdx,
                                        Immediate cacheIdx,
                                        InterpreterInstance* ctx, Cache* cache,
                                        bool smallCache) {
    if (env != R_BaseEnv && env != R_BaseNamespace) {
        SEXP cell = cachedGetBindingCell(env, poolIdx, cacheIdx, ctx, cache,
                                         smallCache);
        if (!cell) {
            SEXP sym = cp_pool_at(ctx, poolIdx);
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            R_varloc_t loc = R_findVarLocInFrame(env, sym);
            if (!R_VARLOC_IS_NULL(loc)) {
                cachedSetBindingCell(cacheIdx, cache, smallCache, loc);
                return loc.cell;
            }
        } else {
            return cell;
        }
    }
    return nullptr;
}

static RIR_INLINE SEXP cachedGetVar(SEXP env, Immediate poolIdx,
                                    Immediate cacheIdx,
                                    InterpreterInstance* ctx, Cache* cache,
                                    bool smallCache) {
    SEXP cell =
        getCellFromCache(env, poolIdx, cacheIdx, ctx, cache, smallCache);
    if (cell) {
        SEXP res = CAR(cell);
        if (res != R_UnboundValue)
            return res;
    }
    SEXP sym = cp_pool_at(ctx, poolIdx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    return Rf_findVar(sym, env);
}

static RIR_INLINE void cachedSetVar(SEXP val, SEXP env, Immediate poolIdx,
                                    Immediate cacheIdx,
                                    InterpreterInstance* ctx,
                                    Cache* cache, bool smallCache,
                                    bool keepMissing = false) {
    SEXP loc = getCellFromCache(env, poolIdx, cacheIdx, ctx, cache, smallCache);
    if (loc && !BINDING_IS_LOCKED(loc) && !IS_ACTIVE_BINDING(loc)) {
        SEXP cur = CAR(loc);
        if (cur == val)
            return;
        INCREMENT_NAMED(val);
        SETCAR(loc, val);
        if (!keepMissing && MISSING(loc)) {
            SET_MISSING(loc, 0);
        }
        return;
    }

    SEXP sym = cp_pool_at(ctx, poolIdx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    INCREMENT_NAMED(val);
    PROTECT(val);
    Rf_defineVar(sym, val, env);
    UNPROTECT(1);
}

#endif
} // namespace rir
#endif