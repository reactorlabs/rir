#ifndef RIR_INTERPRETER_CACHE_H
#define RIR_INTERPRETER_CACHE_H

#include "R/r.h"
#include "instance.h"

namespace rir {

#define MAX_CACHE_SIZE 255
#define ACTIVE_BINDING_MASK (1 << 15)
#define BINDING_LOCK_MASK (1 << 14)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)
#define FRAME_LOCK_MASK (1 << 14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)

#ifdef CACHE_ON_R_STACK
// TODO: Create a version with a cache on the R_stack instead of the C stack
#else
typedef SEXP BindingCacheEntry;
struct BindingCache {
    size_t length;
    BindingCacheEntry entry[];
};
static_assert((sizeof(BindingCache) +
               sizeof(BindingCacheEntry) * MAX_CACHE_SIZE) %
                      64 ==
                  0,
              "Cache should be cache line sized");

static RIR_INLINE void clearCache(BindingCache* cache) {
    memset(cache->entry, 0, sizeof(BindingCacheEntry) * cache->length);
}

static RIR_INLINE SEXP cachedGetBindingCell(Immediate cacheIdx,
                                            BindingCache* cache) {
    SLOWASSERT(cacheIdx < cache->length);
    return cache->entry[cacheIdx];
}

static RIR_INLINE void
cachedSetBindingCell(Immediate cacheIdx, BindingCache* cache, R_varloc_t loc) {
    SLOWASSERT(cacheIdx < cache->length);
    cache->entry[cacheIdx] = loc.cell;
}

static void rirDefineVarWrapper(SEXP symbol, SEXP value, SEXP rho) {
    if (rho == R_EmptyEnv)
        return;

    if (OBJECT(rho) || HASHTAB(rho) != R_NilValue) {
        PROTECT(value);
        INCREMENT_NAMED(value);
        Rf_defineVar(symbol, value, rho);
        UNPROTECT(1);
        return;
    }

    if (rho == R_BaseNamespace || rho == R_BaseEnv) {
        if (SYMVALUE(symbol) == value)
            return;
        INCREMENT_NAMED(value);
        Rf_defineVar(symbol, value, rho);
        return;
    }

    if (IS_SPECIAL_SYMBOL(symbol))
        UNSET_NO_SPECIAL_SYMBOLS(rho);

    /* First check for an existing binding */
    SEXP frame = FRAME(rho);
    while (frame != R_NilValue) {
        if (TAG(frame) == symbol) {
            SEXP cur = CAR(frame);
            // No named increment if we are storing the same value again
            if (cur == value)
                return;
            INCREMENT_NAMED(value);
            // we don't handle these
            if (BINDING_IS_LOCKED(frame) || IS_ACTIVE_BINDING(frame)) {
                PROTECT(value);
                Rf_defineVar(symbol, value, rho);
                UNPROTECT(1);
                return;
            }
            SETCAR(frame, value);
            SET_MISSING(frame, 0); /* Over-ride */
            return;
        }
        frame = CDR(frame);
    }

    if (FRAME_IS_LOCKED(rho))
        Rf_error("cannot add bindings to a locked environment");
    PROTECT(value);
    INCREMENT_NAMED(value);
    SET_FRAME(rho, Rf_cons(value, FRAME(rho)));
    UNPROTECT(1);
    SET_TAG(FRAME(rho), symbol);
}

static RIR_INLINE SEXP getCellFromCache(SEXP env, Immediate poolIdx,
                                        Immediate cacheIdx,
                                        InterpreterInstance* ctx,
                                        BindingCache* cache) {
    if (env != R_BaseEnv && env != R_BaseNamespace) {
        SEXP cell = cachedGetBindingCell(cacheIdx, cache);
        if (!cell) {
            SEXP sym = cp_pool_at(ctx, poolIdx);
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            R_varloc_t loc = R_findVarLocInFrame(env, sym);
            if (!R_VARLOC_IS_NULL(loc)) {
                cachedSetBindingCell(cacheIdx, cache, loc);
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
                                    InterpreterInstance* ctx,
                                    BindingCache* cache) {
    SEXP cell = getCellFromCache(env, poolIdx, cacheIdx, ctx, cache);
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
                                    BindingCache* cache,
                                    bool keepMissing = false) {
    SEXP loc = getCellFromCache(env, poolIdx, cacheIdx, ctx, cache);
    if (loc && !BINDING_IS_LOCKED(loc) && !IS_ACTIVE_BINDING(loc)) {
        SEXP cur = CAR(loc);
        if (cur == val) {
            ENSURE_NAMED(cur);
            return;
        }
        INCREMENT_NAMED(val);
        SETCAR(loc, val);
        if (!keepMissing && MISSING(loc)) {
            SET_MISSING(loc, 0);
        }
        return;
    }

    SEXP sym = cp_pool_at(ctx, poolIdx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    rirDefineVarWrapper(sym, val, env);
}

static inline void rirSetVarWrapper(SEXP sym, SEXP val, SEXP env) {
    if (env != R_BaseEnv && env != R_BaseNamespace) {
        R_varloc_t loc = R_findVarLocInFrame(env, sym);
        if (!R_VARLOC_IS_NULL(loc) && !BINDING_IS_LOCKED(loc.cell) &&
            !IS_ACTIVE_BINDING(loc.cell)) {
            SEXP cur = CAR(loc.cell);
            // Some primitives clear the name expecting a store to happen later
            // Thus, the increment must be done always. See subassign.c:1672 for
            // instance.
            INCREMENT_NAMED(val);
            if (cur == val)
                return;
            SETCAR(loc.cell, val);
            SET_MISSING(loc.cell, 0);
            return;
        }
    }
    PROTECT(val);
    INCREMENT_NAMED(val);
    Rf_setVar(sym, val, ENCLOS(env));
    UNPROTECT(1);
}

#endif
} // namespace rir
#endif
