#ifndef binding_cache_h
#define binding_cache_h

#include "instance.h"

namespace rir {

#define ACTIVE_BINDING_MASK (1 << 15)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_LOCK_MASK (1 << 14)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)
#define MAX_ON_STACK_CHECK 63

static R_INLINE SEXP GET_BINDING_CELL(SEXP symbol, SEXP rho) {
    if (rho == R_BaseEnv || rho == R_BaseNamespace)
        return R_NilValue;
    else {
        R_varloc_t loc = R_findVarLocInFrame(rho, symbol);
        return (!R_VARLOC_IS_NULL(loc)) ? loc.cell : R_NilValue;
    }
}

static R_INLINE SEXP BINDING_VALUE(SEXP loc) {
    if (loc != R_NilValue && !IS_ACTIVE_BINDING(loc))
        return CAR(loc);
    else
        return R_UnboundValue;
}

static R_INLINE Rboolean SET_BINDING_VALUE(SEXP loc, SEXP value) {
    /* This depends on the current implementation of bindings */
    if (loc != R_NilValue && !BINDING_IS_LOCKED(loc) &&
        !IS_ACTIVE_BINDING(loc)) {
        if (CAR(loc) != value) {
            SETCAR(loc, value);
            if (MISSING(loc))
                SET_MISSING(loc, 0);
        }
        return TRUE;
    } else
        return FALSE;
}

// Assumes val is popped off stack, since it could be converted into an SEXP
static RIR_INLINE void setVar(SEXP sym, R_bcstack_t* val, SEXP env,
                              bool super) {
    PROTECT(sym);
    SEXP valSexp = stackObjToSexp(val); // Value should be popped off stack
    UNPROTECT(1);
    INCREMENT_NAMED(valSexp);
    PROTECT(valSexp);
    if (super) {
        Rf_setVar(sym, valSexp, env);
    } else {
        Rf_defineVar(sym, valSexp, env);
    }
    UNPROTECT(1);
}

#define UNSOUND_OPTS
//#define CACHE_ON_STACK
#ifdef CACHE_ON_STACK
#define ON_STACK_CACHE_MAX 128
#define STACK_CACHE_MASK (ON_STACK_CACHE_MAX - 1)

static RIR_INLINE SEXP smallCacheGetCell(Immediate idx,
                                         InterpreterInstance* ctx,
                                         R_bcstack_t* cacheStart) {
    R_bcstack_t* index = cacheStart + idx;
    return stackSexp(index);
}

static RIR_INLINE SEXP smallCacheGetVar(Immediate idx, InterpreterInstance* ctx,
                                        R_bcstack_t* cacheStart) {
    SEXP value = CAR(smallCacheGetCell(idx, ctx, cacheStart));
    int type = TYPEOF(value);
    switch (type) {
    case REALSXP:
    case INTSXP:
    case LGLSXP:
        return value;
        break;
    }

    if (value != R_NilValue && !IS_ACTIVE_BINDING(value)) {
        if (type != SYMSXP) {
            if (type != PROMSXP) {
                // TODO if Promise
                return value;
            }
        }
    }
    return NULL;
}

static RIR_INLINE SEXP cacheGetCell(Immediate idx, InterpreterInstance* ctx,
                                    R_bcstack_t* cacheStart) {
    R_bcstack_t* index = cacheStart + (idx & STACK_CACHE_MASK);
    return stackSexp(index);
}

static RIR_INLINE void cacheSetCell(R_bcstack_t* cacheStart, Immediate idx,
                                    SEXP cacheCell) {
    ostackSetSexpCache(cacheStart, idx & STACK_CACHE_MASK, cacheCell);
}

static RIR_INLINE SEXP cacheGetVar(SEXP env, Immediate idx,
                                   InterpreterInstance* ctx, bool smallCache,
                                   R_bcstack_t* cacheStart) {
    SEXP value, cell;
    /*if (smallCache)
        cell = smallCacheGetVar(idx, ctx, cacheStart);
    else {*/
    cell = cacheGetCell(idx, ctx, cacheStart);
    if (cell == nullptr) {
        SEXP sym = cp_pool_at(ctx, idx);
        cell = GET_BINDING_CELL(sym, env);
        if (cell != R_NilValue)
            cacheSetCell(cacheStart, idx, cell);
        // if (!(TAG(cell) == sym && CAR(cell) != R_UnboundValue)){
        /*if (CAR(cell) == R_UnboundValue) {
            value = GET_BINDING_CELL(cp_pool_at(ctx, idx), env);
            if (value != R_NilValue)
                ostackSetSexpCache(cacheStart, idx, value);
            else if (cell != R_NilValue && CAR(cell) == R_UnboundValue)
                ostackSetSexpCache(cacheStart, idx, R_NilValue);
        }*/
    }
    //}
    if (cell != nullptr) {
        value = BINDING_VALUE(cell);
        if (value && value != R_UnboundValue) {
            return value;
        }
    }
    SEXP sym = cp_pool_at(ctx, idx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    return Rf_findVar(sym, env);
}

// Assumes val is popped off stack, since it could be converted into an SEXP
static void cachedSetVar(R_bcstack_t* value, SEXP env, Immediate idx,
                         InterpreterInstance* ctx, bool smallCache,
                         R_bcstack_t* cacheStart, R_bcstack_t* localsStart,
                         bool keepMissing = false) {
    SEXP cacheCell;
    /*if (smallCache)
        cacheCell = smallCacheGetCell(idx, ctx, cacheStart);
    else {*/
    cacheCell = cacheGetCell(idx, ctx, cacheStart);
    //}

#ifdef USE_TYPED_STACK
    if (cacheCell) {
        if (value->tag && !BINDING_IS_LOCKED(cacheCell)) {
            SEXP cachedValue = CAR(cacheCell);
            if (NOT_SHARED(cachedValue) &&
                IS_SIMPLE_SCALAR(cachedValue, value->tag)) {
                if (!(R_BCNodeStackTop - localsStart > MAX_ON_STACK_CHECK ||
                      FIND_ON_STACK(cachedValue, localsStart, TRUE))) {
                    setInPlace(cachedValue, value);
                    if (!keepMissing && MISSING(cachedValue))
                        SET_MISSING(cachedValue, 0);
                    return;
                }
            }
        }
    }
#endif
    SEXP valSexp = stackObjToSexp(value);
    INCREMENT_NAMED(valSexp);
    if ((!cacheCell) || !SET_BINDING_VALUE(cacheCell, valSexp)) {
        SEXP symbol = cp_pool_at(ctx, idx);
        PROTECT(valSexp);
        defineVar(symbol, valSexp, env);
        UNPROTECT(1);
    }
}

static RIR_INLINE void initializeStackCache(R_bcstack_t* cacheStart,
                                            unsigned size) {
    memset(cacheStart, 0, sizeof(R_bcstack_t) * size);
    /*unsigned i = 0;
    while (i < size) {
        ostackSetSexpCache(cacheStart, i, R_NilValue);
        i++;
    }*/
}
#else
#define BINDING_CACHE_SIZE 5
typedef struct {
    SEXP loc;
    Immediate idx;
} BindingCache;

static RIR_INLINE SEXP cachedGetBindingCell(SEXP env, Immediate idx,
                                            InterpreterInstance* ctx,
                                            BindingCache* bindingCache) {
    if (env == R_BaseEnv || env == R_BaseNamespace)
        return NULL;

    Immediate cidx = idx % BINDING_CACHE_SIZE;
    if (bindingCache[cidx].idx == idx) {
        return bindingCache[cidx].loc;
    }

    SEXP sym = cp_pool_at(ctx, idx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    R_varloc_t loc = R_findVarLocInFrame(env, sym);
    if (!R_VARLOC_IS_NULL(loc)) {
        bindingCache[cidx].loc = loc.cell;
        bindingCache[cidx].idx = idx;
        return loc.cell;
    }
    return NULL;
}

static RIR_INLINE SEXP cacheGetVar(SEXP env, Immediate idx,
                                   InterpreterInstance* ctx,
                                   BindingCache* bindingCache) {
    SEXP loc = cachedGetBindingCell(env, idx, ctx, bindingCache);
    if (loc) {
        SEXP res = CAR(loc);
        if (res != R_UnboundValue)
            return res;
    }
    SEXP sym = cp_pool_at(ctx, idx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    return Rf_findVar(sym, env);
}

// Assumes val is popped off stack, since it could be converted into an SEXP
static void cachedSetVar(R_bcstack_t* val, SEXP env, Immediate idx,
                         InterpreterInstance* ctx, BindingCache* bindingCache,
                         bool keepMissing = false) {
    SEXP loc = cachedGetBindingCell(env, idx, ctx, bindingCache);
    if (loc && !BINDING_IS_LOCKED(loc) && !IS_ACTIVE_BINDING(loc)) {
        SEXP cur = CAR(loc);
        if (val->tag == STACK_OBJ_SEXP && val->u.sxpval == cur) {
            return;
        } else if (val->tag != STACK_OBJ_SEXP && NOT_SHARED(cur) &&
                   IS_SIMPLE_SCALAR(cur, val->tag)) {
            return setInPlace(cur, val);
        }
        SEXP valSexp = stackObjToSexp(val); // Value should be popped off stack
        INCREMENT_NAMED(valSexp);
        SETCAR(loc, valSexp);
        if (!keepMissing && MISSING(loc))
            SET_MISSING(loc, 0);
        return;
    }

    SEXP sym = cp_pool_at(ctx, idx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    setVar(sym, val, env, false);
}
#endif
} // namespace rir
#endif