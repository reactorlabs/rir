#include "jit_cache.h"

#include "R/Symbols.h"
#include "R/r.h"
#include "interpreter/interp_incl.h"
#include "runtime/DispatchTable.h"

/*
 * This is the JIT cache logic copied from GNUR
 *
 * TODO: write our own...
 *
 */
typedef unsigned long R_exprhash_t;

static R_exprhash_t hash(unsigned char* str, int n, R_exprhash_t hash) {
    // djb2 from http://www.cse.yorku.ca/~oz/hash.html
    // (modified for n-byte lengths)

    int i;

    for (i = 0; i < n; i++)
        hash = ((hash << 5) + hash) + str[i]; /* hash * 33 + c */

    return hash;
}

#define HASH(x, h) hash((unsigned char*)&x, sizeof(x), h)

static R_exprhash_t hashexpr1(SEXP e, R_exprhash_t h) {
#define SKIP_NONSCALAR                                                         \
    if (len != 1)                                                              \
    break /* non-scalars hashed by address */
    int len = Rf_length(e);
    int type = TYPEOF(e);
    h = HASH(type, h);
    h = HASH(len, h);

    switch (type) {
    case LANGSXP:
    case LISTSXP:
        /**** safer to only follow while CDR is LANGSXP/LISTSXP */
        for (; e != R_NilValue; e = CDR(e))
            h = hashexpr1(CAR(e), h);
        return h;
    case LGLSXP:
        SKIP_NONSCALAR;
        for (int i = 0; i < len; i++) {
            int ival = LOGICAL(e)[i];
            h = HASH(ival, h);
        }
        return h;
    case INTSXP:
        SKIP_NONSCALAR;
        for (int i = 0; i < len; i++) {
            int ival = INTEGER(e)[i];
            h = HASH(ival, h);
        }
        return h;
    case REALSXP:
        SKIP_NONSCALAR;
        for (int i = 0; i < len; i++) {
            double dval = REAL(e)[i];
            h = HASH(dval, h);
        }
        return h;
    case STRSXP:
        SKIP_NONSCALAR;
        for (int i = 0; i < len; i++) {
            SEXP cval = STRING_ELT(e, i);
            h = hash((unsigned char*)CHAR(cval), LENGTH(cval), h);
        }
        return h;
    }

    return HASH(e, h);
#undef SKIP_NONSCALAR
}

static R_exprhash_t hashsrcref(SEXP e, R_exprhash_t h) {
    if (TYPEOF(e) == INTSXP && LENGTH(e) >= 6) {
        for (int i = 0; i < 6; i++) {
            int ival = INTEGER(e)[i];
            h = HASH(ival, h);
        }
        /* FIXME: update this when deep-comparison of srcref is available */
        SEXP srcfile = getAttrib(e, rir::symbol::srcfile);
        h = HASH(srcfile, h);
    }
    return h;
}
#undef HASH

static R_exprhash_t hashexpr(SEXP e) { return hashexpr1(e, 5381); }

static R_exprhash_t hashfun(SEXP f) {
    R_exprhash_t h = hashexpr(rir::rirDecompile(BODY(f)));
    if (getAttrib(BODY(f), rir::symbol::srcref) == R_NilValue) {
        h = hashsrcref(getAttrib(f, rir::symbol::srcref), h);
    }
    return h;
}

#define JIT_CACHE_SIZE 1024
static SEXP JIT_cache = NULL;
static R_exprhash_t JIT_cache_hashes[JIT_CACHE_SIZE];

static bool initJitCache = []() {
    R_PreserveObject(JIT_cache = allocVector(VECSXP, JIT_CACHE_SIZE));
    return true;
}();

static R_INLINE SEXP jit_cache_code(SEXP entry) { return CAR(entry); }

static R_INLINE SEXP jit_cache_env(SEXP entry) { return CDR(entry); }

static R_INLINE SEXP jit_cache_srcref(SEXP entry) { return TAG(entry); }

static R_INLINE SEXP jit_cache_expr(SEXP entry) {
    return rir::rirDecompile(jit_cache_code(entry));
}

static R_INLINE SEXP get_jit_cache_entry(R_exprhash_t hash) {
    int hashidx = hash % JIT_CACHE_SIZE;
    if (JIT_cache_hashes[hashidx] == hash) {
        SEXP entry = VECTOR_ELT(JIT_cache, hashidx);
        if (TYPEOF(jit_cache_code(entry)) == EXTERNALSXP)
            return entry;
        else
            /* function has been de-compiled; clear the cache entry */
            SET_VECTOR_ELT(JIT_cache, hashidx, R_NilValue);
    }
    return R_NilValue;
}

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
}

#define IS_USER_DATABASE(rho)                                                  \
    (OBJECT((rho)) && inherits((rho), "UserDefinedDatabase"))
#define IS_STANDARD_UNHASHED_FRAME(e)                                          \
    (!IS_USER_DATABASE(e) && HASHTAB(e) == R_NilValue)
#define IS_STANDARD_HASHED_FRAME(e)                                            \
    (!IS_USER_DATABASE(e) && HASHTAB(e) != R_NilValue)
static R_INLINE void cmpenv_enter_frame(SEXP frame, SEXP newenv) {
    for (; frame != R_NilValue; frame = CDR(frame))
        defineVar(TAG(frame), R_NilValue, newenv);
}
static R_INLINE SEXP make_cached_cmpenv(SEXP fun) {
    SEXP frmls = FORMALS(fun);
    SEXP cmpenv = CLOENV(fun);
    SEXP top = topenv(R_NilValue, cmpenv);
    if (cmpenv == top && frmls == R_NilValue)
        return cmpenv;
    else {
        SEXP newenv = PROTECT(Rf_NewEnvironment(R_NilValue, R_NilValue, top));
        for (; frmls != R_NilValue; frmls = CDR(frmls))
            defineVar(TAG(frmls), R_NilValue, newenv);
        for (SEXP env = cmpenv; env != top; env = CDR(env)) {
            if (IS_STANDARD_UNHASHED_FRAME(env))
                cmpenv_enter_frame(FRAME(env), newenv);
            else if (IS_STANDARD_HASHED_FRAME(env)) {
                SEXP h = HASHTAB(env);
                int n = Rf_length(h);
                for (int i = 0; i < n; i++)
                    cmpenv_enter_frame(VECTOR_ELT(h, i), newenv);
            } else {
                UNPROTECT(1); /* newenv */
                return top;
            }
            /* topenv is a safe conservative answer; if a closure
               defines anything, its environment will not match, and
               it will never be compiled */
            /* FIXME: would it be safe to simply ignore elements of
               of these environments? */
        }
        UNPROTECT(1); /* newenv */
        return newenv;
    }
}

static R_INLINE void set_jit_cache_entry(R_exprhash_t hash, SEXP val) {
    int hashidx = hash % JIT_CACHE_SIZE;

    PROTECT(val);
    SEXP entry = CONS_NR(BODY(val), make_cached_cmpenv(val));
    SET_VECTOR_ELT(JIT_cache, hashidx, entry);
    SET_TAG(entry, getAttrib(val, rir::symbol::srcref));
    UNPROTECT(1); /* val */
    JIT_cache_hashes[hashidx] = hash;
}

static R_INLINE SEXP cmpenv_topenv(SEXP cmpenv) {
    return topenv(R_NilValue, cmpenv);
}

static R_INLINE Rboolean cmpenv_exists_local(SEXP sym, SEXP cmpenv, SEXP top) {
    if (cmpenv != top)
        for (SEXP frame = FRAME(cmpenv); frame != R_NilValue;
             frame = CDR(frame))
            if (TAG(frame) == sym)
                return TRUE;
    return FALSE;
}

static R_INLINE Rboolean jit_env_match(SEXP cmpenv, SEXP fun) {
    /* Can code compiled for environment cmpenv be used as compiled
       code for environment env?  These tests rely on the assumption
       that compilation is only affected by what variables are bound,
       not their values. So as long as both cmpenv and env have the
       same top level environment and all local bindings present in
       the formals and environment of fun are also present in cmpenv
       the code for cmpenv can be reused, though it might be less
       efficient if a binding in cmpenv prevents an optimization that
       would be possible in env. */

    SEXP env = CLOENV(fun);
    SEXP top = topenv(R_NilValue, env);

    if (top == cmpenv_topenv(cmpenv)) {
        for (SEXP frmls = FORMALS(fun); frmls != R_NilValue; frmls = CDR(frmls))
            if (!cmpenv_exists_local(TAG(frmls), cmpenv, top))
                return FALSE;
        for (; env != top; env = ENCLOS(env)) {
            if (IS_STANDARD_UNHASHED_FRAME(env)) {
                /* To keep things simple, for a match this code
                   requires that the local frames be standard unhashed
                   frames. */
                for (SEXP frame = FRAME(env); frame != R_NilValue;
                     frame = CDR(frame))
                    if (!cmpenv_exists_local(TAG(frame), cmpenv, top))
                        return FALSE;
            } else
                return FALSE;
        }
        return TRUE;
    } else
        return FALSE;
}

static R_INLINE Rboolean jit_expr_match(SEXP expr, SEXP body) {
    /*** is 16 right here??? does this need to be faster??? */
    return R_compute_identical(expr, body, 16);
}

static R_INLINE Rboolean jit_srcref_match(SEXP cmpsrcref, SEXP srcref) {
    return R_compute_identical(cmpsrcref, srcref, 0);
}

namespace rir {

bool JitCache::enabled = true;

SEXP JitCache::getEntryOrCreate(SEXP closure, std::function<SEXP()> create) {
    if (!enabled)
        return create();

    R_exprhash_t hash = hashfun(closure);
    SEXP entry = get_jit_cache_entry(hash);

    if (entry != R_NilValue) {
        if (jit_env_match(jit_cache_env(entry), closure)) {
            auto dt = DispatchTable::check(BODY(closure));
            if (dt && dt->baseline()->deoptCount() == 0) {
                if (jit_expr_match(jit_cache_expr(entry),
                                   rirDecompile(BODY(closure)))) {
                    /* if function body has a srcref, all srcrefs compiled
                       in that function only depend on the body srcref;
                       but, otherwise the srcrefs compiled in are taken
                       from the function (op) */
                    if (getAttrib(BODY(closure), symbol::srcref) != R_NilValue ||
                        jit_srcref_match(jit_cache_srcref(entry),
                                         getAttrib(closure, symbol::srcref))) {
                        SET_BODY(closure, jit_cache_code(entry));
                        return closure;
                    }
                }
            }
        }
    }

    auto res = create();
    if (isValidClosureSEXP(res))
        set_jit_cache_entry(hash, res);
    return res;
}

} // namespace rir
