#include "interp.h"
#include "R/Funtab.h"
#include "R/Printing.h"
#include "R/Protect.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "cache.h"
#include "compiler/compiler.h"
#include "compiler/osr.h"
#include "compiler/parameter.h"
#include "compiler/pir/continuation_context.h"
#include "recording_hooks.h"
#include "runtime/Deoptimization.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "runtime/TypeFeedback_inl.h"
#include "safe_force.h"
#include "utils/Pool.h"
#include "utils/measuring.h"

#include <assert.h>
#include <deque>
#include <libintl.h>
#include <set>
#include <unordered_set>

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
}

namespace rir {

static SEXP evalRirCode(Code* c, SEXP env, const CallContext* callContext,
                        Opcode* initialPc = nullptr,
                        BindingCache* cache = nullptr);

// #define PRINT_INTERP
// #define PRINT_STACK_SIZE 10
#ifdef PRINT_INTERP
static void printInterp(Opcode* pc, Code* c) {
#ifdef PRINT_STACK_SIZE
#define INTSEQSXP 9999
    // Prevent printing instructions (and recursing) while printing stack
    static bool printingStackSize = false;
    if (printingStackSize)
        return;

    // Print stack
    printingStackSize = true;
    std::cout << "#; Stack:\n";
    for (int i = 0;; i++) {
        auto typ = ostack_cell_at(i)->tag;
        SEXP sexp = ostack_at(i);
        if (sexp == nullptr || ostack_length() - i == 0)
            break;
        else if (i == PRINT_STACK_SIZE) {
            std::cout << "    ...\n";
            break;
        }
        if (typ == 0) {
            std::cout << "    >>> " << Print::dumpSexp(sexp) << " <<<\n";
        } else if (typ == INTSXP || typ == LGLSXP) {
            std::cout << "    int/lgl >>> " << ostack_cell_at(i)->u.ival
                      << " <<<\n";
        } else if (typ == REALSXP) {
            std::cout << "    real >>> " << ostack_cell_at(i)->u.dval
                      << " <<<\n";
        } else if (typ == INTSEQSXP) {
            std::cout << "    intseq >>> " << Print::dumpSexp(sexp) << " <<<\n";
        }
    }
    printingStackSize = false;
#endif
    // Print source
    unsigned sidx = c->getSrcIdxAt(pc, true);
    if (sidx != 0) {
        SEXP src = src_pool_at(sidx);
        std::cout << "#; " << Print::dumpSexp(src) << "\n";
    }
    // Print bc
    BC bc = BC::decode(pc, c);
    std::cout << "#";
    bc.print(std::cout);
}

static void printLastop() { std::cout << "> lastop\n"; }
#endif

static SEXP getSrcAt(Code* c, Opcode* pc) {
    unsigned sidx = c->getSrcIdxAt(pc, true);
    if (sidx == 0)
        return src_pool_at(c->src);
    return src_pool_at(sidx);
}

static SEXP getSrcForCall(Code* c, Opcode* pc) {
    unsigned sidx = c->getSrcIdxAt(pc, false);
    return src_pool_at(sidx);
}

#define PC_BOUNDSCHECK(pc, c)                                                  \
    SLOWASSERT((pc) >= (c)->code() && (pc) < (c)->endCode());

#ifdef THREADED_CODE
#define BEGIN_MACHINE NEXT();
#define INSTRUCTION(name) op_##name:
#ifdef PRINT_INTERP
#define NEXT()                                                                 \
    (__extension__({                                                           \
        printInterp(pc, c);                                                    \
        goto* opAddr[static_cast<uint8_t>(advanceOpcode())];                   \
    }))
#define LASTOP                                                                 \
    { printLastop(); }
#else
#define NEXT()                                                                 \
    (__extension__({ goto* opAddr[static_cast<uint8_t>(advanceOpcode())]; }))
#define LASTOP                                                                 \
    {}
#endif
#else
#define BEGIN_MACHINE                                                          \
    loop:                                                                      \
    switch (advanceOpcode())
#define INSTRUCTION(name) case Opcode::name:
#define NEXT() goto loop
#define LASTOP                                                                 \
    default:                                                                   \
        assert(false && "wrong or unimplemented opcode")
#endif

// bytecode accesses
#define advanceOpcode() (*(pc++))
#define readImmediate() (*(Immediate*)pc)
#define readSignedImmediate() (*(SignedImmediate*)pc)
#define readJumpOffset() (*(JumpOffset*)(pc))
#define advanceImmediate() pc += sizeof(Immediate)
#define advanceImmediateN(n) pc += n * sizeof(Immediate)
#define advanceJump() pc += sizeof(JumpOffset)

#define readConst(idx) (cp_pool_at(idx))

static RCNTXT* deoptimizationStartedAt = nullptr;

static bool isDeoptimizing() {
    if (!deoptimizationStartedAt)
        return false;
    RCNTXT* cur = (RCNTXT*)R_GlobalContext;
    while (cur) {
        if (cur == deoptimizationStartedAt)
            return true;
        cur = cur->nextcontext;
    }
    deoptimizationStartedAt = nullptr;
    return false;
}
static void startDeoptimizing() {
    deoptimizationStartedAt = (RCNTXT*)R_GlobalContext;
}
static void endDeoptimizing() { deoptimizationStartedAt = nullptr; }

void initClosureContext(SEXP ast, RCNTXT* cntxt, SEXP rho, SEXP sysparent,
                        SEXP arglist, SEXP op) {
    /*  If we have a generic function we need to use the sysparent of
       the generic as the sysparent of the method because the method
       is a straight substitution of the generic.  */

    auto global = (RCNTXT*)R_GlobalContext;
    if (global->callflag == CTXT_GENERIC)
        Rf_begincontext(cntxt, CTXT_RETURN, ast, rho, global->sysparent,
                        arglist, op);
    else
        Rf_begincontext(cntxt, CTXT_RETURN, ast, rho, sysparent, arglist, op);
}

static void endClosureContext(RCNTXT* cntxt, SEXP result) {
    cntxt->returnValue = result;
    Rf_endcontext(cntxt);
}

static SEXP createPromise(Code* code, SEXP env) {
    SEXP p = Rf_mkPROMISE(code->container(), env);
    return p;
}

typedef struct RPRSTACK {
    SEXP promise;
    struct RPRSTACK* next;
} RPRSTACK;
extern "C" struct RPRSTACK* R_PendingPromises;

SEXP evaluatePromise(SEXP e, Opcode* pc, bool delayNamed) {
    // if already evaluated, return the value
    if (PRVALUE(e) && PRVALUE(e) != R_UnboundValue) {
        e = PRVALUE(e);
        assert(TYPEOF(e) != PROMSXP);
        return e;
    } else if (TYPEOF(PRCODE(e)) != EXTERNALSXP) {
        assert(!pc);
        return forcePromise(e);
    } else {
        SEXP val;
        if (PRSEEN(e)) {
            if (PRSEEN(e) == 1)
                Rf_errorcall(
                    R_NilValue,
                    "promise already under evaluation: recursive default "
                    "argument reference or earlier problems?");
            else {
                /* set PRSEEN to 1 to avoid infinite recursion */
                SET_PRSEEN(e, 1);
                Rf_warningcall(R_NilValue,
                               "restarting interrupted promise evaluation");
            }
        }
        SET_PRSEEN(e, 1);

        RPRSTACK prstack;
        prstack.promise = e;
        prstack.next = R_PendingPromises;
        R_PendingPromises = &prstack;
        val = evalRirCode(Code::unpack(PRCODE(e)), PRENV(e), nullptr, pc,
                          nullptr);
        R_PendingPromises = prstack.next;
        SET_PRSEEN(e, 0);
        SET_PRVALUE(e, val);
        if (!delayNamed)
            ENSURE_NAMEDMAX(val);
        SET_PRENV(e, R_NilValue);

        assert(TYPEOF(val) != PROMSXP && "promise returned promise");
        return val;
    }
}

SEXP rirForcePromise(SEXP e) { return evaluatePromise(e); }

void jit(SEXP cls, SEXP name) {
    assert(TYPEOF(cls) == CLOSXP);
    if (TYPEOF(BODY(cls)) == EXTERNALSXP)
        return;
    SEXP cmp = globalContext()->closureCompiler(cls, name);
    SET_BODY(cls, BODY(cmp));
}

static void closureDebug(SEXP call, SEXP op, SEXP rho, SEXP newrho,
                         RCNTXT* cntxt) {
    // TODO!!!
}

static void endClosureDebug(SEXP call, SEXP op, SEXP rho) {
    // TODO!!!
}

/** Given argument code offsets, creates the arglist from their promises.
 */
static void __listAppend(SEXP* front, SEXP* last, SEXP value, SEXP name) {
    SLOWASSERT(TYPEOF(*front) == LISTSXP || TYPEOF(*front) == NILSXP);
    SLOWASSERT(TYPEOF(*last) == LISTSXP || TYPEOF(*last) == NILSXP);

    SEXP app = CONS_NR(value, R_NilValue);

    SET_TAG(app, name);

    if (*front == R_NilValue) {
        *front = app;
        PROTECT(*front);
    }

    if (*last != R_NilValue)
        SETCDR(*last, app);
    *last = app;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

SEXP materialize(SEXP wrapper) {
    SEXP res = nullptr;
    RCNTXT* cur = (RCNTXT*)R_GlobalContext;
    if (auto lazyArgs = LazyArglist::check(wrapper)) {
        res = lazyArgs->createArglist();
        // Fixup the contexts chain
        while (cur) {
            if (cur->promargs == wrapper)
                cur->promargs = res;
            cur = cur->nextcontext;
        }
    } else if (auto lazyEnv = LazyEnvironment::check(wrapper)) {
        assert(!lazyEnv->materialized());

        PROTECT(wrapper);
        SEXP arglist = R_NilValue;
        auto names = lazyEnv->names;
        for (size_t i = 0; i < lazyEnv->nargs; ++i) {
            SEXP val = lazyEnv->getArg(i);
            if (val == R_UnboundValue)
                continue;
            SEXP name = cp_pool_at(names[i]);
            if (TYPEOF(name) == LISTSXP)
                name = CAR(name);
            // cons protects its args if needed
            arglist = CONS_NR(val, arglist);
            SET_TAG(arglist, name);
            if (val == R_MissingArg)
                SET_MISSING(arglist, 1);
            else if (lazyEnv->missing[i])
                SET_MISSING(arglist, 2);
        }
        auto parent = lazyEnv->getParent();
        res = Rf_NewEnvironment(R_NilValue, arglist, parent);
        lazyEnv->materialized(res);
        // Make sure wrapper is not collected by the gc (we may still use it to
        // access the materialized env)
        Rf_setAttrib(res, symbol::delayedEnv, wrapper);
        lazyEnv->clear();
        // Fixup the contexts chain
        while (cur) {
            if (cur->cloenv == wrapper)
                cur->cloenv = res;
            if (cur->sysparent == wrapper)
                cur->sysparent = res;
            cur = cur->nextcontext;
        }
        if (LazyEnvironment::check(parent)) {
            parent = materialize(parent);
            SET_ENCLOS(res, parent);
        }

        UNPROTECT(1);
    }
    assert(res);
    return res;
}

SEXP materializeCallerEnv(CallContext& callCtx) {
    if (auto le = LazyEnvironment::check(callCtx.callerEnv)) {
        if (le->materialized())
            callCtx.callerEnv = le->materialized();
        else
            callCtx.callerEnv = materialize(callCtx.callerEnv);
    }
    SLOWASSERT(callCtx.callerEnv == symbol::delayedEnv ||
               TYPEOF(callCtx.callerEnv) == ENVSXP ||
               callCtx.callerEnv == R_NilValue);
    return callCtx.callerEnv;
}

struct ArglistView {
  private:
    SEXP getArgFromStore(size_t i) {
        return onStack ? ostack_at_cell(stackArgs + i) : heapArgs[i];
    }

  public:
    ArglistView(ArglistOrder::CallId id, size_t length,
                const R_bcstack_t* stackArgs, SEXP* heapArgs,
                const Immediate* names, ArglistOrder* reordering)
        : id(id), onStack(stackArgs), stackArgs(stackArgs), heapArgs(heapArgs),
          names(names), reordering(reordering) {}

    bool isStaticallyMatchedDots(size_t i) {
        return reordering->isStaticallyMatchedDots(reordering->index(id, i));
    }

    std::pair<SEXP, SEXP> getArgAndName(size_t i, bool reorder, SEXP astName) {
        if (reorder) {
            if (i >= reordering->originalArglistLength(id))
                return {R_MissingArg, R_NilValue};
            i = ArglistOrder::decodeArg(reordering->index(id, i));
        }
        auto arg = getArgFromStore(i);

        // Sometimes we lose the name and have to restore it from the ast
        // TODO: why? and why do we even keep the names separately then?
        auto name = names ? cp_pool_at(names[i]) : astName;
        return {arg, name};
    }

    ArglistOrder::CallId id;
    bool onStack;
    const R_bcstack_t* stackArgs;
    SEXP* heapArgs;
    const Immediate* names;
    ArglistOrder* reordering;
};

SEXP createLegacyArglist(ArglistOrder::CallId id, size_t length,
                         const R_bcstack_t* stackArgs, SEXP* heapArgs,
                         const Immediate* names, SEXP ast,
                         ArglistOrder* reordering, bool eagerCallee,
                         bool recreateOriginalPromargs) {
    assert((stackArgs && !heapArgs) || (!stackArgs && heapArgs));
    assert(id == ArglistOrder::NOT_REORDERED || reordering);
    assert((!recreateOriginalPromargs || ast) &&
           "need ast to recreate promargs");
    SEXP result = R_NilValue;
    SEXP pos = result;

    bool reorder =
        recreateOriginalPromargs && id != ArglistOrder::NOT_REORDERED;

    ArglistView args(id, length, stackArgs, heapArgs, names, reordering);

    size_t actualLength = (reorder && id != ArglistOrder::NOT_REORDERED)
                              ? reordering->originalArglistLength(id)
                              : length;
    auto a = ast;
    for (size_t i = 0; i < length; ++i) {
        a = CDR(a);
        auto getArg = args.getArgAndName(i, reorder, TAG(a));
        auto arg = getArg.first;
        auto name = getArg.second;

        // This can happen if context dispatch padded the call with "synthetic"
        // missings to be able to call a version which expects more args
        if (recreateOriginalPromargs && arg == R_MissingArg &&
            i >= actualLength)
            continue;

        // This can happen if we materialize the lazy arglist of a statically
        // argmatched call, where dots gets pre-created by the caller.
        if (recreateOriginalPromargs && TYPEOF(arg) == DOTSXP && reorder &&
            args.isStaticallyMatchedDots(i)) {
            i--;
            while (arg != R_NilValue) {
                i++;
                auto v = CAR(arg);
                if (eagerCallee && TYPEOF(v) == PROMSXP)
                    v = evaluatePromise(v);
                if (TYPEOF(v) != PROMSXP)
                    ENSURE_NAMED(v);
                assert(TYPEOF(v) != DOTSXP);
                __listAppend(&result, &pos, v, TAG(arg));
                arg = CDR(arg);
            }
            continue;
        }

        if (eagerCallee && TYPEOF(arg) == PROMSXP)
            arg = evaluatePromise(arg);
        SLOWASSERT(arg != symbol::delayedEnv);

        // This is to ensure we pass named arguments to GNU-R builtins because
        // who knows what assumptions does GNU-R do??? We SHOULD test this.
        if (TYPEOF(arg) != PROMSXP)
            ENSURE_NAMED(arg);
        __listAppend(&result, &pos, arg, name);
    }

    if (result != R_NilValue)
        UNPROTECT(1);

    return result;
}

SEXP createPromargsFromStackValues(CallContext& call) {
    return createLegacyArglist(
        call.callId, call.suppliedArgs, call.stackArgs, nullptr, call.names,
        call.ast, call.caller->arglistOrder(), call.hasEagerCallee(), false);
}

SEXP createEnvironmentFrameFromStackValues(CallContext& call) {
    return createLegacyArglist(ArglistOrder::NOT_REORDERED, call.suppliedArgs,
                               call.stackArgs, nullptr, call.names, call.ast,
                               nullptr, false, false);
}

static SEXP rirCallTrampoline(const CallContext& call, Function* fun, SEXP env,
                              SEXP arglist) {
    assert(TYPEOF(env) == ENVSXP ||
           fun->signature().envCreation ==
               FunctionSignature::Environment::CalleeCreated);

    RCNTXT cntxt;

    // This code needs to be protected, because its slot in the dispatch table
    // could get overwritten while we are executing it.
    PROTECT(fun->container());

    initClosureContext(call.ast, &cntxt, env, call.callerEnv, arglist,
                       call.callee);
    R_Srcref = Rf_getAttrib(call.callee, symbol::srcref);

    closureDebug(call.ast, call.callee, env, R_NilValue, &cntxt);

    // Warning: call.popArgs() between initClosureContext and trampoline will
    // result in broken stack on non-local returns.

    Code* code = fun->body();
    // Pass &cntxt.cloenv, to let evalRirCode update the env of the current
    // context
    SEXP result;
    if ((SETJMP(cntxt.cjmpbuf))) {
        if (R_ReturnedValue == R_RestartToken) {
            cntxt.callflag = CTXT_RETURN; /* turn restart off */
            R_ReturnedValue = R_NilValue; /* remove restart token */
            result = evalRirCode(code, cntxt.cloenv, &call);
        } else {
            result = R_ReturnedValue;
        }
    } else {
        result = evalRirCode(code, env, &call);
    }

    PROTECT(result);

    endClosureDebug(call.ast, call.callee, env);
    endClosureContext(&cntxt, result);

    R_Srcref = cntxt.srcref;
    R_ReturnedValue = R_NilValue;

    UNPROTECT(2);
    return result;
}

#define UI_COUNT_DELTA 1000

static unsigned int count = 0;

// Interrupt Signal Checker - Allows for Ctrl - C functionality to exit out
// of infinite loops
void checkUserInterrupt() {
    if (++count > UI_COUNT_DELTA) {
        R_CheckUserInterrupt();
        R_RunPendingFinalizers();
        count = 0;
    }
}

const static SEXP loopTrampolineMarker = (SEXP)0x7007;
static void loopTrampoline(Code* c, SEXP env, const CallContext* callCtxt,
                           Opcode* pc, BindingCache* cache) {
    assert(env);

    RCNTXT cntxt;
    Rf_begincontext(&cntxt, CTXT_LOOP, R_NilValue, env, R_BaseEnv, R_NilValue,
                    R_NilValue);

    if (int s = SETJMP(cntxt.cjmpbuf)) {
        // incoming non-local break/continue:
        if (s == CTXT_BREAK) {
            Rf_endcontext(&cntxt);
            return;
        }
        // continue case: fall through to do another iteration
    }

    // execute the loop body
    SEXP res = evalRirCode(c, env, callCtxt, pc, cache);
    assert(res == loopTrampolineMarker);
    Rf_endcontext(&cntxt);
}

static SEXP closureArgumentAdaptor(const CallContext& call, SEXP arglist) {
    SEXP op = call.callee;
    if (FORMALS(op) == R_NilValue && arglist == R_NilValue)
        return Rf_NewEnvironment(R_NilValue, R_NilValue, CLOENV(op));

    /*  Set up a context with the call in it so error has access to it */
    RCNTXT cntxt;
    initClosureContext(call.ast, &cntxt, CLOENV(op), call.callerEnv, arglist,
                       op);

    /*  Build a list which matches the actual (unevaluated) arguments
        to the formal paramters.  Build a new environment which
        contains the matched pairs.  Ideally this environment sould be
        hashed.  */
    SEXP newrho, a, f;

    SEXP actuals = arglist;

    bool noArgmatchNeeded =
        call.givenContext.includes(Assumption::StaticallyArgmatched);
    if (!noArgmatchNeeded)
        actuals = Rf_matchArgs_NR(FORMALS(op), actuals, call.ast);

    PROTECT(newrho = Rf_NewEnvironment(FORMALS(op), actuals, CLOENV(op)));

    /*  Use the default code for unbound formals.  FIXME: It looks like
        this code should preceed the building of the environment so that
        this will also go into the hash table.  */

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    f = FORMALS(op);
    a = actuals;
    // get the first Code that is a compiled default value of a formal arg
    // (or nullptr if no such exist)
    Function* fun = DispatchTable::unpack(BODY(op))->baseline();
    size_t pos = 0;
    while (f != R_NilValue) {
        /* Turn on reference counting for the binding cells so local
           assignments arguments increment REFCNT values */
        ENABLE_REFCNT(a);

        Code* c = fun->defaultArg(pos++);
        if (CAR(f) != R_MissingArg) {
            if (CAR(a) == R_MissingArg) {
                assert(c != nullptr && "No more compiled formals available.");
                SETCAR(a, createPromise(c, newrho));
                SET_MISSING(a, 2);
            }
            // Either just used the compiled formal or it was not needed.
            // Skip over current compiled formal and find the next default arg.
        }
        assert(CAR(f) != R_DotsSymbol || TYPEOF(CAR(a)) == DOTSXP);
        f = CDR(f);

        // Statically matched arglist can have trailing missings, lets
        // dyanmically match the length of actuals and formals.
        if (noArgmatchNeeded && f != R_NilValue && CDR(a) == R_NilValue) {
            SETCDR(a, CONS_NR(R_MissingArg, R_NilValue));
            a = CDR(a);
            SET_TAG(a, TAG(f));
        } else {
            a = CDR(a);
        }
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (call.suppliedvars != R_NilValue)
        Rf_addMissingVarsToNewEnv(newrho, call.suppliedvars);

    if (R_envHasNoSpecialSymbols(newrho))
        SET_NO_SPECIAL_SYMBOLS(newrho);

    endClosureContext(&cntxt, R_NilValue);

    UNPROTECT(1);

    return newrho;
}

SEXP getTrivialPromValue(SEXP sym, SEXP env) {
    if (auto le = LazyEnvironment::check(env)) {
        auto v = le->getArg(sym);
        if (v != R_UnboundValue)
            return v;
    } else {
        if (env == R_BaseEnv) {
            auto v = SYMVALUE(sym);
            if (v != R_UnboundValue)
                return v;
        } else {
            R_varloc_t loc = R_findVarLocInFrame(env, sym);
            if (!R_VARLOC_IS_NULL(loc)) {
                if (IS_ACTIVE_BINDING(loc.cell))
                    return R_UnboundValue;
                else
                    return CAR(loc.cell);
            }
        }
    }
    return R_UnboundValue;
}

void inferCurrentContext(CallContext& call, size_t formalNargs) {
    Context& given = call.givenContext;

    if (call.suppliedArgs <= formalNargs) {
        given.add(Assumption::NotTooManyArguments);
        given.numMissing(formalNargs - call.suppliedArgs);
    }

    // S3 dispatch adds additional arguments to the function environment. This
    // is unfortunately not compatible with optimized code, since in PIR we need
    // to know all the formals of a function.
    // TODO: make S3 dispatch a context flag, so we can compile a version of the
    // function that expects the additional suppliedvars on the stack. For now
    // let's just prevent calling into optimized code by removing the
    // notTooManyArguments assumption.
    if (call.suppliedvars != R_NilValue)
        given.remove(Assumption::NotTooManyArguments);

    given.add(Assumption::NoExplicitlyMissingArgs);

    auto testArg = [&](size_t i) {
        SEXP arg = call.stackArg(i);
        bool isEager = true;

        // An explicitly missing arg, such as f(,1)
        if (arg == R_MissingArg) {
            given.remove(Assumption::NoExplicitlyMissingArgs);
            given.setNonRefl(i);
            given.setEager(i);
            return;
        }

        bool reflectionPossible = false;

        if (TYPEOF(arg) == PROMSXP) {
            auto prom = arg;
            arg = PRVALUE(arg);

            // For Lazy promises, lets try to figure out where it points to.
            if (arg == R_UnboundValue) {
                reflectionPossible = true;
                isEager = false;
                // If this is a simple promise, that just looks up an eager
                // value we do not reset the no-reflection flag. The callee
                // can assume that (as long as he does not trigger any other
                // reflection) evaluating this promise does not trigger
                // reflection either.
                while (true) {
                    SEXP v = PRVALUE(prom);

                    if (v == R_MissingArg) {
                        arg = v;
                        reflectionPossible = false;
                        break;
                    }

                    // Let's try to find out if this promise is a trivial
                    // expression (i.e. just a name lookup) and if that lookup
                    // can be easily resolved.
                    if (v == R_UnboundValue) {
                        if (auto sym = getSymbolIfTrivialPromise(prom))
                            v = getTrivialPromValue(sym, PRENV(prom));
                    }

                    if (reflectionPossible) {
                        auto pr = Code::check(PREXPR(prom));
                        if (pr && pr->flags.contains(Code::NoReflection))
                            reflectionPossible = false;
                    }

                    // This is truly lazy and we did not manage to lookup
                    // anything
                    if (v == R_UnboundValue)
                        break;

                    if (TYPEOF(v) != PROMSXP) {
                        reflectionPossible = false;
                        arg = v;
                        break;
                    }
                    prom = v;
                }
            }
        }

        assert(TYPEOF(arg) != PROMSXP);

        if (!reflectionPossible) {
            given.setNonRefl(i);
        }

        if (isEager) {
            given.setEager(i);
            SLOWASSERT(TYPEOF(call.stackArg(i)) != PROMSXP ||
                       PRVALUE(call.stackArg(i)) != R_UnboundValue);
        }

        // Without isEager, these are the results of executing a trivial
        // expression, given no reflective change happens.
        if (arg != R_UnboundValue && arg != R_MissingArg) {
            if (!Rf_isObject(arg))
                given.setNotObj(i);
            if (IS_SIMPLE_SCALAR(arg, REALSXP))
                given.setSimpleReal(i);
            if (IS_SIMPLE_SCALAR(arg, INTSXP))
                given.setSimpleInt(i);
        }

        if (arg == R_MissingArg)
            given.resetNotObj(i);
    };

    bool tryArgmatch = !given.includes(Assumption::StaticallyArgmatched);
    given.add(Assumption::CorrectOrderOfArguments);
    auto sig =
        DispatchTable::unpack(BODY(call.callee))->baseline()->signature();
    if (tryArgmatch && given.includes(Assumption::NotTooManyArguments) &&
        ((!sig.hasDotsFormals) || (call.suppliedArgs <= sig.dotsPosition)))
        given.add(Assumption::StaticallyArgmatched);

    SEXP formals = FORMALS(call.callee);
    for (size_t i = 0; i < call.suppliedArgs; ++i) {
        testArg(i);
        if (call.hasNames()) {
            auto name = call.name(i);
            if (name != R_NilValue && name != TAG(formals)) {
                if (tryArgmatch)
                    given.remove(Assumption::StaticallyArgmatched);
                given.remove(Assumption::CorrectOrderOfArguments);
            }
            formals = CDR(formals);
        }
    }
}

// Watch out: this changes call.nargs! To clean up after the call, you need to
// pop call.nargs number of arguments (which now might be more than the number
// of actually supplied arguments).
static void supplyMissingArgs(CallContext& call, const Function* fun) {
    auto context = fun->context();
    auto expected = fun->expectedNargs();
    assert(expected >= call.suppliedArgs ||
           !context.includes(Assumption::NotTooManyArguments));
    assert(expected == call.suppliedArgs ||
           !context.includes(Assumption::NoExplicitlyMissingArgs));
    if (expected > call.suppliedArgs) {
        for (size_t i = 0; i < expected - call.suppliedArgs; ++i)
            ostack_push(R_MissingArg);
        call.passedArgs = expected;
    }
}

const unsigned pir::Parameter::PIR_WARMUP =
    getenv("PIR_WARMUP") ? atoi(getenv("PIR_WARMUP")) : 100;
const unsigned pir::Parameter::DEOPT_ABANDON =
    getenv("PIR_DEOPT_ABANDON") ? atoi(getenv("PIR_DEOPT_ABANDON")) : 12;
const unsigned pir::Parameter::PIR_OPT_BC_SIZE =
    getenv("PIR_OPT_BC_SIZE") ? atoi(getenv("PIR_OPT_BC_SIZE")) : 20;

static unsigned serializeCounter = 0;

#ifdef DEBUG_SLOWCASES
class SlowcaseCounter {
  public:
    static void count(const std::string& kind, CallContext& call) {
        // setting every time not needed but simple
        Measuring::setEventThreshold(100);
        std::stringstream message;
        message << "Fast case " << kind << " failed for "
                << getBuiltinName(call.callee) << " ("
                << getBuiltinNr(call.callee) << ") "
                << "nargs : " << call.suppliedArgs;
        if (call.suppliedArgs > 0) {
            auto arg = call.stackArg(0);
            if (TYPEOF(arg) == PROMSXP)
                arg = safeForcePromise(arg);
            if (arg == R_UnboundValue)
                message << "arg0 lazy";
            else if (arg == R_MissingArg)
                message << "arg0 missing";
            else
                message << " arg0 : " << type2char(TYPEOF(arg)) << " a "
                        << (ATTRIB(arg) != R_NilValue);
        }
        Measuring::countEvent(message.str());
    }
};
#endif

SEXP doCall(CallContext& call, bool popArgs) {
    assert(call.callee);

    switch (TYPEOF(call.callee)) {

    case SPECIALSXP: {
        if (SEXP res = tryFastSpecialCall(call)) {
            if (popArgs)
                ostack_popn(call.passedArgs - call.suppliedArgs);
            return res;
        }
#ifdef DEBUG_SLOWCASES
        SlowcaseCounter::count("special", call);
#endif
        assert(call.ast != R_NilValue);

        // get the ccode
        CCODE f = getBuiltin(call.callee);
        int flag = getFlag(call.callee);
        R_Visible = static_cast<Rboolean>(flag != 1);
        // call it with the AST only
        SEXP result =
            f(call.ast, call.callee, CDR(call.ast), materializeCallerEnv(call));
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        if (popArgs)
            ostack_popn(call.passedArgs - call.suppliedArgs);
        return result;
    }

    case BUILTINSXP: {
        if (SEXP res = tryFastBuiltinCall(call)) {
            int flag = getFlag(call.callee);
            if (flag < 2)
                R_Visible = static_cast<Rboolean>(flag != 1);
            if (popArgs)
                ostack_popn(call.passedArgs - call.suppliedArgs);
            return res;
        }
#ifdef DEBUG_SLOWCASES
        SlowcaseCounter::count("builtin", call);
#endif

        SEXP arglist = createPromargsFromStackValues(call);
        PROTECT(arglist);

        // Make sure the RHS NAMED value is 0 or NAMEDMAX for when the RHS value
        // is part of the LHS object. See FIXUP_RHS_NAMED in eval.c
        switch (call.callee->u.primsxp.offset) {
        case blt("length<-"):
        case blt("oldClass<-"):
        case blt("class<-"):
        case blt("names<-"):
        case blt("dimnames<-"):
        case blt("dim<-"):
        case blt("attributes<-"):
        case blt("levels<-"):
        case blt("comment<-"):
        case blt("storage.mode<-"):
        case blt("environment<-"):
        case blt("parent.env<-"): {
            if (call.passedArgs == 2) {
                if (NAMED(CADR(arglist)))
                    ENSURE_NAMEDMAX(CADR(arglist));
            }
            break;
        }
        case blt("attr<-"): {
            if (call.passedArgs == 3) {
                if (NAMED(CADDR(arglist)))
                    ENSURE_NAMEDMAX(CADDR(arglist));
            }
            break;
        }
        case blt("substr<-"): {
            if (call.passedArgs == 4) {
                if (NAMED(CADDDR(arglist)))
                    ENSURE_NAMEDMAX(CADDDR(arglist));
            }
            break;
        }
        default: {
        }
        }

        CCODE f = getBuiltin(call.callee);
        int flag = getFlag(call.callee);
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        SEXP res =
            f(call.ast, call.callee, arglist, materializeCallerEnv(call));
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        UNPROTECT(1);
        if (popArgs)
            ostack_popn(call.passedArgs - call.suppliedArgs);
        return res;
    }

    case CLOSXP: {
        if (TYPEOF(BODY(call.callee)) != EXTERNALSXP) {
            SEXP arglist = createPromargsFromStackValues(call);
            PROTECT(arglist);
            SEXP res = Rf_applyClosure(call.ast, call.callee, arglist,
                                       materializeCallerEnv(call), R_NilValue);
            UNPROTECT(1);
            if (popArgs)
                ostack_popn(call.passedArgs - call.suppliedArgs);
            return res;
        }

        R_CheckStack();
        SEXP body = BODY(call.callee);
        if (pir::Parameter::RIR_SERIALIZE_CHAOS) {
            serializeCounter++;
            if (serializeCounter == pir::Parameter::RIR_SERIALIZE_CHAOS) {
                body = copyBySerial(body);
                serializeCounter = 0;
            }
            PROTECT(body);
        }
        assert(DispatchTable::check(body));

        auto table = DispatchTable::unpack(body);

        inferCurrentContext(call, table->baseline()->signature().formalNargs());
        Function* disabledFun;
        auto fun =
            table->dispatchConsideringDisabled(call.givenContext, &disabledFun);

        REC_HOOK(recording::recordInvocationDoCall());
        fun->registerInvocation();

        if (!isDeoptimizing() && RecompileHeuristic(fun, disabledFun)) {
            Context given = call.givenContext;
            // addDynamicAssumptionForOneTarget compares arguments with the
            // signature of the current dispatch target. There the number of
            // arguments might be off. But we want to force compiling a new
            // version exactly for this number of arguments, thus we need to add
            // this as an explicit assumption.

            fun->clearDisabledAssumptions(given);
            if (RecompileCondition(table, fun, given)) {
                if (given.includes(pir::Compiler::minimalContext)) {
                    if (call.caller &&
                        call.caller->function()->invocationCount() > 0 &&
                        !call.caller->isCompiled() &&
                        !call.caller->function()->disabled() &&
                        call.caller->size() < pir::Parameter::MAX_INPUT_SIZE &&
                        fun->body()->codeSize <
                            pir::Parameter::PIR_OPT_BC_SIZE) {

                        REC_HOOK(recording::recordOsrTriggerCallerCallee());
                        call.triggerOsr = true;
                    }
                    DoRecompile(fun, call.ast, call.callee, given);
                    fun = dispatch(call, table);
                }
            }

            REC_HOOK(recording::recordReasonsClear());
        }
        bool needsEnv = fun->signature().envCreation ==
                        FunctionSignature::Environment::CallerProvided;

        if (fun->flags.contains(Function::DepromiseArgs)) {
            // Force arguments and depromise
            call.depromiseArgs();
        }

        LazyArglistOnStack lazyPromargs(
            call.callId,
            call.caller ? call.caller->arglistOrderContainer() : nullptr,
            call.suppliedArgs, call.stackArgs, call.ast);

        SEXP result;
        if (!needsEnv) {
            // Default fast calling convention for pir, environment is created
            // by the callee
            SEXP arglist = call.arglist;
            if (!arglist) {
                assert(call.stackArgs);
                arglist = lazyPromargs.asSexp();
            }

            supplyMissingArgs(call, fun);
            result = rirCallTrampoline(call, fun, symbol::delayedEnv, arglist);
        } else {
            // TODO: figure out why this slowcase happens too often...
            int npreserved = 0;

            // Slowcase: callee expects arguments bound to pre-created
            // environment. We need the list of arguments for creating the
            // environment and also the list of original arguments for the
            // promargs, which we will create lazily if it does not exist yet.
            SEXP frame;
            SEXP promargs;
            if (call.arglist) {
                promargs = call.arglist;
                frame = Rf_shallow_duplicate(promargs);
                PROTECT(promargs);
                npreserved++;
            } else {
                // Wrap the passed args in a linked-list.
                frame = createEnvironmentFrameFromStackValues(call);
                PROTECT(frame);
                npreserved++;
                promargs = lazyPromargs.asSexp();
            }

            SEXP env;
            if (call.givenContext.includes(Assumption::StaticallyArgmatched)) {
                // Statically argmatched means that we can directly bundle the
                // list of arguments with the formals, since they are already in
                // the correct order.
                auto formals = FORMALS(call.callee);
                env = Rf_NewEnvironment(formals, frame, CLOENV(call.callee));
                PROTECT(env);
                npreserved++;

                // Add missing arguments. Statically argmatched means that still
                // some missing args might need to be supplied.
                if (!call.givenContext.includes(
                        Assumption::NoExplicitlyMissingArgs) ||
                    call.passedArgs != fun->nargs()) {

                    auto f = formals;
                    auto a = frame;
                    SEXP prevA = nullptr;
                    size_t pos = 0;
                    while (f != R_NilValue) {
                        if (a == R_NilValue) {
                            a = CONS_NR(R_MissingArg, R_NilValue);
                            SET_TAG(a, TAG(f));
                            SET_MISSING(a, 1);
                            if (prevA) {
                                SETCDR(prevA, a);
                            } else {
                                assert(frame == R_NilValue);
                                SET_FRAME(env, a);
                            }
                            if (auto dflt = fun->defaultArg(pos)) {
                                SETCAR(a, createPromise(dflt, env));
                                SET_MISSING(a, 2);
                            }
                        } else if (CAR(a) == R_MissingArg) {
                            SET_MISSING(a, 1);
                            if (auto dflt = fun->defaultArg(pos)) {
                                SET_MISSING(a, 2);
                                SETCAR(a, createPromise(dflt, env));
                            }
                        }

                        f = CDR(f);
                        prevA = a;
                        a = CDR(a);
                        pos++;
                    }
                }

                if (call.suppliedvars != R_NilValue)
                    Rf_addMissingVarsToNewEnv(env, call.suppliedvars);
            } else {
                // No need for lazy args if we have the non-modified list anyway
                promargs = frame;
                env = closureArgumentAdaptor(call, frame);
                PROTECT(env);
                npreserved++;
            }

            result = rirCallTrampoline(call, fun, env, promargs);
            UNPROTECT(npreserved);
        }

        if (pir::Parameter::RIR_SERIALIZE_CHAOS) {
            UNPROTECT(1);
        }
        assert(result);
        if (popArgs)
            ostack_popn(call.passedArgs - call.suppliedArgs);
        return result;
    }
    default:
        Rf_error("attempt to apply non-function");
    }

    // not reached
    assert(false);
}

SEXP dispatchApply(SEXP ast, SEXP obj, SEXP actuals, SEXP selector,
                   SEXP callerEnv) {
    SEXP op = SYMVALUE(selector);

    // ===============================================
    // First try S4
    if (IS_S4_OBJECT(obj) && R_has_methods(op)) {
        SEXP result = R_possible_dispatch(ast, op, actuals, callerEnv, TRUE);
        if (result)
            return result;
    }

    // ===============================================
    // Then try S3
    const char* generic = CHAR(PRINTNAME(selector));
    SEXP rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, callerEnv);
    PROTECT(rho1);
    RCNTXT cntxt;
    initClosureContext(ast, &cntxt, rho1, callerEnv, actuals, op);
    SEXP result;
    bool success = Rf_usemethod(generic, obj, ast, actuals, rho1, callerEnv,
                                R_BaseEnv, &result);
    UNPROTECT(1);
    endClosureContext(&cntxt, success ? result : R_NilValue);
    if (success)
        return result;

    return nullptr;
}

#define R_INT_MAX INT_MAX
#define R_INT_MIN -INT_MAX
// .. relying on fact that NA_INTEGER is outside of these

static R_INLINE int R_integer_plus(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;

    if (((y > 0) && (x > (R_INT_MAX - y))) ||
        ((y < 0) && (x < (R_INT_MIN - y)))) {
        if (pnaflag)
            *pnaflag = TRUE;
        return NA_INTEGER;
    }
    return x + y;
}

static R_INLINE int R_integer_minus(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;

    if (((y < 0) && (x > (R_INT_MAX + y))) ||
        ((y > 0) && (x < (R_INT_MIN + y)))) {
        if (pnaflag)
            *pnaflag = TRUE;
        return NA_INTEGER;
    }
    return x - y;
}

#define GOODIPROD(x, y, z) ((double)(x) * (double)(y) == (z))
static R_INLINE int R_integer_times(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;
    else {
        int z = x * y;
        if (GOODIPROD(x, y, z) && z != NA_INTEGER)
            return z;
        else {
            if (pnaflag)
                *pnaflag = TRUE;
            return NA_INTEGER;
        }
    }
}

// TODO implement division and maybe others
enum class Binop { PLUSOP, MINUSOP, TIMESOP };
enum class Unop { PLUSOP, MINUSOP };
#define INTEGER_OVERFLOW_WARNING "NAs produced by integer overflow"

#define CHECK_INTEGER_OVERFLOW(ans, naflag)                                    \
    do {                                                                       \
        if (naflag) {                                                          \
            PROTECT(ans);                                                      \
            SEXP call = getSrcForCall(c, pc - 1);                              \
            Rf_warningcall(call, INTEGER_OVERFLOW_WARNING);                    \
            UNPROTECT(1);                                                      \
        }                                                                      \
    } while (0)

#define BINOP_FALLBACK(op)                                                     \
    do {                                                                       \
        static SEXP prim = nullptr;                                            \
        static CCODE blt;                                                      \
        static int flag;                                                       \
        if (!prim) {                                                           \
            prim = Rf_findFun(Rf_install(op), R_GlobalEnv);                    \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
                                                                               \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP call = getSrcForCall(c, pc - 1);                                  \
                                                                               \
        SEXP arglist = CONS_NR(lhs, CONS_NR(rhs, R_NilValue));                 \
        ostack_push(arglist);                                                  \
        res = blt(call, prim, arglist, env);                                   \
        ostack_pop();                                                          \
                                                                               \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
    } while (false)

#define DO_FAST_BINOP(op, op2)                                                 \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {                                  \
            if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                              \
                res_type = REALSXP;                                            \
                real_res = (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)    \
                               ? NA_REAL                                       \
                               : *REAL(lhs) op * REAL(rhs);                    \
            } else if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                        \
                res_type = REALSXP;                                            \
                real_res =                                                     \
                    (*REAL(lhs) == NA_REAL || *INTEGER(rhs) == NA_INTEGER)     \
                        ? NA_REAL                                              \
                        : *REAL(lhs) op * INTEGER(rhs);                        \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {                            \
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                               \
                Rboolean naflag = FALSE;                                       \
                switch (op2) {                                                 \
                case Binop::PLUSOP:                                            \
                    int_res =                                                  \
                        R_integer_plus(*INTEGER(lhs), *INTEGER(rhs), &naflag); \
                    break;                                                     \
                case Binop::MINUSOP:                                           \
                    int_res = R_integer_minus(*INTEGER(lhs), *INTEGER(rhs),    \
                                              &naflag);                        \
                    break;                                                     \
                case Binop::TIMESOP:                                           \
                    int_res = R_integer_times(*INTEGER(lhs), *INTEGER(rhs),    \
                                              &naflag);                        \
                    break;                                                     \
                }                                                              \
                res_type = INTSXP;                                             \
                CHECK_INTEGER_OVERFLOW(R_NilValue, naflag);                    \
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                       \
                res_type = REALSXP;                                            \
                real_res =                                                     \
                    (*INTEGER(lhs) == NA_INTEGER || *REAL(rhs) == NA_REAL)     \
                        ? NA_REAL                                              \
                        : *INTEGER(lhs) op * REAL(rhs);                        \
            }                                                                  \
        }                                                                      \
    } while (false)

#define STORE_BINOP(res_type, int_res, real_res)                               \
    do {                                                                       \
        SEXP a = ostack_at(0);                                                 \
        SEXP b = ostack_at(1);                                                 \
        if (NO_REFERENCES(a)) {                                                \
            TYPEOF(a) = res_type;                                              \
            res = a;                                                           \
            ostack_pop();                                                      \
            ostack_set(0, a);                                                  \
        } else if (NO_REFERENCES(b)) {                                         \
            TYPEOF(b) = res_type;                                              \
            res = b;                                                           \
            ostack_pop();                                                      \
        } else {                                                               \
            ostack_pop();                                                      \
            res = Rf_allocVector(res_type, 1);                                 \
            ostack_set(0, res);                                                \
        }                                                                      \
        switch (res_type) {                                                    \
        case INTSXP:                                                           \
            INTEGER(res)[0] = int_res;                                         \
            break;                                                             \
        case REALSXP:                                                          \
            REAL(res)[0] = real_res;                                           \
            break;                                                             \
        }                                                                      \
    } while (false)

#define DO_BINOP(op, op2)                                                      \
    do {                                                                       \
        int int_res = -1;                                                      \
        double real_res = -2.0;                                                \
        int res_type = 0;                                                      \
        DO_FAST_BINOP(op, op2);                                                \
        if (res_type) {                                                        \
            STORE_BINOP(res_type, int_res, real_res);                          \
            R_Visible = (Rboolean) true;                                       \
        } else {                                                               \
            BINOP_FALLBACK(#op);                                               \
            ostack_pop();                                                      \
            ostack_set(0, res);                                                \
        }                                                                      \
    } while (false)

static double myfloor(double x1, double x2) {
    double q = x1 / x2, tmp;

    if (x2 == 0.0)
        return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp / x2);
}

static double myfmod(double x1, double x2) {
    if (x2 == 0.0)
        return R_NaN;
    double q = x1 / x2, tmp = x1 - floor(q) * x2;
    if (R_FINITE(q) && (fabs(q) > 1 / R_AccuracyInfo.eps))
        Rf_warning("probable complete loss of accuracy in modulus");
    q = floor(tmp / x2);
    return tmp - q * x2;
}

static R_INLINE int R_integer_uplus(int x, Rboolean* pnaflag) {
    if (x == NA_INTEGER)
        return NA_INTEGER;

    return x;
}

static R_INLINE int R_integer_uminus(int x, Rboolean* pnaflag) {
    if (x == NA_INTEGER)
        return NA_INTEGER;

    return -x;
}

#define UNOP_FALLBACK(op)                                                      \
    do {                                                                       \
        static SEXP prim = nullptr;                                            \
        static CCODE blt;                                                      \
        static int flag;                                                       \
        if (!prim) {                                                           \
            prim = Rf_findFun(Rf_install(op), R_GlobalEnv);                    \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        SEXP call = getSrcForCall(c, pc - 1);                                  \
        SEXP arglist = CONS_NR(val, R_NilValue);                               \
        ostack_push(arglist);                                                  \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        res = blt(call, prim, arglist, env);                                   \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostack_pop();                                                          \
    } while (false)

#define DO_UNOP(op, op2)                                                       \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(val, REALSXP)) {                                  \
            res = Rf_allocVector(REALSXP, 1);                                  \
            *REAL(res) = (*REAL(val) == NA_REAL) ? NA_REAL : op * REAL(val);   \
            R_Visible = (Rboolean) true;                                       \
        } else if (IS_SIMPLE_SCALAR(val, INTSXP)) {                            \
            Rboolean naflag = FALSE;                                           \
            res = Rf_allocVector(INTSXP, 1);                                   \
            switch (op2) {                                                     \
            case Unop::PLUSOP:                                                 \
                *INTEGER(res) = R_integer_uplus(*INTEGER(val), &naflag);       \
                break;                                                         \
            case Unop::MINUSOP:                                                \
                *INTEGER(res) = R_integer_uminus(*INTEGER(val), &naflag);      \
                break;                                                         \
            }                                                                  \
            CHECK_INTEGER_OVERFLOW(res, naflag);                               \
            R_Visible = (Rboolean) true;                                       \
        } else {                                                               \
            UNOP_FALLBACK(#op);                                                \
        }                                                                      \
        ostack_set(0, res);                                                    \
    } while (false)

#define DO_RELOP(op)                                                           \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(lhs, LGLSXP)) {                                   \
            if (IS_SIMPLE_SCALAR(rhs, LGLSXP)) {                               \
                if (*LOGICAL(lhs) == NA_LOGICAL ||                             \
                    *LOGICAL(rhs) == NA_LOGICAL) {                             \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *LOGICAL(lhs) op * LOGICAL(rhs) ? R_TrueValue        \
                                                          : R_FalseValue;      \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {                           \
            if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                              \
                if (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL) {          \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *REAL(lhs) op * REAL(rhs) ? R_TrueValue              \
                                                    : R_FalseValue;            \
                }                                                              \
                break;                                                         \
            } else if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                        \
                if (*REAL(lhs) == NA_REAL || *INTEGER(rhs) == NA_INTEGER) {    \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *REAL(lhs) op * INTEGER(rhs) ? R_TrueValue           \
                                                       : R_FalseValue;         \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {                            \
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                               \
                if (*INTEGER(lhs) == NA_INTEGER ||                             \
                    *INTEGER(rhs) == NA_INTEGER) {                             \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *INTEGER(lhs) op * INTEGER(rhs) ? R_TrueValue        \
                                                          : R_FalseValue;      \
                }                                                              \
                break;                                                         \
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                       \
                if (*INTEGER(lhs) == NA_INTEGER || *REAL(rhs) == NA_REAL) {    \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *INTEGER(lhs) op * REAL(rhs) ? R_TrueValue           \
                                                       : R_FalseValue;         \
                }                                                              \
                break;                                                         \
            }                                                                  \
        }                                                                      \
        BINOP_FALLBACK(#op);                                                   \
    } while (false)

SEXP seq_int(int n1, int n2) {
    int n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
    SEXP ans = Rf_allocVector(INTSXP, n);
    int* data = INTEGER(ans);
    int64_t current = n1;
    int64_t end = n2;
    if (current <= end) {
        while (current <= end)
            *data++ = current++;
    } else {
        while (current >= end)
            *data++ = current--;
    }
    return ans;
}

bool isMissing(SEXP symbol, SEXP environment, Code* code, Opcode* pc) {
    SEXP val = R_findVarLocInFrame(environment, symbol).cell;
    if (val == nullptr) {
        if (code)
            Rf_errorcall(getSrcAt(code, pc - 1),
                         "'missing' can only be used for arguments");
        else
            Rf_errorcall(R_NilValue,
                         "'missing' can only be used for arguments");
    }

    if (MISSING(val) || CAR(val) == R_MissingArg)
        return true;

    val = CAR(val);
    if (TYPEOF(val) != PROMSXP)
        return false;

    val = findRootPromise(val);
    auto sym = getSymbolIfTrivialPromise(val);
    if (!sym) {
        return false;
    } else {
        if (sym == R_MissingArg)
            return true;
        if (auto le = LazyEnvironment::check(val->u.promsxp.env)) {
            if (le->materialized())
                SET_PRENV(val, le->materialized());
            else
                return le->isMissing(sym);
        }
        return R_isMissing(sym, PRENV(val));
    }
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

// This happens since enabling -fno-exceptions, but the error message is
// terrible, can't find out where in the evalRirCode function
#pragma GCC diagnostic ignored "-Wstrict-overflow"

/*
 * This function takes some deopt metadata and stack frame contents on the
 * interpreter stack. It first recursively reconstructs a context for each
 * inlined frame, bottom up. This means either reuse the already existing
 * context, or synthesize a new one. Then the frames are executed (in the
 * deoptimized version), top down. At the end we long-jump out of the outermost
 * context and thus return from the R function that triggered this deopt
 * routine.
 */
void deoptFramesWithContext(const CallContext* callCtxt,
                            DeoptMetadata* deoptData, SEXP sysparent,
                            size_t pos, size_t stackHeight,
                            RCNTXT* currentContext) {
    size_t excessStack = stackHeight;

    const FrameInfo& f = deoptData->frames[pos];
    stackHeight -= f.stackSize + 1;
    SEXP deoptEnv = ostack_at(stackHeight);
    auto code = f.code;

    bool outermostFrame = pos == deoptData->numFrames - 1;
    bool innermostFrame = pos == 0;
    bool inPromise = f.inPromise;

    if (outermostFrame)
        startDeoptimizing();

    if (auto le = LazyEnvironment::check(deoptEnv)) {
        if (le->materialized())
            deoptEnv = le->materialized();
    }

    RCNTXT fake;
    RCNTXT* cntxt;
    if (inPromise) {
        cntxt = currentContext;
    } else {
        RCNTXT* originalCntxt = findFunctionContextFor(deoptEnv);
        if (originalCntxt) {
            cntxt = originalCntxt;
        } else {
            // NOTE: this assert triggers if we can't find the context of the
            // current function. Usually the reason is that a wrong environment
            // is stored in the context.
            assert(!outermostFrame && "Cannot find outermost function context");
            // If the inlinee had no context, we need to synthesize one
            // TODO: need to add ast and closure to the deopt metadata to create
            // a complete context
            cntxt = &fake;
            initClosureContext(R_NilValue, cntxt, deoptEnv, sysparent,
                               FRAME(sysparent), R_NilValue);
        }
    }

    if (auto le = LazyEnvironment::check(deoptEnv)) {
        assert(!le->materialized());
        deoptEnv = materialize(deoptEnv);
        cntxt->cloenv = deoptEnv;
    }
    assert(TYPEOF(deoptEnv) == ENVSXP);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
    auto frameBaseSize = ostack_length() - excessStack;
#pragma GCC diagnostic pop

    auto trampoline = [&]() {
        // 1. Set up our (outer) context
        //
        // The inlinees need to be bound to a new trampoline, or they could
        // long-jump out of this deopt routine into the inlineContextTrampoline.
        // The outermost frame is the caller, not an inlinee, thus we need not
        // change its context.
        if (!outermostFrame && !inPromise) {
            // The longjump is initialized, when we are still reconstructing
            // the frames. But if we restart from here, we need to remove
            // all the extra stuff from the stack used for reconstruction.
            cntxt->nodestack = ostack_cell_at(excessStack - 1);
            if ((SETJMP(cntxt->cjmpbuf))) {
                assert(ostack_length() == frameBaseSize);
                if (R_ReturnedValue == R_RestartToken) {
                    cntxt->callflag = CTXT_RETURN; /* turn restart off */
                    R_ReturnedValue = R_NilValue;  /* remove restart token */
                    return evalRirCode(code, cntxt->cloenv, callCtxt);
                } else {
                    return R_ReturnedValue;
                }
            }
        }

        // 2. Execute the inner frames
        if (!innermostFrame) {
            deoptFramesWithContext(callCtxt, deoptData, deoptEnv, pos - 1,
                                   stackHeight, cntxt);
        }

        // 3. Execute our frame
        //
        // This wrapper consumes the environment from the deopt metadata and the
        // result of the previous frame.
        assert(ostack_length() ==
               frameBaseSize + f.stackSize + (innermostFrame ? 1 : 2));
        SEXP res = nullptr;
        if (!innermostFrame)
            res = ostack_pop();
        assert(
            ostack_top() == deoptEnv ||
            (LazyEnvironment::check(ostack_top()) &&
             LazyEnvironment::check(ostack_top())->materialized() == deoptEnv));
        ostack_pop();
        if (!innermostFrame)
            ostack_push(res);
        if (inPromise) {
            SEXP p = createPromise(code, deoptEnv);
            PROTECT(p);
            auto r = evaluatePromise(p, f.pc);
            UNPROTECT(1);
            return r;
        }
        return evalRirCode(code, cntxt->cloenv, callCtxt, f.pc, nullptr);
    };

    SEXP res = trampoline();
    assert(ostack_length() == frameBaseSize);

    if (!outermostFrame) {
        if (!inPromise)
            endClosureContext(cntxt, res);
    } else {
        // Deopt in promise is only possible if the promise is not the outermost
        // frame. The reason is that the promise does not have a context
        // associated and we can therefore not jump out of deoptimized promise
        // evaluation. Therefore promises should only occur in inner frames
        // during deoptimization.
        assert(!inPromise);
        endDeoptimizing();
        assert(findFunctionContextFor(deoptEnv) == cntxt);
        // long-jump out of all the inlined contexts
        Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, cntxt->cloenv, res);
        assert(false);
    }

    ostack_push(res);
}

size_t expandDotDotDotCallArgs(size_t n, Immediate* names_, SEXP env,
                               bool explicitDots) {
    Protect p;
    std::vector<SEXP> args;
    std::vector<SEXP> names;
    bool hasNames = false;
    for (size_t i = 0; i < n; ++i) {
        auto arg = ostack_at(n - i - 1);
        auto name = cp_pool_at(names_[i]);
        if (name == symbol::expandDotsTrigger) {
            // If we come from rir, we have to look up `...`, marked by the arg
            // being the symbol expandDotsTrigger If we come from pir, `...` is
            // already loaded
            SEXP ellipsis = (arg == symbol::expandDotsTrigger)
                                ? Rf_findVar(R_DotsSymbol, env)
                                : arg;

            if (TYPEOF(ellipsis) == PROMSXP) {
                ellipsis = evaluatePromise(ellipsis);
            }

            if (TYPEOF(ellipsis) == DOTSXP || ellipsis == R_NilValue) {
                while (ellipsis != R_NilValue) {
                    auto dotArg = CAR(ellipsis);
                    if (TYPEOF(dotArg) == LANGSXP ||
                        (TYPEOF(dotArg) == SYMSXP && dotArg != R_MissingArg)) {
                        arg = Rf_mkPROMISE(arg, env);
                        p(arg);
                    }
                    args.push_back(dotArg);
                    names.push_back(TAG(ellipsis));
                    if (TAG(ellipsis) != R_NilValue)
                        hasNames = true;
                    ellipsis = CDR(ellipsis);
                }
            } else if (ellipsis == R_MissingArg) {
                // Empty `...` occurring in the middle of an argument list needs
                // to be explicit, since pir optimized functions expect it that
                // way.
                if (explicitDots) {
                    args.push_back(R_MissingArg);
                    names.push_back(R_NilValue);
                }
            }
        } else {
            args.push_back(arg);
            names.push_back(name);
            if (name != R_NilValue)
                hasNames = true;
        }
    }
    if (hasNames) {
        SEXP namesStore =
            Rf_allocVector(RAWSXP, sizeof(Immediate) * names.size());
        ostack_popn(n);
        ostack_push(namesStore);
        {
            Immediate* nstore = (Immediate*)DATAPTR(namesStore);
            for (const auto& n : names) {
                *nstore = Pool::insert(n);
                nstore++;
            }
        }
    } else {
        ostack_popn(n);
        ostack_push(R_NilValue);
    }

    for (const auto& a : args)
        ostack_push(a);
    return args.size();
}

bool isColonFastcase(SEXP lhs, SEXP rhs) {
    // In order for the fastcase 2 conditions must be met:
    // - Either lhs or rhs must not be factors
    // - rhs must not be coerced to INT_MIN or INT_MAX (because we expect lhs >
    // rhs or lhs < rhs). To ensure this:
    //   - lhs must not be a number which gets coerced into an integer
    //   (implying, it can't be a real/complex which is integral)
    //   - rhs must not be a number which gets coerced into INT_MIN or INT_MAX
    //   (implying, it can't be the real/complex representation of INT_MIN or
    //   INT_MAX)
    if (Rf_inherits(lhs, "factor") && Rf_inherits(rhs, "factor"))
        return false;

    // TODO(o):
    // I don't like this part of the condition. It prevents us from constant
    // folding the colonEffects instruction. Can we do this differently?
    if (XLENGTH(lhs) == 0 || XLENGTH(rhs) == 0)
        return true;

    switch (TYPEOF(lhs)) {
    case INTSXP:
    case LGLSXP:
        break;
    case REALSXP:
        if (!doubleCanBeCastedToInteger(*REAL(lhs)))
            return true;
        break;
    case CPLXSXP:
        if (!doubleCanBeCastedToInteger(COMPLEX(lhs)->r))
            return true;
        break;
    default:
        break;
    }

    // This is the case where LHS might be an INTSXP
    int coerced = 0;
    switch (TYPEOF(rhs)) {
    case INTSXP:
    case LGLSXP:
        coerced = *INTEGER(rhs);
        break;
    case REALSXP:
        if (!doubleCanBeCastedToInteger(*REAL(rhs)))
            return false;
        coerced = (int)*REAL(rhs);
        break;
    case CPLXSXP:
        if (!doubleCanBeCastedToInteger(COMPLEX(rhs)->r))
            return false;
        coerced = (int)COMPLEX(rhs)->r;
        break;
    default:
        return true;
    }
    return coerced != INT_MAX;
}

bool doubleCanBeCastedToInteger(double n) {
    double intpart;
    return n > ((double)INT_MIN - 1) && n < ((double)INT_MAX + 1) &&
           std::modf(n, &intpart) == 0.0;
}

// int because the llvm backend represents bools as int
int colonInputEffects(SEXP lhs, SEXP rhs, unsigned srcIdx) {
    auto getSrc = [&]() { return src_pool_at(srcIdx); };

    // 1. decide fastcase
    bool fastcase = isColonFastcase(lhs, rhs);

    // 2. in fastcase, run type error effects
    if (fastcase) {
        int lhsLen = Rf_length(lhs);
        int rhsLen = Rf_length(rhs);
        if (lhsLen == 0 || rhsLen == 0)
            Rf_errorcall(getSrc(), "argument of length 0");
        if (lhsLen > 1)
            Rf_warningcall(getSrc(),
                           ngettext("numerical expression has %d element: "
                                    "only the first used",
                                    "numerical expression has %d elements: "
                                    "only the first used",
                                    (int)lhsLen),
                           (int)lhsLen);
        if (rhsLen > 1)
            Rf_warningcall(getSrc(),
                           ngettext("numerical expression has %d element: "
                                    "only the first used",
                                    "numerical expression has %d elements: "
                                    "only the first used",
                                    (int)rhsLen),
                           (int)rhsLen);
    }

    return fastcase;
}

SEXP colonCastLhs(SEXP lhs) {
    double lhsNum = Rf_asReal(lhs);

    if (std::isnan(lhsNum)) {
        Rf_error("NA/NaN argument");
    }
    SEXP result = doubleCanBeCastedToInteger(lhsNum)
                      ? Rf_ScalarInteger((int)lhsNum)
                      : Rf_ScalarReal(lhsNum);
    return result;
}

SEXP colonCastRhs(SEXP newLhs, SEXP rhs) {
    double newLhsNum = Rf_asReal(newLhs);
    double rhsNum = Rf_asReal(rhs);

    if (std::isnan(rhsNum)) {
        Rf_error("NA/NaN argument");
    }
    double newRhsNum = (newLhsNum <= rhsNum)
                           ? (newLhsNum + floor(rhsNum - newLhsNum) + 1)
                           : (newLhsNum - floor(newLhsNum - rhsNum) - 1);
    // nan RHS - should've went to slowcase
    assert(TYPEOF(newLhs) != INTSXP ||
           (newRhsNum >= INT_MIN && newRhsNum <= INT_MAX));
    SEXP result = (TYPEOF(newLhs) == INTSXP) ? Rf_ScalarInteger((int)newRhsNum)
                                             : Rf_ScalarReal(newRhsNum);
    return result;
}

bool pir::Parameter::ENABLE_OSR =
    !getenv("PIR_OSR") || *getenv("PIR_OSR") != '0';
static size_t osrLimit =
    getenv("PIR_OSR_LIMIT") ? std::atoi(getenv("PIR_OSR_LIMIT")) : 5000;
static SEXP osr(const CallContext* callCtxt, R_bcstack_t* basePtr, SEXP env,
                Code* c, Opcode* pc) {
    if (basePtr && callCtxt && !isDeoptimizing() && callCtxt->stackArgs &&
        !pir::Parameter::RIR_SERIALIZE_CHAOS && pir::Parameter::ENABLE_OSR) {
        long size = R_BCNodeStackTop - basePtr;
        assert(size >= 0);
        auto l = Rf_length(FRAME(env));
        auto dt = DispatchTable::check(BODY(callCtxt->callee));
        if (dt &&
            !dt->baseline()->flags.includes(Function::Flag::NotOptimizable) &&
            size <= (long)pir::ContinuationContext::MAX_STACK &&
            l <= (long)pir::ContinuationContext::MAX_ENV) {
            pir::ContinuationContext ctx(pc, env, true, basePtr, size);
            if (auto fun = pir::OSR::compile(callCtxt->callee, c, ctx)) {
                PROTECT(fun->container());
                dt->baseline()->flags.set(Function::Flag::MarkOpt);
                auto code = fun->body();
                auto nc = code->nativeCode();
                auto res = nc(code, basePtr, env, callCtxt->callee);
                ostack_popn(size);
                UNPROTECT(1);
                return res;
            }
        }
    }
    REC_HOOK(recording::recordReasonsClear());
    return nullptr;
}

SEXP evalRirCode(Code* c, SEXP env, const CallContext* callCtxt,
                 Opcode* initialPC, BindingCache* cache) {
    assert(env != symbol::delayedEnv || (callCtxt != nullptr));

    checkUserInterrupt();
    auto native = c->nativeCode();
    assert((!initialPC || !native) && "Cannot jump into native code");
    if (native) {
        return native(c, callCtxt ? (void*)callCtxt->stackArgs : nullptr, env,
                      callCtxt ? callCtxt->callee : nullptr);
    }

#ifdef THREADED_CODE
    static void* opAddr[static_cast<uint8_t>(Opcode::num_of)] = {
#define DEF_INSTR(name, ...) (__extension__ && op_##name),
#include "bc/insns.h"
#undef DEF_INSTR
    };
#endif

    assert(c->info.magic == CODE_MAGIC);
    assert(c->codeSize != 0);

    R_bcstack_t* basePtr = nullptr;

    BindingCache* bindingCache;
    if (cache) {
        bindingCache = cache;
    } else {
        bindingCache = (BindingCache*)(alloca(sizeof(BindingCache) +
                                              sizeof(BindingCacheEntry) *
                                                  c->bindingCacheSize));
        bindingCache->length = c->bindingCacheSize;
        // Optimized functions explicitly manage the cache
        if (env != symbol::delayedEnv) {
            clearCache(bindingCache);
#ifdef DEBUG_ENV_ALLOC
            Measuring::countEvent("env allocated");
#endif
        }

        // If this is an inner loop context (ie. cache exists), or a deopt (ie.
        // initialPC is set), we do not know the actual base pointer. Otherwise
        // we do...
        if (!initialPC)
            basePtr = R_BCNodeStackTop;
    }

    // make sure there is enough room on the stack
    // there is some slack of 5 to make sure the call instruction can store
    // some intermediate values on the stack
    ostack_ensureSize(c->stackLength + 5);

    Opcode* pc;

    if (initialPC) {
        pc = initialPC;
    } else {
        R_Visible = TRUE;
        pc = c->code();
    }

    // This is used in loads for recording if the loaded value was a promise
    // and if it was forced. Looks at the next instruction, if it's a force,
    // marks how this load behaved.
    auto recordForceBehavior = [&](SEXP s) {
        // Bail if this load not recorded or we are in already optimized code
        if (*pc != Opcode::record_type_)
            return;

        ObservedValues::StateBeforeLastForce state =
            ObservedValues::StateBeforeLastForce::unknown;
        if (TYPEOF(s) != PROMSXP) {
            state = ObservedValues::StateBeforeLastForce::value;
        } else if (PRVALUE(s) != R_UnboundValue) {
            state = ObservedValues::StateBeforeLastForce::evaluatedPromise;
        } else {
            // This is a lazy loading stub, it replaces the promise with the
            // actual value. From now on it will be a value...
            if (CAR(PREXPR(s)) == symbol::lazyLoadDBfetch)
                state = ObservedValues::StateBeforeLastForce::value;
            else
                state = ObservedValues::StateBeforeLastForce::promise;
        }

        auto idx = *(Immediate*)(pc + 1);
        // FIXME: cf. #1260
        c->function()->typeFeedback()->record_type(idx, [&](auto& feedback) {
            if (feedback.stateBeforeLastForce < state) {
                feedback.stateBeforeLastForce = state;
            }
        });
    };

    auto function = c->function();
    auto typeFeedback = function->typeFeedback();

    // main loop
    BEGIN_MACHINE {

        INSTRUCTION(invalid_) assert(false && "wrong or unimplemented opcode");

        INSTRUCTION(nop_) NEXT();

        INSTRUCTION(clear_binding_cache_) {
            size_t start = readImmediate();
            advanceImmediate();
            size_t len = readImmediate();
            advanceImmediate();
            if (len) {
                SLOWASSERT(start + len <= bindingCache->length);
                memset(&bindingCache->entry[start], 0,
                       sizeof(BindingCacheEntry) * len);
            }
            NEXT();
        }

        INSTRUCTION(ldfun_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            SEXP res = Rf_findFun(sym, env);

            // TODO something should happen here
            if (res == R_UnboundValue)
                assert(false && "Unbound var");
            if (res == R_MissingArg)
                assert(false && "Missing argument");

            switch (TYPEOF(res)) {
            case CLOSXP:
                // Prevent recompilation loops in lapply and similar
                if (sym != symbol::FUN)
                    jit(res, sym);
                break;
            case SPECIALSXP:
            case BUILTINSXP:
                // special and builtin functions are ok
                break;
            default:
                Rf_error("attempt to apply non-function");
            }
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ldvar_for_update_) {
            Immediate id = readImmediate();
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            R_varloc_t loc = R_findVarLocInFrame(env, cp_pool_at(id));
            bool isLocal = !R_VARLOC_IS_NULL(loc);
            SEXP res = nullptr;

            if (isLocal && CAR(loc.cell) != R_UnboundValue) {
                res = CAR(loc.cell);
            } else {
                SEXP sym = cp_pool_at(id);
                res = Rf_findVar(sym, ENCLOS(env));
            }

            if (res == R_UnboundValue) {
                SEXP sym = cp_pool_at(id);
                Rf_error("object '%s' not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                SEXP sym = cp_pool_at(id);
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res);

            if (res != R_NilValue) {
                if (isLocal)
                    ENSURE_NAMED(res);
                else
                    res = Rf_shallow_duplicate(res);
            }

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ldvar_for_update_cache_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate cacheIndex = readImmediate();
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            SEXP loc = getCellFromCache(env, id, cacheIndex, bindingCache);
            bool isLocal = loc;
            SEXP res = nullptr;

            if (isLocal && CAR(loc) != R_UnboundValue) {
                res = CAR(loc);
            } else {
                SEXP sym = cp_pool_at(id);
                res = Rf_findVar(sym, ENCLOS(env));
            }

            if (res == R_UnboundValue) {
                SEXP sym = cp_pool_at(id);
                Rf_error("object '%s' not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                SEXP sym = cp_pool_at(id);
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res);

            if (res != R_NilValue) {
                if (isLocal)
                    ENSURE_NAMED(res);
                else
                    res = Rf_shallow_duplicate(res);
            }

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ldvar_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            SEXP res = Rf_findVar(sym, env);
            R_Visible = TRUE;

            recordForceBehavior(res);

            if (res == R_UnboundValue) {
                Rf_error("object '%s' not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            } else if (TYPEOF(res) == PROMSXP) {
                // if promise, evaluate & return
                res = evaluatePromise(res);
            }

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            SEXP res = Rf_findVar(sym, env);
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object '%s' not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            } else if (TYPEOF(res) == PROMSXP) {
                // if already evaluated, return the value
                if (PRVALUE(res) && PRVALUE(res) != R_UnboundValue) {
                    res = PRVALUE(res);
                    assert(TYPEOF(res) != PROMSXP);

                    if (res != R_NilValue)
                        ENSURE_NAMED(res);
                }
            }

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ldvar_cached_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate cacheIndex = readImmediate();
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            SEXP res = cachedGetVar(env, id, cacheIndex, bindingCache);
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                SEXP sym = cp_pool_at(id);
                Rf_error("object '%s' not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                SEXP sym = cp_pool_at(id);
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ldvar_super_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            SEXP res = Rf_findVar(sym, ENCLOS(env));

            if (res == R_UnboundValue) {
                Rf_error("object '%s' not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ldddvar_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            SEXP res = Rf_ddfindVar(sym, env);

            if (res == R_UnboundValue) {
                Rf_error("object '%s' not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(stvar_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SEXP val = ostack_top();

            assert(!LazyEnvironment::check(env));

            rirDefineVarWrapper(sym, val, env);
            ostack_pop();
            NEXT();
        }

        INSTRUCTION(stvar_cached_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate cacheIndex = readImmediate();
            advanceImmediate();
            SEXP val = ostack_pop();

            assert(!LazyEnvironment::check(env));

            cachedSetVar(val, env, id, cacheIndex, bindingCache);
            NEXT();
        }

        INSTRUCTION(stvar_super_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SEXP val = ostack_pop();
            auto le = LazyEnvironment::check(env);
            assert(!le || !le->materialized());
            SEXP superEnv;
            if (le)
                superEnv = le->getParent();
            else
                superEnv = ENCLOS(env);
            rirSetVarWrapper(sym, val, superEnv);
            NEXT();
        }

        INSTRUCTION(record_call_) {
            Immediate idx = readImmediate();
            advanceImmediate();
            SEXP callee = ostack_top();
            typeFeedback->record_callee(idx, function, callee);
            NEXT();
        }

        INSTRUCTION(record_test_) {
            Immediate idx = readImmediate();
            advanceImmediate();
            SEXP t = ostack_top();
            typeFeedback->record_test(idx, t);
            NEXT();
        }

        INSTRUCTION(record_type_) {
            Immediate idx = readImmediate();
            advanceImmediate();
            SEXP t = ostack_top();
            typeFeedback->record_type(idx, t);
            NEXT();
        }

        INSTRUCTION(call_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length();
            int ttt = R_PPStackTop;
#endif

            // Stack contains [callee, arg1, ..., argn]
            Immediate n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Context given(pc);
            pc += sizeof(Context);

            CallContext call(ArglistOrder::NOT_REORDERED, c, ostack_at(n), n,
                             ast, ostack_cell_at((long)n - 1), env, R_NilValue,
                             given);
            SEXP res = doCall(call);
            ostack_popn(call.passedArgs + 1);
            ostack_push(res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs == ostack_length());

            if (call.triggerOsr) {
                if (auto res = osr(callCtxt, basePtr, env, c, pc)) {
                    return res;
                }
            }

            NEXT();
        }

        INSTRUCTION(named_call_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length();
            int ttt = R_PPStackTop;
#endif

            // Stack contains [callee, arg1, ..., argn]
            Immediate n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Context given(pc);
            pc += sizeof(Context);
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(ArglistOrder::NOT_REORDERED, c, ostack_at(n), n,
                             ast, ostack_cell_at((long)n - 1), names, env,
                             R_NilValue, given);
            SEXP res = doCall(call);
            ostack_popn(call.passedArgs + 1);
            ostack_push(res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs == ostack_length());

            if (call.triggerOsr) {
                if (auto res = osr(callCtxt, basePtr, env, c, pc)) {
                    return res;
                }
            }

            NEXT();
        }

        INSTRUCTION(call_dots_) {
#ifdef ENABLE_SLOWASSERT
            int ttt = R_PPStackTop;
            auto lll = ostack_length();
#endif

            // Stack contains [callee, arg1, ..., argn]
            Immediate n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Context given(pc);
            pc += sizeof(Context);
            auto names_ = (Immediate*)pc;
            advanceImmediateN(n);

            SEXP callee = ostack_at(n);
            Immediate* names = names_;
            int pushed = 0;
#ifdef ENABLE_SLOWASSERT
            auto oldn = n;
#endif
            if (needsExpandedDots(callee)) {
                n = expandDotDotDotCallArgs(
                    n, names_, env,
                    given.includes(Assumption::StaticallyArgmatched));
                auto namesStore = ostack_at(n);
                if (namesStore == R_NilValue)
                    names = nullptr;
                else
                    names = (Immediate*)DATAPTR(namesStore);
                pushed = 1;
            }
            CallContext call(ArglistOrder::NOT_REORDERED, c, callee, n, ast,
                             ostack_cell_at((long)n - 1), names, env,
                             R_NilValue, given);

            SEXP res = doCall(call);
            ostack_popn(call.passedArgs + 1 + pushed);
            ostack_push(res);

            SLOWASSERT(lll - oldn == ostack_length());
            SLOWASSERT(ttt == R_PPStackTop);

            if (call.triggerOsr) {
                if (auto res = osr(callCtxt, basePtr, env, c, pc)) {
                    return res;
                }
            }
            NEXT();
        }

        INSTRUCTION(call_builtin_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length();
            int ttt = R_PPStackTop;
#endif

            // Stack contains [arg1, ..., argn], callee is immediate
            Immediate n = readImmediate();
            advanceImmediate();
            Immediate ast = readImmediate();
            advanceImmediate();
            SEXP callee = cp_pool_at(readImmediate());
            advanceImmediate();
            CallContext call(ArglistOrder::NOT_REORDERED, c, callee, n, ast,
                             ostack_cell_at((long)n - 1), env, R_NilValue,
                             Context());
            SEXP res = doCall(call);
            ostack_popn(call.passedArgs);
            ostack_push(res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs + 1 == ostack_length());
            NEXT();
        }

        INSTRUCTION(close_) {
            SEXP srcref = ostack_at(0);
            SEXP body = ostack_at(1);
            SEXP formals = ostack_at(2);
            SEXP res = Rf_allocSExp(CLOSXP);
            if (!DispatchTable::check(body)) {
                std::cout << body << "\n";
                Rf_PrintValue(body);
                assert(false);
            }
            SET_FORMALS(res, formals);
            SET_BODY(res, body);
            SET_CLOENV(res, env);
            Rf_setAttrib(res, symbol::srcref, srcref);
            ostack_popn(3);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(check_function_) {
            SEXP val = ostack_top();

            switch (TYPEOF(val)) {
            case CLOSXP:
                jit(val, R_NilValue);
                break;
            case SPECIALSXP:
            case BUILTINSXP:
                // builtins and specials are fine
                // TODO for now - we might be fancier here later
                break;
            default:
                Rf_error("attempt to apply non-function");
            }
            NEXT();
        }

        INSTRUCTION(mk_eager_promise_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP prom = Rf_mkPROMISE(c->getPromise(id)->container(), env);
            SEXP val = ostack_pop();
            assert(TYPEOF(val) != PROMSXP);
            ENSURE_NAMEDMAX(val);
            SET_PRVALUE(prom, val);
            ostack_push(prom);
            NEXT();
        }

        INSTRUCTION(mk_promise_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP prom = Rf_mkPROMISE(c->getPromise(id)->container(), env);
            ostack_push(prom);
            NEXT();
        }

        INSTRUCTION(force_) {
            if (TYPEOF(ostack_top()) == PROMSXP) {
                SEXP val = ostack_pop();
                // If the promise is already evaluated then push the value
                // inside the promise onto the stack, otherwise push the value
                // from forcing the promise
                ostack_push(evaluatePromise(val));
            }
            NEXT();
        }

        INSTRUCTION(push_) {
            SEXP res = readConst(readImmediate());
            advanceImmediate();
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(dup_) {
            ostack_push(ostack_top());
            NEXT();
        }

        INSTRUCTION(dup2_) {
            ostack_push(ostack_at(1));
            ostack_push(ostack_at(1));
            NEXT();
        }

        INSTRUCTION(pop_) {
            ostack_pop();
            NEXT();
        }

        INSTRUCTION(popn_) {
            Immediate i = readImmediate();
            advanceImmediate();
            ostack_popn(i);
            NEXT();
        }

        INSTRUCTION(swap_) {
            SEXP lhs = ostack_pop();
            SEXP rhs = ostack_pop();
            ostack_push(lhs);
            ostack_push(rhs);
            NEXT();
        }

        INSTRUCTION(put_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t* pos = ostack_cell_at(0);
            SEXP val = pos->u.sxpval;
            while (i--) {
                pos->u.sxpval = (pos - 1)->u.sxpval;
                pos--;
            }
            pos->u.sxpval = val;
            NEXT();
        }

        INSTRUCTION(pick_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t* pos = ostack_cell_at(i);
            SEXP val = pos->u.sxpval;
            while (i--) {
                pos->u.sxpval = (pos + 1)->u.sxpval;
                pos++;
            }
            pos->u.sxpval = val;
            NEXT();
        }

        INSTRUCTION(pull_) {
            Immediate i = readImmediate();
            advanceImmediate();
            SEXP val = ostack_at(i);
            ostack_push(val);
            NEXT();
        }

        INSTRUCTION(inc_) {
            SEXP val = ostack_top();
            SLOWASSERT(TYPEOF(val) == INTSXP);
            if (MAYBE_REFERENCED(val)) {
                int i = INTEGER(val)[0];
                ostack_pop();
                SEXP n = Rf_allocVector(INTSXP, 1);
                INTEGER(n)[0] = i + 1;
                ostack_push(n);
            } else {
                INTEGER(val)[0]++;
            }
            NEXT();
        }

        INSTRUCTION(uplus_) {
            SEXP val = ostack_at(0);
            SEXP res = nullptr;
            DO_UNOP(+, Unop::PLUSOP);
            NEXT();
        }

        INSTRUCTION(uminus_) {
            SEXP val = ostack_at(0);
            SEXP res = nullptr;
            DO_UNOP(-, Unop::MINUSOP);
            NEXT();
        }

        INSTRUCTION(not_) {
            SEXP val = ostack_at(0);
            SEXP res = nullptr;

            if (IS_SIMPLE_SCALAR(val, LGLSXP)) {
                if (*LOGICAL(val) == NA_LOGICAL) {
                    res = R_LogicalNAValue;
                } else {
                    res = *LOGICAL(val) == 0 ? R_TrueValue : R_FalseValue;
                }
            } else if (IS_SIMPLE_SCALAR(val, REALSXP)) {
                if (*REAL(val) == NA_REAL) {
                    res = R_LogicalNAValue;
                } else {
                    res = *REAL(val) == 0.0 ? R_TrueValue : R_FalseValue;
                }
            } else if (IS_SIMPLE_SCALAR(val, INTSXP)) {
                if (*INTEGER(val) == NA_INTEGER) {
                    res = R_LogicalNAValue;
                } else {
                    res = *INTEGER(val) == 0 ? R_TrueValue : R_FalseValue;
                }
            } else {
                UNOP_FALLBACK("!");
            }

            ostack_popn(1);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(add_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_BINOP(+, Binop::PLUSOP);
            NEXT();
        }

        INSTRUCTION(sub_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_BINOP(-, Binop::MINUSOP);
            NEXT();
        }

        INSTRUCTION(mul_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_BINOP(*, Binop::TIMESOP);
            NEXT();
        }

        INSTRUCTION(div_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res =
                    (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)
                        ? NA_REAL
                        : *REAL(lhs) / *REAL(rhs);
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                double real_res;
                int r = *INTEGER(rhs);
                if (*REAL(lhs) == NA_REAL || r == NA_INTEGER)
                    real_res = NA_REAL;
                else
                    real_res = *REAL(lhs) / (double)r;
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res;
                int l = *INTEGER(lhs);
                if (l == NA_INTEGER || *REAL(rhs) == NA_REAL)
                    real_res = NA_REAL;
                else
                    real_res = (double)l / *REAL(rhs);
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                double real_res;
                int l = *INTEGER(lhs);
                int r = *INTEGER(rhs);
                if (l == NA_INTEGER || r == NA_INTEGER)
                    real_res = NA_REAL;
                else
                    real_res = (double)l / (double)r;
                STORE_BINOP(REALSXP, 0, real_res);
            } else {
                BINOP_FALLBACK("/");
                ostack_popn(2);
                ostack_push(res);
            }
            NEXT();
        }

        INSTRUCTION(idiv_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = myfloor(*REAL(lhs), *REAL(rhs));
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                double real_res;
                int r = *INTEGER(rhs);
                if (r == NA_INTEGER) {
                    real_res = NA_REAL;
                } else {
                    real_res = myfloor(*REAL(lhs), (double)r);
                }
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res;
                int l = *INTEGER(lhs);
                if (l == NA_INTEGER) {
                    real_res = NA_REAL;
                } else {
                    real_res = myfloor((double)l, *REAL(rhs));
                }
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                int int_res;
                int l = *INTEGER(lhs);
                int r = *INTEGER(rhs);
                /* This had x %/% 0 == 0 prior to 2.14.1, but
                   it seems conventionally to be undefined */
                if (l == NA_INTEGER || r == NA_INTEGER || r == 0)
                    int_res = NA_INTEGER;
                else
                    int_res = (int)floor((double)l / (double)r);
                STORE_BINOP(INTSXP, int_res, 0);
            } else {
                BINOP_FALLBACK("%/%");
                ostack_popn(2);
                ostack_push(res);
            }
            NEXT();
        }

        INSTRUCTION(mod_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = myfmod(*REAL(lhs), *REAL(rhs));
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                double real_res;
                int r = *INTEGER(rhs);
                if (r == NA_INTEGER) {
                    real_res = NA_REAL;
                } else {
                    real_res = myfmod(*REAL(lhs), (double)r);
                }
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res;
                int l = *INTEGER(lhs);
                if (l == NA_INTEGER && *REAL(rhs) != 0.0) {
                    real_res = NA_REAL;
                } else {
                    real_res = myfmod((double)l, *REAL(rhs));
                }
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                int int_res;
                int l = *INTEGER(lhs);
                int r = *INTEGER(rhs);
                if (l == NA_INTEGER || r == NA_INTEGER || r == 0) {
                    int_res = NA_INTEGER;
                } else {
                    int_res = (l >= 0 && r > 0)
                                  ? l % r
                                  : (int)myfmod((double)l, (double)r);
                }
                STORE_BINOP(INTSXP, int_res, 0);
            } else {
                BINOP_FALLBACK("%%");
                ostack_popn(2);
                ostack_push(res);
            }
            NEXT();
        }

        INSTRUCTION(pow_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            BINOP_FALLBACK("^");
            ostack_popn(2);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(eq_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_RELOP(==);
            ostack_popn(2);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ne_) {
            assert(R_PPStackTop >= 0);
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_RELOP(!=);
            ostack_popn(2);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(lt_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_RELOP(<);
            ostack_popn(2);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(le_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_RELOP(<=);
            ostack_popn(2);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(gt_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_RELOP(>);
            ostack_popn(2);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(ge_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;
            DO_RELOP(>=);
            ostack_popn(2);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(lgl_and_) {
            SEXP s2 = ostack_pop();
            SEXP s1 = ostack_pop();
            assert(TYPEOF(s2) == LGLSXP);
            assert(TYPEOF(s1) == LGLSXP);
            int x2 = XLENGTH(s2) == 0 ? NA_LOGICAL : LOGICAL(s2)[0];
            int x1 = XLENGTH(s1) == 0 ? NA_LOGICAL : LOGICAL(s1)[0];
            assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            assert(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
            if (x1 == 1 && x2 == 1)
                ostack_push(R_TrueValue);
            else if (x1 == 0 || x2 == 0)
                ostack_push(R_FalseValue);
            else
                ostack_push(R_LogicalNAValue);
            NEXT();
        }

        INSTRUCTION(lgl_or_) {
            SEXP s2 = ostack_pop();
            SEXP s1 = ostack_pop();
            assert(TYPEOF(s2) == LGLSXP);
            assert(TYPEOF(s1) == LGLSXP);
            int x2 = XLENGTH(s2) == 0 ? NA_LOGICAL : LOGICAL(s2)[0];
            int x1 = XLENGTH(s1) == 0 ? NA_LOGICAL : LOGICAL(s1)[0];
            assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            assert(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
            if (x1 == 1 || x2 == 1)
                ostack_push(R_TrueValue);
            else if (x1 == 0 && x2 == 0)
                ostack_push(R_FalseValue);
            else
                ostack_push(R_LogicalNAValue);
            NEXT();
        }

        INSTRUCTION(colon_) {

            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);
            SEXP res = nullptr;

            if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {
                int from = *INTEGER(lhs);
                if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                    int to = *INTEGER(rhs);
                    if (from != NA_INTEGER && to != NA_INTEGER) {
                        res = seq_int(from, to);
                    }
                } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                    double to = *REAL(rhs);
                    if (from != NA_INTEGER && to != NA_REAL && R_FINITE(to) &&
                        INT_MIN <= to && INT_MAX >= to && to == (int)to) {
                        res = seq_int(from, (int)to);
                    }
                }
            } else if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {
                double from = *REAL(lhs);
                if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                    int to = *INTEGER(rhs);
                    if (from != NA_REAL && to != NA_INTEGER && R_FINITE(from) &&
                        INT_MIN <= from && INT_MAX >= from &&
                        from == (int)from) {
                        res = seq_int((int)from, to);
                    }
                } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                    double to = *REAL(rhs);
                    if (from != NA_REAL && to != NA_REAL && R_FINITE(from) &&
                        R_FINITE(to) && INT_MIN <= from && INT_MAX >= from &&
                        INT_MIN <= to && INT_MAX >= to && from == (int)from &&
                        to == (int)to) {
                        res = seq_int((int)from, (int)to);
                    }
                }
            }

            if (res) {
                R_Visible = (Rboolean) true;
            } else {
                BINOP_FALLBACK(":");
            }

            ostack_popn(2);
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(identical_noforce_) {
            SEXP rhs = ostack_pop();
            SEXP lhs = ostack_pop();
            // This instruction does not force, but we should still compare
            // the actual promise value if it is already forced.
            // Especially important since all the inlined functions are probably
            // behind lazy loading stub promises.
            if (TYPEOF(rhs) == PROMSXP && PRVALUE(rhs) != R_UnboundValue)
                rhs = PRVALUE(rhs);
            if (TYPEOF(lhs) == PROMSXP && PRVALUE(lhs) != R_UnboundValue)
                lhs = PRVALUE(lhs);
            // Special case for closures: (level 1) deep compare with body
            // expression instead of body object, to ensure that a compiled
            // closure is equal to the uncompiled one
            if (lhs != rhs && TYPEOF(lhs) == CLOSXP && TYPEOF(rhs) == CLOSXP &&
                CLOENV(lhs) == CLOENV(rhs) && FORMALS(lhs) == FORMALS(rhs) &&
                BODY_EXPR(lhs) == BODY_EXPR(rhs))
                ostack_push(R_TrueValue);
            else
                ostack_push(rhs == lhs ? R_TrueValue : R_FalseValue);

            NEXT();
        }

        INSTRUCTION(aslogical_) {
            SEXP val = ostack_top();
            // TODO
            // 1. currently aslogical_ is used for &&, || only, and this
            // checking
            //    is to mimic the behavior of builtin &&, ||. Technically
            //    asLogical is less strict than this.
            // 2. the error message also doesn't suggest which argument is
            // wrong, or
            //    which boolean operation it was. To achieve the exact behavior,
            //    one could potentially compile this check in `ir/Compiler.cpp`
            if (!Rf_isNumber(val)) {
                SEXP call = getSrcAt(c, pc - 1);
                Rf_errorcall(call, "argument has the wrong type for && or ||");
            }
            int x1 = Rf_asLogical(val);
            assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            SEXP res = Rf_ScalarLogical(x1);
            ostack_pop();
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(asbool_) {
            SEXP val = ostack_top();
            int cond = NA_LOGICAL;
            if (XLENGTH(val) > 1)
                Rf_warningcall(
                    getSrcAt(c, pc - 1),
                    "the condition has length > 1 and only the first "
                    "element will be used");

            if (XLENGTH(val) > 0) {
                switch (TYPEOF(val)) {
                case LGLSXP:
                    cond = LOGICAL(val)[0];
                    break;
                case INTSXP:
                    cond =
                        INTEGER(val)[0]; // relies on NA_INTEGER == NA_LOGICAL
                    break;
                default:
                    cond = Rf_asLogical(val);
                }
            }

            if (cond == NA_LOGICAL) {
                const char* msg =
                    XLENGTH(val)
                        ? (Rf_isLogical(val)
                               ? ("missing value where TRUE/FALSE needed")
                               : ("argument is not interpretable as logical"))
                        : ("argument is of length zero");
                Rf_errorcall(getSrcAt(c, pc - 1), msg);
            }

            ostack_pop();
            ostack_push(cond ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(colon_input_effects_) {
            SEXP lhs = ostack_at(1);
            SEXP rhs = ostack_at(0);

            bool fastcase = colonInputEffects(lhs, rhs, 0);
            ostack_push(fastcase ? R_TrueValue : R_FalseValue);

            NEXT();
        }

        INSTRUCTION(colon_cast_lhs_) {
            SEXP lhs = ostack_pop();
            SEXP newLhs = colonCastLhs(lhs);
            ostack_push(newLhs);
            NEXT();
        }

        INSTRUCTION(colon_cast_rhs_) {
            SEXP rhs = ostack_pop();
            SEXP newLhs = ostack_top();
            SEXP newRhs = colonCastRhs(newLhs, rhs);
            ostack_push(newRhs);
            NEXT();
        }

        INSTRUCTION(is_) {
            SEXP val = ostack_pop();
            Immediate type = readImmediate();
            advanceImmediate();
            bool res = false;
            switch ((BC::RirTypecheck)type) {
            case BC::RirTypecheck::isNILSXP:
            case BC::RirTypecheck::isLGLSXP:
            case BC::RirTypecheck::isREALSXP:
            case BC::RirTypecheck::isSTRSXP:
            case BC::RirTypecheck::isINTSXP:
            case BC::RirTypecheck::isCPLXSXP:
            case BC::RirTypecheck::isRAWSXP:
            case BC::RirTypecheck::isEXPRSXP:
                res = TYPEOF(val) == type;
                break;

            case BC::RirTypecheck::isVECSXP:
                res = TYPEOF(val) == VECSXP || TYPEOF(val) == LISTSXP;
                break;

            case BC::RirTypecheck::isLISTSXP:
                res = TYPEOF(val) == LISTSXP || TYPEOF(val) == NILSXP;
                break;
            case BC::RirTypecheck::isNonObject:
                res = !Rf_isObject(val);
                break;
            case BC::RirTypecheck::isFactor:
                res = TYPEOF(val) == INTSXP && Rf_inherits(val, "factor");
                break;
            case BC::RirTypecheck::isVector:
                res = Rf_isVector(val);
                break;
#ifdef ENABLE_SLOWASSERT
            default:
                assert(false);
                res = false;
                break;
#endif
            }
            ostack_push(res ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(missing_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SLOWASSERT(!DDVAL(sym));
            assert(env);
            ostack_push(isMissing(sym, env, c, pc) ? R_TrueValue
                                                   : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(brtrue_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (ostack_pop() == R_TrueValue) {
                checkUserInterrupt();
                pc += offset;
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brfalse_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (ostack_pop() == R_FalseValue) {
                checkUserInterrupt();
                pc += offset;
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(br_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            checkUserInterrupt();
            pc += offset;
            PC_BOUNDSCHECK(pc, c);
            // TODO: why does osr-in deserialized code break?
            if (!pir::Parameter::RIR_SERIALIZE_CHAOS) {
                static size_t loopCounter = 0;
                if (offset < 0 && ++loopCounter >= osrLimit) {
                    REC_HOOK(recording::recordOsrTriggerLoop(loopCounter));
                    loopCounter = 0;
                    if (auto res = osr(callCtxt, basePtr, env, c, pc))
                        return res;
                    REC_HOOK(recording::recordReasonsClear());
                }
            }
            NEXT();
        }

        INSTRUCTION(extract1_1_) {
            SEXP val = ostack_at(1);
            SEXP idx = ostack_at(0);

            SEXP args = CONS_NR(val, CONS_NR(idx, R_NilValue));
            ostack_push(args);

            SEXP res = nullptr;
            if (Rf_isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1);
                res = dispatchApply(call, val, args, symbol::Bracket, env);
                if (!res) {
                    forceAll(args);
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
                }
            } else {
                forceAll(args);
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            }

            ostack_popn(3);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(extract1_2_) {
            SEXP val = ostack_at(2);
            SEXP idx = ostack_at(1);
            SEXP idx2 = ostack_at(0);

            SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
            ostack_push(args);

            SEXP res = nullptr;
            if (Rf_isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1);
                res = dispatchApply(call, val, args, symbol::Bracket, env);
                if (!res) {
                    forceAll(args);
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
                }
            } else {
                forceAll(args);
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            }

            ostack_popn(4);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(extract1_3_) {
            SEXP val = ostack_at(3);
            SEXP idx = ostack_at(2);
            SEXP idx2 = ostack_at(1);
            SEXP idx3 = ostack_at(0);

            SEXP args = CONS_NR(
                val, CONS_NR(idx, CONS_NR(idx2, CONS_NR(idx3, R_NilValue))));
            ostack_push(args);

            SEXP res = nullptr;
            if (Rf_isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1);
                res = dispatchApply(call, val, args, symbol::Bracket, env);
                if (!res) {
                    forceAll(args);
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
                }
            } else {
                forceAll(args);
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            }

            ostack_popn(5);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(extract2_1_) {
            SEXP val = ostack_at(1);
            SEXP idx = ostack_at(0);
            SEXP res = nullptr;
            int i = -1;

            if (ATTRIB(val) != R_NilValue || ATTRIB(idx) != R_NilValue)
                goto fallback;

            switch (TYPEOF(idx)) {
            case REALSXP:
                if (STDVEC_LENGTH(idx) != 1 || *REAL(idx) == NA_REAL)
                    goto fallback;
                i = (int)*REAL(idx) - 1;
                break;
            case INTSXP:
                if (STDVEC_LENGTH(idx) != 1 || *INTEGER(idx) == NA_INTEGER)
                    goto fallback;
                i = *INTEGER(idx) - 1;
                break;
            case LGLSXP:
                if (STDVEC_LENGTH(idx) != 1 || *LOGICAL(idx) == NA_LOGICAL)
                    goto fallback;
                i = (int)*LOGICAL(idx) - 1;
                break;
            default:
                goto fallback;
            }

            if (i >= XLENGTH(val) || i < 0)
                goto fallback;

            switch (TYPEOF(val)) {

#define SIMPLECASE(vectype, vecaccess)                                         \
    case vectype: {                                                            \
        if (XLENGTH(val) == 1 && NO_REFERENCES(val)) {                         \
            res = val;                                                         \
        } else {                                                               \
            res = Rf_allocVector(vectype, 1);                                  \
            vecaccess(res)[0] = vecaccess(val)[i];                             \
        }                                                                      \
        break;                                                                 \
    }

                SIMPLECASE(REALSXP, REAL);
                SIMPLECASE(INTSXP, INTEGER);
                SIMPLECASE(LGLSXP, LOGICAL);
#undef SIMPLECASE

            case VECSXP: {
                res = VECTOR_ELT(val, i);
                ENSURE_NAMED(res);
                break;
            }

            default:
                goto fallback;
            }

            ostack_popn(2);
            ostack_push(res);
            R_Visible = (Rboolean) true;
            NEXT();

        // ---------
        fallback : {
            SEXP args = CONS_NR(val, CONS_NR(idx, R_NilValue));
            ostack_push(args);
            if (Rf_isObject(val)) {
                SEXP call = getSrcAt(c, pc - 1);
                res =
                    dispatchApply(call, val, args, symbol::DoubleBracket, env);
                if (!res) {
                    forceAll(args);
                    res =
                        do_subset2_dflt(call, symbol::DoubleBracket, args, env);
                }
            } else {
                forceAll(args);
                res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args,
                                      env);
            }
            ostack_popn(3);

            ostack_push(res);
            NEXT();
        }
        }

        INSTRUCTION(extract2_2_) {
            SEXP val = ostack_at(2);
            SEXP idx = ostack_at(1);
            SEXP idx2 = ostack_at(0);

            SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
            ostack_push(args);

            SEXP res = nullptr;
            if (Rf_isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1);
                res =
                    dispatchApply(call, val, args, symbol::DoubleBracket, env);
                if (!res) {
                    forceAll(args);
                    res =
                        do_subset2_dflt(call, symbol::DoubleBracket, args, env);
                }
            } else {
                forceAll(args);
                res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args,
                                      env);
            }
            ostack_popn(4);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(subassign1_1_) {
            SEXP idx = ostack_at(0);
            SEXP vec = ostack_at(1);
            SEXP val = ostack_at(2);

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(vec)) {
                vec = Rf_shallow_duplicate(vec);
                ostack_set(1, vec);
            }

            SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
            SET_TAG(CDDR(args), symbol::value);
            PROTECT(args);

            SEXP call = getSrcForCall(c, pc - 1);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignBracket);
            SEXP res = nullptr;
            if (Rf_isObject(vec)) {
                res =
                    dispatchApply(call, vec, args, symbol::AssignBracket, env);
            }
            if (!res) {
                res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
                // We duplicated the vector above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(3);
            UNPROTECT(1);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(subassign1_2_) {
            SEXP idx2 = ostack_at(0);
            SEXP idx1 = ostack_at(1);
            SEXP mtx = ostack_at(2);
            SEXP val = ostack_at(3);

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_shallow_duplicate(mtx);
                ostack_set(2, mtx);
            }

            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
            SET_TAG(CDDDR(args), symbol::value);
            PROTECT(args);

            SEXP call = getSrcForCall(c, pc - 1);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignBracket);
            SEXP res = nullptr;
            if (Rf_isObject(mtx)) {
                res =
                    dispatchApply(call, mtx, args, symbol::AssignBracket, env);
            }

            if (!res) {
                res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
                // We duplicated the matrix above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(4);
            UNPROTECT(1);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(subassign1_3_) {
            SEXP idx3 = ostack_at(0);
            SEXP idx2 = ostack_at(1);
            SEXP idx1 = ostack_at(2);
            SEXP mtx = ostack_at(3);
            SEXP val = ostack_at(4);

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_shallow_duplicate(mtx);
                ostack_set(2, mtx);
            }

            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1,
                             CONS_NR(idx2,
                                     CONS_NR(idx3, CONS_NR(val, R_NilValue)))));
            SET_TAG(CDDDR(args), symbol::value);
            PROTECT(args);

            SEXP call = getSrcForCall(c, pc - 1);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignBracket);
            SEXP res = nullptr;
            if (Rf_isObject(mtx)) {
                res =
                    dispatchApply(call, mtx, args, symbol::AssignBracket, env);
            }

            if (!res) {
                res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
                // We duplicated the matrix above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(5);
            UNPROTECT(1);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(set_vec_elt_) {
            SEXP idx = ostack_at(0);
            SEXP vec = ostack_at(1);
            SEXP val = ostack_at(2);
            assert(IS_SIMPLE_SCALAR(idx, INTSXP));
            if (MAYBE_REFERENCED(val))
                val = Rf_lazy_duplicate(val);
            SET_VECTOR_ELT(vec, INTEGER(idx)[0] - 1, val);
            ostack_popn(3);
            ostack_push(vec);
            NEXT();
        }

        INSTRUCTION(subassign2_1_) {
            SEXP idx = ostack_at(0);
            SEXP vec = ostack_at(1);
            SEXP val = ostack_at(2);

            // Fast case
            if (NOT_SHARED(vec) && !Rf_isObject(vec)) {
                SEXPTYPE vectorT = TYPEOF(vec);
                SEXPTYPE valT = TYPEOF(val);
                SEXPTYPE idxT = TYPEOF(idx);

                // Fast case only if
                // 1. index is numerical and scalar
                // 2. vector is real and shape of value fits into real
                //      or vector is int and shape of value is int
                //      or vector is generic and value is not NULL
                // 3. value fits into one cell of the vector
                if ((idxT == INTSXP || idxT == REALSXP) &&
                    (XLENGTH(idx) == 1) && // 1
                    ((vectorT == REALSXP &&
                      (valT == REALSXP || valT == INTSXP)) || // 2
                     (vectorT == INTSXP && valT == INTSXP) ||
                     (vectorT == VECSXP && val != R_NilValue)) &&
                    (XLENGTH(val) == 1 || vectorT == VECSXP)) { // 3

                    int idx_ = -1;

                    if (idxT == REALSXP) {
                        if (*REAL(idx) != NA_REAL)
                            idx_ = (int)*REAL(idx) - 1;
                    } else {
                        if (*INTEGER(idx) != NA_INTEGER)
                            idx_ = *INTEGER(idx) - 1;
                    }

                    if (idx_ >= 0 && idx_ < XLENGTH(vec)) {
                        switch (vectorT) {
                        case REALSXP: {
                            double realRes = 0;
                            if (valT == REALSXP) {
                                realRes = *REAL(val);
                            } else {
                                if (*INTEGER(val) == NA_INTEGER) {
                                    realRes = R_NaN;
                                } else {
                                    realRes = (double)*INTEGER(val);
                                }
                            }
                            REAL(vec)[idx_] = realRes;
                            break;
                        }
                        case INTSXP:
                            INTEGER(vec)[idx_] = *INTEGER(val);
                            break;
                        case VECSXP:
                            // Avoid recursive vectors
                            if (val == vec)
                                val = Rf_shallow_duplicate(val);
                            SET_VECTOR_ELT(vec, idx_, val);
                            break;
                        }
                        ostack_popn(3);

                        ostack_push(vec);
                        NEXT();
                    }
                }
            }

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(vec)) {
                vec = Rf_shallow_duplicate(vec);
                ostack_set(1, vec);
            }

            SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
            SET_TAG(CDDR(args), symbol::value);
            PROTECT(args);

            SEXP call = getSrcForCall(c, pc - 1);

            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignDoubleBracket);
            SEXP res = nullptr;
            if (Rf_isObject(vec)) {
                res = dispatchApply(call, vec, args,
                                    symbol::AssignDoubleBracket, env);
            }

            if (!res) {
                res = do_subassign2_dflt(call, symbol::AssignDoubleBracket,
                                         args, env);
                // We duplicated the vector above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(3);
            UNPROTECT(1);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(subassign2_2_) {
            SEXP idx2 = ostack_at(0);
            SEXP idx1 = ostack_at(1);
            SEXP mtx = ostack_at(2);
            SEXP val = ostack_at(3);

            // Fast case
            if (NOT_SHARED(mtx) && !Rf_isObject(mtx)) {
                SEXPTYPE matrixT = TYPEOF(mtx);
                SEXPTYPE valT = TYPEOF(val);
                SEXPTYPE idx1T = TYPEOF(idx1);
                SEXPTYPE idx2T = TYPEOF(idx2);

                // Fast case only if
                // 1. index is numerical and scalar
                // 2. matrix is real and shape of value fits into real
                //      or matrix is int and shape of value is int
                //      or matrix is generic
                // 3. value fits into one cell of the matrix
                if ((idx1T == INTSXP || idx1T == REALSXP) &&
                    (XLENGTH(idx1) == 1) && // 1
                    (idx2T == INTSXP || idx2T == REALSXP) &&
                    (XLENGTH(idx2) == 1) &&
                    ((matrixT == REALSXP &&
                      (valT == REALSXP || valT == INTSXP)) || // 2
                     (matrixT == INTSXP && (valT == INTSXP)) ||
                     (matrixT == VECSXP)) &&
                    (XLENGTH(val) == 1 || matrixT == VECSXP)) { // 3

                    int idx1_ = -1;
                    int idx2_ = -1;

                    if (idx1T == REALSXP) {
                        if (*REAL(idx1) != NA_REAL)
                            idx1_ = (int)*REAL(idx1) - 1;
                    } else {
                        if (*INTEGER(idx1) != NA_INTEGER)
                            idx1_ = *INTEGER(idx1) - 1;
                    }

                    if (idx2T == REALSXP) {
                        if (*REAL(idx2) != NA_REAL)
                            idx2_ = (int)*REAL(idx1) - 1;
                    } else {
                        if (*INTEGER(idx2) != NA_INTEGER)
                            idx2_ = *INTEGER(idx2) - 1;
                    }

                    if (idx1_ >= 0 && idx1_ < Rf_nrows(mtx) && idx2_ >= 0 &&
                        idx2_ < Rf_ncols(mtx)) {
                        int idx_ = idx1_ + (idx2_ * Rf_nrows(mtx));
                        SEXPTYPE mtxT = TYPEOF(mtx);
                        switch (mtxT) {
                        case REALSXP:
                            REAL(mtx)
                            [idx_] = valT == REALSXP ? *REAL(val)
                                                     : (double)*INTEGER(val);
                            break;
                        case INTSXP:
                            INTEGER(mtx)[idx_] = *INTEGER(val);
                            break;
                        case VECSXP:
                            // Avoid recursive vectors
                            if (val == mtx)
                                val = Rf_shallow_duplicate(val);
                            SET_VECTOR_ELT(mtx, idx_, val);
                            break;
                        }
                        ostack_popn(4);

                        ostack_push(mtx);
                        NEXT();
                    }
                }
            }

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_shallow_duplicate(mtx);
                ostack_set(2, mtx);
            }

            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
            SET_TAG(CDDDR(args), symbol::value);
            PROTECT(args);

            SEXP call = getSrcForCall(c, pc - 1);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignDoubleBracket);
            SEXP res = nullptr;
            if (Rf_isObject(mtx)) {
                res = dispatchApply(call, mtx, args,
                                    symbol::AssignDoubleBracket, env);
            }

            if (!res) {
                res = do_subassign2_dflt(call, symbol::AssignDoubleBracket,
                                         args, env);
                // We duplicated the matrix above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(4);
            UNPROTECT(1);

            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(guard_fun_) {
            SEXP sym = readConst(readImmediate());
            advanceImmediate();
            SEXP guard = readConst(readImmediate());
            advanceImmediate();
            advanceImmediate();
            if (guard != Rf_findFun(sym, env))
                Rf_error("Invalid Callee");
            NEXT();
        }

        INSTRUCTION(names_) {
            ostack_push(Rf_getAttrib(ostack_pop(), R_NamesSymbol));
            NEXT();
        }

        INSTRUCTION(set_names_) {
            SEXP names = ostack_pop();
            Rf_setAttrib(ostack_top(), R_NamesSymbol, names);
            NEXT();
        }

        INSTRUCTION(length_) {
            SEXP res = Rf_ScalarInteger(Rf_length(ostack_pop()));
            ostack_push(res);
            NEXT();
        }

        INSTRUCTION(as_switch_idx_) {
            if (TYPEOF(ostack_top()) != INTSXP) {
                auto v = Rf_asInteger(ostack_pop());
                ostack_push(Rf_ScalarInteger(v == NA_INTEGER ? -1 : v));
            }
            NEXT();
        }

        INSTRUCTION(for_seq_size_) {
            SEXP seq = ostack_at(0);
            // TODO: we should extract the length just once at the begining of
            // the loop and generally have somthing more clever here...
            SEXP value = Rf_allocVector(INTSXP, 1);
            if (Rf_isVector(seq)) {
                INTEGER(value)[0] = LENGTH(seq);
            } else if (Rf_isList(seq) || Rf_isNull(seq)) {
                INTEGER(value)[0] = Rf_length(seq);
            } else {
                Rf_errorcall(R_NilValue, "invalid for() loop sequence");
            }
            // TODO: Even when the for loop sequence is an object, R won't
            // dispatch on it. Since in RIR we use the normals extract2_1
            // BC on it, we would. To prevent this we strip the object
            // flag here. What we should do instead, is use a non-dispatching
            // extract BC.
            if (Rf_isObject(seq)) {
                if (Rf_inherits(seq, "factor"))
                    seq = Rf_asCharacterFactor(seq);
                else
                    seq = Rf_shallow_duplicate(seq);
                SET_OBJECT(seq, 0);
                ostack_set(0, seq);
            }
            ENSURE_NAMEDMAX(seq);
            ostack_push(value);
            NEXT();
        }

        INSTRUCTION(visible_) {
            R_Visible = TRUE;
            NEXT();
        }

        INSTRUCTION(invisible_) {
            R_Visible = FALSE;
            NEXT();
        }

        INSTRUCTION(ensure_named_) {
            SEXP val = ostack_top();
            ENSURE_NAMED(val);
            NEXT();
        }

        INSTRUCTION(set_shared_) {
            SEXP val = ostack_top();
            if (NAMED(val) < 2)
                SET_NAMED(val, 2);
            NEXT();
        }

        INSTRUCTION(beginloop_) {
            SLOWASSERT(env);
            int offset = readJumpOffset();
            advanceJump();
            loopTrampoline(c, env, callCtxt, pc, bindingCache);
            pc += offset;
            checkUserInterrupt();
            assert(*pc == Opcode::endloop_);
            advanceOpcode();
            NEXT();
        }

        INSTRUCTION(endloop_) { return loopTrampolineMarker; }

        INSTRUCTION(return_) {
            SEXP res = ostack_pop();
            // this restores stack pointer to the value from the target context
            Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, env, res);
            // not reached
            assert(false);
        }

        INSTRUCTION(ret_) { goto eval_done; }

        INSTRUCTION(int3_) {
            asm("int3");
            NEXT();
        }

        LASTOP;
    }

eval_done:
    return ostack_pop();
}

#pragma GCC diagnostic pop

SEXP evalRirCodeExtCaller(Code* c, SEXP env) {
    return evalRirCode(c, env, nullptr);
}

SEXP rirApplyClosure(SEXP ast, SEXP op, SEXP arglist, SEXP rho,
                     SEXP suppliedvars) {

    size_t nargs = 0;
    Immediate* names = nullptr;
    {
        RList args(arglist);
        auto n = Pool::insert(R_NilValue);
        std::vector<Immediate> namesList;
        for (auto arg = args.begin(), end = args.end(); arg != end; ++arg) {
            ostack_push(*arg);
            if (arg.hasTag()) {
                namesList.resize(nargs + 1, n);
                namesList[nargs] = Pool::insert(arg.tag());
            }
            nargs++;
        }
        if (!namesList.empty()) {
            auto namesStore = Rf_allocVector(RAWSXP, sizeof(Immediate) * nargs);
            names = (Immediate*)RAW(namesStore);
            for (size_t i = 0; i < nargs; ++i) {
                if (i < namesList.size())
                    names[i] = namesList[i];
                else
                    names[i] = n;
            }
            PROTECT(namesStore);
        }
    }

    CallContext call(ArglistOrder::NOT_REORDERED, nullptr, op, nargs, ast,
                     ostack_cell_at((long)nargs - 1), names, rho, suppliedvars,
                     Context());
    call.arglist = arglist;
    call.safeForceArgs();

    auto res = doCall(call);
    ostack_popn(call.passedArgs);
    if (names)
        UNPROTECT(1);
    return res;
}

SEXP rirEval(SEXP what, SEXP env) {
    assert(TYPEOF(what) == EXTERNALSXP);

    // TODO: do we not need an RCNTXT here?

    if (auto code = Code::check(what)) {
        return evalRirCodeExtCaller(code, env);
    }

    if (auto table = DispatchTable::check(what)) {
        // TODO: add an adapter frame to be able to call something else than
        // the baseline version!
        Function* fun = table->baseline();
        fun->registerInvocation();
        auto res = evalRirCodeExtCaller(fun->body(), env);
        return res;
    }

    if (auto fun = Function::check(what)) {
        fun->registerInvocation();
        auto res = evalRirCodeExtCaller(fun->body(), env);
        return res;
    }

    assert(false && "Expected a code object or a dispatch table");
    return nullptr;
}

} // namespace rir
