#ifndef RIR_INTERPRETER_C_H
#define RIR_INTERPRETER_C_H

#include "builtins.h"
#include "call_context.h"
#include "instance.h"

#include "compiler/parameter.h"
#include "interp_incl.h"
#include "ir/Deoptimization.h"

#include "R/BuiltinIds.h"

#include <R/r.h>

#undef length

#if defined(__GNUC__) && (!defined(NO_THREADED_CODE))
#define THREADED_CODE
#endif

extern "C" void __asan_poison_memory_region(const volatile void* p, size_t n);

namespace rir {
SEXP dispatchApply(SEXP ast, SEXP obj, SEXP actuals, SEXP selector,
                   SEXP callerEnv, InterpreterInstance* ctx);
bool isMissing(SEXP symbol, SEXP environment, Code* code, Opcode* op);

inline RCNTXT* getFunctionContext(size_t pos = 0,
                                  RCNTXT* cptr = (RCNTXT*)R_GlobalContext) {
    while (cptr->nextcontext != NULL) {
        if (cptr->callflag & CTXT_FUNCTION) {
            if (pos == 0)
                return cptr;
            pos--;
        }
        cptr = cptr->nextcontext;
    }
    assert(false);
    return nullptr;
}

inline RCNTXT* findFunctionContextFor(SEXP e) {
    auto cptr = (RCNTXT*)R_GlobalContext;
    while (cptr->nextcontext != NULL) {
        if (cptr->callflag & CTXT_FUNCTION) {
            if (cptr->cloenv == e)
                return cptr;
        }
        cptr = cptr->nextcontext;
    }
    return nullptr;
}

inline bool RecompileHeuristic(DispatchTable* table, Function* fun,
                               unsigned factor = 1) {
    auto& flags = fun->flags;
    return (!flags.contains(Function::NotOptimizable) &&
            (flags.contains(Function::MarkOpt) ||
             (fun->deoptCount() < pir::Parameter::DEOPT_ABANDON &&
              ((fun != table->baseline() && fun->invocationCount() >= 2 &&
                fun->invocationCount() <= pir::Parameter::RIR_WARMUP) ||
               (fun->invocationCount() %
                (factor * (pir::Parameter::RIR_WARMUP))) == 0))));
}

inline bool RecompileCondition(DispatchTable* table, Function* fun,
                               const Context& context) {
    return (fun->flags.contains(Function::MarkOpt) ||
            fun == table->baseline() ||
            (context.smaller(fun->context()) && context.isImproving(fun)) ||
            fun->body()->flags.contains(Code::Reoptimise));
}

inline void DoRecompile(Function* fun, SEXP ast, SEXP callee, Context given,
                        InterpreterInstance* ctx) {
    // We have more assumptions available, let's recompile
    // More assumptions are available than this version uses. Let's
    // try compile a better matching version.
    auto flags = fun->flags;
#ifdef DEBUG_DISPATCH
    std::cout << "Optimizing for new context " << fun->invocationCount()
              << ": ";
    Rf_PrintValue(ast);
    std::cout << given << " vs " << fun->context() << "\n";
#endif
    SEXP lhs = CAR(ast);
    SEXP name = R_NilValue;
    if (TYPEOF(lhs) == SYMSXP)
        name = lhs;
    if (flags.contains(Function::MarkOpt))
        fun->flags.reset(Function::MarkOpt);
    ctx->closureOptimizer(callee, given, name);
}

inline bool matches(const CallContext& call, Function* f) {
    return call.givenContext.smaller(f->context());
}

inline Function* dispatch(const CallContext& call, DispatchTable* vt) {
    auto f = vt->dispatch(call.givenContext);
    assert(f);
    return f;
};

void inferCurrentContext(CallContext& call, size_t formalNargs,
                         InterpreterInstance* ctx);

SEXP builtinCall(CallContext& call, InterpreterInstance* ctx);
SEXP doCall(CallContext& call, InterpreterInstance* ctx);
size_t expandDotDotDotCallArgs(InterpreterInstance* ctx, size_t n,
                               Immediate* names_, SEXP env, bool explicitDots);
void deoptFramesWithContext(InterpreterInstance* ctx,
                            const CallContext* callCtxt,
                            DeoptMetadata* deoptData, SEXP sysparent,
                            size_t pos, size_t stackHeight,
                            RCNTXT* currentContext);
void recordDeoptReason(SEXP val, const DeoptReason& reason);
void jit(SEXP cls, SEXP name, InterpreterInstance* ctx);

SEXP seq_int(int n1, int n2);
bool doubleCanBeCastedToInteger(double n);
int colonInputEffects(SEXP lhs, SEXP rhs, unsigned srcIdx);
bool isColonFastcase(SEXP, SEXP);
SEXP colonCastLhs(SEXP lhs);
SEXP colonCastRhs(SEXP newLhs, SEXP rhs);

inline void forceAll(SEXP list, InterpreterInstance* ctx) {
    while (list != R_NilValue) {
        if (TYPEOF(CAR(list)) == PROMSXP)
            SETCAR(list, evaluatePromise(CAR(list), ctx));
        list = CDR(list);
    }
}

inline bool needsExpandedDots(SEXP callee) {
    return TYPEOF(callee) != SPECIALSXP ||
           // forceAndCall is fully handled in tryFastSpecialCall
           // and expects expanded dots
           callee->u.primsxp.offset == blt("forceAndCall");
}

inline void createFakeSEXP(SEXPREC& res, SEXPTYPE t) {
    memset(&res, 0, sizeof(SEXPREC));
    res.attrib = R_NilValue;
    res.gengc_next_node = R_NilValue;
    res.gengc_prev_node = R_NilValue;
    res.sxpinfo.gcgen = 1;
    res.sxpinfo.mark = 1;
    res.sxpinfo.named = 2;
    res.sxpinfo.type = t;
}

inline void createFakeCONS(SEXPREC& res, SEXP cdr) {
    createFakeSEXP(res, LISTSXP);
    res.u.listsxp.carval = R_NilValue;
    res.u.listsxp.tagval = R_NilValue;
    res.u.listsxp.cdrval = cdr;
    __asan_poison_memory_region(&res.u.listsxp.cdrval, sizeof(SEXP));
}

inline SEXPREC createFakeCONS(SEXP cdr) {
    SEXPREC res;
    createFakeSEXP(res, LISTSXP);
    res.u.listsxp.carval = R_NilValue;
    res.u.listsxp.tagval = R_NilValue;
    res.u.listsxp.cdrval = cdr;
    __asan_poison_memory_region(&res.u.listsxp.cdrval, sizeof(SEXP));
    return res;
}

#define FAKE_ARGS1(res, a1)                                                    \
    SEXPREC __a1__cell__;                                                      \
    createFakeCONS(__a1__cell__, R_NilValue);                                  \
    __a1__cell__.u.listsxp.carval = a1;                                        \
    res = &__a1__cell__

#define FAKE_ARGS2(res, a1, a2)                                                \
    SEXPREC __a2__cell__;                                                      \
    createFakeCONS(__a2__cell__, R_NilValue);                                  \
    SEXPREC __a1__cell__;                                                      \
    createFakeCONS(__a1__cell__, &__a2__cell__);                               \
    __a1__cell__.u.listsxp.carval = a1;                                        \
    __a2__cell__.u.listsxp.carval = a2;                                        \
    res = &__a1__cell__

#define FAKE_ARGS3(res, a1, a2, a3)                                            \
    SEXPREC __a3__cell__;                                                      \
    createFakeCONS(__a3__cell__, R_NilValue);                                  \
    SEXPREC __a2__cell__;                                                      \
    createFakeCONS(__a2__cell__, &__a3__cell__);                               \
    SEXPREC __a1__cell__;                                                      \
    createFakeCONS(__a1__cell__, &__a2__cell__);                               \
    __a1__cell__.u.listsxp.carval = a1;                                        \
    __a2__cell__.u.listsxp.carval = a2;                                        \
    __a3__cell__.u.listsxp.carval = a3;                                        \
    res = &__a1__cell__

#define FAKE_ARGS4(res, a1, a2, a3, a4)                                        \
    SEXPREC __a4__cell__;                                                      \
    createFakeCONS(__a4__cell__, R_NilValue);                                  \
    SEXPREC __a3__cell__;                                                      \
    createFakeCONS(__a3__cell__, &__a4__cell__);                               \
    SEXPREC __a2__cell__;                                                      \
    createFakeCONS(__a2__cell__, &__a3__cell__);                               \
    SEXPREC __a1__cell__;                                                      \
    createFakeCONS(__a1__cell__, &__a2__cell__);                               \
    __a1__cell__.u.listsxp.carval = a1;                                        \
    __a2__cell__.u.listsxp.carval = a2;                                        \
    __a3__cell__.u.listsxp.carval = a3;                                        \
    __a4__cell__.u.listsxp.carval = a4;                                        \
    res = &__a1__cell__

#define FAKE_ARGS5(res, a1, a2, a3, a4, a5)                                    \
    SEXPREC __a5__cell__;                                                      \
    createFakeCONS(__a5__cell__, R_NilValue);                                  \
    SEXPREC __a4__cell__;                                                      \
    createFakeCONS(__a4__cell__, &__a5__cell__);                               \
    SEXPREC __a3__cell__;                                                      \
    createFakeCONS(__a3__cell__, &__a4__cell__);                               \
    SEXPREC __a2__cell__;                                                      \
    createFakeCONS(__a2__cell__, &__a3__cell__);                               \
    SEXPREC __a1__cell__;                                                      \
    createFakeCONS(__a1__cell__, &__a2__cell__);                               \
    __a1__cell__.u.listsxp.carval = a1;                                        \
    __a2__cell__.u.listsxp.carval = a2;                                        \
    __a3__cell__.u.listsxp.carval = a3;                                        \
    __a4__cell__.u.listsxp.carval = a4;                                        \
    __a5__cell__.u.listsxp.carval = a5;                                        \
    res = &__a1__cell__

} // namespace rir
#endif // RIR_INTERPRETER_C_H
