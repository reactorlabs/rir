#include "interp_context.h"

void initializeResizeableList(ResizeableList * l, size_t capacity, SEXP parent, size_t index) {
    l->capacity = capacity;
    l->list = Rf_allocVector(VECSXP, capacity);
    SET_VECTOR_ELT(parent, index, l->list);
    rl_setLength(l, 0);
}

SEXP R_Subset2Sym;
SEXP R_SubsetSym;
SEXP R_valueSym;
SEXP setterPlaceholderSym;
SEXP getterPlaceholderSym;
SEXP quoteSym;

Context* context_create(CompilerCallback compiler) {
    Context* c = malloc(sizeof(Context));
    c->list = Rf_allocVector(VECSXP, 3);
    c->compiler = compiler;
    R_PreserveObject(c->list);
    initializeResizeableList(&c->cp, POOL_CAPACITY, c->list, CONTEXT_INDEX_CP);
    initializeResizeableList(&c->src, POOL_CAPACITY, c->list, CONTEXT_INDEX_SRC);
    initializeResizeableList(&c->ostack, STACK_CAPACITY, c->list, CONTEXT_INDEX_OSTACK);
    c->fstack = malloc(sizeof(FStack));
    c->fstack->length = 0;
    c->fstack->prev = NULL;
    // first item in source and constant pools is R_NilValue so that we can use the index 0 for other purposes
    src_pool_add(c, R_NilValue);
    cp_pool_add(c, R_NilValue);
    R_Subset2Sym = Rf_install("[[");
    R_SubsetSym = Rf_install("[");
    R_valueSym = Rf_install("value");
    setterPlaceholderSym = Rf_install("*.placeholder.setter.*");
    getterPlaceholderSym = Rf_install("*.placeholder.getter.*");
    quoteSym = Rf_install("quote");
    return c;
}


extern Context * globalContext_;

extern void R_SetErrorHook(void (*hook)(SEXP, char *));
extern void rirBacktrace(Context* ctx);

void rirErrorHook(SEXP call, char * msg) {
    Rprintf("RIR backtrace:\n");
    rirBacktrace(globalContext_);
    Rprintf("\n");
    Rf_errorcall(call, msg);
}



