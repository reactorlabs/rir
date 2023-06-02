#include "instance.h"
#include "api.h"
#include "compiler/parameter.h"
#include "utils/UUIDPool.h"

namespace rir {

#ifdef DO_INTERN
static std::unordered_map<SEXP, size_t> src_pool_interned;
#endif

void initializeResizeableList(ResizeableList* l, size_t capacity, SEXP parent,
                              size_t index) {
    l->capacity = capacity;
    l->list = Rf_allocVector(VECSXP, capacity);
    SET_VECTOR_ELT(parent, index, l->list);
    rl_setLength(l, 0);
}

SEXP R_Subset2Sym;
SEXP R_SubsetSym;
SEXP R_SubassignSym;
SEXP R_Subassign2Sym;
SEXP R_valueSym;
SEXP setterPlaceholderSym;
SEXP getterPlaceholderSym;
SEXP quoteSym;

void context_init() {
    InterpreterInstance* c = globalContext();
    c->list = Rf_allocVector(VECSXP, 2);
    R_PreserveObject(c->list);
    initializeResizeableList(&c->cp, ResizeableList::POOL_CAPACITY, c->list,
                             ResizeableList::CONTEXT_INDEX_CP);
    initializeResizeableList(&c->src, ResizeableList::POOL_CAPACITY, c->list,
                             ResizeableList::CONTEXT_INDEX_SRC);
    // first item in source and constant pools is R_NilValue so that we can use
    // the index 0 for other purposes
    src_pool_add(R_NilValue);
    cp_pool_add(R_NilValue);
    R_Subset2Sym = Rf_install("[[");
    R_SubsetSym = Rf_install("[");
    R_SubassignSym = Rf_install("[<-");
    R_Subassign2Sym = Rf_install("[[<-");
    R_valueSym = Rf_install("value");
    setterPlaceholderSym = Rf_install("*.placeholder.setter.*");
    getterPlaceholderSym = Rf_install("*.placeholder.getter.*");
    quoteSym = Rf_install("quote");

    auto pir = getenv("PIR_ENABLE");

    c->closureCompiler = [](SEXP closure, SEXP name) {
        return rirCompile(closure, R_NilValue);
    };
    c->closureOptimizer = [](SEXP f, const Context&, SEXP n) { return f; };

    if (pir && std::string(pir).compare("off") == 0) {
        pir::Parameter::ENABLE_OSR = false;
        // do nothing; use defaults
    } else if (pir && std::string(pir).compare("force") == 0) {
        c->closureCompiler = [](SEXP f, SEXP n) {
            SEXP rir = rirCompile(f, R_NilValue);
            return rirOptDefaultOpts(rir, Context(), n);
        };
    } else if (pir && std::string(pir).compare("force_dryrun") == 0) {
        c->closureCompiler = [](SEXP f, SEXP n) {
            SEXP rir = rirCompile(f, R_NilValue);
            return rirOptDefaultOptsDryrun(rir, Context(), n);
        };
    } else {
        c->closureOptimizer = rirOptDefaultOpts;
    }
}

size_t src_pool_read_item(SEXP ref_table, R_inpstream_t in) {
    auto item = UUIDPool::readItem(ref_table, in);
#ifdef DO_INTERN
    if (src_pool_interned.count(item)) {
        return src_pool_interned.at(item);
    }
#endif
    size_t i = src_pool_add(item);
#ifdef DO_INTERN
    src_pool_interned[item] = i;
#endif
    return i;
}

void src_pool_write_item(size_t idx, SEXP ref_table, R_outpstream_t out) {
    UUIDPool::writeItem(src_pool_at(idx), ref_table, out);
}

} // namespace rir
