#include "JITModule.h"

using namespace llvm;

SEXP JITModule::getNativeSXP(SEXP ast, std::vector<SEXP> const& objects,
                             Function* f) {

    SEXP objs = allocVector(VECSXP, objects.size());
    PROTECT(objs);
    for (size_t i = 0; i < objects.size(); ++i)
        SET_VECTOR_ELT(objs, i, objects[i]);
    SEXP result = CONS(nullptr, objs);
    // all objects in objects + objs itself (now part of result)
    UNPROTECT(objects.size() + 1);
    SET_TAG(result, reinterpret_cast<SEXP>(f));
    SET_TYPEOF(result, NATIVESXP);
    relocations[f] = result;
    return result;
}

void JITModule::finalizeNativeSEXPs(llvm::ExecutionEngine* engine) {
    // perform all the relocations
    for (auto r : relocations) {
        SEXP s = std::get<1>(r);
        auto f = reinterpret_cast<Function*>(TAG(s));
        auto fp = engine->getPointerToFunction(f);
        assert(fp);
        SETCAR(s, reinterpret_cast<SEXP>(fp));
    }
}
