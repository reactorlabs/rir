#include "ir/Optimizer.h"
#include "optimization/cleanup.h"
#include "optimization/localize.h"
#include "optimization/stupid_inline.h"

namespace rir {

bool Optimizer::optimize(CodeEditor& code, int steam) {
    bool changed = false;
    BCCleanup cleanup(code);
    for (int i = 0; i < steam; ++i) {
        // puts("******");
        // code.print();
        cleanup.run();
        changed = changed || code.changed;
        if (!code.changed)
            break;
        code.commit();
    }
    return changed;
}

bool Optimizer::inliner(CodeEditor& code, bool stable) {
    Localizer local(code, stable);
    local.run();
    bool changed = code.changed;
    if (code.changed)
        code.commit();
    StupidInliner inl(code);
    inl.run();
    changed = changed || code.changed;
    if (code.changed)
        code.commit();
    return changed;
}

SEXP Optimizer::reoptimizeFunction(SEXP s) {
    Function* fun = isValidFunctionObject(s);
    assert(fun);
    bool safe = !fun->envLeaked && !fun->envChanged;

    CodeEditor code(s);

    for (int i = 0; i < 16; ++i) {
        bool changedInl = Optimizer::inliner(code, safe);
        bool changedOpt = Optimizer::optimize(code, 8);
        if (!changedInl && !changedOpt) {
            if (i == 0)
                return s;
            else
                break;
        }
    }

    FunctionHandle opt = code.finalize();
    EXTERNALSXP_SET_ENTRY(opt.store, FUNCTION_ORIGIN_OFFSET, s);

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(opt.store, globalContext());
#endif
    return opt.store;
}
}
