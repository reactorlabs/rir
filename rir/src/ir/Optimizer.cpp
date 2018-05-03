#include "ir/Optimizer.h"
#include "ir/cleanup.h"

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

SEXP Optimizer::reoptimizeFunction(SEXP s) {
    Function* fun = Function::unpack(s);

    CodeEditor code(s);

    for (int i = 0; i < 16; ++i) {
        bool changedOpt = Optimizer::optimize(code, 8);
        if (!changedOpt) {
            if (i == 0)
                return nullptr;
            break;
        }
    }

    Function* opt = code.finalize();
    opt->origin(fun);
    fun->next(opt);

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(opt->container(), globalContext());
#endif
    return opt->container();
}

}  // namespace rir
