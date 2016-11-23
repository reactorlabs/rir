#include "ir/Optimizer.h"
#include "optimizer/cleanup.h"
#include "optimizer/stupid_inline.h"
#include "optimizer/localize.h"

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

bool Optimizer::inliner(CodeEditor& code) {
    StupidInliner inl(code);
    Localizer local(code);
    local.run();
    bool changed = code.changed;
    if (code.changed)
        code.commit();
    inl.run();
    changed = changed || code.changed;
    if (code.changed)
        code.commit();
    return changed;
}

SEXP Optimizer::reoptimizeFunction(SEXP s) {
    CodeEditor code(s);

    for (int i = 0; i < 8; ++i) {
        if (!Optimizer::inliner(code))
            break;
        if (!Optimizer::optimize(code, 2))
            break;
    }

    FunctionHandle opt = code.finalize();
    CodeVerifier::vefifyFunctionLayout(opt.store, globalContext());
    return opt.store;
}
}
