#include "ir/Optimizer.h"
#include "optimizer/cleanup.h"
#include "optimizer/stupid_inline.h"
#include "optimizer/localize.h"

namespace rir {

void Optimizer::optimize(CodeEditor& code, int steam) {
    BCCleanup cleanup(code);
    for (int i = 0; i < 8; ++i) {
        // puts("******");
        // code.print();
        cleanup.run();
        code.commit();
        if (!code.changed)
            break;
        code.commit();
    }
}

void Optimizer::inliner(CodeEditor& code) {
    StupidInliner inl(code);
    inl.run();
    code.commit();
}

SEXP Optimizer::reoptimizeFunction(SEXP s) {
    CodeEditor code(s);

    for (int i = 0; i < 4; ++i) {
        Optimizer::inliner(code);
        Optimizer::optimize(code, 1);
    }

    FunctionHandle opt = code.finalize();
    // TODO: just a hack to make sure optimization is not triggered again
    opt.function->invocationCount = 2001;
    CodeVerifier::vefifyFunctionLayout(opt.store, globalContext());
    return opt.store;
}
}
