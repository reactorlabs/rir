#include "ir/Optimizer.h"
#include "ir/cleanup.h"
#include "compiler/translations/pir_2_rir.h"
#include "compiler/translations/rir_2_pir/rir_2_pir.h"

#include <memory>

namespace rir {

bool Optimizer::cleanupRIR(CodeEditor& code, int steam) {
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

bool Optimizer::tryOptimize(SEXP what, bool verbose) {
    if (!isValidClosureSEXP(what))
        return false;
    if (DispatchTable::unpack(BODY(what))->capacity() != 2)
        return false;
    if (DispatchTable::unpack(BODY(what))->slot(1) != nullptr)
        return false;

    Protect p(what);
    std::unique_ptr<pir::Module> m(new pir::Module);
    pir::Rir2PirCompiler cmp(m.get());
    cmp.setVerbose(verbose);
    auto c = cmp.compileClosure(what);

    if (c == nullptr)
        return false;

    cmp.optimizeModule();

    if (verbose)
        m->print();

    pir::Pir2RirCompiler p2r;
    p2r.verbose = verbose;
    p2r.compile(c, what);

    return true;
}

SEXP Optimizer::reoptimizeFunction(SEXP s) {
    Rf_warning("rir: reoptimizing not implemented as of now");
    return s;
}

} // namespace rir
