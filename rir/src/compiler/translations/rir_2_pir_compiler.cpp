#include "rir_2_pir_compiler.h"
#include "../pir/pir_impl.h"
#include "R/RList.h"
#include "rir_2_pir.h"

#include "../analysis/query.h"
#include "../analysis/verifier.h"
#include "../opt/cleanup.h"
#include "../opt/delay_env.h"
#include "../opt/delay_instr.h"
#include "../opt/elide_env.h"
#include "../opt/force_dominance.h"
#include "../opt/inline.h"
#include "../opt/scope_resolution.h"
#include "ir/BC.h"

namespace rir {
namespace pir {

Closure* Rir2PirCompiler::compileClosure(SEXP closure) {
    assert(isValidClosureSEXP(closure));
    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));
    auto formals = RList(FORMALS(closure));

    std::vector<SEXP> fmls;
    for (auto it = formals.begin(); it != formals.end(); ++it)
        fmls.push_back(it.tag());

    rir::Function* srcFunction = tbl->first();
    return compileClosure(srcFunction, fmls, module->getEnv(CLOENV(closure)));
}

Closure* Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                          const std::vector<SEXP>& args) {
    return compileClosure(srcFunction, args, Env::notClosed());
}

Closure* Rir2PirCompiler::compileClosure(rir::Function* srcFunction,
                                         const std::vector<SEXP>& args,
                                         Value* closureEnv) {
    Closure* pirFunction = module->declare(srcFunction, args);

    Builder builder(pirFunction, closureEnv);

    {
        Rir2Pir rir2pir(*this, builder, srcFunction, srcFunction->body());
        rir2pir.translate();
    }

    assert(Verify::apply(pirFunction));

    return pirFunction;
}

void Rir2PirCompiler::optimizeModule() {
    size_t passnr = 0;
    bool verbose = isVerbose();

    auto print = [&](const std::string& pass, Closure* f) {
        std::cout << "============== " << pass << " == " << passnr++
                  << " ======================\n";
        f->print(std::cout);
    };

    auto apply = [&](Closure* f, bool verb) {
        ForceDominance::apply(f);
        if (verb)
            print("force", f);
        ScopeResolution::apply(f);
        if (verb)
            print("scope", f);
        Cleanup::apply(f);
        if (verb)
            print("cleanup", f);
        DelayInstr::apply(f);
        if (verb)
            print("delay instr", f);
        ElideEnv::apply(f);
        if (verb)
            print("elide env", f);
        DelayEnv::apply(f);
        if (verb)
            print("delay env", f);
    };

    module->eachPirFunction([&](Module::VersionedClosure& v) {
        auto f = v.current();
        if (verbose)
            v.saveVersion();
        apply(f, verbose);
        apply(f, verbose);
    });

    for (int i = 0; i < 5; ++i) {
        module->eachPirFunction([&](Module::VersionedClosure& v) {
            auto f = v.current();
            if (verbose)
                v.saveVersion();
            Inline::apply(f);
            if (verbose)
                print("inline", f);

            apply(f, verbose);
        });
    }
}

}
}