#include "rir_2_pir_compiler.h"
#include "../../pir/pir_impl.h"
#include "R/RList.h"
#include "rir_2_pir.h"

#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../opt/cleanup.h"
#include "../../opt/delay_env.h"
#include "../../opt/delay_instr.h"
#include "../../opt/elide_env.h"
#include "../../opt/force_dominance.h"
#include "../../opt/inline.h"
#include "../../opt/scope_resolution.h"
#include "ir/BC.h"

#include "interpreter/runtime.h"

namespace rir {
namespace pir {

Rir2PirCompiler::Rir2PirCompiler(Module* module) : RirCompiler(module) {
    for (auto optimization: pirConfigurations()->pirOptimizations()) {
        translations.push_back(optimization->translator);
    }
}

Closure* Rir2PirCompiler::compileClosure(SEXP closure) {
    assert(isValidClosureSEXP(closure));
    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));

    assert(tbl->slot(1) == nullptr && "Closure already compiled to PIR");

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
                                         Env* closureEnv) {
    return module->getOrCreate(
        srcFunction, args, closureEnv, [&](Closure* pirFunction) {
            Builder builder(pirFunction, closureEnv);

            {
                Rir2Pir rir2pir(*this, srcFunction);
                rir2pir.compile(srcFunction->body(), builder);
                if (isVerbose()) {
                    std::cout << " ========== Done compiling " << srcFunction
                              << "\n";
                    builder.function->print(std::cout);
                    std::cout << " ==========\n";
                }
            }

            assert(Verify::apply(pirFunction));
        });
}

void Rir2PirCompiler::optimizeModule() {
    size_t passnr = 0;
    bool verbose = isVerbose();

    module->eachPirFunction([&](Module::VersionedClosure& v) {
        auto f = v.current();
        if (verbose)
            v.saveVersion();
        applyOptimizations(f, "Optimizations 1st Pass");
        applyOptimizations(f, "Optimizations 2nd Pass");
    });

    for (int i = 0; i < 5; ++i) {
        module->eachPirFunction([&](Module::VersionedClosure& v) {
            auto f = v.current();
            if (verbose)
                v.saveVersion();
            Inline::apply(f);
            if (verbose)
                printAfterPass("inline", "Inlining", f, passnr++);
            applyOptimizations(f, "Optimizations After Inlining");
        });
    }
}

void Rir2PirCompiler::printAfterPass(const std::string& pass,
                                     const std::string& category, Closure* f,
                                     size_t passnr) {
    std::cout << "============== " << category << ": " << pass
              << " == " << passnr << " ======================\n";
    f->print(std::cout);
}

void Rir2PirCompiler::applyOptimizations(Closure* f,
                                         const std::string& category) {
    size_t passnr = 0;
    for (auto& translation : this->translations) {
        translation->apply(f);
        if (isVerbose())
            printAfterPass(translation->getName(), category, f, passnr++);
        assert(Verify::apply(f));
    }
}

} // namespace pir
} // namespace rir
