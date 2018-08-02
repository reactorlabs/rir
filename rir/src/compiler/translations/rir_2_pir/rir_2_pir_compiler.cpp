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

#include <iostream>

#include "interpreter/runtime.h"

namespace rir {
namespace pir {

Rir2PirCompiler::Rir2PirCompiler(Module* module) : RirCompiler(module) {
    for (auto optimization : pirConfigurations()->pirOptimizations()) {
        translations.push_back(optimization->translator);
    }
}

void Rir2PirCompiler::compileClosure(SEXP closure, MaybeCls success,
                                     Maybe fail) {
    assert(isValidClosureSEXP(closure));
    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));

    if (tbl->available(1)) {
        if (isVerbose())
            std::cerr << "Closure already compiled to PIR\n";
    }

    FormalArgs formals(FORMALS(closure));
    rir::Function* srcFunction = tbl->first();
    compileClosure(srcFunction, formals, module->getEnv(CLOENV(closure)),
                   success, fail);
}

void Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                      FormalArgs const& formals,
                                      MaybeCls success, Maybe fail) {
    compileClosure(srcFunction, formals, Env::notClosed(),
                   success, fail);
}

void Rir2PirCompiler::compileClosure(rir::Function* srcFunction,
                                     FormalArgs const& formals, Env* closureEnv,
                                     MaybeCls success, Maybe fail) {

    // TODO: Support default arguments and dots
    if (formals.hasDefaultArgs || formals.hasDots)
        return fail();

    bool failed = false;
    module->createIfMissing(
        srcFunction, formals.names, closureEnv, [&](Closure* pirFunction) {
            Builder builder(pirFunction, closureEnv);
            Rir2Pir rir2pir(*this, srcFunction);
            if (isVerbose()) {
                // clang-format off
                std::cout << "\n\n**************************************************************\n";
                std::cout << "*********** Start compiling:" << srcFunction << " **********\n";
                std::cout << "**************************************************************\n";
                // clang-format on
                if (shouldPrintOriginalVersion()) {
                    std::cout << "=============== Original version:\n";
                    auto it = srcFunction->begin();
                    while (it != srcFunction->end()) {
                        (*it)->print();
                        ++it;
                    }
                }
            }

            if (rir2pir.tryCompile(srcFunction->body(), builder)) {
                if (shouldPrintCompiledVersion()) {
                    std::cout << " ========= Compiled to PIR Version:";
                    builder.function->print(std::cout);
                }
                if (!Verify::apply(pirFunction)) {
                    failed = true;
                    if (isVerbose()) {
                        std::cout << " Failed verification after p2r compile "
                                  << srcFunction << "\n";
                        std::cout << " ========= Finish compiling " << srcFunction
                              << "\n\n";
                    }
                    assert(false);
                    return false;
                }
                return true;
            }
            if (isVerbose()) {
                std::cout << " Failed p2r compile " << srcFunction << "\n";
                std::cout << " ========= Finish compiling " << srcFunction
                          << "\n\n";
            }
            failed = true;
            return false;
        });

    if (failed)
        fail();
    else
        success(module->get(srcFunction));
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
            if (shouldPrintInliningVersions()) {
                printAfterPass("inline", "Inlining", f, passnr++);
            }
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
        if (shouldPrintOptimizations())
            printAfterPass(translation->getName(), category, f, passnr++);
#if 0
        assert(Verify::apply(f));
#endif
    }
    assert(Verify::apply(f));
}

} // namespace pir
} // namespace rir
