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

Rir2PirCompiler::Rir2PirCompiler(Module* module, const DebugOptions& debug)
    : RirCompiler(module, debug), log(StreamLogger(debug)) {
    for (auto optimization : pirConfigurations()->pirOptimizations()) {
        translations.push_back(optimization->translator);
    }
}

void Rir2PirCompiler::compileClosure(SEXP closure, MaybeCls success, Maybe fail,
                                     bool isIndependent = false) {
    assert(isValidClosureSEXP(closure));
    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));

    if (tbl->available(1)) {
        if (!debug.includes(DebugFlag::ShowWarnings))
            std::cerr << "Closure already compiled to PIR\n";
    }

    FormalArgs formals(FORMALS(closure));
    rir::Function* srcFunction = tbl->first();
    compileClosure(srcFunction, formals, module->getEnv(CLOENV(closure)),
                   success, fail, isIndependent);
}

void Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                      FormalArgs const& formals,
                                      MaybeCls success, Maybe fail) {
    compileClosure(srcFunction, formals, Env::notClosed(), success, fail, true);
}

void Rir2PirCompiler::compileClosure(rir::Function* srcFunction,
                                     FormalArgs const& formals, Env* closureEnv,
                                     MaybeCls success, Maybe fail,
                                     bool isIndependent) {

    // TODO: Support default arguments and dots
    if (formals.hasDefaultArgs || formals.hasDots)
        return fail();

    if (isIndependent)
        log.startLogging(srcFunction);

    bool failed = false;
    module->createIfMissing(
        srcFunction, formals.names, closureEnv, [&](Closure* pirFunction) {
            Builder builder(pirFunction, closureEnv);
            Rir2Pir rir2pir(*this, srcFunction);
            log.compilationInit();

            if (rir2pir.tryCompile(srcFunction->body(), builder)) {
                log.compilationEarlyPir(*(builder.function));
                if (!Verify::apply(pirFunction)) {
                    failed = true;
                    log.failCompilingPir();
                    assert(false);
                    return false;
                }
                return true;
            }
            log.failCompilingPir();
            failed = true;
            return false;
        });

    if (failed)
        fail();
    else {
        module->get(srcFunction)->print(std::cout);
        success(module->get(srcFunction));
    }

    log.endLogging();
}

void Rir2PirCompiler::optimizeModule() {
    size_t passnr = 0;
    module->eachPirFunction([&](Module::VersionedClosure& v) {
        auto f = v.current();
        if (debug.includes(DebugFlag::PreserveVersions))
            v.saveVersion();
        applyOptimizations(f, "Optimizations 1st Pass");
        applyOptimizations(f, "Optimizations 2nd Pass");
    });

    for (int i = 0; i < 5; ++i) {
        module->eachPirFunction([&](Module::VersionedClosure& v) {
            auto f = v.current();
            if (debug.includes(DebugFlag::PreserveVersions))
                v.saveVersion();
            Inline::apply(f);
            if (debug.includes(DebugFlag::PrintInlining)) {
                printAfterPass("inline", "Inlining", f, passnr++);
            }
            applyOptimizations(f, "Optimizations After Inlining");
        });
    }
}

void Rir2PirCompiler::printAfterPass(const std::string& pass,
                                     const std::string& category, Closure* f,
                                     size_t passnr) {
    log.pirOptimizations(*f, category, pass, passnr);
}

void Rir2PirCompiler::applyOptimizations(Closure* f,
                                         const std::string& category) {
    size_t passnr = 0;
    for (auto& translation : this->translations) {
        translation->apply(f);
        if (debug.includes(DebugFlag::PrintOptimizationPasses))
            printAfterPass(translation->getName(), category, f, passnr++);
#if 0
        assert(Verify::apply(f));
#endif
    }
    assert(Verify::apply(f));
}

} // namespace pir
} // namespace rir
