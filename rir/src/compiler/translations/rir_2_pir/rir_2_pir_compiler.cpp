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
    : RirCompiler(module, debug), log(debug) {
    for (auto& optimization : pirConfigurations()->pirOptimizations()) {
        translations.push_back(optimization);
    }
}

void Rir2PirCompiler::compileClosure(SEXP closure, MaybeCls success,
                                     Maybe fail) {
    assert(isValidClosureSEXP(closure));

    // TODO: we need to keep track of this compiled closure, since for example
    // the parent_env_ instruction refers back to the closure. What we should do
    // is have the code object link back to the closure object. But for that we
    // will need to make Code objects proper objects. For now let's just put it
    // in the constant pool, so it never gets GC'd.
    Pool::insert(closure);

    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));

    if (tbl->available(1)) {
        if (debug.includes(DebugFlag::ShowWarnings))
            std::cerr << "Closure already compiled to PIR\n";
    }

    FormalArgs formals(FORMALS(closure));
    rir::Function* srcFunction = tbl->first();
    auto env = module->getEnv(CLOENV(closure));
    assert(env != Env::notClosed());
    compileClosure(srcFunction, formals, env, success, fail);
}

void Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                      FormalArgs const& formals,
                                      MaybeCls success, Maybe fail) {
    compileClosure(srcFunction, formals, Env::notClosed(), success, fail);
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

            if (rir2pir.tryCompile(srcFunction->body(), builder)) {
                LOGGING(log.compilationEarlyPir(*(builder.function)));
                if (!Verify::apply(pirFunction)) {
                    failed = true;
                    LOGGING(log.failCompilingPir(srcFunction));
                    assert(false);
                    return false;
                }
                return true;
            }
            LOGGING(log.failCompilingPir(srcFunction));
            failed = true;
            return false;
        });

    if (failed)
        fail();
    else
        success(module->get(Module::FunctionAndEnv(srcFunction, closureEnv)));
}

void Rir2PirCompiler::optimizeModule() {
    LOGGING(size_t passnr = 0);
    for (auto& translation : translations) {
        module->eachPirFunction([&](Module::VersionedClosure& v) {
            auto f = v.current();
            if (debug.includes(DebugFlag::PreserveVersions))
                v.saveVersion();

            translation->apply(f);
            LOGGING(log.pirOptimizations(*f, translation->getName(), passnr++));

#ifdef ENABLE_SLOWASSERT
            assert(Verify::apply(f));
#endif
        });
    }
#ifndef ENABLE_SLOWASSERT
    module->eachPirFunction([&](Module::VersionedClosure& v) {
        assert(Verify::apply(v.current()));
    });
#endif
}
} // namespace pir
} // namespace rir
