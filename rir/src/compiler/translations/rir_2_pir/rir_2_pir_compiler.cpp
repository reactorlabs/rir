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

Rir2PirCompiler::Rir2PirCompiler(Module* module, StreamLogger& logger)
    : RirCompiler(module), logger(logger) {
    for (auto& optimization : pirConfigurations()->pirOptimizations()) {
        translations.push_back(optimization);
    }
}

void Rir2PirCompiler::compileClosure(SEXP closure, const std::string& name,
                                     MaybeCls success, Maybe fail) {
    assert(isValidClosureSEXP(closure));

    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));

    if (tbl->available(1))
        logger.warn("Closure already compiled to PIR");

    FormalArgs formals(FORMALS(closure));
    rir::Function* srcFunction = tbl->first();
    auto env = module->getEnv(CLOENV(closure));
    assert(env != Env::notClosed());
    auto frame = RList(FRAME(CLOENV(closure)));

    std::string closureName = name;
    if (name.compare("") == 0) {
        // Serach for name in environment
        for (auto e = frame.begin(); e != frame.end(); ++e) {
            if (*e == closure)
                closureName = CHAR(PRINTNAME(e.tag()));
        }
    }
    compileClosure(srcFunction, closureName, formals, env, success, fail);
}

void Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                      const std::string& name,
                                      FormalArgs const& formals,
                                      MaybeCls success, Maybe fail) {
    compileClosure(srcFunction, name, formals, Env::notClosed(), success, fail);
}

void Rir2PirCompiler::compileClosure(rir::Function* srcFunction,
                                     const std::string& name,
                                     FormalArgs const& formals, Env* closureEnv,
                                     MaybeCls success, Maybe fail) {

    // TODO: Support default arguments and dots
    if (formals.hasDefaultArgs) {
        logger.warn("no support for default args");
        return fail();
    }
    if (formals.hasDots) {
        logger.warn("no support for ...");
        return fail();
    }

    bool failed = false;
    // TODO: if compilation fails, we should remember that somehow. Otherwise
    // we will continue on trying to compile the same function over and over
    // again.
    module->createIfMissing(
        srcFunction, formals.names, closureEnv, [&](Closure* pirFunction) {
            Builder builder(pirFunction, closureEnv);
            auto& log = logger.begin(pirFunction, name);
            Rir2Pir rir2pir(*this, srcFunction, log, name);

            if (rir2pir.tryCompile(srcFunction->body(), builder)) {
                log.compilationEarlyPir(pirFunction);
                if (!Verify::apply(pirFunction)) {
                    failed = true;
                    log.failed("rir2pir failed to verify");
                    log.flush();
                    logger.close(pirFunction);
                    assert(false);
                    return false;
                }
                log.flush();
                return true;
            }
            log.failed("rir2pir aborted");
            failed = true;
            log.flush();
            logger.close(pirFunction);
            return false;
        });

    if (failed)
        fail();
    else
        success(module->get(Module::FunctionAndEnv(srcFunction, closureEnv)));
}

void Rir2PirCompiler::optimizeModule(StreamLogger& logger,
                                     bool preserveVersions) {
    size_t passnr = 0;
    for (auto& translation : translations) {
        module->eachPirFunction([&](Module::VersionedClosure& v) {
            auto f = v.current();
            if (preserveVersions)
                v.saveVersion();

            auto& log = logger.get(f);
            translation->apply(f);
            log.pirOptimizations(f, translation->getName(), passnr++);

#ifdef ENABLE_SLOWASSERT
            assert(Verify::apply(f));
#endif
        });
    }
    module->eachPirFunction([&](Module::VersionedClosure& v) {
        logger.get(v.current()).pirOptimizationsFinished(v.current());
        assert(Verify::apply(v.current()));
    });
    logger.flush();
}

} // namespace pir
} // namespace rir
