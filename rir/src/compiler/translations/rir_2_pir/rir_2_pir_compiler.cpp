#include "rir_2_pir_compiler.h"
#include "../../pir/pir_impl.h"
#include "R/RList.h"
#include "rir_2_pir.h"

#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../opt/pass_definitions.h"
#include "ir/BC.h"

#include "../../debugging/PerfCounter.h"

#include "interpreter/runtime.h"

#include <chrono>

namespace rir {
namespace pir {

// Currently PIR optimized functions cannot handle too many arguments or
// mis-ordered arguments. The caller needs to take care.
const Assumptions Rir2PirCompiler::minimalAssumptions =
    Assumptions() | Assumption::CorrectOrderOfArguments |
    Assumption::NotTooManyArguments;

Rir2PirCompiler::Rir2PirCompiler(Module* module, StreamLogger& logger)
    : RirCompiler(module), logger(logger) {
    for (auto& optimization : pirConfigurations()->pirOptimizations()) {
        translations.push_back(optimization);
    }
}

void Rir2PirCompiler::compileClosure(SEXP closure, const std::string& name,
                                     const Assumptions& assumptions,
                                     MaybeCls success, Maybe fail_) {
    assert(isValidClosureSEXP(closure));

    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));
    auto fun = tbl->baseline();

    if (fun->unoptimizable)
        return fail_();

    auto fail = [&]() {
        fun->unoptimizable = true;
        fail_();
    };

    if (tbl->size() > 1)
        logger.warn("Closure already compiled to PIR");

    auto frame = RList(FRAME(CLOENV(closure)));

    std::string closureName = name;
    if (name.compare("") == 0) {
        // Serach for name in environment
        for (auto e = frame.begin(); e != frame.end(); ++e) {
            if (*e == closure)
                closureName = CHAR(PRINTNAME(e.tag()));
        }
    }
    auto pirClosure = module->getOrDeclareRirClosure(name, closure, fun);
    OptimizationContext context(assumptions | minimalAssumptions);
    compileClosure(pirClosure, context, success, fail);
}

void Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                      const std::string& name, SEXP formals,
                                      SEXP srcRef,
                                      const Assumptions& assumptions,
                                      MaybeCls success, Maybe fail_) {
    if (srcFunction->unoptimizable)
        return fail_();

    auto fail = [&]() {
        srcFunction->unoptimizable = true;
        fail_();
    };

    OptimizationContext context(assumptions | minimalAssumptions);
    auto closure =
        module->getOrDeclareRirFunction(name, srcFunction, formals, srcRef);
    compileClosure(closure, context, success, fail);
}

void Rir2PirCompiler::compileClosure(Closure* closure,
                                     const OptimizationContext& ctx,
                                     MaybeCls success, Maybe fail) {

    // TODO: Support default arguments and dots
    if (closure->formals().hasDefaultArgs()) {
        if (!ctx.assumptions.includes(Assumption::NoMissingArguments)) {
            logger.warn("no support for default args");
            return fail();
        }
    }
    if (closure->formals().hasDots()) {
        logger.warn("no support for ...");
        return fail();
    }

    if (auto existing = closure->findCompatibleVersion(ctx))
        return success(existing);

    auto version = closure->declareVersion(ctx);

    Builder builder(version, closure->closureEnv());
    auto& log = logger.begin(version);
    Rir2Pir rir2pir(*this, closure->rirFunction(), log, closure->name());

    if (rir2pir.tryCompile(builder)) {
        log.compilationEarlyPir(version);
        if (Verify::apply(version)) {
            log.flush();
            return success(version);
        }

        log.failed("rir2pir failed to verify");
        log.flush();
        logger.close(version);
        assert(false);
    }

    log.failed("rir2pir aborted");
    log.flush();
    logger.close(version);
    closure->erase(ctx);
    return fail();
}

bool MEASURE_COMPILER_PERF = getenv("PIR_MEASURE_COMPILER") ? true : false;
std::chrono::time_point<std::chrono::high_resolution_clock> startTime;
std::chrono::time_point<std::chrono::high_resolution_clock> endTime;
std::unique_ptr<CompilerPerf> PERF = std::unique_ptr<CompilerPerf>(
    MEASURE_COMPILER_PERF ? new CompilerPerf : nullptr);

void Rir2PirCompiler::optimizeModule() {
    logger.flush();
    size_t passnr = 0;
    for (auto& translation : translations) {
        module->eachPirClosure([&](Closure* c) {
            c->eachVersion([&](ClosureVersion* v) {
                auto& log = logger.get(v);
                log.pirOptimizationsHeader(v, translation, passnr++);

                if (MEASURE_COMPILER_PERF)
                    startTime = std::chrono::high_resolution_clock::now();

                translation->apply(*this, v, log);
                if (MEASURE_COMPILER_PERF) {
                    endTime = std::chrono::high_resolution_clock::now();
                    std::chrono::duration<double> passDuration =
                        endTime - startTime;
                    PERF->addTime(translation->getName(), passDuration.count());
                }

                log.pirOptimizations(v, translation);

#ifdef ENABLE_SLOWASSERT
                assert(Verify::apply(v));
#endif
            });
        });
    }
    if (MEASURE_COMPILER_PERF)
        startTime = std::chrono::high_resolution_clock::now();

    module->eachPirClosure([&](Closure* c) {
        c->eachVersion([&](ClosureVersion* v) {
            logger.get(v).pirOptimizationsFinished(v);
            assert(Verify::apply(v));
        });
    });

    if (MEASURE_COMPILER_PERF) {
        endTime = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> passDuration = endTime - startTime;
        PERF->addTime("Verification", passDuration.count());
    }

    logger.flush();
}

} // namespace pir
} // namespace rir
