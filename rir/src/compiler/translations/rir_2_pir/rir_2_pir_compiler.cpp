#include "rir_2_pir_compiler.h"
#include "../../pir/pir_impl.h"
#include "R/RList.h"
#include "rir_2_pir.h"

#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../opt/pass_definitions.h"
#include "ir/BC.h"

#include "interpreter/runtime.h"

namespace rir {
namespace pir {

// Currently PIR optimized functions cannot handle too many arguments or
// mis-ordered arguments. The caller needs to take care.
const Assumptions Rir2PirCompiler::minimalAssumptions =
    Assumptions() | Assumption::CorrectOrderOfArguments |
    Assumption::MaxNumberOfArguments;

Rir2PirCompiler::Rir2PirCompiler(Module* module, StreamLogger& logger)
    : RirCompiler(module), logger(logger) {
    for (auto& optimization : pirConfigurations()->pirOptimizations()) {
        translations.push_back(optimization);
    }
}

void Rir2PirCompiler::compileClosure(SEXP closure, const std::string& name,
                                     const Assumptions& assumptions,
                                     MaybeCls success, Maybe fail) {
    assert(isValidClosureSEXP(closure));

    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));

    if (tbl->size() > 1)
        logger.warn("Closure already compiled to PIR");

    FormalArgs formals(FORMALS(closure));
    rir::Function* srcFunction = tbl->baseline();
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
    OptimizationContext context(env, assumptions | minimalAssumptions);
    compileClosure(srcFunction, closureName, formals, context, success, fail);
}

void Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                      const std::string& name,
                                      FormalArgs const& formals,
                                      const Assumptions& assumptions,
                                      MaybeCls success, Maybe fail) {
    OptimizationContext context(Env::notClosed(),
                                assumptions | minimalAssumptions);
    compileClosure(srcFunction, name, formals, context, success, fail);
}

void Rir2PirCompiler::compileClosure(rir::Function* srcFunction,
                                     const std::string& name,
                                     FormalArgs const& formals,
                                     const OptimizationContext& ctx,
                                     MaybeCls success, Maybe fail) {

    // TODO: Support default arguments and dots
    if (formals.hasDefaultArgs) {
        if (!ctx.assumptions.includes(Assumption::CorrectNumberOfArguments)) {
            logger.warn("no support for default args");
            return fail();
        }
    }
    if (formals.hasDots) {
        logger.warn("no support for ...");
        return fail();
    }

    // TODO: if compilation fails, we should remember that somehow. Otherwise
    // we will continue on trying to compile the same function over and over
    // again.
    if (module->exists(srcFunction, ctx))
        return success(module->get(srcFunction, ctx));

    auto closure = module->declare(name, srcFunction, ctx, formals.names);

    Builder builder(closure, ctx.environment);
    auto& log = logger.begin(closure);
    Rir2Pir rir2pir(*this, srcFunction, log, name);

    if (rir2pir.tryCompile(builder)) {
        log.compilationEarlyPir(closure);
        if (Verify::apply(closure)) {
            log.flush();
            return success(closure);
        }

        log.failed("rir2pir failed to verify");
        log.flush();
        logger.close(closure);
        assert(false);
    }

    log.failed("rir2pir aborted");
    log.flush();
    logger.close(closure);
    module->erase(srcFunction, ctx);
    return fail();
}

void Rir2PirCompiler::optimizeModule() {
    logger.flush();
    size_t passnr = 0;
    for (auto& translation : translations) {
        module->eachPirFunction([&](Closure* c) {
            auto& log = logger.get(c);
            log.pirOptimizationsHeader(c, translation->getName(), passnr++);
            if (dynamic_cast<const ScopeResolution*>(translation))
              log.pirOptimizations(c);
            translation->apply(*this, c, log);
            if (dynamic_cast<const ScopeResolution*>(translation))
              log.pirOptimizations(c);

#ifdef ENABLE_SLOWASSERT
            assert(Verify::apply(c));
#endif
        });
    }
    module->eachPirFunction([&](Closure* c) {
        logger.get(c).pirOptimizationsFinished(c);
        assert(Verify::apply(c));
    });
    logger.flush();
}

} // namespace pir
} // namespace rir
