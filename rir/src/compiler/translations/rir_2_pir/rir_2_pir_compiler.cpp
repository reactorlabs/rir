#include "rir_2_pir_compiler.h"
#include "../../pir/pir_impl.h"
#include "R/RList.h"
#include "rir_2_pir.h"

#include "compiler/parameter.h"

#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../opt/pass_definitions.h"
#include "ir/BC.h"
#include "ir/Compiler.h"

#include "../../debugging/PerfCounter.h"

#include "compiler/opt/pass_scheduler.h"

#include <chrono>

namespace rir {
namespace pir {

// Currently PIR optimized functions cannot handle too many arguments or
// mis-ordered arguments. The caller needs to take care.
constexpr Assumptions::Flags Rir2PirCompiler::minimalAssumptions;
constexpr Assumptions Rir2PirCompiler::defaultAssumptions;

void Rir2PirCompiler::compileClosure(SEXP closure, const std::string& name,
                                     const Assumptions& assumptions_,
                                     MaybeCls success, Maybe fail) {
    assert(isValidClosureSEXP(closure));

    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));
    auto fun = tbl->baseline();
    Assumptions assumptions = assumptions_;
    fun->clearDisabledAssumptions(assumptions);

    auto frame = RList(FRAME(CLOENV(closure)));

    std::string closureName = name;
    if (name.compare("") == 0) {
        // Serach for name in environment
        for (auto e = frame.begin(); e != frame.end(); ++e) {
            if (*e == closure)
                closureName = CHAR(PRINTNAME(e.tag()));
        }
    }
    auto pirClosure = module->getOrDeclareRirClosure(closureName, closure, fun);
    OptimizationContext context(assumptions);
    compileClosure(pirClosure, context, success, fail);
}

void Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                      const std::string& name, SEXP formals,
                                      SEXP srcRef,
                                      const Assumptions& assumptions_,
                                      MaybeCls success, Maybe fail) {
    Assumptions assumptions = assumptions_;
    srcFunction->clearDisabledAssumptions(assumptions);
    OptimizationContext context(assumptions);
    auto closure =
        module->getOrDeclareRirFunction(name, srcFunction, formals, srcRef);
    compileClosure(closure, context, success, fail);
}

void Rir2PirCompiler::compileClosure(Closure* closure,
                                     const OptimizationContext& ctx,
                                     MaybeCls success, Maybe fail) {

    if (!ctx.assumptions.includes(minimalAssumptions)) {
        for (const auto& a : minimalAssumptions) {
            if (!ctx.assumptions.includes(a)) {
                std::stringstream as;
                as << "Missing minimal assumption " << a;
                logger.warn(as.str());
                return fail();
            }
        }
    }

    // Currently dots args are not supported in PIR. Unless if we statically
    // matched all arguments correctly and are therefore guaranteed to receive a
    // `...` list as DOTSXP in the correct location, we can support them.
    // TODO: extend call instruction to do the necessary argument shuffling to
    // support it in all cases
    if (!ctx.assumptions.includes(Assumption::StaticallyArgmatched) &&
        closure->formals().hasDots()) {
        closure->rirFunction()->flags.set(Function::NotOptimizable);
        logger.warn("no support for ...");
        return fail();
    }

    if (closure->rirFunction()->body()->codeSize > Parameter::MAX_INPUT_SIZE) {
        closure->rirFunction()->flags.set(Function::NotOptimizable);
        logger.warn("skipping huge function");
        return fail();
    }

    if (auto existing = closure->findCompatibleVersion(ctx))
        return success(existing);

    auto version = closure->declareVersion(ctx);
    Builder builder(version, closure->closureEnv());
    auto& log = logger.begin(version);
    Rir2Pir rir2pir(*this, closure->rirFunction(), log, closure->name());

    auto& assumptions = version->assumptions();

    bool failedToCompileDefaultArgs = false;
    auto compileDefaultArg = [&](size_t idx) {
        if (closure->formals().names()[idx] == R_DotsSymbol) {
            // function(...=exp) is syntactically valid but a runtime error
            failedToCompileDefaultArgs = true;
            return;
        }

        auto arg = closure->formals().defaultArgs()[idx];
        Value* res = nullptr;
        if (TYPEOF(arg) != EXTERNALSXP) {
            // A bit of a hack to compile default args, which somehow
            // are not compiled.
            // TODO: why are they sometimes not compiled??
            auto funexp = rir::Compiler::compileExpression(arg);
            preserve_(funexp);
            arg = Function::unpack(funexp)->body()->container();
        }
        if (rir::Code::check(arg)) {
            auto code = rir::Code::unpack(arg);
            res = rir2pir.tryCreateArg(code, builder, false);
            if (!res) {
                failedToCompileDefaultArgs = true;
                return;
            }
            if (MkArg::Cast(res)) {
                // Need to cast promise-as-a-value to lazy-value, to make
                // it evaluate on access
                res = builder(new CastType(res, CastType::Upcast, RType::prom,
                                           PirType::any()));
            }
        }

        builder(new StArg(closure->formals().names()[idx], res, builder.env));
    };

    if (closure->formals().hasDefaultArgs()) {
        if (!ctx.assumptions.includes(Assumption::NoExplicitlyMissingArgs)) {
            for (unsigned i = 0;
                 i < closure->nargs() - assumptions.numMissing(); ++i) {
                if (closure->formals().defaultArgs()[i] != R_MissingArg) {
                    // If this arg has a default, then test if the argument is
                    // missing and if so, load the default arg.
                    auto a = builder(new LdArg(i));
                    auto testMissing =
                        builder(new Identical(a, MissingArg::instance()));
                    builder(new Branch(testMissing));

                    auto isMissing = builder.createBB();
                    auto notMissing = builder.createBB();
                    auto done = builder.createBB();

                    builder.setBranch(isMissing, notMissing);
                    builder.enterBB(isMissing);

                    compileDefaultArg(i);

                    builder.setNext(done);

                    builder.enterBB(notMissing);
                    builder.setNext(done);

                    builder.enterBB(done);
                }
            }
        }

        // if we supplied less arguments than required, we know the rest is
        // missing
        for (unsigned i = closure->nargs() - assumptions.numMissing();
             i < closure->nargs(); ++i)
            if (closure->formals().defaultArgs()[i] != R_MissingArg)
                compileDefaultArg(i);
    }

    if (failedToCompileDefaultArgs) {
        logger.warn("Failed to compile default arg");
        logger.close(version);
        closure->erase(ctx);
        return fail();
    }

    if (rir2pir.tryCompile(builder)) {
        log.compilationEarlyPir(version);
#ifdef FULLVERIFIER
        Verify::apply(version, "Error after initial translation", true);
#else
#ifndef NDEBUG
        Verify::apply(version, "Error after initial translation");
#endif
#endif
        log.flush();
        return success(version);
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
    for (const auto& translation : PassScheduler::instance()) {
        module->eachPirClosure([&](Closure* c) {
            c->eachVersion([&](ClosureVersion* v) {
                auto log = logger.get(v).forPass(passnr);
                log.pirOptimizationsHeader(translation.get());

                if (MEASURE_COMPILER_PERF)
                    startTime = std::chrono::high_resolution_clock::now();

                translation->apply(*this, v, log.out());
                if (MEASURE_COMPILER_PERF) {
                    endTime = std::chrono::high_resolution_clock::now();
                    std::chrono::duration<double> passDuration =
                        endTime - startTime;
                    PERF->addTime(translation->getName(), passDuration.count());
                }

                log.pirOptimizations(translation.get());
                log.flush();

#ifdef FULLVERIFIER
                Verify::apply(v, "Error after pass " + translation->getName(),
                              true);
#else
#ifdef ENABLE_SLOWASSERT
                Verify::apply(v, "Error after pass " + translation->getName());
#endif
#endif
            });
        });
        passnr++;
    }
    if (MEASURE_COMPILER_PERF)
        startTime = std::chrono::high_resolution_clock::now();

    module->eachPirClosure([&](Closure* c) {
        c->eachVersion([&](ClosureVersion* v) {
            logger.get(v).pirOptimizationsFinished(v);
#ifdef ENABLE_SLOWASSERT
            Verify::apply(v, "Error after optimizations", true);
#else
#ifndef NDEBUG
            Verify::apply(v, "Error after optimizations");
#endif
#endif
        });
    });

    if (MEASURE_COMPILER_PERF) {
        endTime = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> passDuration = endTime - startTime;
        PERF->addTime("Verification", passDuration.count());
    }

    logger.flush();
}

size_t Parameter::MAX_INPUT_SIZE =
    getenv("PIR_MAX_INPUT_SIZE") ? atoi(getenv("PIR_MAX_INPUT_SIZE")) : 6000;

} // namespace pir
} // namespace rir
