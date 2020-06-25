#include "rir_2_pir_compiler.h"
#include "../../pir/pir_impl.h"
#include "R/RList.h"
#include "rir_2_pir.h"

#include "compiler/parameter.h"
#include "event_counters/event_stream.h"

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

using Clock = std::chrono::system_clock;
using Timestamp = Clock::time_point;

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
    compileClosure(pirClosure, tbl->dispatch(assumptions), context, success,
                   fail);
}

void Rir2PirCompiler::compileFunction(rir::DispatchTable* src,
                                      const std::string& name, SEXP formals,
                                      SEXP srcRef,
                                      const Assumptions& assumptions_,
                                      MaybeCls success, Maybe fail) {
    Assumptions assumptions = assumptions_;
    auto srcFunction = src->baseline();
    srcFunction->clearDisabledAssumptions(assumptions);
    OptimizationContext context(assumptions);
    auto closure =
        module->getOrDeclareRirFunction(name, srcFunction, formals, srcRef);
    compileClosure(closure, src->dispatch(assumptions), context, success, fail);
}

void Rir2PirCompiler::compileClosure(Closure* closure,
                                     rir::Function* optFunction,
                                     const OptimizationContext& ctx,
                                     MaybeCls success, Maybe fail) {
#ifdef MEASURE
    if (EventStream::isEnabled) {
        EventStream::instance().setNameOf(optFunction, closure->name());
    }

    MaybeCls originalSuccess = success;
    Maybe originalFail = fail;
    success = EventCounters::isEnabled ? [&](ClosureVersion* result) {
        if (EventCounters::isEnabled) {
            EventCounters::instance().count(events::PirOptimized);
        }
        originalSuccess(result);
    } : originalSuccess;
    fail = EventCounters::isEnabled ? [&]() {
        if (EventCounters::isEnabled) {
            EventCounters::instance().count(events::Unoptimizable);
        }
        originalFail();
    } : originalFail;

    Timestamp startTime = Clock::now();
    auto finishProfiling = [&]() {
        Timestamp endTime = Clock::now();
        Timestamp::duration duration = endTime - startTime;
        size_t durationMicros =
            (size_t)std::chrono::duration_cast<std::chrono::microseconds>(
                duration)
                .count();
        return durationMicros;
    };
#endif

    if (!ctx.assumptions.includes(minimalAssumptions)) {
        for (const auto& a : minimalAssumptions) {
            if (!ctx.assumptions.includes(a)) {
                std::stringstream as;
                as << "Missing minimal assumption " << a;
                logger.warn(as.str());

                fail();

#ifdef MEASURE
                size_t totalDuration = finishProfiling();
                if (EventStream::isEnabled) {
                    std::stringstream messageBuf;
                    messageBuf << "it's missing minimal assumption " << a;
                    EventStream::instance().recordEvent(
                        new EventStream::FailedPirCompiling(
                            optFunction, totalDuration, messageBuf.str()));
                }
#endif
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

        fail();

#ifdef MEASURE
        size_t totalDuration = finishProfiling();
        if (EventStream::isEnabled) {
            EventStream::instance().recordEvent(
                new EventStream::FailedPirCompiling(optFunction, totalDuration,
                                                    "it has '...'"));
        }
#endif

        return;
    }

    if (closure->rirFunction()->body()->codeSize > Parameter::MAX_INPUT_SIZE) {
        closure->rirFunction()->flags.set(Function::NotOptimizable);
        logger.warn("skipping huge function");

        fail();

#ifdef MEASURE
        size_t totalDuration = finishProfiling();
        if (EventStream::isEnabled) {
            EventStream::instance().recordEvent(
                new EventStream::FailedPirCompiling(optFunction, totalDuration,
                                                    "it's too big"));
        }
#endif

        return;
    }

    if (auto existing = closure->findCompatibleVersion(ctx)) {
        success(existing);

#ifdef MEASURE
        size_t totalDuration = finishProfiling();
        if (EventStream::isEnabled) {
            EventStream::instance().recordEvent(
                new EventStream::ReusedPirCompiled(existing, totalDuration));
        }
#endif

        return;
    }

    auto version = closure->declareVersion(ctx, optFunction);
    Builder builder(version, closure->closureEnv());
    auto& log = logger.begin(version);
    Rir2Pir rir2pir(*this, version, log, closure->name());

#ifdef MEASURE
    if (EventStream::isEnabled) {
        EventStream::instance().setNameOf(version);
        EventStream::instance().recordEvent(
            new EventStream::StartedPirCompiling(version, ctx.assumptions));
    }
#endif

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

        fail();

#ifdef MEASURE
        size_t totalDuration = finishProfiling();
        if (EventStream::isEnabled) {
            EventStream::instance().recordEvent(
                new EventStream::FailedPirCompiling(
                    version, totalDuration,
                    "we couldn't compile a default argument"));
        }
#endif

        return;
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

#ifdef MEASURE
        size_t totalDuration = finishProfiling();
        if (EventStream::isEnabled) {
            EventStream::instance().recordEvent(
                new EventStream::SucceededRir2Pir(version, totalDuration));
        }
#endif

        success(version);

#ifdef MEASURE
        totalDuration = finishProfiling();
        if (EventStream::isEnabled) {
            EventStream::instance().recordEvent(
                new EventStream::FinishedCompiling(version, totalDuration));
        }
#endif

        return;
    }

    log.failed("rir2pir aborted");
    log.flush();
    logger.close(version);
    closure->erase(ctx);

    fail();

#ifdef MEASURE
    size_t totalDuration = finishProfiling();
    if (EventStream::isEnabled) {
        EventStream::instance().recordEvent(new EventStream::FailedPirCompiling(
            version, totalDuration, "of an issue encountered in rir2pir"));
    }
#endif
}

bool MEASURE_COMPILER_PERF = getenv("PIR_MEASURE_COMPILER") ? true : false;
std::chrono::time_point<std::chrono::high_resolution_clock> startTime;
std::chrono::time_point<std::chrono::high_resolution_clock> endTime;
std::unique_ptr<CompilerPerf> PERF = std::unique_ptr<CompilerPerf>(
    MEASURE_COMPILER_PERF ? new CompilerPerf : nullptr);

void Rir2PirCompiler::optimizeModuleFor(const ClosureVersion* version) {
#ifdef MEASURE
    Timestamp startTime = Clock::now();
    auto finishProfiling = [&]() {
        Timestamp endTime = Clock::now();
        Timestamp::duration duration = endTime - startTime;
        size_t durationMicros =
            (size_t)std::chrono::duration_cast<std::chrono::microseconds>(
                duration)
                .count();
        return durationMicros;
    };
#endif

    // Silence any unused warning, the argument is passed because we need it
    // when MEASURE is enabled, but otherwise it's unused
    (void)version;
    optimizeModule();

#ifdef MEASURE
    if (EventStream::isEnabled) {
        size_t totalDuration = finishProfiling();
        EventStream::instance().recordEvent(
            new EventStream::OptimizedPir(version, totalDuration));
    }
#endif
}

void Rir2PirCompiler::optimizeModule() {
    logger.flush();
    size_t passnr = 0;
    PassScheduler::instance().run([&](const PirTranslator* translation) {
        bool changed = false;
        module->eachPirClosure([&](Closure* c) {
            c->eachVersion([&](ClosureVersion* v) {
                auto log = logger.get(v).forPass(passnr);
                log.pirOptimizationsHeader(translation);

                if (MEASURE_COMPILER_PERF)
                    startTime = std::chrono::high_resolution_clock::now();

                if (translation->apply(*this, v, log.out()))
                    changed = true;
                if (MEASURE_COMPILER_PERF) {
                    endTime = std::chrono::high_resolution_clock::now();
                    std::chrono::duration<double> passDuration =
                        endTime - startTime;
                    PERF->addTime(translation->getName(), passDuration.count());
                }

                log.pirOptimizations(translation);
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
        return changed;
    });
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
