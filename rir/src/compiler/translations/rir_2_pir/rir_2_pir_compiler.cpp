#include "rir_2_pir_compiler.h"
#include "../../pir/pir_impl.h"
#include "R/RList.h"
#include "rir_2_pir.h"

#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../opt/pass_definitions.h"
#include "ir/BC.h"
#include "ir/Compiler.h"

#include "interpreter/runtime.h"

namespace rir {
namespace pir {

// Currently PIR optimized functions cannot handle too many arguments or
// mis-ordered arguments. The caller needs to take care.
const Assumptions::Flags Rir2PirCompiler::minimalAssumptions =
    Assumptions::Flags(
        {Assumption::CorrectOrderOfArguments, Assumption::NotTooManyArguments});
const Assumptions Rir2PirCompiler::defaultAssumptions = Assumptions(
    {Assumption::CorrectOrderOfArguments, Assumption::NotTooManyArguments}, 0);

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
    auto fun = tbl->baseline();

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
    OptimizationContext context(assumptions);
    compileClosure(pirClosure, context, success, fail);
}

void Rir2PirCompiler::compileFunction(rir::Function* srcFunction,
                                      const std::string& name, SEXP formals,
                                      SEXP srcRef,
                                      const Assumptions& assumptions,
                                      MaybeCls success, Maybe fail) {
    OptimizationContext context(assumptions);
    auto closure =
        module->getOrDeclareRirFunction(name, srcFunction, formals, srcRef);
    compileClosure(closure, context, success, fail);
}

void Rir2PirCompiler::compileClosure(Closure* closure,
                                     const OptimizationContext& ctx,
                                     MaybeCls success, Maybe fail_) {

    if (!ctx.assumptions.includes(minimalAssumptions)) {
        for (const auto& a : minimalAssumptions) {
            if (!ctx.assumptions.includes(a)) {
                std::stringstream as;
                as << a;
                return fail_();
            }
        }
    }

    if (closure->formals().hasDefaultArgs()) {
        if (!ctx.assumptions.includes(Assumption::NoExplicitlyMissingArgs)) {
            logger.warn("TODO: don't know which are explicitly missing");
            return fail_();
        }
        if (!ctx.assumptions.includes(Assumption::NotTooFewArguments)) {
            logger.warn("TODO: don't know how many are missing");
            return fail_();
        }
    }

    // Above failures are context dependent. From here on we assume that
    // failures always happen, so we mark the function as unoptimizable on
    // failure.
    auto fail = [&]() {
        closure->rirFunction()->unoptimizable = true;
        fail_();
    };

    if (closure->formals().hasDots()) {
        logger.warn("no support for ...");
        return fail();
    }

    if (closure->rirFunction()->body()->codeSize > MAX_INPUT_SIZE) {
        logger.warn("skipping huge function");
        return fail();
    }

    if (auto existing = closure->findCompatibleVersion(ctx))
        return success(existing);

    auto version = closure->declareVersion(ctx);

    Builder builder(version, closure->closureEnv());
    auto& log = logger.begin(version);
    Rir2Pir rir2pir(*this, closure->rirFunction(), log, closure->name());

    Protect protect;
    auto& assumptions = version->assumptions();
    for (unsigned i = closure->nargs() - assumptions.numMissing();
         i < closure->nargs(); ++i) {
        if (closure->formals().hasDefaultArgs()) {
            auto arg = closure->formals().defaultArgs()[i];
            if (arg != R_MissingArg) {
                Value* res = nullptr;
                if (TYPEOF(arg) != EXTERNALSXP) {
                    // A bit of a hack to compile default args, which somehow
                    // are not compiled.
                    // TODO: why are they sometimes not compiled??
                    auto funexp = rir::Compiler::compileExpression(arg);
                    protect(funexp);
                    arg = Function::unpack(funexp)->body()->container();
                }
                if (rir::Code::check(arg)) {
                    auto code = rir::Code::unpack(arg);
                    res = rir2pir.tryCreateArg(code, builder, false);
                    if (!res) {
                        logger.warn("Failed to compile default arg");
                        return fail();
                    }
                    // Need to cast promise-as-a-value to lazy-value, to make
                    // it evaluate on access
                    res =
                        builder(new CastType(res, RType::prom, PirType::any()));
                }

                builder(
                    new StArg(closure->formals().names()[i], res, builder.env));
            }
        }
    }

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

void Rir2PirCompiler::optimizeModule() {
    logger.flush();
    size_t passnr = 0;
    for (auto& translation : translations) {
        module->eachPirClosure([&](Closure* c) {
            c->eachVersion([&](ClosureVersion* v) {
                auto& log = logger.get(v);
                log.pirOptimizationsHeader(v, translation, passnr++);
                translation->apply(*this, v, log);
                log.pirOptimizations(v, translation);

#ifdef ENABLE_SLOWASSERT
                assert(Verify::apply(v));
#endif
            });
        });
    }
    module->eachPirClosure([&](Closure* c) {
        c->eachVersion([&](ClosureVersion* v) {
            logger.get(v).pirOptimizationsFinished(v);
#ifdef ENABLE_SLOWASSERT
            assert(Verify::apply(v, true));
#else
            assert(Verify::apply(v));
#endif
        });
    });
    logger.flush();
}

} // namespace pir
} // namespace rir
