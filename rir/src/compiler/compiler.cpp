#include "compiler.h"
#include "R/RList.h"
#include "pir/pir_impl.h"
#include "rir2pir/rir2pir.h"
#include "utils/Map.h"
#include "utils/measuring.h"

#include "compiler/analysis/query.h"
#include "compiler/analysis/verifier.h"
#include "compiler/opt/pass_definitions.h"
#include "compiler/opt/pass_scheduler.h"
#include "compiler/parameter.h"
#include "compiler/util/visitor.h"

#include "ir/BC.h"
#include "ir/Compiler.h"

#include <chrono>
#include <unordered_map>

namespace rir {
namespace pir {

// Currently PIR optimized functions cannot handle too many arguments or
// mis-ordered arguments. The caller needs to take care.
constexpr Context::Flags Compiler::minimalContext;
constexpr Context Compiler::defaultContext;

void Compiler::compileClosure(SEXP closure, const std::string& name,
                              const Context& assumptions_, bool root,
                              MaybeCls success, Maybe fail,
                              std::list<PirTypeFeedback*> outerFeedback) {
    assert(isValidClosureSEXP(closure));

    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));
    auto fun = tbl->baseline();

    Context assumptions = assumptions_;
    fun->clearDisabledAssumptions(assumptions);
    assumptions = tbl->combineContextWith(assumptions);

    auto frame = RList(FRAME(CLOENV(closure)));

    std::string closureName = name;
    if (name.compare("") == 0) {
        // Serach for name in environment
        for (auto e = frame.begin(); e != frame.end(); ++e) {
            if (*e == closure)
                closureName = CHAR(PRINTNAME(e.tag()));
        }
    }
    auto pirClosure = module->getOrDeclareRirClosure(closureName, closure, fun,
                                                     tbl->userDefinedContext());
    Context context(assumptions);
    compileClosure(pirClosure, tbl->baseline(), tbl->dispatch(assumptions),
                   context, root, success, fail, outerFeedback);
}

void Compiler::compileFunction(rir::DispatchTable* src, const std::string& name,
                               SEXP formals, SEXP srcRef,
                               const Context& assumptions_, MaybeCls success,
                               Maybe fail,
                               std::list<PirTypeFeedback*> outerFeedback) {
    Context assumptions = assumptions_;
    auto srcFunction = src->baseline();
    srcFunction->clearDisabledAssumptions(assumptions);
    assumptions = src->combineContextWith(assumptions);
    Context context(assumptions);
    auto closure = module->getOrDeclareRirFunction(
        name, srcFunction, formals, srcRef, src->userDefinedContext());
    compileClosure(closure, srcFunction, src->dispatch(assumptions), context,
                   false, success, fail, outerFeedback);
}

void Compiler::compileClosure(Closure* closure, rir::Function* srcCode,
                              rir::Function* optFunction, const Context& ctx,
                              bool root, MaybeCls success, Maybe fail,
                              std::list<PirTypeFeedback*> outerFeedback) {

    if (!ctx.includes(minimalContext)) {
        for (const auto a : minimalContext) {
            if (!ctx.includes(a)) {
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
    if (!ctx.includes(Assumption::StaticallyArgmatched) &&
        closure->formals().hasDots()) {
        logger.warn("no support for ...");
        return fail();
    }

    auto sz = closure->bodySize();
    if (sz > Parameter::MAX_INPUT_SIZE) {
        closure->rirFunction(
            [&](rir::Function* f) { f->flags.set(Function::NotOptimizable); });
        logger.warn("skipping huge function");
        return fail();
    }

    if (auto existing = closure->findCompatibleVersion(ctx))
        return success(existing);

    auto version = closure->declareVersion(ctx, root, optFunction);
    Builder builder(version, closure->closureEnv());
    auto& log = logger.begin(version);
    Rir2Pir rir2pir(*this, version, log, closure->name(), outerFeedback);

    auto& context = version->context();
    bool failedToCompileDefaultArgs = false;
    auto compileDefaultArg = [&](size_t idx) {
        if (closure->formals().names()[idx] == R_DotsSymbol) {
            // function(...=exp) is syntactically valid but a runtime error
            failedToCompileDefaultArgs = true;
            return;
        }

        auto arg = closure->formals().defaultArgs()[idx];
        assert(rir::Code::check(arg) && "Default arg not compiled");
        auto code = rir::Code::unpack(arg);
        auto res = rir2pir.tryCreateArg(code, builder, false, -1);
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

        builder(new StArg(closure->formals().names()[idx], res, builder.env));
    };

    if (closure->formals().hasDefaultArgs()) {
        if (!ctx.includes(Assumption::NoExplicitlyMissingArgs)) {
            for (unsigned i = 0; i < closure->nargs() - context.numMissing();
                 ++i) {
                if (closure->formals().defaultArgs()[i] != R_MissingArg) {
                    // If this arg has a default, then test if the argument is
                    // missing and if so, load the default arg.
                    auto a = builder(new LdArg(i));
                    auto testMissing = builder(new Identical(
                        a, MissingArg::instance(), PirType::any()));
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
        for (unsigned i = closure->nargs() - context.numMissing();
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

    if (rir2pir.tryCompile(srcCode->body(), builder)) {
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

static void findUnreachable(Module* m) {
    std::unordered_map<Closure*, std::unordered_set<Context>> reachable;
    bool changed = true;

    auto found = [&](ClosureVersion* v) {
        if (!v)
            return;
        auto& reachableVersions = reachable[v->owner()];
        if (!reachableVersions.count(v->context())) {
            reachableVersions.insert(v->context());
            changed = true;
        }
    };

    while (changed) {
        changed = false;
        m->eachPirClosure([&](Closure* c) {
            auto& reachableVersions = reachable[c];
            c->eachVersion([&](ClosureVersion* v) {
                if (v->root) {
                    if (!reachableVersions.count(v->context())) {
                        reachableVersions.insert(v->context());
                        changed = true;
                    }
                }
                if (reachableVersions.count(v->context())) {
                    auto check = [&](Instruction* i) {
                        if (auto call = StaticCall::Cast(i)) {
                            assert(call->tryDispatch());
                            found(call->tryDispatch());
                            found(call->tryOptimisticDispatch());
                        } else if (auto call = CallInstruction::CastCall(i)) {
                            if (auto cls = call->tryGetCls())
                                found(call->tryDispatch(cls));
                        } else {
                            i->eachArg([&](Value* v) {
                                if (auto mk = MkFunCls::Cast(i)) {
                                    if (mk->tryGetCls())
                                        mk->tryGetCls()->eachVersion(found);
                                }
                            });
                        }
                    };
                    Visitor::run(v->entry, check);
                    v->eachPromise(
                        [&](Promise* p) { Visitor::run(p->entry, check); });
                }
            });
        });
    }

    std::vector<std::pair<Closure*, Context>> toErase;
    m->eachPirClosure([&](Closure* c) {
        const auto& reachableVersions = reachable[c];
        c->eachVersion([&](ClosureVersion* v) {
            if (!reachableVersions.count(v->context()))
                toErase.push_back({v->owner(), v->context()});
        });
    });

    for (auto e : toErase)
        e.first->erase(e.second);
};

void Compiler::optimizeModule() {
    logger.flush();
    size_t passnr = 0;
    PassScheduler::instance().run([&](const Pass* translation) {
        bool changed = false;
        if (translation->isSlow()) {
            if (MEASURE_COMPILER_PERF)
                Measuring::startTimer("compiler.cpp: module cleanup");
            findUnreachable(module);
            if (MEASURE_COMPILER_PERF)
                Measuring::countTimer("compiler.cpp: module cleanup");
        }
        module->eachPirClosure([&](Closure* c) {
            c->eachVersion([&](ClosureVersion* v) {
                auto log = logger.get(v).forPass(passnr);
                log.pirOptimizationsHeader(translation);

                if (MEASURE_COMPILER_PERF)
                    Measuring::startTimer("compiler.cpp: " +
                                          translation->getName());

                if (translation->apply(*this, v, log.out()))
                    changed = true;
                if (MEASURE_COMPILER_PERF)
                    Measuring::countTimer("compiler.cpp: " +
                                          translation->getName());

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
        Measuring::startTimer("compiler.cpp: verification");

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

    if (MEASURE_COMPILER_PERF)
        Measuring::countTimer("compiler.cpp: verification");

    logger.flush();
}

size_t Parameter::MAX_INPUT_SIZE =
    getenv("PIR_MAX_INPUT_SIZE") ? atoi(getenv("PIR_MAX_INPUT_SIZE")) : 8000;

} // namespace pir
} // namespace rir
