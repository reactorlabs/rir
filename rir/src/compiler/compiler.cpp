#include "compiler.h"
#include "R/RList.h"
#include "pir/continuation.h"
#include "pir/pir_impl.h"
#include "rir2pir/rir2pir.h"
#include "runtime/TypeFeedback.h"
#include "utils/Map.h"
#include "utils/measuring.h"

#include "compiler/analysis/query.h"
#include "compiler/analysis/verifier.h"
#include "compiler/opt/pass_definitions.h"
#include "compiler/opt/pass_scheduler.h"
#include "compiler/parameter.h"
#include "compiler/util/visitor.h"

#include "bc/BC.h"
#include "bc/Compiler.h"

#include <chrono>
#include <unordered_map>
#include <unordered_set>

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
    compileClosure(pirClosure, tbl->dispatch(assumptions), context, root,
                   success, fail, outerFeedback,
                   tbl->baseline()->typeFeedback());
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
    compileClosure(closure, src->dispatch(assumptions), context, false, success,
                   fail, outerFeedback, src->baseline()->typeFeedback());
}

void Compiler::compileContinuation(SEXP closure, rir::Function* curFun,
                                   const ContinuationContext* ctx,
                                   MaybeCnt success, Maybe fail) {

    assert(isValidClosureSEXP(closure));

    DispatchTable* tbl = DispatchTable::unpack(BODY(closure));
    auto fun = tbl->baseline();

    auto pirClosure = module->getOrDeclareRirClosure(
        ctx->asDeoptContext() ? "deoptless" : "osr", closure, fun, {});
    auto version = pirClosure->declareContinuation(ctx, curFun);

    Builder builder(version, pirClosure->closureEnv());
    auto& log = logger.open(version);
    auto typeFeedback = tbl->baseline()->typeFeedback();

    REC_HOOK(recording::addCompilationSC(version, typeFeedback));

    Rir2Pir rir2pir(*this, version, log, pirClosure->name(), {}, typeFeedback);

    if (rir2pir.tryCompileContinuation(builder, ctx->pc(), ctx->stack())) {
        log.flush();
        return success(version);
    }

    log.failed("rir2pir aborted");
    log.flush();
    logger.close(version);
    return fail();
}

void Compiler::compileClosure(Closure* closure, rir::Function* optFunction,
                              const Context& ctx, bool root, MaybeCls success,
                              Maybe fail,
                              std::list<PirTypeFeedback*> outerFeedback,
                              rir::TypeFeedback* typeFeedback) {

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
    // matched all arguments correctly and are therefore guaranteed to
    // receive a
    // `...` list as DOTSXP in the correct location, we can support them.
    // TODO: extend call instruction to do the necessary argument shuffling
    // to support it in all cases
    if (!ctx.includes(Assumption::StaticallyArgmatched) &&
        closure->formals().hasDots()) {
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

    auto version = closure->declareVersion(ctx, root, optFunction);

    REC_HOOK(recording::addCompilationSC(version, typeFeedback));

    Builder builder(version, closure->closureEnv());
    auto& log = logger.open(version);
    Rir2Pir rir2pir(*this, version, log, closure->name(), outerFeedback,
                    typeFeedback);

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
        auto res = rir2pir.tryCreateArg(code, builder, false);
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
                    // If this arg has a default, then test if the argument
                    // is missing and if so, load the default arg.
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
        delete version;
        return fail();
    }

    RelaxContext::singleton().startRecording(version);

    if (rir2pir.tryCompile(builder)) {
#ifdef FULLVERIFIER
        Verify::apply(version, "Error after initial translation", true);
#else
#ifndef NDEBUG
        Verify::apply(version, "Error after initial translation");
#endif
#endif
        log.flush();
        RelaxContext::singleton().stopRecording();

        return success(version);
    }
    RelaxContext::singleton().stopRecording();

    log.failed("rir2pir aborted");
    log.flush();
    logger.close(version);
    closure->erase(ctx);
    delete version;
    return fail();
}

bool MEASURE_COMPILER_PERF = getenv("PIR_MEASURE_COMPILER") ? true : false;

static void findUnreachable(Module* m, Log& log, const std::string& where) {
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
                            if (!call->tryDispatch()) {
                                std::stringstream msg;
                                msg << "After pass " << where
                                    << " found a broken static call. "
                                       "Available "
                                       "versions:\n";
                                call->cls()->eachVersion(
                                    [&](ClosureVersion* v) {
                                        msg << v->context() << "\n";
                                    });
                                msg << "Available Assumptions:\n"
                                    << call->inferAvailableAssumptions()
                                    << "\n";
                                msg << "In:\n";
                                i->printRecursive(msg, 2);
                                log.warn(msg.str());
                            }
                            found(call->tryDispatch());
                            found(call->tryOptimisticDispatch());
                            found(call->hint);
                        } else if (auto call = CallInstruction::CastCall(i)) {
                            if (auto cls = call->tryGetCls())
                                found(call->tryDispatch(cls));
                        } else {
                            i->eachArg([&](Value* v) {
                                if (auto mk = MkCls::Cast(i)) {
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
            if (!reachableVersions.count(v->context())) {
                toErase.push_back({v->owner(), v->context()});
                log.close(v);
                delete v;
            }
        });
    });

    for (auto e : toErase)
        e.first->erase(e.second);
}

void Compiler::optimizeClosureVersion(ClosureVersion* v) {
    bool alreadyOptimizing = !currentlyOptimizing.empty();

    currentlyOptimizing.insert(v);
    if (alreadyOptimizing) {
        return;
    }

    auto apply = [&](ClosureVersion* v) {
        size_t passnr = 20;
        PassScheduler::quick().run(
            [&](const Pass* translation, size_t iteration) {
                auto& clog = logger.get(v);
                auto pirLog = clog.forPass(passnr++, translation->getName());
                bool changed = translation->apply(*this, v, clog, iteration);
                pirLog.pirOptimizations(translation);
                pirLog.flush();
                return changed;
            });
    };

    std::unordered_set<ClosureVersion*> done;
    while (done.size() < currentlyOptimizing.size()) {
        for (auto v : currentlyOptimizing)
            if (!done.count(v)) {
                done.insert(v);
                apply(v);
            }
    }
}

void Compiler::optimizeModule() {
    logger.flushAll();
    size_t passnr = 10;

    PassScheduler::instance().run([&](const Pass* translation,
                                      size_t iteration) {
        auto newIterStarted = translation->isPhaseMarker();

        bool changed = false;
        if (translation->isSlow()) {
            if (MEASURE_COMPILER_PERF)
                Measuring::startTimer("compiler.cpp: module cleanup");
            findUnreachable(module, logger, translation->getName());
            if (MEASURE_COMPILER_PERF)
                Measuring::countTimer("compiler.cpp: module cleanup");
        }

        if (newIterStarted) {
            module->eachPirClosure([&](Closure* c) {
                c->eachVersion([&](ClosureVersion* v) {
                    if (iteration == 0) {
                        // first iteration of new phase
                        v->anyChangePreviousIter = true;
                    } else if (iteration > 0) {
                        v->anyChangePreviousIter = v->anyChangeCurrentIter;
                    }

                    v->anyChangeCurrentIter = false;
                });
            });
        }

        module->eachPirClosure([&](Closure* c) {
            c->eachVersion([&](ClosureVersion* v) {
                auto& clog = logger.get(v);
                auto pirLog = clog.forPass(passnr, translation->getName());
                pirLog.pirOptimizationsHeader(translation);

                if (MEASURE_COMPILER_PERF)
                    Measuring::startTimer("compiler.cpp: " +
                                          translation->getName());

                if (v->anyChangePreviousIter) {
                    RelaxContext::singleton().startRecording(v); // **********
                    auto resApply =
                        translation->apply(*this, v, clog, iteration);
                    RelaxContext::singleton().stopRecording(); // **********

                    v->anyChangeCurrentIter |= resApply;
                    changed |= resApply;
                }

                if (MEASURE_COMPILER_PERF)
                    Measuring::countTimer("compiler.cpp: " +
                                          translation->getName());

                pirLog.pirOptimizations(translation);
                pirLog.flush();

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
            logger.get(v).pirOptimizationsFinished();
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

    // relax ctx
    static int totalCompilations = 0;
    static int totalRelaxed = 0;

    std::stringstream ss;

    std::cerr << "\n\n\n ------------- New compilation session "
                 "-----------------\n\n\n";

    module->eachPirClosure([&](Closure* c) {
        c->eachVersion([&](ClosureVersion* v) {
            totalCompilations++;

            ss << "\n\n\n";
            ss << "**************************************";
            ss << "\n";

            ss << "Function '" << c->name() << "' context '";
            ss << v->context() << "' can relax: \n\n";

            auto anyRelaxed = false;

            Visitor::run(v->entry, [&](Instruction* i) {
                if (auto ldArg = LdArg::Cast(i)) {
                    std::stringstream ldss;

                    auto anyRelaxedLdArg = false;
                    if (v->context().isNotObj(ldArg->pos) && !ldArg->notObj) {
                        ldss << "!Obj";
                        ldss << ", ";
                        anyRelaxedLdArg = true;
                    }

                    if ((v->context().isSimpleInt(ldArg->pos) ||
                         v->context().isSimpleReal(ldArg->pos)) &&
                        !ldArg->simpleScalar) {
                        ldss << "simpleScalar";
                        ldss << ", ";
                        anyRelaxedLdArg = true;
                    }

                    if (v->context().isEager(ldArg->pos) && !ldArg->eager) {
                        // ldArg->print(ss, true);
                        // ss << "\n";
                        ldss << "eager";
                        ldss << ", ";
                        anyRelaxedLdArg = true;
                    }

                    // std::cerr << v->nonReflArgsQueried;
                    if (v->context().isNonRefl(ldArg->pos) &&
                        !v->nonReflArgsQueried.count(ldArg->pos)) {
                        ldss << "nonRefl";
                        ldss << ", ";
                        anyRelaxedLdArg = true;
                        // assert(false);
                    }

                    if (anyRelaxedLdArg) {
                        ldArg->print(ss, true);
                        ss << "\n";
                        ss << ldss.rdbuf();
                        ss << "\n";
                    }

                    anyRelaxed |= anyRelaxedLdArg;

                    ss << "\n";
                }
            });

            if (anyRelaxed) {

                totalRelaxed++;

                ss << "\n";
                ss << "****************************************";
                ss << "\n\n\n";
                std::cerr << ss.str();
            }
        });
    });

    std::cerr << "============================================================="
                 "========\n";
    std::cerr << "Relaxed " << totalRelaxed << " out of " << totalCompilations
              << " compilations (";
    std::cerr << round((float)totalRelaxed * 100 / totalCompilations);
    std::cerr << "%) in total \n";
    std::cerr << "============================================================="
                 "========\n\n";

    logger.flushAll();
}

size_t Parameter::MAX_INPUT_SIZE =
    getenv("PIR_MAX_INPUT_SIZE") ? atoi(getenv("PIR_MAX_INPUT_SIZE")) : 12000;
size_t Parameter::RECOMPILE_THRESHOLD =
    getenv("PIR_RECOMPILE_THRESHOLD") ? atoi(getenv("PIR_RECOMPILE_THRESHOLD"))
                                      : 2000;

} // namespace pir
} // namespace rir
