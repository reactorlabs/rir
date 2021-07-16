#include "R/Symbols.h"
#include "compiler/compiler.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/arg_match.h"
#include "compiler/util/visitor.h"
#include "interpreter/instance.h"
#include "interpreter/interp_incl.h"
#include "ir/Compiler.h"
#include "pass_definitions.h"
#include "runtime/DispatchTable.h"

namespace rir {
namespace pir {

// Try to match callsite arguments to formals
bool MatchCallArgs::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                          LogStream& log) const {
    bool anyChange = false;
    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;

            if (auto calli = CallInstruction::CastCall(*ip)) {
                SEXP overrideTarget = nullptr;
                if (auto cls = calli->tryGetCls()) {
                    auto ast = src_pool_at(globalContext(),
                                           cls->rirFunction()->body()->src);
                    if (CAR(ast) == symbol::UseMethod &&
                        TYPEOF(CADR(ast)) == STRSXP &&
                        CDDR(ast) == R_NilValue) {
                        bool nonObj = true;
                        calli->eachCallArg([&](Value* v) {
                            if (!v->type.isA(PirType::valOrLazy().notObject()))
                                nonObj = false;
                        });
                        if (nonObj) {
                            auto method = CHAR(STRING_ELT(CADR(ast), 0));
                            auto defName = Rf_install(
                                (std::string(method) + ".default").c_str());
                            auto def = Rf_findVar(defName, R_BaseEnv);
                            if (TYPEOF(def) == PROMSXP)
                                def = PRVALUE(def);
                            if (def && TYPEOF(def) == CLOSXP) {
                                overrideTarget = def;
                            }
                        } else {
                            ip = next;
                            continue;
                        }
                    }
                }

                if (!Call::Cast(*ip) && !NamedCall::Cast(*ip) &&
                    !overrideTarget) {
                    ip = next;
                    continue;
                }

                SEXP formals = nullptr;
                ClosureVersion* target = nullptr;

                if (overrideTarget) {
                    formals = FORMALS(overrideTarget);
                } else {
                    if (auto cls = calli->tryGetCls()) {
                        target = calli->tryDispatch(cls);
                        formals = cls->formals().original();
                    }
                    if (!target) {
                        if (auto cnst = LdConst::Cast(calli->tryGetClsArg())) {
                            if (TYPEOF(cnst->c()) == CLOSXP)
                                formals = FORMALS(cnst->c());
                        }
                        if (auto mk = MkFunCls::Cast(calli->tryGetClsArg())) {
                            formals = mk->formals;
                        }
                    }
                }

                std::vector<Value*> matchedArgs;
                ArglistOrder::CallArglistOrder argOrderOrig;
                auto call = Call::Cast(*ip);
                auto namedCall = NamedCall::Cast(*ip);
                auto staticCall = StaticCall::Cast(*ip);
                bool staticallyArgmatched = false;

                if (formals) {
                    staticallyArgmatched = ArgumentMatcher::reorder(
                        [&](DotsList* d) {
                            ip = bb->insert(ip, d) + 1;
                            next = ip + 1;
                        },
                        formals,
                        {[&]() { return calli->nCallArgs(); },
                         [&](size_t i) { return calli->callArg(i).val(); },
                         [&](size_t i) {
                             SLOWASSERT(!namedCall ||
                                        i < namedCall->names.size());
                             return namedCall ? namedCall->names[i]
                                              : R_NilValue;
                         }},
                        matchedArgs, argOrderOrig);
                }

                Context asmpt;

                if (staticallyArgmatched) {
                    if (staticCall) {
                        StaticCall fake((*ip)->env(), calli->tryGetCls(),
                                        Context(), matchedArgs, argOrderOrig,
                                        Tombstone::framestate(), (*ip)->srcIdx);
                        asmpt = fake.inferAvailableAssumptions();
                    } else {
                        Call fake((*ip)->env(), calli->tryGetClsArg(),
                                  matchedArgs, Tombstone::framestate(),
                                  (*ip)->srcIdx);
                        asmpt = fake.inferAvailableAssumptions();
                    }

                    // We can add these because arguments will be statically
                    // matched
                    asmpt.add(Assumption::StaticallyArgmatched);
                    asmpt.add(Assumption::CorrectOrderOfArguments);
                    asmpt.add(Assumption::NotTooManyArguments);
                    asmpt.add(Assumption::NoExplicitlyMissingArgs);
                    for (auto a : matchedArgs)
                        if (a == MissingArg::instance())
                            asmpt.remove(Assumption::NoExplicitlyMissingArgs);
                    asmpt.numMissing(Rf_length(formals) - matchedArgs.size());

                    if (overrideTarget) {
                        if (!DispatchTable::check(BODY(overrideTarget)))
                            rir::Compiler::compileClosure(overrideTarget);
                        if (DispatchTable::check(BODY(overrideTarget)))
                            cmp.compileClosure(overrideTarget,
                                               "unknown--fromOverride", asmpt,
                                               false,
                                               [&](ClosureVersion* fun) {
                                                   target = fun;
                                               },
                                               []() {}, {});
                    } else if (auto cnst =
                                   LdConst::Cast(calli->tryGetClsArg())) {
                        if (DispatchTable::check(BODY(cnst->c())))
                            cmp.compileClosure(
                                cnst->c(), "unknown--fromConstant", asmpt,
                                false,
                                [&](ClosureVersion* fun) { target = fun; },
                                []() {}, {});
                    } else if (auto mk =
                                   MkFunCls::Cast(calli->tryGetClsArg())) {
                        if (auto cls = mk->tryGetCls())
                            target = cls->findCompatibleVersion(asmpt);
                        auto dt = mk->originalBody;
                        if (!target && dt) {
                            auto srcRef = mk->srcRef;
                            cmp.compileFunction(dt, "unknown--fromMkFunCls",
                                                formals, srcRef, asmpt,
                                                [&](ClosureVersion* fun) {
                                                    mk->setCls(fun->owner());
                                                    target = fun;
                                                },
                                                []() {}, {});
                        }
                    }
                }

                if (staticallyArgmatched && target) {
                    anyChange = true;
                    Value* cls = nullptr;
                    if (overrideTarget) {
                        ip = bb->insert(ip, new LdConst(overrideTarget));
                        cls = *ip;
                        ip++;
                        next = ip + 1;
                    }
                    if (auto c = call) {
                        if (!cls)
                            cls = c->cls()->followCastsAndForce();
                        auto nc = new StaticCall(
                            c->env(), target->owner(), asmpt, matchedArgs,
                            argOrderOrig, c->frameStateOrTs(), c->srcIdx, cls);
                        (*ip)->replaceUsesAndSwapWith(nc, ip);
                    } else if (auto c = namedCall) {
                        if (!cls)
                            cls = c->cls()->followCastsAndForce();
                        auto nc = new StaticCall(
                            c->env(), target->owner(), asmpt, matchedArgs,
                            argOrderOrig, c->frameStateOrTs(), c->srcIdx, cls);
                        (*ip)->replaceUsesAndSwapWith(nc, ip);
                    } else if (auto c = staticCall) {
                        assert(overrideTarget);
                        assert(cls);
                        auto nc = new StaticCall(
                            c->env(), target->owner(), asmpt, matchedArgs,
                            argOrderOrig, c->frameStateOrTs(), c->srcIdx, cls);
                        (*ip)->replaceUsesAndSwapWith(nc, ip);
                    } else {
                        assert(false);
                    }
                }
            }

            ip = next;
        }
    });
    return anyChange;
}

} // namespace pir
} // namespace rir
