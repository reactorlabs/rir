#include "R/Symbols.h"
#include "bc/Compiler.h"
#include "compiler/compiler.h"
#include "compiler/parameter.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/arg_match.h"
#include "compiler/util/visitor.h"
#include "interpreter/instance.h"
#include "interpreter/interp_incl.h"
#include "pass_definitions.h"
#include "runtime/DispatchTable.h"

namespace rir {
namespace pir {

// Try to match callsite arguments to formals
bool MatchCallArgs::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                          AbstractLog& log, size_t) const {
    bool anyChange = false;
    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto i = *ip;

            if (auto calli = CallInstruction::CastCall(i)) {
                SEXP usemethodTarget = nullptr;

                auto staticCall = StaticCall::Cast(i);
                if (staticCall && staticCall->nCallArgs() > 0) {
                    auto ast = src_pool_at(
                        staticCall->tryGetCls()->rirFunction()->body()->src);
                    if (CAR(ast) == symbol::UseMethod &&
                        TYPEOF(CADR(ast)) == STRSXP &&
                        CDDR(ast) == R_NilValue) {
                        bool nonObj = false;
                        auto testNonObj = [&](Value* v) {
                            if (v->type.isA(PirType::valOrLazy()
                                                .notT(RType::prom)
                                                .notObject()))
                                nonObj = true;
                        };
                        if (auto d =
                                DotsList::Cast(staticCall->callArg(0).val())) {
                            if (d->nargs() > 0)
                                testNonObj(d->arg(0).val());
                        } else {
                            testNonObj(staticCall->callArg(0).val());
                        }
                        if (nonObj) {
                            auto method = CHAR(STRING_ELT(CADR(ast), 0));
                            auto defName = Rf_install(
                                (std::string(method) + ".default").c_str());
                            auto def = Rf_findVar(defName, R_BaseEnv);
                            if (TYPEOF(def) == PROMSXP)
                                def = PRVALUE(def);
                            if (def && TYPEOF(def) == CLOSXP) {
                                usemethodTarget = def;
                            }
                        } else {
                            ip = next;
                            continue;
                        }
                    }
                }

                if (!Call::Cast(i) && !NamedCall::Cast(i) && !usemethodTarget) {
                    ip = next;
                    continue;
                }

                SEXP formals = nullptr;
                ClosureVersion* target = nullptr;

                if (usemethodTarget) {
                    formals = FORMALS(usemethodTarget);
                } else {
                    if (auto cls = calli->tryGetCls()) {
                        target = calli->tryDispatch(cls);
                        formals = cls->formals().original();
                    }
                    if (!target) {
                        if (auto cnst = Const::Cast(calli->tryGetClsArg())) {
                            if (TYPEOF(cnst->c()) == CLOSXP)
                                formals = FORMALS(cnst->c());
                        }
                        if (auto mk = MkCls::Cast(calli->tryGetClsArg())) {
                            formals = mk->formals;
                        }
                    }
                }

                std::vector<Value*> matchedArgs;
                ArglistOrder::CallArglistOrder argOrderOrig;
                auto call = Call::Cast(i);
                auto namedCall = NamedCall::Cast(i);
                bool staticallyArgmatched = false;

                if (formals) {
                    bool failed = false;
                    std::vector<std::pair<SEXP, Value*>> usemethodTargetArgs;
                    if (usemethodTarget) {
                        const auto& myFormals =
                            staticCall->tryGetCls()->formals();
                        size_t i = 0;
                        staticCall->eachCallArg([&](Value* v) {
                            if (v == MissingArg::instance()) {
                                if (myFormals.hasDefaultArgs() &&
                                    myFormals.nargs() > i) {
                                    auto def = myFormals.defaultArgs().at(i);
                                    if (TYPEOF(def) != LANGSXP &&
                                        TYPEOF(def) != SYMSXP &&
                                        TYPEOF(def) != BCODESXP &&
                                        TYPEOF(def) != EXTERNALSXP) {
                                        auto defA = cmp.module->c(def);
                                        usemethodTargetArgs.push_back(
                                            {myFormals.names().at(i), defA});
                                    } else {
                                        failed = true;
                                    }
                                } else {
                                    failed = true;
                                }
                            } else if (auto d = DotsList::Cast(v)) {
                                d->eachElement([&](SEXP n, Value* v) {
                                    usemethodTargetArgs.push_back({n, v});
                                });
                            } else {
                                auto n = myFormals.nargs() > i
                                             ? myFormals.names()[i]
                                             : R_NilValue;
                                usemethodTargetArgs.push_back({n, v});
                            }
                            i++;
                        });
                    }
                    staticallyArgmatched =
                        !failed &&
                        ArgumentMatcher::reorder(
                            [&](DotsList* d) {
                                ip = bb->insert(ip, d) + 1;
                                next = ip + 1;
                            },
                            formals,
                            {[&]() {
                                 if (usemethodTarget)
                                     return usemethodTargetArgs.size();
                                 return calli->nCallArgs();
                             },
                             [&](size_t i) -> Value* {
                                 if (usemethodTarget)
                                     return usemethodTargetArgs[i].second;
                                 return calli->callArg(i).val();
                             },
                             [&](size_t i) -> SEXP {
                                 if (usemethodTarget)
                                     return usemethodTargetArgs[i].first;
                                 SLOWASSERT(!namedCall ||
                                            i < namedCall->names.size());
                                 return namedCall ? namedCall->names[i]
                                                  : R_NilValue;
                             }},
                            matchedArgs, argOrderOrig);
                }

                Context asmpt;

                if (staticallyArgmatched) {
                    if (usemethodTarget) {
                        if (!DispatchTable::check(BODY(usemethodTarget)))
                            rir::Compiler::compileClosure(usemethodTarget);

                        auto cls = cmp.module->c(usemethodTarget);
                        Call fake(i->env(), cls, matchedArgs,
                                  Tombstone::framestate(), i->srcIdx);
                        asmpt = fake.inferAvailableAssumptions();
                    } else {
                        Call fake(i->env(), calli->tryGetClsArg(), matchedArgs,
                                  Tombstone::framestate(), i->srcIdx);
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

                    if (usemethodTarget) {
                        if (!DispatchTable::check(BODY(usemethodTarget)))
                            rir::Compiler::compileClosure(usemethodTarget);
                        if (auto dt =
                                DispatchTable::check(BODY(usemethodTarget))) {
                            std::stringstream ss;
                            ss << "unknown--fromOverride(" << dt->closureName
                               << ")";
                            cmp.compileClosure(
                                usemethodTarget, ss.str(), asmpt, false,
                                [&](ClosureVersion* fun) {
                                    cmp.optimizeClosureVersion(fun);
                                    target = fun;
                                },
                                []() {}, {});
                        }
                    } else if (auto cnst = Const::Cast(calli->tryGetClsArg())) {
                        if (auto dt = DispatchTable::check(BODY(cnst->c()))) {
                            if (dt->baseline()->body()->codeSize <
                                Parameter::RECOMPILE_THRESHOLD) {
                                std::stringstream ss;
                                ss << "unknown--fromConstant("
                                   << dt->closureName << ")";
                                cmp.compileClosure(
                                    cnst->c(), ss.str(), asmpt, false,
                                    [&](ClosureVersion* fun) {
                                        cmp.optimizeClosureVersion(fun);
                                        target = fun;
                                    },
                                    []() {}, {});
                            }
                        }
                    } else if (auto mk = MkCls::Cast(calli->tryGetClsArg())) {
                        if (auto cls = mk->tryGetCls())
                            target = cls->findCompatibleVersion(asmpt);
                        auto dt = mk->originalBody;
                        if (!target && dt) {
                            auto srcRef = mk->srcRef;
                            if (dt->baseline()->body()->codeSize <
                                Parameter::RECOMPILE_THRESHOLD) {
                                std::stringstream ss;
                                ss << "unknown--fromMkCls(" << dt->closureName
                                   << ")";
                                cmp.compileFunction(
                                    dt, ss.str(), formals, srcRef, asmpt,
                                    [&](ClosureVersion* fun) {
                                        mk->setCls(fun->owner());
                                        cmp.optimizeClosureVersion(fun);
                                        target = fun;
                                    },
                                    []() {}, {});
                            }
                        }
                    }
                }

                if (staticallyArgmatched && target) {
                    anyChange = true;
                    if (auto c = call) {
                        assert(!usemethodTarget);
                        auto cls = c->cls()->followCastsAndForce();
                        auto nc = new StaticCall(
                            c->env(), target, asmpt, matchedArgs, argOrderOrig,
                            c->frameStateOrTs(), c->srcIdx, cls);
                        (*ip)->replaceUsesAndSwapWith(nc, ip);
                    } else if (auto c = namedCall) {
                        assert(!usemethodTarget);
                        auto cls = c->cls()->followCastsAndForce();
                        auto nc = new StaticCall(
                            c->env(), target, asmpt, matchedArgs, argOrderOrig,
                            c->frameStateOrTs(), c->srcIdx, cls);
                        (*ip)->replaceUsesAndSwapWith(nc, ip);
                    } else if (auto c = staticCall) {
                        assert(usemethodTarget);
                        auto cls = cmp.module->c(usemethodTarget);
                        auto nc = new StaticCall(
                            c->env(), target, asmpt, matchedArgs, argOrderOrig,
                            c->frameStateOrTs(), c->srcIdx, cls);
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
