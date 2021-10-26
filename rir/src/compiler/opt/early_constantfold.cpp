#include "compiler/opt/pass_definitions.h"

#include "../pir/pir_impl.h"
#include "../util/phi_placement.h"
#include "../util/visitor.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/available_checkpoints.h"
#include "compiler/analysis/cfg.h"
#include "compiler/compiler.h"
#include "compiler/opt/type_test.h"
#include "compiler/util/bb_transform.h"
#include "interpreter/interp.h"
#include "runtime/DispatchTable.h"

#include <cmath>
#include <iterator>
#include <list>
#include <unordered_set>

namespace rir {
namespace pir {

static SEXP isStaticInternal(Call* call, const char* name) {
    if (auto c = Const::Cast(call->cls())) {
        if ((TYPEOF(c->c()) == SPECIALSXP || TYPEOF(c->c()) == BUILTINSXP) &&
            c->c()->u.primsxp.offset == blt(name))
            return c->c();
    }
    return nullptr;
}

static long isStaticForceAndCall(Call* call) {
    if (isStaticInternal(call, "forceAndCall")) {
        if (call->nCallArgs() >= 2) {
            if (auto n = Const::Cast(call->callArg(0).val())) {
                if (TYPEOF(n->c()) == INTSXP || TYPEOF(n->c()) == REALSXP) {
                    unsigned nForce = 0;
                    if (Rf_length(n->c()) > 0) {
                        if (TYPEOF(n->c()) == INTSXP) {
                            auto t = INTEGER(n->c())[0];
                            nForce = t < 0 || t == NA_INTEGER ? 0 : t;
                        } else {
                            auto t = REAL(n->c())[0];
                            nForce = t < 0 || t != t ? 0 : t;
                        }
                    }
                    return nForce;
                }
            }
        }
    }
    return -1;
}

bool EarlyConstantfold::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                              LogStream& log, size_t) const {
    bool anyChange = false;
    AvailableCheckpoints checkpoint(cls, code, log);
    DominanceGraph dom(code);

    Visitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

            // Constantfold forceAndCall if the n parameter is statically
            // known
            if (auto call = Call::Cast(i)) {
                auto nForce = isStaticForceAndCall(call);

                if (nForce == -1) {
                    ip = next;
                    continue;
                }

                bool nodots = true;
                for (unsigned i = 2; i < call->nCallArgs() && i < nForce + 2;
                     ++i) {
                    if (call->callArg(i).val()->type.isA(RType::expandedDots)) {
                        nodots = false;
                    }
                }

                if (!nodots) {
                    ip = next;
                    continue;
                }

                if (auto cp = checkpoint.at(call)) {

                    auto given = Instruction::Cast(call->callArg(1).val());
                    if (!given) {
                        ip = next;
                        continue;
                    }

                    const auto fb = given->callFeedback();
                    if (given->typeFeedbackUsed || fb.taken < 2 ||
                        (fb.type != CLOSXP && fb.type != BUILTINSXP &&
                         fb.type != SPECIALSXP)) {
                        ip = next;
                        continue;
                    }
                    assert(!fb.monomorphic ||
                           TYPEOF(fb.monomorphic) == fb.type);

                    anyChange = true;
                    given->typeFeedbackUsed = true;

                    Value* callee = given;
                    if (fb.monomorphic) {
                        callee = BBTransform::insertCalleeGuard(
                            cmp, fb,
                            DeoptReason(fb.feedbackOrigin,
                                        DeoptReason::ForceAndCall),
                            given, fb.stableEnv, nullptr, cp, bb, ip);
                    } else {
                        auto type =
                            (fb.type == CLOSXP ? PirType::closure()
                                               : (fb.type == BUILTINSXP
                                                      ? PirType::builtin()
                                                      : PirType::special()));

                        BBTransform::insertAssume(new IsType(type, given), true,
                                                  cp, fb.feedbackOrigin,
                                                  DeoptReason::ForceAndCall, bb,
                                                  ip);

                        if (auto argi = Instruction::Cast(given)) {
                            argi->typeFeedbackUsed = true;
                            auto cast = new CastType(argi, CastType::Downcast,
                                                     PirType::val(), type);
                            cast->effects.set(Effect::DependsOnAssume);
                            ip = bb->insert(ip, cast);
                            ip++;
                            argi->replaceDominatedUses(cast, dom);
                            callee = cast;
                        }
                    }

                    // "inline" forceAndCall
                    if (!callee->type.isA(PirType::function())) {
                        auto chk = new ChkFunction(callee);
                        ip = bb->insert(ip, chk);
                        ++ip;
                        callee = chk;
                    }

                    auto kind = fb.type;
                    std::vector<Value*> args;
                    for (unsigned i = 2; i < call->nCallArgs(); ++i) {
                        auto a = call->callArg(i).val();
                        if (kind != SPECIALSXP &&
                            (kind == BUILTINSXP || i - 2 < nForce)) {
                            if (a->type.isA(RType::prom) ||
                                a->type.maybePromiseWrapped()) {
                                auto mk = MkArg::Cast(a);
                                if (mk && mk->isEager()) {
                                    args.push_back(a);
                                    continue;
                                }
                                if (mk) {
                                    auto cast = new CastType(
                                        a, CastType::Upcast, RType::prom,
                                        PirType::any());
                                    ip = bb->insert(ip, cast);
                                    a = *ip;
                                    ++ip;
                                }
                                auto force = new Force(a, call->env(),
                                                       Tombstone::framestate());
                                ip = bb->insert(ip, force);
                                a = *ip;
                                ++ip;

                                if (kind == CLOSXP) {
                                    auto newMk =
                                        new MkArg(mk->prom(), a, call->env());
                                    ip = bb->insert(ip, newMk);
                                    a = *ip;
                                    ++ip;
                                }
                            }
                        }
                        args.push_back(a);
                    }

                    // rewrite the ast (essential if the callee is
                    // special)
                    auto origSrc = cp_pool_at(globalContext(), call->srcIdx);
                    auto newSrc =
                        PROTECT(LCONS(CADDR(origSrc), CDDDR(origSrc)));
                    auto newSrcIdx = cp_pool_add(globalContext(), newSrc);
                    UNPROTECT(1);

                    // replace the old forceAndCall
                    auto newCall = new Call(call->env(), callee, args,
                                            call->frameState(), newSrcIdx);
                    call->replaceUsesAndSwapWith(newCall, ip);
                    next = ip + 1;
                }
            }

            ip = next;
        }
    });

    return anyChange;
}

} // namespace pir
} // namespace rir
