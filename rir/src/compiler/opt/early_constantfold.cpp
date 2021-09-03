#include "compiler/opt/pass_definitions.h"

#include "../pir/pir_impl.h"
#include "../util/phi_placement.h"
#include "../util/visitor.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/cfg.h"
#include "compiler/compiler.h"
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

bool EarlyConstantfold::apply(Compiler&, ClosureVersion* cls, Code* code,
                              LogStream&, size_t) const {
    bool anyChange = false;

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
                if (nForce != -1) {

                    bool nodots = true;
                    for (unsigned i = 2;
                         i < call->nCallArgs() && i < nForce + 2; ++i) {
                        if (call->callArg(i).val()->type.isA(
                                RType::expandedDots)) {
                            nodots = false;
                        }
                    }

                    if (nodots) {
                        anyChange = true;
                        ip = bb->insert(ip,
                                        new ChkClosure(call->callArg(1).val()));
                        auto callee = *ip;
                        ip++;
                        std::vector<Value*> args;
                        for (unsigned i = 2; i < call->nCallArgs(); ++i) {
                            auto a = call->callArg(i).val();
                            if (i - 2 < nForce) {
                                if (a->type.isA(RType::prom)) {
                                    ip = bb->insert(
                                        ip, new CastType(a, CastType::Upcast,
                                                         RType::prom,
                                                         PirType::any()));
                                    a = *ip;
                                    ip++;
                                }
                                if (a->type.isA(RType::prom) ||
                                    a->type.maybePromiseWrapped()) {
                                    ip = bb->insert(
                                        ip, new Force(a, call->env(),
                                                      Tombstone::framestate()));
                                    a = *ip;
                                    ip++;
                                }
                            }
                            args.push_back(a);
                        }
                        auto newCall =
                            new Call(call->env(), callee, args,
                                     call->frameState(), call->srcIdx);
                        call->replaceUsesAndSwapWith(newCall, ip);
                        next = ip + 1;
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
