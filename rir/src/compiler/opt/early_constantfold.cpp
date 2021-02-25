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

static long isStaticForceAndCall(Call* call) {
    if (auto c = LdConst::Cast(call->cls())) {
        if (TYPEOF(c->c()) == SPECIALSXP &&
            c->c()->u.primsxp.offset == blt("forceAndCall") &&
            call->nCallArgs() >= 2) {
            if (auto n = LdConst::Cast(call->callArg(0).val())) {
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
                              LogStream&) const {
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
                         i < call->nCallArgs() && i <= nForce + 2; ++i) {
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
                            if (i - 2 <= nForce) {
                                ip = bb->insert(
                                    ip,
                                    new CastType(call->callArg(i).val(),
                                                 CastType::Upcast, RType::prom,
                                                 PirType::any()));
                                ip = bb->insert(
                                    ip + 1, new Force(*ip, call->env(),
                                                      Tombstone::framestate()));
                                args.push_back(*ip);
                                ip++;
                            } else {
                                args.push_back(call->callArg(i).val());
                            }
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
