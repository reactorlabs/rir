#include "scope_resolution.h"
#include "../analysis/query.h"
#include "../analysis/scope.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;
class TheScopeResolution {
  public:
    Closure* function;
    CFG cfg;
    TheScopeResolution(Closure* function)
        : function(function), cfg(function->entry) {}
    void operator()() {
        ScopeAnalysis analysis(function);

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                LdArg* lda = LdArg::Cast(i);
                LdFun* ldfun = LdFun::Cast(i);
                Instruction* ld = LdVar::Cast(i);
                StVar* s = StVar::Cast(i);
                StVarSuper* ss = StVarSuper::Cast(i);
                LdVarSuper* sld = LdVarSuper::Cast(i);
                if (lda)
                    ld = lda;
                else if (ldfun)
                    ld = ldfun;

                if (sld) {
                    // LdVarSuper where the parent environment is known and
                    // local, can be replaced by a simple LdVar
                    auto e = Env::parentEnv(sld->env());
                    if (e) {
                        auto r = new LdVar(sld->varName, e);
                        bb->replace(ip, r);
                        sld->replaceUsesWith(r);
                    }
                } else if (ss) {
                    // StVarSuper where the parent environment is known and
                    // local, can be replaced by simple StVar, if the variable
                    // exists in the super env.
                    auto e = Env::parentEnv(ss->env());
                    auto aload = analysis.loads.at(ss);
                    if (e && aload.env != AbstractREnvironment::UnknownParent) {
                        if ((Env::isPirEnv(aload.env) &&
                             !aload.result.isUnknown()) ||
                            Env::isStaticEnv(aload.env)) {
                            auto r = new StVar(ss->varName, ss->arg<0>().val(),
                                               aload.env);
                            bb->replace(ip, r);
                            ss->replaceUsesWith(r);
                        }
                    }
                } else if (s) {
                    // Dead store to non-escaping environment can be removed
                    if (Env::isPirEnv(s->env()) &&
                        !analysis.finalState[s->env()].leaked &&
                        analysis.deadStore(s)) {
                        next = bb->remove(ip);
                    }
                } else if (ld && analysis.loads.count(ld)) {
                    // If we have a non-ambiguous load, we can replace the load
                    // with the actual values.
                    auto aload = analysis.loads.at(ld);
                    auto aval = aload.result;
                    // inter-procedural scope analysis can drag in values
                    // from other functions, which we cannot use here!
                    bool onlyLocalVals = true;
                    aval.eachSource([&](ValOrig& src) {
                        if (src.origin->bb()->owner != function)
                            onlyLocalVals = false;
                    });
                    if (aval.isUnknown() &&
                        aload.env != AbstractREnvironment::UnknownParent) {
                        // We have no clue what we load, but we know from where
                        ld->env(aload.env);
                    } else if (onlyLocalVals) {
                        auto replaceLdFun = [&](Value* val) {
                            if (val->type.isA(PirType::closure())) {
                                next = bb->remove(ip);
                                ld->replaceUsesWith(val);
                                return;
                            }
                            auto fz = new Force(val);
                            auto ch = new ChkClosure(fz);
                            bb->replace(ip, fz);
                            next = bb->insert(ip + 1, ch);
                            next++;
                            ld->replaceUsesWith(ch);
                        };
                        // This load can be resolved to a unique value
                        aval.ifSingleValue([&](Value* val) {
                            if (ldfun) {
                                replaceLdFun(val);
                            } else {
                                ld->replaceUsesWith(val);
                                next = bb->remove(ip);
                            }
                        });
                        // This load can have multiple values. We need a phi
                        // to distinguish them. (LdFun case is not yet
                        // handled here)
                        if (!aval.isSingleValue() && !aval.isUnknown() &&
                            !ldfun) {
                            auto hasAllInputs = [&](BB* load) -> bool {
                                return aval.checkEachSource([&](ValOrig& src) {
                                    return cfg.transitivePredecessors[load->id]
                                        .count(src.origin->bb());
                                });
                            };
                            BB* phiBlock = bb;
                            // Shift phi up until we see at least two inputs
                            // comming from different paths.
                            for (bool up = true; up;) {
                                auto preds = cfg.predecessors[phiBlock->id];
                                for (auto pre : preds)
                                    up = up & hasAllInputs(pre);
                                if (up)
                                    phiBlock = *preds.begin();
                            }
                            auto phi = new Phi;
                            aval.eachSource([&](ValOrig& src) {
                                phi->addInput(src.origin->bb(), src.val);
                            });
                            phi->updateType();
                            ld->replaceUsesWith(phi);
                            if (phiBlock == bb)
                                bb->replace(ip, phi);
                            else
                                phiBlock->insert(phiBlock->begin(), phi);
                        }
                    }
                }
                ip = next;
            }
        });
    }
};
}

namespace rir {
namespace pir {

void ScopeResolution::apply(Closure* function) {
    TheScopeResolution s(function);
    s();
}
}
}
