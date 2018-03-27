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
    Function* function;
    TheScopeResolution(Function* function) : function(function) {}
    void operator()() {
        ScopeAnalysis analysis(function);

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                LdArg* lda = LdArg::Cast(i);
                LdFun* ldf = LdFun::Cast(i);
                Instruction* ld = LdVar::Cast(i);
                StVar* s = StVar::Cast(i);
                StVarSuper* ss = StVarSuper::Cast(i);
                LdVarSuper* sld = LdVarSuper::Cast(i);
                if (lda)
                    ld = lda;
                else if (ldf)
                    ld = ldf;

                if (sld) {
                    // LdVarSuper where the parent environment is known and
                    // local, can be replaced by a simple LdVar
                    auto e = Env::parentEnv(sld->env());
                    if (e != Env::theContext()) {
                        auto r = new LdVar(sld->varName, e);
                        bb->replace(ip, r);
                    }
                } else if (ss) {
                    // StVarSuper where the parent environment is known and
                    // local, can be replaced by simple StVar
                    auto e = Env::parentEnv(ss->env());
                    if (e != Env::theContext()) {
                        auto r = new StVar(ss->varName, ss->arg<0>().val(), e);
                        bb->replace(ip, r);
                    }
                } else if (s) {
                    // Dead store to non-escaping environment can be removed
                    if (!analysis.finalState[s->env()].leaked &&
                        analysis.observedStores.find(s) ==
                            analysis.observedStores.end())
                        next = bb->remove(ip);
                } else if (ld && analysis.loads.count(ld)) {
                    // If we have a non-ambiguous load, we can replace the load
                    // with the actual values.
                    auto aload = analysis.loads.at(ld);
                    auto aval = aload.result;
                    bool localVals = true;
                    aval.eachSource([&](ValOrig& src) {
                        // inter-procedural scope analysis can drag in values
                        // from other functions, which we cannot use here!
                        if (src.origin->bb()->owner != function) {
                            localVals = false;
                        }
                    });
                    if (localVals) {
                        aval.ifSingleValue([&](Value* val) {
                            if (ldf) {
                                auto f = new Force(val);
                                // TODO
                                // !(PirType(RType::closure) >= phi->type)
                                ld->replaceUsesWith(f);
                                bb->replace(ip, f);
                            } else {
                                ld->replaceUsesWith(val);
                                next = bb->remove(ip);
                            }
                        });
                        if (!aval.isSingleValue() && !aval.isUnknown()) {
                            auto phi = new Phi;
                            aval.eachSource([&](ValOrig& src) {
                                phi->addInput(src.origin->bb(), src.val);
                            });
                            phi->updateType();
                            if (ldf) {
                                auto f = new Force(phi);
                                // TODO
                                // !(PirType(RType::closure) >= phi->type)
                                ld->replaceUsesWith(f);
                                bb->replace(ip, phi);
                                next = bb->insert(ip + 1, f);
                                next++;
                            } else {
                                ld->replaceUsesWith(phi);
                                bb->replace(ip, phi);
                            }
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

void ScopeResolution::apply(Function* function) {
    TheScopeResolution s(function);
    s();
}
}
}
