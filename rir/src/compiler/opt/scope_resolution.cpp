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
                    auto e = Env::parentEnv(sld->env());
                    if (e != Env::theContext()) {
                        auto r = new LdVar(sld->varName, e);
                        bb->replace(ip, r);
                    }
                } else if (ss) {
                    auto e = Env::parentEnv(ss->env());
                    if (e != Env::theContext()) {
                        auto r = new StVar(ss->varName, ss->arg<0>(), e);
                        bb->replace(ip, r);
                    }
                } else if (s) {
                    if (!analysis.finalState[s->env()].leaked &&
                        analysis.observedStores.find(s) ==
                            analysis.observedStores.end())
                        next = bb->remove(ip);
                } else if (ld) {
                    auto aload = analysis.loads[ld];
                    auto v = aload.second;
                    bool localVals = true;
                    for (auto i : v.vals)
                        if (i.orig()->bb()->fun != function) {
                            localVals = false;
                            break;
                        }
                    if (localVals) {
                        if (v.singleValue()) {
                            Value* val = (*v.vals.begin()).val();
                            ld->replaceUsesWith(val);
                            if (!ld->changesEnv() || !val->type.maybeLazy())
                                next = bb->remove(ip);
                        } else if (!v.vals.empty()) {
                            auto phi = new Phi;
                            for (auto a : v.vals) {
                                phi->addInput(a.orig()->bb(), a.val());
                            }
                            phi->updateType();
                            ld->replaceUsesWith(phi);
                            ip++;
                            next = bb->insert(ip, phi);
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
