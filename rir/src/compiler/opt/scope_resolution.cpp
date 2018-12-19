#include "../analysis/query.h"
#include "../analysis/scope.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;
class TheScopeResolution {
  public:
    Closure* function;
    CFG cfg;
    DominanceGraph doms;
    LogStream& log;
    explicit TheScopeResolution(Closure* function, LogStream& log)
        : function(function), cfg(function), doms(function), log(log) {}

    void operator()() {
        ScopeAnalysis analysis(function, log);
        analysis();
        auto& finalState = analysis.result();

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;

                auto before = analysis.at<ScopeAnalysis::BeforeInstruction>(i);
                auto after = analysis.at<ScopeAnalysis::AfterInstruction>(i);

                // Force can only see our env only through reflection
                if (Force::Cast(i) && after.noReflection())
                    i->elideEnv();

                // Dead store to non-escaping environment can be removed
                if (auto st = StVar::Cast(i)) {
                    if (finalState.envNotEscaped(st->env()) &&
                        finalState.deadStore(st)) {
                        next = bb->remove(ip);
                    }
                    ip = next;
                    continue;
                }

                // StVarSuper where the parent environment is known and
                // local, can be replaced by simple StVar, if the variable
                // exists in the super env.
                if (auto sts = StVarSuper::Cast(i)) {
                    auto aLoad =
                        analysis.superLoad(before, sts->varName, sts->env());
                    if (aLoad.env != AbstractREnvironment::UnknownParent &&
                        !aLoad.result.isUnknown() &&
                        aLoad.env->validIn(function)) {
                        auto r = new StVar(sts->varName, sts->val(), aLoad.env);
                        bb->replace(ip, r);
                        sts->replaceUsesWith(r);
                    }
                    ip = next;
                    continue;
                }

                analysis.lookup(after, i, [&](const AbstractLoad& aLoad) {
                    auto& res = aLoad.result;

                    // Narrow down type according to what the analysis reports
                    if (i->type.isRType()) {
                        auto inferedType = res.type;
                        if (!i->type.isA(inferedType))
                            i->type = inferedType;
                    }

                    // In case the scope analysis is sure that this is
                    // actually the same as some other PIR value. So let's just
                    // replace it.
                    if (res.isSingleValue()) {
                        auto value = res.singleValue().val;
                        if (value->validIn(function)) {
                            i->replaceUsesWith(value);
                            next = bb->remove(ip);
                        }
                        return;
                    }

                    // The generic case where we have a bunch of potential
                    // values we will insert a phi to group all of them. In
                    // general this is only possible if they all come from the
                    // current function (and not through inter procedural
                    // analysis from other functions).
                    //
                    // Also, we shold only do this for actual loads and not
                    // in general. Otherwise there is a danger that we insert
                    // the same phi twice (e.g. if a force returns the result
                    // of a load, we will resolve the load and the force) which
                    // ends up being rather painful.
                    bool isActualLoad =
                        LdVar::Cast(i) || LdFun::Cast(i) || LdVarSuper::Cast(i);
                    if (!res.isUnknown() && isActualLoad) {

                        bool onlyLocalVals = true;
                        res.eachSource([&](ValOrig& src) {
                            if (!src.val->validIn(function))
                                onlyLocalVals = false;
                        });

                        if (onlyLocalVals && !res.isUnknown()) {
                            // Find optimal phi placement:
                            // Shift phi up until we see at least two inputs
                            // coming from different paths.
                            auto hasAllInputs = [&](BB* load) -> bool {
                                return res.checkEachSource([&](ValOrig& src) {
                                    auto i = Instruction::Cast(src.val);
                                    if (!i)
                                        return true;
                                    // we cannot move the phi above its src
                                    if (i->bb() == load)
                                        return false;
                                    return cfg.isPredecessor(i->bb(), load);
                                });
                            };
                            BB* phiBlock = bb;
                            for (bool up = true; up;) {
                                auto preds =
                                    cfg.immediatePredecessors(phiBlock);
                                if (preds.empty())
                                    break;
                                for (auto pre : preds)
                                    up = up && hasAllInputs(pre);
                                if (up)
                                    phiBlock = *preds.begin();
                            }

                            // Insert a new phi
                            auto phi = new Phi;
                            res.eachSource([&](ValOrig& src) {
                                phi->addInput(src.origin->bb(), src.val);
                            });
                            phi->updateType();
                            if (!phi->type.isA(res.type))
                                i->type = res.type;
                            i->replaceUsesWith(phi);
                            if (phiBlock == bb) {
                                bb->replace(ip, phi);
                            } else {
                                phiBlock->insert(phiBlock->begin(), phi);
                                next = bb->remove(ip);
                            }

                            return;
                        }
                    }

                    // LdVarSuper where the parent environment is known and
                    // local, can be replaced by a simple LdVar
                    if (auto lds = LdVarSuper::Cast(i)) {
                        auto e = Env::parentEnv(lds->env());
                        if (e) {
                            auto r = new LdVar(lds->varName, e);
                            bb->replace(ip, r);
                            lds->replaceUsesWith(r);
                        }
                        return;
                    }

                    // Ldfun needs some special treatment sometimes:
                    // Since non closure bindings are skipped at runtime, we can
                    // only resolve ldfun if we are certain which one is the
                    // first binding that holds a closure. Often this is only
                    // possible after inlining a promise. But inlining a promise
                    // requires a force instruction. But ldfun does force
                    // implicitly. To get out of this vicious circle, we add the
                    // first binding we find with a normal load (as opposed to
                    // loadFun) from the abstract state as a "guess" This will
                    // enable other passes (especially the promise inliner pass)
                    // to work on the guess and maybe the next time we end up
                    // here, we can actually prove that the guess was right.
                    if (auto ldfun = LdFun::Cast(i)) {
                        auto guess = ldfun->guessedBinding();
                        // If we already have a guess, let's see if now know
                        // that it is a closure.
                        if (guess) {
                            // TODO: if !guess->maybe(closure) we know that the
                            // guess is wrong and could try the next binding.
                            if (!guess->type.isA(PirType::closure())) {
                                analysis.lookup(
                                    before, guess,
                                    [&](const AbstractPirValue& res) {
                                        if (res.isSingleValue())
                                            guess = res.singleValue().val;
                                    });
                            }
                            if (guess->type.isA(PirType::closure()) &&
                                guess->validIn(function)) {
                                ldfun->replaceUsesWith(guess);
                                next = bb->remove(ip);
                            }
                        } else {
                            auto res =
                                analysis
                                    .load(before, ldfun->varName, ldfun->env())
                                    .result;
                            if (res.isSingleValue()) {
                                auto firstBinding = res.singleValue().val;
                                if (firstBinding->validIn(function)) {
                                    ip =
                                        bb->insert(ip, new Force(firstBinding,
                                                                 ldfun->env()));
                                    ldfun->guessedBinding(*ip);
                                    next = ip + 2;
                                }
                            }
                        }
                        return;
                    }

                    // If nothing else, narrow down the environment (in case we
                    // found something more concrete).
                    if (i->hasEnv() &&
                        aLoad.env != AbstractREnvironment::UnknownParent)
                        i->env(aLoad.env);
                });

                ip = next;
            }
        });
    }
};
} // namespace

namespace rir {
namespace pir {

void ScopeResolution::apply(RirCompiler&, Closure* function,
                            LogStream& log) const {
    TheScopeResolution s(function, log);
    s();
}

} // namespace pir
} // namespace rir
