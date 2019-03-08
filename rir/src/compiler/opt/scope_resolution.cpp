#include "../analysis/query.h"
#include "../analysis/scope.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/phi_placement.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;
class TheScopeResolution {
  public:
    ClosureVersion* function;
    CFG cfg;
    DominanceGraph dom;
    DominanceFrontier dfront;
    ;
    LogStream& log;
    explicit TheScopeResolution(ClosureVersion* function, LogStream& log)
        : function(function), cfg(function), dom(function),
          dfront(function, cfg, dom), log(log) {}

    void operator()() {
        ScopeAnalysis analysis(function, log);
        analysis();
        auto& finalState = analysis.result();
        if (finalState.noReflection())
            function->properties.set(ClosureVersion::Property::NoReflection);

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;

                auto before = analysis.at<ScopeAnalysis::BeforeInstruction>(i);
                auto after = analysis.at<ScopeAnalysis::AfterInstruction>(i);

                // Force and callees can only see our env only through
                // reflection
                if (i->hasEnv() &&
                    (CallInstruction::CastCall(i) || Force::Cast(i))) {
                    if (after.noReflection()) {
                        i->elideEnv();
                        i->effects.reset(Effect::Reflection);
                    }
                    if (after.envNotEscaped(i->env())) {
                        i->effects.reset(Effect::LeaksEnv);
                    }
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

                // Constant fold "missing" if we can
                if (auto missing = Missing::Cast(i)) {
                    auto res =
                        analysis.load(before, missing->varName, missing->env());
                    if (!res.result.type.maybeMissing()) {
                        // Missing still returns TRUE, if the argument was
                        // initially missing, but then overwritten by a default
                        // argument. Currently our analysis cannot really
                        // distinguish those cases. Therefore, if the current
                        // value of the variable is guaranteed to not be a
                        // missing value, we additionally need verify that the
                        // initial argument (the argument to the mkenv) was also
                        // guaranteed to not be a missing value.
                        // TODO: this is a bit brittle and might break as soon
                        // as we start improving the handling of missing args in
                        // MkEnv.
                        if (auto env = MkEnv::Cast(missing->env())) {
                            bool initiallyMissing = false;
                            env->eachLocalVar([&](SEXP name, Value* val) {
                                if (name == missing->varName)
                                    initiallyMissing = val->type.maybeMissing();
                            });
                            if (!initiallyMissing) {
                                missing->replaceUsesAndSwapWith(
                                    new LdConst(R_FalseValue), ip);
                            }
                        }
                    } else {
                        res.result.ifSingleValue([&](Value* v) {
                            if (v == MissingArg::instance()) {
                                missing->replaceUsesAndSwapWith(
                                    new LdConst(R_TrueValue), ip);
                            }
                        });
                    }
                }

                analysis.lookup(after, i, [&](const AbstractLoad& aLoad) {
                    auto& res = aLoad.result;

                    // In case the scope analysis is sure that this is
                    // actually the same as some other PIR value. So let's just
                    // replace it.
                    if (res.isSingleValue()) {
                        auto value = res.singleValue();
                        if (value.val->type.isA(i->type) &&
                            value.recursionLevel == 0) {
                            i->replaceUsesWith(value.val);
                            next = bb->remove(ip);
                            return;
                        }
                    }

                    // Narrow down type according to what the analysis reports
                    if (i->type.isRType()) {
                        auto inferedType = res.type;
                        if (!i->type.isA(inferedType))
                            i->type = inferedType;
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
                        res.eachSource([&](const ValOrig& src) {
                            if (src.recursionLevel > 0)
                                onlyLocalVals = false;
                        });

                        if (onlyLocalVals && !res.isUnknown()) {
                            std::unordered_map<BB*, Value*> inputs;
                            bool fail = false;
                            res.eachSource([&](ValOrig v) {
                                auto i = Instruction::Cast(v.val);
                                if (fail || !i || inputs.count(i->bb())) {
                                    fail = true;
                                }
                                inputs[i->bb()] = i;
                            });
                            if (!fail) {
                                auto placement = PhiPlacement::compute(
                                    function, inputs, cfg, dom, dfront);
                                auto dominators = dom.dominators(bb);
                                int maxDom = 0;
                                BB* resPhiPos = nullptr;
                                for (auto& phi : placement) {
                                    if (phi.first == bb) {
                                        resPhiPos = bb;
                                    } else {
                                        auto pos = std::find(dominators.begin(),
                                                             dominators.end(),
                                                             phi.first);
                                        if (pos != dominators.end()) {
                                            if (pos - dominators.begin() >
                                                maxDom)
                                                maxDom =
                                                    pos - dominators.begin();
                                        }
                                    }
                                }
                                if (!resPhiPos)
                                    resPhiPos = dominators[maxDom];

                                if (placement.size() > 0) {
                                    std::unordered_map<BB*, Phi*> created;
                                    for (auto& phi : placement) {
                                        created[phi.first] = new Phi;
                                    }

                                    bool replaced = false;
                                    for (auto& computed : placement) {
                                        auto& pos = computed.first;
                                        auto& phi = created.at(pos);

                                        assert(computed.second.size() > 1);
                                        for (auto& p : computed.second) {
                                            if (p.aValue) {
                                                phi->addInput(p.inputBlock,
                                                              p.aValue);
                                            } else {
                                                phi->addInput(
                                                    p.inputBlock,
                                                    created.at(p.otherPhi));
                                            }
                                        };

                                        if (pos == bb) {
                                            replaced = true;
                                            bb->replace(ip, phi);
                                        } else {
                                            pos->insert(pos->begin(), phi);
                                        }
                                    }

                                    for (auto& phi : created)
                                        phi.second->updateType();
                                    for (auto& phi : created) {
                                        if (!phi.second->type.isA(res.type))
                                            phi.second->type = res.type;
                                    }
                                    for (auto& phi : created)
                                        phi.second->updateType();

                                    if (!replaced)
                                        next = bb->remove(ip);

                                    auto resPhi = created.at(resPhiPos);
                                    i->replaceUsesWith(resPhi);
                                    return;
                                }
                            }
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
                                return;
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
                                return;
                            }
                        }
                    }

                    // If nothing else, narrow down the environment (in case we
                    // found something more concrete).
                    if (i->hasEnv() &&
                        aLoad.env != AbstractREnvironment::UnknownParent)
                        i->env(aLoad.env);
                });

                if (auto b = CallBuiltin::Cast(i)) {
                    bool noObjects = true;
                    i->eachArg([&](Value* v) {
                        if (v != i->env())
                            if (v->cFollowCastsAndForce()->type.maybeObj())
                                noObjects = false;
                    });

                    if (noObjects &&
                        SafeBuiltinsList::nonObject(b->builtinId)) {
                        std::vector<Value*> args;
                        i->eachArg([&](Value* v) {
                            if (v != i->env()) {
                                auto mk = MkArg::Cast(v);
                                if (mk && mk->isEager())
                                    args.push_back(mk->eagerArg());
                                else
                                    args.push_back(v);
                            }
                        });
                        auto safe =
                            new CallSafeBuiltin(b->blt, args, b->srcIdx);
                        b->replaceUsesWith(safe);
                        bb->replace(ip, safe);
                    }
                }

                ip = next;
            }
        });
    }
};
} // namespace

namespace rir {
namespace pir {

void ScopeResolution::apply(RirCompiler&, ClosureVersion* function,
                            LogStream& log) const {
    TheScopeResolution s(function, log);
    s();
}

} // namespace pir
} // namespace rir
