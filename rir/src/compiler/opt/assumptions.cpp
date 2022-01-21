#include "../analysis/available_checkpoints.h"
#include "../analysis/dead.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"

#include "R/r.h"
#include "pass_definitions.h"

#include <algorithm>
#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

struct AAssumption {
    AAssumption(Value* i, const PirType& t) : yesNo(true), kind(Typecheck) {
        c.typecheck = {i, t};
    }
    explicit AAssumption(Assume* a) : yesNo(a->assumeTrue) {
        auto cond = a->condition();
        switch (a->reason.reason) {
        case DeoptReason::Unknown:
            break;
        case DeoptReason::Typecheck: {
            if (auto t = IsType::Cast(cond)) {
                c.typecheck = {t->arg(0).val(), t->typeTest};
                kind = Typecheck;
                return;
            }
            break;
        }
        case DeoptReason::ForceAndCall:
        case DeoptReason::CallTarget: {
            if (auto t = Identical::Cast(cond)) {
                c.equality = {t->arg(0).val(), t->arg(1).val()};
                kind = Equality;
                return;
            }
            break;
        }
        case DeoptReason::EnvStubMaterialized: {
            if (auto t = IsEnvStub::Cast(cond)) {
                c.env = t->env();
                kind = IsEnvStub;
                return;
            }
            break;
        }
        case DeoptReason::DeadBranchReached:
            break;
        case DeoptReason::DeadCall:
            assert(false);
            break;
        }

        // Fallthrough generic case. Assumptions are identified by the concrete
        // condition alone.
        kind = Other;
        c.misc = cond;
    }

    AAssumption& operator=(const AAssumption& o) {
        yesNo = o.yesNo;
        kind = o.kind;
        switch (kind) {
        case IsEnvStub:
            c.env = o.c.env;
            break;
        case Typecheck:
            c.typecheck = o.c.typecheck;
            break;
        case Equality:
            c.equality = o.c.equality;
            break;
        case Other:
            c.misc = o.c.misc;
            break;
        }
        return *this;
    }

    AAssumption(const AAssumption& o) { (*this) = o; }

    bool yesNo;

    enum Kind {
        IsEnvStub,
        Typecheck,
        Equality,
        Other,
    };
    Kind kind;

    union Content {
        Content() {}
        std::pair<Value*, PirType> typecheck;
        std::pair<Value*, Value*> equality;
        Value* env;
        Value* misc;
    };
    Content c;

    bool operator==(const AAssumption& other) const {
        if (yesNo != other.yesNo)
            return false;
        if (kind != other.kind)
            return false;
        switch (kind) {
        case IsEnvStub:
            return other.c.env == c.env;
        case Typecheck:
            return other.c.typecheck == c.typecheck;
        case Equality:
            return other.c.equality == c.equality ||
                   (other.c.equality.first == c.equality.second &&
                    other.c.equality.second == c.equality.first);
        case Other:
            return c.misc == other.c.misc;
        }
        assert(false);
        return false;
    }
    void print(std::ostream& out, bool) {
        if (!yesNo)
            out << "!";
        switch (kind) {
        case Typecheck:
            c.typecheck.first->printRef(out);
            out << ".isA(";
            c.typecheck.second.print(out);
            out << ")";
            break;
        case Equality:
            c.equality.first->printRef(out);
            out << "==";
            c.equality.second->printRef(out);
            break;
        case IsEnvStub:
            out << "isEnvStub(";
            c.misc->printRef(out);
            out << ")";
            break;
        case Other:
            c.misc->printRef(out);
            break;
        }
    }
};

struct AvailableAssumptions
    : public StaticAnalysis<IntersectionSet<AAssumption>> {
    AvailableAssumptions(ClosureVersion* cls, Code* code, AbstractLog& log)
        : StaticAnalysis("AvailableAssumptions", cls, code, log) {}
    AbstractResult apply(IntersectionSet<AAssumption>& state,
                         Instruction* i) const override {
        AbstractResult res;
        if (auto a = Assume::Cast(i)) {
            AAssumption am(a);
            auto contains = state.available.find(am);
            if (contains == state.available.end()) {
                state.available.insert(am);
                res.update();
            }
        } else if (i->effects.contains(Effect::ExecuteCode) ||
                   /* Inlined code can jump to its PopContext and thus skip some
                      IsEnvStub assumptions. We clear these here so that they
                      don't get merged across a PopContext. */
                   PopContext::Cast(i)) {
            auto it = state.available.begin();
            while (it != state.available.end()) {
                auto next = it + 1;
                if (it->kind == AAssumption::Kind::IsEnvStub) {
                    next = state.available.erase(it);
                }
                it = next;
            }
        }
        return res;
    }
    const SmallSet<AAssumption> at(Instruction* i) const {
        auto res = before(i);
        return res.available;
    }
};

bool OptimizeAssumptions::apply(Compiler&, ClosureVersion* vers, Code* code,
                                AbstractLog& log, size_t) const {
    {
        Visitor::run(code->entry, [&](BB* bb) {
            if (bb->isBranch()) {
                if (auto cp = Checkpoint::Cast(bb->last())) {
                    if (cp->nextBB()->isMerge()) {
                        // Ensure that bb at the beginning of a loop has a bb
                        // to hoist assumes out of the loop.
                        auto preheader = new BB(vers, vers->nextBBId++);
                        bb->replaceSuccessor(cp->nextBB(), preheader);
                        preheader->setNext(cp->nextBB());
                    }
                }
            }
        });
    }

    AvailableCheckpoints checkpoint(vers, code, log);
    AvailableAssumptions assumptions(vers, code, log);
    DominanceGraph dom(code);
    std::unordered_map<Checkpoint*, Checkpoint*> replaced;

    std::unordered_map<Instruction*,
                       std::tuple<Instruction*, Checkpoint*, Assume*>>
        hoistAssume;
    std::unordered_map<Force*, std::tuple<IsType*, Checkpoint*, Assume*>>
        hoistCheck;

    bool anyChange = false;
    Visitor::runPostChange(code->entry, [&checkpoint, &assumptions, &dom,
                                         &replaced, &hoistAssume, &hoistCheck,
                                         &anyChange](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto instr = *ip;

            // This check tries to find type tests, whose result is not
            // consumed by all uses of the value. E.g. something like:
            //
            //     q = isType(a, t)
            //     b = castType(a, t)
            //     assume(q)
            //     someUse(b)
            //     someUse(a)
            //
            // here, the someUse(a) does not use the available assumption,
            // that a is of type t.
            if (auto tt = CastType::Cast(instr)) {
                if (tt->kind == CastType::Downcast) {
                    auto arg = tt->arg(0).val();
                    if (auto in = Instruction::Cast(arg)) {
                        if (assumptions.at(tt).count(
                                AAssumption(arg, tt->type))) {
                            // ensure that it does not look like we are
                            // replacing an already non-lazy value with a lazy
                            // one, since the cast was not yet updated.
                            tt->updateTypeAndEffects();
                            if (!in->type.isA(tt->type)) {
                                in->replaceDominatedUses(tt, dom);
                            }
                        }
                    }
                }
            }

            auto assumptionsIncludes = [&](AAssumption a) {
                auto as = assumptions.at(instr);
                for (const auto& e : as) {
                    if (e == a)
                        return true;
                    if (e.kind == AAssumption::Typecheck &&
                        a.kind == AAssumption::Typecheck)
                        if (a.c.typecheck.first == e.c.typecheck.first &&
                            e.c.typecheck.second.isA(a.c.typecheck.second))
                            return true;
                }
                return false;
            };

            if (auto assume = Assume::Cast(instr)) {
                if (assumptionsIncludes(AAssumption(assume))) {
                    anyChange = true;
                    next = bb->remove(ip);
                } else {
                    // We are trying to group multiple assumes into the same
                    // checkpoint by finding for each assume the topmost
                    // compatible checkpoint.
                    // TODO: we could also try to move up the assume itself,
                    // since if we move both at the same time, we could even
                    // jump over effectful instructions.
                    if (auto cp0 = checkpoint.at(instr)) {
                        while (replaced.count(cp0))
                            cp0 = replaced.at(cp0);
                        if (assume->checkpoint() != cp0)
                            assume->checkpoint(cp0);
                    }
                    auto guard = Instruction::Cast(assume->condition());
                    auto cp = assume->checkpoint();
                    if (guard && guard->bb() != cp->bb()) {
                        if (auto cp0 = checkpoint.at(assume)) {
                            while (replaced.count(cp0))
                                cp0 = replaced.at(cp0);
                            if (assume->checkpoint() != cp0) {
                                anyChange = true;
                                hoistAssume[guard] = {guard, cp0, assume};
                                next = bb->remove(ip);
                            }
                        }
                    } else if (auto tt = IsType::Cast(assume->condition())) {
                        if (auto f = Force::Cast(tt->arg(0).val())) {
                            if (f->hasEnv() && !tt->typeTest.maybeLazy() &&
                                tt->typeTest.isA(f->typeFeedback().type) &&
                                (f->observed == Force::ArgumentKind::value ||
                                 f->observed ==
                                     Force::ArgumentKind::evaluatedPromise)) {
                                if (auto cp = checkpoint.at(f)) {
                                    while (replaced.count(cp))
                                        cp = replaced.at(cp);
                                    hoistCheck[f] = {tt, cp, assume};
                                    anyChange = true;
                                }
                            }
                        }
                    }
                }
            }

            // A Checkpoint where the normal continue branch ends in a deopt is
            // unnecessary. We remove it by unconditionally going into the deopt
            // branch.
            if (auto d = Deopt::Cast(*ip)) {
                if (auto c = checkpoint.at(d))
                    if (!c->deleted)
                        if (c->bb()->trueBranch() == d->bb()) {
                            auto deoptBranch = c->bb()->deoptBranch();

                            assert(ip + 1 == bb->cend());
                            auto actualDeopt = Deopt::Cast(deoptBranch->last());
                            actualDeopt->setDeoptReason(d->deoptReason(),
                                                        d->deoptTrigger());
                            c->bb()->overrideSuccessors({bb});
                            c->bb()->remove(c->bb()->end() - 1);

                            bb->remove(ip);
                            bb->setNext(deoptBranch);
                            next = bb->end();
                        }
            }

            ip = next;
        }

        if (bb->isEmpty())
            return;

        // Remove Unneccessary checkpoints. If we arrive at a checkpoint and
        // the previous checkpoint is still available, and there is also a
        // next checkpoint available we might as well remove this one.
        if (auto cp = Checkpoint::Cast(bb->last())) {
            if (checkpoint.next(cp, cp, dom))
                if (auto previousCP = checkpoint.at(cp)) {
                    while (replaced.count(previousCP))
                        previousCP = replaced.at(previousCP);
                    replaced[cp] = previousCP;

                    anyChange = true;
                    cp->replaceUsesWith(previousCP);
                    auto toDel = bb->deoptBranch();
                    bb->remove(bb->end() - 1);
                    bb->convertBranchToJmp(true);
                    delete toDel;
                    return;
                }
        }
    });

    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            {
                auto h = hoistAssume.find(*ip);
                if (h != hoistAssume.end()) {
                    auto g = h->second;
                    ip++;
                    auto cp = std::get<Checkpoint*>(h->second);
                    while (replaced.count(cp))
                        cp = replaced.at(cp);
                    auto assume = new Assume(std::get<Instruction*>(g), cp,
                                             std::get<Assume*>(g)->reason);
                    ip = bb->insert(ip, assume);
                    anyChange = true;
                }
            }
            if (auto f = Force::Cast(*ip)) {
                auto h = hoistCheck.find(f);
                if (h != hoistCheck.end()) {
                    // Convert:
                    //   y = Force(x)     <feedback: eager>
                    //   t = IsType(y, eager)
                    //       Assume(t)
                    // To:
                    //   t = IsType(x, eager)
                    //       Assume(t)
                    //   y = CastType(x, eager)
                    //       Force(y)   <-  Force becomes silent...
                    auto tt = std::get<IsType*>(h->second);
                    auto cp = std::get<Checkpoint*>(h->second);
                    while (replaced.count(cp))
                        cp = replaced.at(cp);
                    auto a = std::get<Assume*>(h->second);
                    assert(tt->arg(0).val() == f);
                    auto inp = f->arg(0).val();
                    auto expected = tt->typeTest;
                    if (f->observed != Force::ArgumentKind::value) {
                        expected = expected.orPromiseWrapped();
                        if (!tt->typeTest.maybeMissing())
                            expected = expected.notWrappedMissing();
                    }
                    assert(!expected.maybeLazy());
                    auto newTT = new IsType(expected, tt->arg<0>().val());
                    newTT->arg(0).val() = inp;
                    ip = bb->insert(ip, newTT) + 1;
                    ip = bb->insert(ip, new Assume(newTT, cp, a->reason)) + 1;
                    auto casted = new CastType(inp, CastType::Downcast,
                                               PirType::any(), newTT->typeTest);
                    casted->effects.set(Effect::DependsOnAssume);
                    f->arg(0).val() = casted;
                    f->elideEnv();
                    f->clearFrameState();
                    f->effects.reset();
                    f->updateTypeAndEffects();
                    assert(!f->type.isVoid());
                    ip = bb->insert(ip, casted) + 1;
                }
            }
            ip++;
        }
    });
    return anyChange;
}

} // namespace pir
} // namespace rir
