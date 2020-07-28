#include "../analysis/available_checkpoints.h"
#include "../analysis/dead.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

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
        if (auto t = IsType::Cast(cond)) {
            kind = Typecheck;
            c.typecheck = {t->arg(0).val(), t->typeTest};
        } else if (auto t = Identical::Cast(cond)) {
            kind = Equality;
            c.equality = {t->arg(0).val(), t->arg(1).val()};
        } else if (auto t = IsEnvStub::Cast(cond)) {
            kind = IsEnvStub;
            c.env = t->env();
        } else {
            kind = Other;
            c.misc = cond;
        }
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
    AvailableAssumptions(ClosureVersion* cls, Code* code, LogStream& log)
        : StaticAnalysis("AvailableAssumptions", cls, code, log) {}
    AbstractResult apply(IntersectionSet<AAssumption>& state,
                         Instruction* i) const {
        AbstractResult res;
        if (auto a = Assume::Cast(i)) {
            if (!IsEnvStub::Cast(a->arg(0).val())) {
                AAssumption am(a);
                auto contains = state.available.find(am);
                if (contains == state.available.end()) {
                    state.available.insert(am);
                    res.update();
                }
            }
        }
        return res;
    }
    const SmallSet<AAssumption> at(Instruction* i) const {
        auto res = StaticAnalysis::at<
            StaticAnalysis::PositioningStyle::BeforeInstruction>(i);
        return res.available;
    }
};

bool OptimizeAssumptions::apply(RirCompiler&, ClosureVersion* vers, Code* code,
                                LogStream& log) const {
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

    bool anyChange = false;
    Visitor::runPostChange(code->entry, [&](BB* bb) {
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

            if (auto assume = Assume::Cast(instr)) {
                if (assumptions.at(instr).includes(AAssumption(assume))) {
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
                        if (auto cp0 = checkpoint.at(guard)) {
                            while (replaced.count(cp0))
                                cp0 = replaced.at(cp0);
                            if (assume->checkpoint() != cp0) {
                                anyChange = true;
                                hoistAssume[guard] = {guard, cp0, assume};
                                next = bb->remove(ip);
                            }
                        }
                    }
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
            auto h = hoistAssume.find(*ip);
            if (h != hoistAssume.end()) {
                auto g = h->second;
                ip++;
                auto assume = new Assume(std::get<0>(g), std::get<1>(g));
                assume->feedbackOrigin.insert(
                    assume->feedbackOrigin.end(),
                    std::get<2>(g)->feedbackOrigin.begin(),
                    std::get<2>(g)->feedbackOrigin.end());
                ip = bb->insert(ip, assume);
                anyChange = true;
            }
            ip++;
        }
    });
    return anyChange;
}

} // namespace pir
} // namespace rir
