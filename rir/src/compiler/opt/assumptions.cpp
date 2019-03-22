#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../translations/pir_translator.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

struct AAssumption {
    AAssumption(Assume* a) : yesNo(a->assumeTrue), assumption(a->condition()) {}
    bool yesNo;
    Value* assumption;
    bool operator==(const AAssumption& other) const {
        return yesNo == other.yesNo && assumption == other.assumption;
    }
    void print(std::ostream& out, bool) {
        if (!yesNo)
            out << "!";
        assumption->printRef(out);
    }
};

struct AvailableAssumptions
    : public StaticAnalysis<IntersectionSet<AAssumption>> {
    AvailableAssumptions(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("AvailableAssumptions", cls, cls, log) {}
    AbstractResult apply(IntersectionSet<AAssumption>& state,
                         Instruction* i) const {
        AbstractResult res;
        if (auto a = Assume::Cast(i)) {
            if (!state.available.includes(a)) {
                state.available.insert(a);
                res.update();
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

void OptimizeAssumptions::apply(RirCompiler&, ClosureVersion* function,
                                LogStream& log) const {
    CFG cfg(function);
    AvailableCheckpoints checkpoint(function, log);
    AvailableAssumptions assumptions(function, log);

    Visitor::runPostChange(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto instr = *ip;

            // Remove Unneccessary checkpoints. If we arrive at a checkpoint and
            // the previous checkpoint is still available, we might as well take
            // this one.
            if (auto cp = Checkpoint::Cast(instr)) {
                if (auto cp0 = checkpoint.at(instr)) {
                    assert(bb->last() == instr);
                    cp->replaceUsesWith(cp0);
                    bb->remove(ip);
                    delete bb->next1;
                    bb->next1 = nullptr;
                    next = bb->end();
                }
            }

            if (auto assume = Assume::Cast(instr)) {
                if (assumptions.at(instr).includes(assume)) {
                    next = bb->remove(ip);
                } else {
                    // We are trying to group multiple assumes into the same
                    // checkpoint by finding for each assume the topmost
                    // compatible
                    // checkpoint.
                    // TODO: we could also try to move up the assume itself,
                    // since
                    // if we move both at the same time, we could even jump over
                    // effectful instructions.
                    if (auto cp0 = checkpoint.at(instr)) {
                        if (assume->checkpoint() != cp0)
                            assume->checkpoint(cp0);
                    }
                }
            }
            ip = next;
        }
    });
}

} // namespace pir
} // namespace rir
