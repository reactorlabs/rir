#pragma once

#include "../../analysis/generic_static_analysis.h"
#include "../../pir/pir.h"
#include "liveness.h"

#include <unordered_map>
#include <vector>

namespace rir {
namespace pir {

struct StackUseAnalysisState {
    struct AbstractStack : public std::vector<Value*> {
        using StackSlotsIterator = std::function<void(Value*)>;
        void eachSlot(StackSlotsIterator action) const {
            for (auto i : *this)
                action(i);
        }
        void push(Value* v);
        void eraseValue(Value* v);
        void erasePhiInput(Phi* phi);
        void matchContents(AbstractStack const& other) const;
    };

    Instruction* i = nullptr;

    // Stack contents
    AbstractStack stack;

    // List of values to be removed from stack before executing this instruction
    std::vector<Value*> toDrop;

    // If this instruction should be popped after it's executed
    bool isDead;

    AbstractResult merge(const StackUseAnalysisState& other);
    void print(std::ostream& out, bool tty) const;
};

/*
 * For every instruction, get a snapshot of what's on the stack after it is
 * executed. Also get a set of values that need to be removed from the stack
 * after each instruction.
 */
class StackUseAnalysis
    : public StaticAnalysis<StackUseAnalysisState, AnalysisDebugLevel::None> {

  private:
    LivenessIntervals const& liveness;

  public:
    StackUseAnalysis(ClosureVersion* cls, Code* code, LogStream& log,
                     LivenessIntervals const& liveness)
        : StaticAnalysis("Stack Use", cls, code, log), liveness(liveness) {}

    AbstractResult apply(StackUseAnalysisState& state,
                         Instruction* i) const override;

    StackUseAnalysisState::AbstractStack stackAfter(Instruction* i) const {
        static StackUseAnalysisState::AbstractStack empty;
        if (!i)
            return empty;
        return at<PositioningStyle::AfterInstruction>(i).stack;
    }

    std::vector<Value*> toDrop(Instruction* i) const {
        return at<PositioningStyle::AfterInstruction>(i).toDrop;
    }

    bool dead(Instruction* i) const {
        return at<PositioningStyle::AfterInstruction>(i).isDead;
    }
};

} // namespace pir
} // namespace rir
