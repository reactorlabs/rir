#pragma once

#include "../analysis/generic_static_analysis.h"
#include "../pir/pir.h"

#include <unordered_map>
#include <vector>

namespace rir {
namespace pir {

/*
 * For every instruction, get a snapshot of what's on the stack after it is
 * executed. Also get a set of values that need to be removed from the stack
 * after each instruction.
 */
class StackAnalysis {
  public:
    struct AbstractStack {
        std::vector<Value*> data;
        size_t size() const;
        using StackSlotsIterator = std::function<void(Value*)>;
        void eachSlot(StackSlotsIterator it) const;
        void push(Value* v);
        void erase(Value* v);
        void erasePhiInput(Phi* phi);
        void matchContents(AbstractStack const& other) const;
    };
    using StackSnapshots = std::unordered_map<Instruction*, AbstractStack>;
    using InstructionCompensation =
        std::unordered_map<Instruction*, std::vector<Value*>>;

    CFG const& cfg;
    StackSnapshots stacks;
    InstructionCompensation toDrop;
    std::unordered_set<Instruction*> dead;

    StackAnalysis(CFG const& cfg) : cfg(cfg) {}
    void operator()(ClosureVersion* fun, Code* code, LogStream& log);

    void print() const {
        std::cout << "Stacks\n";
        for (auto x : stacks) {
            std::cout << "  ";
            x.first->printRef(std::cout);
            std::cout << ": ";
            for (auto y : x.second.data) {
                y->printRef(std::cout);
                std::cout << " ";
            }
            std::cout << "\n";
        }
        std::cout << "Compensation\n";
        for (auto x : toDrop) {
            std::cout << "  ";
            x.first->printRef(std::cout);
            std::cout << ": ";
            for (auto y : x.second) {
                y->printRef(std::cout);
                std::cout << " ";
            }
            std::cout << "\n";
        }
        std::cout << "Dead\n";
        for (auto x : dead) {
            std::cout << "  ";
            x->printRef(std::cout);
            std::cout << "\n";
        }
    }
};

} // namespace pir
} // namespace rir
