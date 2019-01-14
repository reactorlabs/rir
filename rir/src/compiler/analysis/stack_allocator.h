#pragma once

#include "../analysis/generic_static_analysis.h"
#include "../pir/pir.h"
#include "stack_values.h"

namespace rir {
namespace pir {

/*
 *
 */
class StackAllocator {
  public:
    using Compensation = std::vector<std::pair<rir::Opcode, uint32_t>>;
    using ApplyCompensation = std::unordered_map<Instruction*, Compensation>;
    using MergeCompensation =
        std::unordered_map<BB*, std::unordered_map<BB*, Compensation>>;

    ApplyCompensation applyCompensation;
    MergeCompensation mergeCompensation;

    StackAllocator(Closure* fun, Code* code, LogStream& log);

    void print() const {
        std::cout << "stack allocator\napply:\n";
        for (auto x : applyCompensation) {
            x.first->printRef(std::cout);
            std::cout << ": ";
            for (auto y : x.second) {
                switch (y.first) {
                case rir::Opcode::pop_:
                    std::cout << "pop() ";
                    break;
                case rir::Opcode::swap_:
                    std::cout << "swap() ";
                    break;
                case rir::Opcode::dup_:
                    std::cout << "dup() ";
                    break;
                case rir::Opcode::put_:
                    std::cout << "put(" << y.second << ") ";
                    break;
                case rir::Opcode::pull_:
                    std::cout << "pull(" << y.second << ") ";
                    break;
                case rir::Opcode::pick_:
                    std::cout << "pick(" << y.second << ") ";
                    break;
                default:
                    assert(false);
                }
            }
            std::cout << "\n";
        }
        std::cout << "merge:\n";
        for (auto x : mergeCompensation) {
            std::cout << "BB" << x.first->id << "\n";
            for (auto y : x.second) {
                std::cout << " -> BB" << y.first->id << ": ";
                for (auto z : y.second) {
                    switch (z.first) {
                    case rir::Opcode::pop_:
                        std::cout << "pop() ";
                        break;
                    case rir::Opcode::swap_:
                        std::cout << "swap() ";
                        break;
                    case rir::Opcode::dup_:
                        std::cout << "dup() ";
                        break;
                    case rir::Opcode::put_:
                        std::cout << "put(" << z.second << ") ";
                        break;
                    case rir::Opcode::pull_:
                        std::cout << "pull(" << z.second << ") ";
                        break;
                    case rir::Opcode::pick_:
                        std::cout << "pick(" << z.second << ") ";
                        break;
                    default:
                        assert(false);
                    }
                }
                std::cout << "\n";
            }
        }
    }
};

} // namespace pir
} // namespace rir
