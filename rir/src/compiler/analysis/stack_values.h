#pragma once

#include "../analysis/generic_static_analysis.h"
#include "../pir/pir.h"

namespace rir {
namespace pir {

/*
 *
 */
class StackValuesAnalysis {
  public:

    std::unordered_map<BB*, std::unordered_set<Value*>> stackValues;

    StackValuesAnalysis(ClosureVersion* fun, Code* code, LogStream& log,
                        CFG const& cfg);

    void print(BB* bb) const {
        std::cout << "SVA for BB" << bb->id << ": [ ";
        if (stackValues.count(bb)) {
            for (auto& v : stackValues.at(bb)) {
                v->printRef(std::cout);
                std::cout << " ";
            }
        }
        std::cout << "]\n";
    }
};

} // namespace pir
} // namespace rir
