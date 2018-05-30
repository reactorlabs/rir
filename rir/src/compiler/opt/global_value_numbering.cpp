#include "global_value_numbering.h"
#include "../analysis/global_redundancies.h"
#include <map>

namespace rir {
namespace pir {

void GlobalValueNumbering::apply(Closure* cls) {
    GlobalRedundanciesAnalysis analysis(cls);
    analysis();
    std::map<Instruction*, Value*> redundancies;
    analysis.foreach<PositioningStyle::AfterInstruction>(
        [&](const GRPartition& partition, Instruction* i) {
            Value* variable = partition.variableDefiningEquivalentExpression(i);
            if (variable != nullptr){
                redundancies.insert({i, variable});
            } else if (partition.at(i).phis != nullptr){
                // Todo: Support redundancies based on phi functions
            }
        }
    );

    Visitor::run(cls->entry, [&redundancies](BB* bb) {
        auto instruction = bb->begin();
        while (instruction != bb->end()) {
            auto redundancy = redundancies.find(*instruction);
            auto next = instruction + 1;
            if (redundancy != redundancies.end()){
                (*instruction)->replaceUsesWith(redundancy->second);
                next = bb->remove(instruction);
            }
            instruction = next;
        }
    });
}
}
}
