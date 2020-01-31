#include "../analysis/unnecessary_contexts.h"
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

void OptimizeContexts::apply(RirCompiler&, ClosureVersion* function,
                             LogStream& log) const {
    UnnecessaryContexts unnecessary(function, log);

    std::unordered_set<Instruction*> toRemove;
    Visitor::run(function->entry, [&](BB* bb) {
        for (auto i : *bb) {
            if (auto pop = PopContext::Cast(i)) {
                if (unnecessary.canRemove(pop)) {
                    toRemove.insert(pop->push());
                    toRemove.insert(pop);
                    auto affected = unnecessary.affectedEnvs(pop);
                    for (auto& mk : affected) {
                        mk->context--;
                    }
                }
            }
        }
    });

    if (toRemove.empty())
        return;

    assert(toRemove.size() % 2 == 0);

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto instr = *ip;

            auto context = toRemove.find(instr);
            if (context != toRemove.end()) {
                if (auto popc = PopContext::Cast(*context))
                    popc->replaceUsesWith(popc->result());
                next = bb->remove(ip);
                toRemove.erase(context);
            }

            ip = next;
        }
    });

    assert(toRemove.size() == 0);
}

} // namespace pir
} // namespace rir
