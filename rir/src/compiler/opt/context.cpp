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

            auto e = toRemove.find(instr);
            if (e != toRemove.end()) {
                next = bb->remove(ip);
                toRemove.erase(e);
            }

            ip = next;
        }
    });
}

} // namespace pir
} // namespace rir
