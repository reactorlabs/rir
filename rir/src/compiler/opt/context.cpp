#include "../analysis/unnecessary_contexts.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "compiler/analysis/cfg.h"

#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool OptimizeContexts::apply(Compiler&, ClosureVersion* cls, Code* code,
                             LogStream& log) const {
    bool anyChange = false;
    Visitor::run(code->entry, [&](BB* bb) {
        for (auto it = bb->begin(); it != bb->end(); ++it) {
            if (auto ret = NonLocalReturn::Cast(*it)) {
                if (auto env = MkEnv::Cast(ret->env())) {
                    if (env->context == 1) {
                        anyChange = true;
                        ret->replaceUsesAndSwapWith(
                            new Return(ret->arg<0>().val()), it);
                    }
                }
            }
        }
    });

    UnnecessaryContexts unnecessary(cls, code, log);

    std::unordered_set<Instruction*> toRemove;
    Visitor::run(code->entry, [&](BB* bb) {
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
        return anyChange;

    assert(toRemove.size() % 2 == 0);

    Visitor::run(code->entry, [&](BB* bb) {
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
    return true;
}

} // namespace pir
} // namespace rir
