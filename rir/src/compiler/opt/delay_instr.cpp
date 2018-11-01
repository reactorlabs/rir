#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

void DelayInstr::apply(Closure* function) const {
    std::vector<MkEnv*> envs;

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

            if (!i->hasEnv() && !i->hasEffect() && !Phi::Cast(i) &&
                !i->branchOrExit()) {
                Instruction* usage = i->hasSingleUse();
                if (usage && usage->bb() != bb) {
                    auto phi = Phi::Cast(usage);
                    if (phi) {
                        // If the usage is a phi, we make sure to keep this
                        // value only in the input branch, where it is
                        // actually needed.
                        for (size_t j = 0; j < phi->nargs(); ++j) {
                            if (phi->arg(j).val() == i) {
                                if (phi->input[j] != bb) {
                                    next = bb->moveToEnd(ip, phi->input[j]);
                                }
                                break;
                            }
                        }
                    } else {
                        next = bb->moveToBegin(ip, usage->bb());
                    }
                } else if (usage && usage != *next) {
                    auto swap = ip;
                    while (swap + 1 != bb->end() && *(swap + 1) != usage) {
                        bb->swapWithNext(swap);
                        ++swap;
                    }
                }
            }

            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
