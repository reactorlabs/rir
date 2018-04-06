#include "delay_instr.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

void DelayInstr::applyTranslation(Closure* function) {
    std::vector<MkEnv*> envs;

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

            if (i->accessesEnv() == false && i->mightIO() == false) {
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
                }
            }

            ip = next;
        }
    });
}
}
}
