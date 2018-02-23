#include "delay_instr.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

#include <set>

namespace rir {
namespace pir {

void DelayInstr::apply(Function* function) {
    std::vector<MkEnv*> envs;

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;
            auto next = ip + 1;

            if (i->accessesEnv() == false && i->mightIO() == false) {
                Instruction* usage = i->hasSingleUse();
                if (usage && usage->bb() != bb)
                    next = bb->moveToBegin(ip, usage->bb());
            }

            ip = next;
        }
    });
}
}
}
