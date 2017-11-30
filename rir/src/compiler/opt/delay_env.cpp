#include "delay_env.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

void DelayEnv::apply(Function* function) {
    std::vector<MkEnv*> envs;

    Visitor::run(function->entry, [&](BB* bb) {
        MkEnv* e = nullptr;
        auto it = bb->begin();
        while (it != bb->end()) {
            e = MkEnv::Cast(*it);
            if (e)
                break;
            it++;
        }
        while (it != bb->end() && (it + 1) != bb->end()) {
            assert(*it == e);

            auto next = *(it + 1);

            if (Branch::Cast(next) || Return::Cast(next) || Deopt::Cast(next))
                break;

            auto st = StVar::Cast(next);
            if (st && st->env() == e) {
                size_t i = 0;
                for (; i < e->nLocals(); ++i) {
                    if (e->varName[i] == st->varName) {
                        e->localVars()[i] = st->val();
                        break;
                    }
                }
                if (i == e->nLocals()) {
                    e->push_arg(PirType::any(), st->val());
                    e->varName.push_back(st->varName);
                }
                it = bb->remove(it + 1);
                it--;
                continue;
            }

            if (next->hasEnv() && next->env() == e)
                break;

            bb->swap(it);
            it++;
        }

        if (it == bb->end() || (it + 1) == bb->end())
            return;

        auto b = Branch::Cast(*(it + 1));
        if (e && b) {
            auto d = Deopt::Cast(bb->next0->last());
            if (d) {
                it = bb->insert(it, e->clone());
                e->replaceUsesIn(*it, bb->next0);
                it = bb->moveToBegin(it, bb->next0);
                it = bb->moveToBegin(it, bb->next1);
            }
        }
    });
}
}
}
