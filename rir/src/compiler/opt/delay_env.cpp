#include "delay_env.h"
#include "../pir/pir_impl.h"
#include "../transform/replace.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

#include <set>

namespace rir {
namespace pir {

void DelayEnv::apply(Function* function) {
    std::vector<MkEnv*> envs;

    Visitor::run(function->entry, [&](BB* bb) {
        std::set<MkEnv*> done;
        MkEnv* e;

        while (true) {
            e = nullptr;

            auto it = bb->end();
            while (it != bb->begin()) {
                it--;
                auto t = MkEnv::Cast(*it);
                if (t && done.find(t) == done.end()) {
                    e = t;
                    break;
                }
            }

            if (!e)
                break;
            done.insert(e);

            while (it != bb->end() && (it + 1) != bb->end()) {
                assert(*it == e);

                auto next = *(it + 1);

                if (Branch::Cast(next) || Return::Cast(next) ||
                    Deopt::Cast(next))
                    break;

                auto consumeStVar = [&](StVar* st) {
                    bool exists = false;
                    e->eachLocalVar([&](SEXP name, InstrArg& arg) {
                        if (name == st->varName) {
                            exists = true;
                            arg.val() = st->val();
                        }
                    });
                    if (!exists) {
                        e->pushArg(st->val(), PirType::any());
                        e->varName.push_back(st->varName);
                    }
                };

                {
                    auto st = StVar::Cast(next);
                    if (st && st->env() == e) {
                        consumeStVar(st);
                        it = bb->remove(it + 1);
                        it--;
                        continue;
                    }
                }

                if (next->hasEnv() && next->env() == e)
                    break;

                bb->swapWithNext(it);
                it++;
            }

            if (it != bb->end() && (it + 1) != bb->end()) {
                auto b = Branch::Cast(*(it + 1));
                if (e && b) {
                    auto d = Deopt::Cast(bb->next0->last());
                    if (d) {
                        auto newE = e->clone();
                        it = bb->insert(it, newE);
                        e->replaceUsesIn(newE, bb->next0);
                        // Closure wrapper in MkEnv can be circular
                        Replace::usesOfValue(newE, e, newE);
                        it = bb->moveToBegin(it, bb->next0);
                        it = bb->moveToBegin(it, bb->next1);
                    }
                }
            }
        }
    });
}
}
}
