#include "../pir/pir_impl.h"
#include "../transform/replace.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "pass_definitions.h"

#include <unordered_set>

namespace rir {
namespace pir {

void DelayEnv::apply(RirCompiler&, Closure* function) const {
    Visitor::run(function->entry, [&](BB* bb) {
        std::unordered_set<MkEnv*> done;
        MkEnv* envInstr;

        while (true) {
            envInstr = nullptr;

            auto it = bb->end();
            while (it != bb->begin()) {
                it--;
                auto t = MkEnv::Cast(*it);
                if (t && done.find(t) == done.end()) {
                    envInstr = t;
                    break;
                }
            }

            if (!envInstr)
                break;
            done.insert(envInstr);

            while (it != bb->end() && (it + 1) != bb->end()) {
                assert(*it == envInstr);

                auto next = *(it + 1);

                if (Branch::Cast(next) || Return::Cast(next) ||
                    Deopt::Cast(next) || Checkpoint::Cast(next))
                    break;

                auto consumeStVar = [&](StVar* st) {
                    bool exists = false;
                    envInstr->eachLocalVar([&](SEXP name, InstrArg& arg) {
                        if (name == st->varName) {
                            exists = true;
                            arg.val() = st->val();
                        }
                    });
                    if (!exists) {
                        envInstr->pushArg(st->val(), PirType::any());
                        envInstr->varName.push_back(st->varName);
                    }
                };

                {
                    auto st = StVar::Cast(next);
                    if (st && st->env() == envInstr) {
                        consumeStVar(st);
                        it = bb->remove(it + 1);
                        it--;
                        continue;
                    }
                }

                if (next->hasEnv() && next->env() == envInstr)
                    break;

                bb->swapWithNext(it);
                it++;
            }

            auto moveMkEnvToDeoptBranch = [&](BB* deoptBranch,
                                              BB* fastPathBranch) {
                auto newEnvInstr = envInstr->clone();
                it = bb->insert(it, newEnvInstr);
                envInstr->replaceUsesIn(newEnvInstr, fastPathBranch);
                // Closure wrapper in MkEnv can be circular
                Replace::usesOfValue(newEnvInstr, envInstr, newEnvInstr);
                it = bb->moveToBegin(it, fastPathBranch);
                it = bb->moveToBegin(it, deoptBranch);
            };

            if (it != bb->end() && (it + 1) != bb->end()) {
                auto branch = (*(it + 1))->branches();
                if (envInstr && branch) {
                    Deopt* deopt;
                    if (!bb->falseBranch()->isEmpty() &&
                        (deopt = Deopt::Cast(bb->falseBranch()->last()))) {
                        moveMkEnvToDeoptBranch(bb->falseBranch(),
                                               bb->trueBranch());
                    } else if (!bb->trueBranch()->isEmpty() &&
                               (deopt =
                                    Deopt::Cast(bb->trueBranch()->last()))) {
                        moveMkEnvToDeoptBranch(bb->trueBranch(),
                                               bb->falseBranch());
                    }
                }
            }
        }
    });
}
} // namespace pir
} // namespace rir
